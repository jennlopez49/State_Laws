######### Cleaned ################
library(dplyr)
library(readxl)
library(stringr)
library(tidycensus)

# Step 1: Read in ICE 287(g) agreements
hist287g <- read_xlsx("ICE_Agreements_Historical.xlsx") %>%
  mutate(Type_287g = str_trim(Type_287g)) %>%
  separate_rows(Type_287g, sep = ",\\s*") %>%
  rename(Source = Ext.Source, Other_Source = ...9)

# Step 2: Clean jurisdiction names (strip police dept etc. from municipalities)
municipal_patterns <- c("MPD$", "PD$", "Police Department$", "Dept$", "Department$", "Corrections$")
hist287g <- hist287g %>%
  mutate(
    Place = case_when(
      str_detect(Jurisdiction_name, str_c(municipal_patterns, collapse = "|")) ~ 
        str_remove(Jurisdiction_name, "\\s+(MPD|PD|Police Department|Dept\\.?|Department|Corrections)$") %>% str_trim(),
      TRUE ~ Jurisdiction_name
    )
  )

# Step 3: Normalize jurisdiction types
juris_needed <- hist287g %>%
  filter(!Type_Jurisdiction == "State") %>%
  mutate(
    Geo = case_when(
      Type_Jurisdiction %in% c("City", "Municipality") ~ "place",
      Type_Jurisdiction == "County" ~ "county",
      TRUE ~ tolower(Type_Jurisdiction)
    )
  ) %>%
  select(Place, State, Year_Signed, Last_Year, Geo, Type_Jurisdiction)

# Step 4: Add state FIPS codes
data("fips_codes")  # from tidycensus
state_codes <- fips_codes %>% select(state, state_code) %>% unique()

juris <- juris_needed %>%
  left_join(state_codes, by = c("State" = "state"))

# Step 5: Make sure GEOID column exists for each jurisdiction
#   (for now assume `Place` or `county` mapping already handled)
#   --> in your full pipeline you already matched counties/places to GEOIDs
#   --> so here we just assume `GEOID` is present in `full_287g_clean`

# Example: load your already-clean dataset instead of rebuilding geometry
full_287g_clean <- read.csv("full_hist_287g.csv")  

# Step 6: Create grouped_juris for ACS pulls
grouped_juris <- full_287g_clean %>%
  group_by(state_code, Geo) %>%
  summarise(filter_codes = list(GEOID), .groups = "drop")


### 2014 pull ------- 
library(dplyr)
library(purrr)

# Vector of ACS years we want to pull
acs_years_vec <- 2014  

# Default parameters for your get_acs_filtered() function
year_signed_default <- 2010
last_year_default <- 2023

# Pull ACS data for each state / geo / code combination
acs_data_2014 <- pmap_dfr(
  list(
    state = grouped_juris$state_code,
    geo = grouped_juris$Geo,
    codes = grouped_juris$filter_codes
  ),
  function(state, geo, codes) {
    if (geo == "place") {
      get_acs_filtered(
        state = state,
        geo = geo,
        place_codes = codes,
        year_signed = year_signed_default,
        last_year = last_year_default,
        acs_years = acs_years_vec
      )
    } else if (geo == "county") {
      get_acs_filtered(
        state = state,
        geo = geo,
        county_codes = codes,
        year_signed = year_signed_default,
        last_year = last_year_default,
        acs_years = acs_years_vec
      )
    } else {
      NULL
    }
  }
)

# Combine with existing state-level totals if needed
state_merge <- acs_data_2014 %>% 
  filter(year == 2014) %>%   # filter to 2014
  mutate(
    GEOID = as.numeric(GEOID),
    total_juris_pop = B01003_001E,
    total_latino_pop = B03001_003E,
    total_foreign_pop = B05002_013E,
    state_year = year
  )

state_merge_wide <- state_merge %>%
  pivot_wider(
    id_cols = c("GEOID", "NAME", "state_code"),
    names_from = year,
    values_from = c(total_juris_pop, total_latino_pop, total_foreign_pop),
    names_sep = "_"
  )

# creating the active in 2014 var &  file to merge it with - 2010, 2012, 2016, 2020 
full_287g_census <- read.csv("full_data_287g_census_inds.csv")

## getting the non-pulled juris 

missing_states <- setdiff(full_287g_census$state_code, state_merge_wide$state_code)

acs_missing <- get_acs(
  geography = "state",
  variables = c("B01003_001E", "B03001_003E", "B05002_013E") ,  # total population as an example
  state = missing_states,
  year = 2014,
  survey = "acs5"
)



# 2014 active 

final_287g_census <- full_287g_census %>% left_join(state_merge_wide, by = c("GEOID", "NAME", "state_code"))

