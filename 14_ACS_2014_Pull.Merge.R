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

acs_missing_ready <- acs_missing %>%
  select(GEOID, NAME, variable, estimate) %>%
  pivot_wider(
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(
    GEOID = as.numeric(GEOID),   # match state_merge format
    NAME,
    total_juris_pop   = B01003_001,
    total_latino_pop  = B03001_003,
    total_foreign_pop = B05002_013,
    state_year = 2014,
    state_code = GEOID
  )

## more missing 
# identify the missing GEOIDs and their corresponding states and geo types
# Check what GEOID values exist in each dataset
unique_287g_geoids <- unique(full_287g_census$GEOID)
unique_acs_geoids <- unique(state_acs$GEOID)

# See which ones don't overlap
non_overlapping_287g <- setdiff(unique_287g_geoids, unique_acs_geoids)
non_overlapping_acs <- setdiff(unique_acs_geoids, unique_287g_geoids)

print("GEOIDs only in 287g data:")
print(non_overlapping_287g)

print("GEOIDs only in ACS data:")
print(non_overlapping_acs)

missing_geoids <- full_287g_census %>%
  filter(GEOID %in% non_overlapping_287g) %>%
  distinct(state_code, Geo, GEOID) %>%
  group_by(state_code, Geo) %>%
  summarise(filter_codes = list(GEOID), .groups = "drop")

# Now pull ACS data for these missing jurisdictions
more_missing <- pmap_dfr(
  list(
    state = missing_geoids$state_code,
    geo = missing_geoids$Geo,
    codes = missing_geoids$filter_codes
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

non_overlapping_miss <- setdiff(non_overlapping_287g, more_missing$GEOID)

## doing it by hand ---> 
# write.csv(non_overlapping_miss, "missing_geos.csv")

# importing
# Combine with your existing ACS data
complete_acs_data <- bind_rows(state_acs, acs_data_missing)

# Now try the join again
final_287g_census <- full_287g_census %>%
  left_join(complete_acs_data, by = c("GEOID", "state_code"))


## combining with state_acs_wide
state_merge_wide <- state_merge_wide %>% mutate(
  total_juris_pop = total_juris_pop_2014, 
  total_latino_pop = total_latino_pop_2014,
  total_foreign_pop = total_foreign_pop_2014
) %>% select(GEOID, NAME, state_code,
             total_juris_pop, total_latino_pop,
             total_foreign_pop)

acs_miss <- acs_missing_ready %>% select(GEOID, NAME, state_code,
                                         total_juris_pop, total_latino_pop,
                                         total_foreign_pop)

state_acs <- state_merge_wide %>% bind_rows(acs_miss) %>% mutate(
  total_juris_pop_2014 = total_juris_pop, 
  total_latino_pop_2014 = total_latino_pop,
  total_foreign_pop_2014 = total_foreign_pop
) %>% select(GEOID, NAME, state_code,total_juris_pop_2014, total_latino_pop_2014, total_foreign_pop_2014)

state_acs <- state_acs %>%
  mutate(
    GEOID = case_when(
      GEOID %in% missing_geos$GEOID ~ missing_geos$CORRECT_GEOID[match(GEOID, missing_geos$GEOID)],
      TRUE ~ GEOID
    )
  )

## binding 
missing_geos <- read.csv("missing_geos.csv")
missing_geos <- missing_geos[,-1]
missing_geos <- missing_geos %>%
  mutate(
    GEOID = as.character(GEOID),
    GEOID = case_when(
      nchar(GEOID) <= 2  ~ str_pad(GEOID, 2,  pad = "0"),  # states
      nchar(GEOID) <= 5  ~ str_pad(GEOID, 5,  pad = "0"),  # counties
      nchar(GEOID) <= 7  ~ str_pad(GEOID, 7,  pad = "0"),  # places
      nchar(GEOID) <= 10 ~ str_pad(GEOID, 10, pad = "0"),  # county subdivisions
      TRUE ~ GEOID
    ),
    state_code = substr(GEOID, 1, 2)
  )

missing_geos <- missing_geos %>% mutate(
  total_juris_pop_2014 = total_juris_pop, 
  total_latino_pop_2014 = total_latino_pop,
  total_foreign_pop_2014 = total_foreign_pop,
  GEOID = as.numeric(GEOID),
  state_code = as.numeric(state_code)
) %>% select(GEOID,total_juris_pop_2014, total_latino_pop_2014, total_foreign_pop_2014, state_code)
# Remove temporary columns

state_full <- state_acs %>% bind_rows(missing_geos)


# 2014 active 


# names <- final_287g_census[final_287g_census$GEOID %in% missing_geos$GEOID,]
# 
# names <- names %>% select(GEOID, NAME)
# write.csv(names, "names.csv")

final_287g_census <- full_287g_census %>% left_join(state_full, by = "GEOID")

suspect_cases <- final_287g_census %>%
  filter(percent_latino_2010 == 100)

# Write them to CSV for manual inspection
write.csv(suspect_cases, "suspect_cases.csv", row.names = FALSE)

### Redoing the Calculations ----- 
clean_census <- read.csv("cleaned_census_data.csv")

clean_census_wide <- clean_census %>%
  select(GEOID, NAME, state_code, year,
         total_juris_pop = total_juris_pop,
         total_latino_pop = latino_juris_pop,
         total_foreign_pop = foreign_juris_pop) %>%
  pivot_wider(
    names_from = year,
    values_from = c(total_juris_pop, total_latino_pop, total_foreign_pop),
    names_sep = "_"
  )

census_287g <- full_287g_census %>% select(GEOID, NAME, state_code, Year_Signed, Last_Year,
                                           Type_287g)
full_census <- census_287g %>% left_join(clean_census_wide, by = "GEOID")

full_census <- full_census[!duplicated(full_census),]

final_census <- full_census %>% left_join(state_full, by = "GEOID")
final_census <- final_census[!duplicated(final_census),] %>% drop_na(GEOID)

### writing csv to manually input NAs (very few)

write.csv(final_census, "final_census.csv")

# reimporting with inputted data 

final_census_data <- read.csv("final_census.csv")

final_census_data <- final_census_data[,c(1:25)]

### creating the indicators 

merged_data_287gINDs <- final_census_data %>%                                   ### System --> Below 5% of latino/foreign pop --> -.10
  mutate(                                                                       ### System --> Between 5-10% of latino/foreign pop --> -.25
    # Define type multiplier: Warrant gets reduced penalty                      ### System --> Between 10-25% of latino/foreign pop --> -.5
    type_multiplier = case_when(                                                ### System --> Above 25% of latino/foreign pop --> -1
      Type_287g == "Warrant" ~ 0.95,                                            ### System --> If a "warrant type" multiplied by .25 
      TRUE ~ 1
    ),
    percent_latino_2010 = (total_latino_pop_2010/total_juris_pop_2010)*100,
    percent_foreign_2010 = (total_foreign_pop_2010/total_juris_pop_2010)*100,
    percent_latino_2014 = (total_latino_pop_2014/total_juris_pop_2014)*100,
    percent_foreign_2014 = (total_foreign_pop_2014/total_juris_pop_2014)*100,
    percent_latino_2016 = (total_latino_pop_2016/total_juris_pop_2016)*100,
    percent_foreign_2016 = (total_foreign_pop_2016/total_juris_pop_2016)*100,
    percent_latino_2020 = (total_latino_pop_2020/total_juris_pop_2020)*100,
    percent_foreign_2020 = (total_foreign_pop_2020/total_juris_pop_2020)*100,
    percent_latino_2023 = (total_latino_pop_2023/total_juris_pop_2023)*100,
    percent_foreign_2023 = (total_foreign_pop_2023/total_juris_pop_2023)*100,
    # 2010 active and exposure based on percent_latino_2020
    active_2010 = ifelse(Year_Signed <= 2008 & Last_Year >= 2012, 1, 0),
    base_exp_2010 = case_when(
      active_2010 == 1 & percent_latino_2010 >= 25 ~ -1,
      active_2010 == 1 & percent_latino_2010 >= 10 ~ -0.75,
      active_2010 == 1 & percent_latino_2010 >= 5  ~ -0.5,
      active_2010 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    exp_lat_2010 = ifelse(Type_287g == "Warrant", base_exp_2010 * .95, base_exp_2010),
    
    # Alternative exposure for 2010 based on percent_foreign_2010
    alt_base_exp_2010 = case_when(
      active_2010 == 1 & percent_foreign_2010 >= 25 ~ -1,
      active_2010 == 1 & percent_foreign_2010 >= 10 ~ -0.75,
      active_2010 == 1 & percent_foreign_2010 >= 5  ~ -0.5,
      active_2010 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    # exp_for_2010 = alt_base_exp_2010 * type_multiplier,
    exp_for_2010 = ifelse(Type_287g == "Warrant", alt_base_exp_2010 * .95, alt_base_exp_2010),
    
    ## 2012 
    active_2014 = ifelse(Year_Signed <= 2016 & Last_Year >= 2014, 1, 0),        ### 2014 - 2016 ~ can average out exposure 
    base_exp_2014 = case_when(
      active_2014 == 1 & percent_latino_2014 >= 25 ~ -1,
      active_2014 == 1 & percent_latino_2014 >= 10 ~ -0.75,
      active_2014 == 1 & percent_latino_2014 >= 5  ~ -0.5,
      active_2014 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    exp_lat_2014 = ifelse(Type_287g == "Warrant", base_exp_2014 * .95, base_exp_2014),
    alt_base_exp_2014 = case_when(
      active_2014 == 1 & percent_foreign_2014 >= 25 ~ -1,
      active_2014 == 1 & percent_foreign_2014 >= 10 ~ -0.75,
      active_2014 == 1 & percent_foreign_2014 >= 5  ~ -0.5,
      active_2014 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    exp_for_2014 = ifelse(Type_287g == "Warrant", alt_base_exp_2010 * .95, alt_base_exp_2014),
    # 2016 active and exposure based on percent_latino_2020
    active_2016 = ifelse(Year_Signed <= 2016 & Last_Year >= 2012, 1, 0),
    base_exp_2016 = case_when(
      active_2016 == 1 & percent_latino_2016 >= 25 ~ -1,
      active_2016 == 1 & percent_latino_2016 >= 10 ~ -0.75,
      active_2016 == 1 & percent_latino_2016 >= 5  ~ -0.5,
      active_2016 == 1 ~ -.25,
      TRUE ~ 0
    ),
    #exp_lat_2016 = base_exp_2016 * type_multiplier,
    exp_lat_2016 = ifelse(Type_287g == "Warrant", base_exp_2016 * .95, base_exp_2016),
    
    # Alternative exposure for 2016 based on percent_foreign_2016
    alt_base_exp_2016 = case_when(
      active_2016 == 1 & percent_foreign_2016 >= 25 ~ -1,
      active_2016 == 1 & percent_foreign_2016 >= 10 ~ -0.75,
      active_2016 == 1 & percent_foreign_2016 >= 5  ~ -0.5,
      active_2016 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    #exp_for_2016 = alt_base_exp_2016 * type_multiplier,
    exp_for_2016 = ifelse(Type_287g == "Warrant", alt_base_exp_2016 * .95, alt_base_exp_2016),
    # 2020 active and exposure based on percent_latino_2020
    active_2020 = ifelse(Year_Signed <= 2020 & Last_Year >= 2016, 1, 0),
    base_exp_2020 = case_when(
      active_2020 == 1 & percent_latino_2020 >= 25 ~ -1,
      active_2020 == 1 & percent_latino_2020 >= 10 ~ -0.75,
      active_2020 == 1 & percent_latino_2020 >= 5  ~ -0.5,
      active_2020 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    #exp_lat_2020 = base_exp_2020 * type_multiplier,
    exp_lat_2020 = ifelse(Type_287g == "Warrant", base_exp_2020 * .95, base_exp_2020),
    
    # Alternative exposure for 2020 based on percent_foreign_2020
    alt_base_exp_2020 = case_when(
      active_2020 == 1 & percent_foreign_2020 >= 25 ~ -1,
      active_2020 == 1 & percent_foreign_2020 >= 10 ~ -0.75,
      active_2020 == 1 & percent_foreign_2020 >= 5  ~ -0.5,
      active_2020 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    #exp_for_2020 = alt_base_exp_2020 * type_multiplier,
    exp_for_2020 = ifelse(Type_287g == "Warrant", alt_base_exp_2020 * .95, alt_base_exp_2020),
    # 2025 active and exposure based on percent_latino_2020
    active_2025 = ifelse(Year_Signed <= 2025 & Last_Year >= 2020, 1, 0),
    base_exp_2025 = case_when(
      active_2025 == 1 & percent_latino_2023 >= 25 ~ -1,
      active_2025 == 1 & percent_latino_2023 >= 10 ~ -0.75,
      active_2025 == 1 & percent_latino_2023 >= 5  ~ -0.5,
      active_2025 == 1 ~ -.25,
      TRUE ~ 0
    ),
    #exp_lat_2025 = base_exp_2025 * type_multiplier,
    exp_lat_2025 = ifelse(Type_287g == "Warrant", base_exp_2025 * .95, base_exp_2025),
    # Alternative exposure for 2025 based on percent_foreign_2023 (using latest available)
    alt_base_exp_2025 = case_when(
      active_2025 == 1 & percent_foreign_2023 >= 25 ~ -1,
      active_2025 == 1 & percent_foreign_2023 >= 10 ~ -0.75,
      active_2025 == 1 & percent_foreign_2023 >= 5  ~ -0.5,
      active_2025 == 1 ~ -0.25,
      TRUE ~ 0
    ),
    #exp_for_2025 = alt_base_exp_2025 * type_multiplier
    exp_for_2025 = ifelse(Type_287g == "Warrant", alt_base_exp_2025 * .95, alt_base_exp_2025),
  )

### full copy of data before indicator ---- 
write.csv(merged_data_287gINDs, "full_final_census.csv")

