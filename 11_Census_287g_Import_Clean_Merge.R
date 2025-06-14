#### Adding Pop Data For Each 287(g) Jurisdiction ==============================
hist287g <- read_xlsx("ICE_Agreements_Historical.xlsx")

hist287g  <- hist287g %>%
  mutate(Type_287g = str_trim(Type_287g)) %>%
  separate_rows(Type_287g, sep = ",\\s*")

hist287g <- hist287g %>% 
  rename(Source = Ext.Source,
         Other_Source = ...9)
## extracting only place name 
municipal_patterns <- c(
  "MPD$", "PD$", "Police Department$", "Dept$", "Department$", "Corrections$"
)

# Apply the mutation to add the 'Place' column
hist287g <- hist287g %>%
  mutate(
    Place = case_when(
      str_detect(Jurisdiction_name, str_c(municipal_patterns, collapse = "|")) ~ 
        str_remove(Jurisdiction_name, "\\s+(MPD|PD|Police Department|Dept\\.?|Department|Corrections)$") %>% str_trim(),
      TRUE ~ Jurisdiction_name  # leave counties and state agencies as-is
    )
  )

# ## Restricting to Municipalites & Counties (State agenices separate) ===========
# 
hist287g$Last_Year <- as.numeric(hist287g$Last_Year)
# 
# # List of Years for the 5-Year ACS
# 
# years_needed <- sort(unique(na.omit(c(hist287g$Year_Signed, hist287g$Last_Year))))
# 
# # List of Counties
# counties_needed <- hist287g %>%
#   filter(Type_Jurisdiction == "County") %>%
#   select(Jurisdiction_name, Type_Jurisdiction, State, year = Year_Signed) %>%
#   bind_rows(
#     hist287g %>%
#       filter(Type_Jurisdiction == "County") %>%
#       select(Jurisdiction_name, Type_Jurisdiction, State, year = Last_Year)
#   ) %>%
#   distinct() %>%
#   drop_na()
# 
# # List of Municipalities
# places_needed <- hist287g %>%
#   filter(Type_Jurisdiction == "Municipality") %>% 
#   select(Place, State, year = Year_Signed) %>%
#   bind_rows(hist287g %>% 
#               filter(Type_Jurisdiction == "Municipality")%>% 
#               select(Place, State, year = Last_Year)) %>%
#   distinct() %>%
#   drop_na()
# 
# # list of State Agencies (Pulling State-Wide Pop Data)
# states_needed <- hist287g %>%
#   filter(Type_Jurisdiction == "State") %>% 
#   select(Jurisdiction_name, State, year = Year_Signed) %>%
#   bind_rows(hist287g %>% 
#               filter(Type_Jurisdiction == "State")%>% 
#               select(Jurisdiction_name, State, year = Last_Year)) %>%
#   distinct() %>%
#   drop_na()

### Pulling Census Data (Total Pop, Total Latino Pop, Total Foreign Born) ######


# View all ACS 5-year variables for 2020 --- to see what's available
vars_2020 <- load_variables(2020, "acs5", cache = TRUE)

juris_needed <- hist287g %>% filter(!Type_Jurisdiction == "State") %>% 
  select(Place, Year_Signed, Last_Year, State, Type_Jurisdiction)

available_acs_years <- c(2016, 2020, 2023)


juris_needed <- juris_needed %>% mutate(
  Type_Jurisdiction = ifelse(Type_Jurisdiction == "City", "Municipality", 
                             ifelse(Type_Jurisdiction == "Municipality", "Place", 
                                    ifelse(Type_Jurisdiction == "Louisiana Alcohol and Tobacco Control", "State", Type_Jurisdiction))),
  Geo = tolower(Type_Jurisdiction)
)

# juris_needed <- juris_needed %>%
#   rowwise() %>%
#   mutate(
#     acs_year_signed = max(available_acs_years[available_acs_years <= Year_Signed], na.rm = TRUE),
#     acs_year_ended = max(available_acs_years[available_acs_years <= Last_Year], na.rm = TRUE),
#     acs_year_signed = ifelse(is.infinite(acs_year_signed), NA, acs_year_signed),
#     acs_year_ended = ifelse(is.infinite(acs_year_ended), NA, acs_year_ended)
#   ) %>%
#   ungroup()




### Pulling County FIP Codes & Matching to Current Data ========================

# Load county FIPS lookup table
data("fips_codes")  # comes with tidycensus

juris_needed <- juris_needed %>% mutate(
  Place = ifelse(Geo == "county", paste(Place, "County"), Place)
)

juris_with_fips <- juris_needed %>%
  left_join(fips_codes, by = c("State" = "state", "Place" = "county"))

# Get places for all states (or just one if you prefer)
places_all <- places(cb = TRUE, year = 2023) %>%
  transmute(
    state_code = STATEFP,
    place_code = PLACEFP,
    name = NAME,
    GEOID = GEOID
  )

state_codes <- fips_codes %>% select(state, state_code) %>% unique()

juris <- juris_needed %>% left_join(state_codes, by = c("State" = "state"))

places_only <- juris %>% filter(Geo == "place") %>% 
  mutate(place_clean = tolower(gsub("\\s+(city|town|village|CDP).*", "", Place)))

places_all <- places_all %>%
  mutate(name_clean = tolower(gsub("\\s+(city|town|village|CDP).*", "", name)))

juris_places <- places_only %>% left_join(places_all, by = c("state_code", "place_clean" = "name_clean"))

## Pulling NAs to fill in manually ---> 

place_NAs <- juris_places %>% filter(is.na(place_code) | is.na(GEOID))
write.csv(place_NAs, "place_NAs.csv")

### loading filled in NAs --->
place_codes_rest <- read.csv("place_NAs.csv", encoding = "UTF-8") %>% 
  mutate(
    Place_Name = as.character(name),
  )
place_codes_rest$geometry <- NA

### Filling in Geometry ----> 
get_place_geo <- function(state_fips, place_code) {
  tryCatch({
    pl <- tigris::places(state = state_fips, year = 2020)
    pl %>% filter(PLACEFP == place_code)
  }, error = function(e) NULL)
}

get_sub_geo <- function(state_fips, place_code) {
  tryCatch({
    county_subs <- tigris::county_subdivisions(state = state_fips, year = 2020)
    county_subs %>% filter(GEOID == paste0(state_fips, place_code))
  }, error = function(e) NULL)
}

# Fetch geometries row by row
geometry_fills <- pmap_dfr(
  place_codes_rest,
  function(state_code, place_code, Type_Jurisdiction, ...) {
    if (is.na(place_code)) return(NULL)
    
    place_code_str <- stringr::str_pad(place_code, width = 5, side = "left", pad = "0")
    state_code_str <- stringr::str_pad(state_code, width = 2, side = "left", pad = "0")
    
    if (tolower(Type_Jurisdiction) == "place") {
      get_place_geo(state_code_str, place_code_str)
    } else {
      get_sub_geo(state_code_str, place_code_str)
    }
  }
)

places_add <- place_codes_rest %>% select(-c(geometry, Place_Name, place_clean))

geometry_add <- geometry_fills %>% select(PLACEFP, geometry, NAME) %>% mutate(
  place_code = as.numeric(PLACEFP)
)

places_last <- places_add %>% left_join(geometry_add, by = c("place_code" = "place_code"))

places_last <- places_last %>% filter(!is.na(PLACEFP)) %>% distinct(Place, .keep_all = TRUE)


###
places_no_nas <- juris_places %>% filter(!is.na(place_code)) 
common_cols <- intersect(names(places_no_nas), names(places_last))
places_no_nas$state_code <- as.numeric(places_no_nas$state_code)
places_no_nas$place_code <- as.numeric(places_no_nas$place_code)
places_no_nas$GEOID <- as.numeric(places_no_nas$GEOID)

places_combined <- bind_rows(
  places_no_nas %>% select(all_of(common_cols)),
  places_last %>% select(all_of(common_cols))
)


juris_with_fips <- juris_with_fips %>% filter(!is.na(state_code))

get_county_geo <- function(state_fips, county_fips) {
  tryCatch({
    counties <- tigris::counties(state = state_fips, year = 2020)
    target_geoid <- paste0(state_fips, county_fips)
    
    match <- counties %>% dplyr::filter(GEOID == target_geoid)
    
    if (nrow(match) == 0) {
      message(paste("No match for county GEOID:", target_geoid))
      return(NULL)
    }
    
    match
  }, error = function(e) {
    message(paste("get_county_geo failed:", e$message))
    return(NULL)
  })
}

juris_geo_fills <- pmap_dfr(
  .l = juris_with_fips,
  .f = function(state_code, county_code, Type_Jurisdiction, ...) {
    if (is.na(county_code)) return(NULL)
    
    county_code_str <- stringr::str_pad(county_code, 3, side = "left", pad = "0")
    state_code_str <- stringr::str_pad(state_code, 2, side = "left", pad = "0")
    
    if (tolower(Type_Jurisdiction) == "county") {
      get_county_geo(state_code_str, county_code_str)
    } else {
      NULL  # you can plug in your get_place_geo or get_sub_geo here
    }
  }
)

## keeping only original columns + polygon info

common_cols <- intersect(names(juris_with_fips), names(places_combined))

common_cols <- append(common_cols, "geometry")

juris_county <- juris_geo_fills %>% select(STATEFP, COUNTYFP, GEOID, NAME, geometry)

juris_nondup <- juris_with_fips %>% distinct(Place, Year_Signed, .keep_all = TRUE)
juris_county_full <- juris_nondup %>% left_join(juris_county, 
                                                   by = c("county_code" = "COUNTYFP",
                                                          "state_code" = "STATEFP"))

juris_county_full <- juris_county_full %>% distinct(GEOID, Year_Signed, .keep_all = TRUE)

## Full List ------- 
common_cols <- intersect(names(juris_county_full), names(places_combined))
places_combined <- places_combined %>%
  mutate(state_code = str_pad(as.character(state_code), width = 2, side = "left", pad = "0"),
         GEOID = str_pad(as.character(GEOID), width = 7, side = "left", pad = "0"))

fulL_hist_287g <- bind_rows(
  juris_county_full %>% select(all_of(common_cols)),
  places_combined %>% select(all_of(common_cols))
)


# Convert geometry to WKT string
fulL_hist_287g$geometry <- st_as_text(fulL_hist_287g$geometry)

# Save as CSV
write.csv(fulL_hist_287g, "full_hist_287g.csv", row.names = FALSE)




#### pull state data

states.only <- hist287g %>% filter(Type_Jurisdiction == "State") %>% 
  left_join(state_codes, by = c("State" = "state"))

state_fips_codes <- states.only$state_code

states_geom <- states(year = 2020) %>%
  filter(STATEFP %in% state_fips_codes) 

state_geom <- states_geom %>% select(STATEFP, geometry) %>% mutate(
  state_code = STATEFP
)

states.only <- states.only %>% left_join(state_geom, 
                                         by = c("state_code" = "state_code"))


acs_vars <- c("B01003_001", "B03001_003", "B05002_013")  # example variables

state_acs_data <- map_dfr(state_fips_codes, function(fips) {
  get_acs_filtered(
    state = fips,
    geo = "state",
    vars = acs_vars,
    year_signed = year_signed_default,
    last_year = last_year_default,
    acs_years = acs_years_vec
  )
})


#### Two versions of variables pulled - VAR Name_E & VAR Name_M ---> E is Estimate & M is Margin of Error # 

full_census_data <- rbind(acs_data_all, state_acs_data)

## adding state + geometry 
states.only <- states.only %>% mutate(
  Place = Jurisdiction_name
)

## adding type of 287(g) back to 
type287g <- hist287g %>% 
  mutate(Place = ifelse(Type_Jurisdiction == "County", paste0(Place, " County"), Place)) %>% 
  select(Place, Year_Signed, Last_Year, Type_287g, State)

updated_fl <- read_xlsx("ICE_Agreements_Historical.xlsx")
updated_fl <- updated_fl %>% 
  mutate(Place = ifelse(Type_Jurisdiction == "County", paste0(Jurisdiction_name, " County"), Jurisdiction_name))

### Using this instead to bring back type of 287g 
newhist <- updated_fl %>% select(Place, Year_Signed, Last_Year, State, Type_Jurisdiction, Type_287g) %>% 
  mutate(Geo = tolower(Type_Jurisdiction),
         Last_Year = as.numeric(Last_Year), 
         Geo = ifelse(Geo == "city", "place",
                      ifelse(Geo == "municipality", "place",
                             Geo)),
         Type_Jurisdiction = ifelse(Type_Jurisdiction == "City", "Place",
                      ifelse(Type_Jurisdiction == "Municipality", "Place",
                             Type_Jurisdiction)))

# municipal_patterns <- c(
#   "MPD$", "PD$", "Police Department$", "Dept$", "Department$", "Corrections$"
# )

# Apply the mutation to add the 'Place' column
newhist <- newhist %>%
  mutate(
    Place = case_when(
      str_detect(Place, str_c(municipal_patterns, collapse = "|")) ~ 
        str_remove(Place, "\\s+(MPD|PD|Police Department|Dept\\.?|Department|Corrections)$") %>% str_trim(),
      TRUE ~ Place  # leave counties and state agencies as-is
    )
  )


newhist.c  <- newhist %>%
  mutate(Type_287g = str_trim(Type_287g)) %>%
  separate_rows(Type_287g, sep = ",\\s*")

newhist.nostate <- newhist.c %>% filter(!Geo == "state")

newhist.statecodes <- newhist.nostate %>% left_join(state_codes, by = c("State" = "state"))


fulL_hist_287g.clean <- fulL_hist_287g %>% full_join(newhist.statecodes,
                                                     by = c("State" = "State",
                                                            "Place" = "Place",
                                                            "Year_Signed" = "Year_Signed",
                                                            "Geo" = "Geo",
                                                            "Type_Jurisdiction" = "Type_Jurisdiction",
                                                            "state_code" = "state_code"))
### Should be 623 without state-level agencies --- matches 

fulL_hist_287g.nodups<- fulL_hist_287g.clean %>%
  filter(!duplicated(select(., Place, State, Year_Signed, Type_287g))) %>% mutate(
    Last_Year = Last_Year.x
  ) %>% select(!c(Last_Year.x, Last_Year.y))

### state-level agencies only ----- 

newhist.states <- newhist.c %>% left_join(state_codes, 
                                          by = c("State" = "state")) %>% 
  filter(Geo == "state")

### Adding back in to the state level data ---- 

### GEOID for states --> FIPS codes (state_code in data), for consistency adding separate column
names(states.only)

states.only <- states.only %>% mutate(
  GEOID = state_code,
  Place = Jurisdiction_name,
  Geo = Type_Jurisdiction
)

states.data.287g <- states.only %>% select(Place, Year_Signed, State, 
                                           Type_Jurisdiction, Geo, state_code,
                                           GEOID, geometry, Type_287g, Last_Year)
                                           
full_287g_data <- rbind(fulL_hist_287g.nodups, states.data.287g)

## saving full cleaned data (no census yet) ----------------------------------->

## as a shapefile 
library(sf)

# Reconstruct geometry from WKT
final_hist_287g <- full_287g_data %>%
  mutate(geometry = st_as_sfc(geometry)) %>%
  st_as_sf()
# checking 


sum(is.na(full_287g_data$geometry))
any(st_is_empty(st_as_sfc(full_287g_data$geometry)))

### NAs in geometry --> 
nas_geometry <- full_287g_data %>% select(GEOID, Place, State, geometry) %>% filter(is.na(geometry))
nas_geometry$geometry <- NA
#write.csv(nas_geometry, "nas_geometry.csv")

### Inserting Filled Out GEo ----- >> ################ WIP RIGHT HERE -------->>>>>>>

filled_geo <- read.csv("nas_geometry.csv")
filled_geo$GEOID <- as.character(filled_geo$GEOID)
filled_geo <- filled_geo %>%
  mutate(GEOID = gsub('\"', '', GEOID))
filled_geo <- filled_geo %>% left_join(state_codes, by = c("State" = "state"))

### ERRORS ----------------------------------------------------------------------> COME BACK TO THIS TO CREATE MAPS WITH ACC SPOTS 

# geo_fills <- pmap_dfr(
#   .l = filled_geo,
#   .f = function(state_code, GEOID, ...) {
#     if (is.na(GEOID)) {
#       return(tibble(GEOID = NA_character_, geometry = NA))
#     }
#     
#     state_code_str <- str_pad(state_code, 2, pad = "0")
#     GEOID_str <- str_pad(as.character(GEOID), width = nchar(as.character(GEOID)), pad = "0")
#     
#     result <- NULL
#     
#     if (nchar(GEOID_str) == 5) {
#       county_code_str <- str_sub(GEOID_str, 3, 5)
#       result <- tryCatch(
#         get_county_geo(state_code_str, county_code_str),
#         error = function(e) NULL
#       )
#     } else if (nchar(GEOID_str) == 7) {
#       # full GEOID_str is the place code
#       place_code_str <- str_sub(GEOID_str, 2, 7)  # optional: just use GEOID_str if already clean
#       result <- tryCatch(
#         get_place_geo(state_code_str, place_code_str),
#         error = function(e) NULL
#       )
#     } else if (nchar(GEOID_str) == 10) {
#       sub_code <- str_sub(GEOID_str, 6, 10)
#       result <- tryCatch(
#         get_sub_geo(state_code_str, sub_code),
#         error = function(e) NULL
#       )
#     }
#     
#     if (is.null(result)) {
#       return(tibble(GEOID = GEOID_str, geometry = NA))
#     }
#     
#     return(result)
#   }
# )


### This county -> was present in march 2025 ICE docs, gone by May 2025 --> KEPT as NAs 
full_287g_data$Year_Signed <- ifelse(full_287g_data$Place == "Crow Wing County" & full_287g_data$State == "MA", NA_real_, 
                                     full_287g_data$Year_Signed)

### Filling in GEOIDs --->
full_287g_data <- full_287g_data %>% left_join(filled_geo, by = c("Place",
                                                                  "State")) 
full_287g_data <- full_287g_data %>%  mutate(GEOID = coalesce(GEOID.x, GEOID.y)) %>%
  select(-GEOID.x, -GEOID.y)

full_287g_clean <- full_287g_data %>% select(-c(geometry.y, X, state_code.y)) %>%
  mutate(geometry = geometry.x,
         state_code = state_code.x)


any(st_is_empty(final_hist_287g))
any(!st_is_valid(final_hist_287g))

st_write(final_hist_287g, "cleaned_287g_data.shp")


## Adding Census -------------------------------------------------------------->
###### Subsetting & grouping by county / place ----
grouped_juris <- full_287g_clean %>%
  group_by(state_code, Geo) %>%
  summarise(filter_codes = list(GEOID), .groups = "drop")

###### Pulling the data --------------------------------------------------------
get_acs_filtered <- function(state, geo, place_codes = NULL, county_codes = NULL,
                             year_signed, last_year,
                             vars = c("B01003_001", "B03001_003", "B05002_013"),
                             acs_years = c(2010, 2016, 2020, 2023)) {
  
  valid_years <- acs_years[acs_years >= year_signed & acs_years <= last_year]
  
  if (length(valid_years) == 0) {
    message(glue::glue("⏩ Skipping {geo}, {state} – no ACS years fall within {year_signed}–{last_year}"))
    return(NULL)
  }
  
  acs_list <- list()
  
  for (yr in valid_years) {
    tryCatch({
      df <- get_acs(
        geography = geo,
        variables = vars,
        year = yr,
        state = state,
        survey = "acs5",
        output = "wide"
      ) %>%
        mutate(geo = geo, state_code = state, year = yr) %>%
        mutate(GEOID = as.character(GEOID))
      
      if (geo == "place" & !is.null(place_codes)) {
        # place_codes should be character vector of full GEOIDs (state + place code)
        place_codes <- as.character(place_codes)
        df_filtered <- df %>% filter(GEOID %in% place_codes)
        acs_list[[as.character(yr)]] <- df_filtered
        
      } else if (geo == "county" & !is.null(county_codes)) {
        # county_codes should be character vector of full GEOIDs (state + county code)
        county_codes <- as.character(county_codes)
        df_filtered <- df %>% filter(GEOID %in% county_codes)
        acs_list[[as.character(yr)]] <- df_filtered
        
      } else if (geo == "state") {
        # for state geo, usually just one row
        acs_list[[as.character(yr)]] <- df
      } else {
        message(glue::glue("⚠️ No matching codes provided for {geo}"))
        acs_list[[as.character(yr)]] <- NULL
      }
    }, error = function(e) {
      message(glue::glue("⚠️ Failed: {geo}, {state}, {yr} – {e$message}"))
      acs_list[[as.character(yr)]] <- NULL
    })
  }
  
  combined_df <- bind_rows(acs_list)
  return(combined_df)
}


# grouped_juris tibble with columns: state_code, Geo, filter_codes (list of GEOIDs)

acs_years_vec <- c(2010, 2016, 2020, 2023)

year_signed_default <- 2010
last_year_default <- 2023

# Run the batch pull:
acs_data_all <- pmap_dfr(
  list(
    state = grouped_juris$state_code,
    geo = grouped_juris$Geo,
    codes = grouped_juris$filter_codes
  ),
  function(state, geo, codes) {
    # Call your function, passing the codes either as place_codes or county_codes depending on geo
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

full_census_data <- rbind(acs_data_all, state_acs_data)

names(full_census_data)

## making sure that each GEOID also has state total popultations -------------->

summary(state_acs_data)

state_merge <- state_acs_data %>% select(GEOID, B01003_001E, B03001_003E,
                                         B05002_013E,
                                         year)

state_merge <- state_merge %>% mutate(
  total_state_pop = B01003_001E,
  total_latino_pop = B03001_003E,
  total_foreign_pop = B05002_013E,
  state_year = year
) %>% select(GEOID, total_state_pop, total_latino_pop, total_foreign_pop, state_year)


#### Some States do not have state-level agreements ---> pulling ACS data for those here --> 

full_state_list <- unique(full_census_data$state_code)

missing_states <- full_state_list[!full_state_list %in% state_merge$GEOID]


missingstate_acs_data <- map_dfr(missing_states, function(fips) {
  get_acs_filtered(
    state = fips,
    geo = "state",
    vars = acs_vars,
    year_signed = year_signed_default,
    last_year = last_year_default,
    acs_years = acs_years_vec
  )
})

miss_merge <- missingstate_acs_data %>% select(GEOID, B01003_001E, B03001_003E,
                                         B05002_013E,
                                         year)

miss_merge <- miss_merge %>% mutate(
  total_state_pop = B01003_001E,
  total_latino_pop = B03001_003E,
  total_foreign_pop = B05002_013E,
  state_year = year
) %>% select(GEOID, total_state_pop, total_latino_pop, total_foreign_pop, state_year)

## combining full state totals 

state_totals <- rbind(state_merge, miss_merge)

f_census_data <- full_census_data %>% left_join(state_totals, 
                                                by = c("state_code" = "GEOID",
                                                       "year" = "state_year"))

count_by_place_year <- f_census_data %>%
  group_by(NAME, year) %>%
  summarise(n_rows = n(), .groups = "drop")

clean_census_data <- f_census_data %>% filter(!duplicated(.))

## to re-check -->

count_by_place_year <- clean_census_data %>%
  group_by(NAME, year) %>%
  summarise(n_rows = n(), .groups = "drop")

### Keeping Only Relevant Columns (& Naming Them)

clean_census <- clean_census_data %>% select(-c(B01003_001M, 
                                                B03001_003M, 
                                                B05002_013M)) %>%
  mutate(
    total_juris_pop = B01003_001E,
    latino_juris_pop = B03001_003E,
    foreign_juris_pop = B05002_013E,
    percent_latino = (latino_juris_pop/total_latino_pop)*100,
    percent_foreign = (foreign_juris_pop/total_foreign_pop)*100
  )
clean_census <- clean_census %>% select(-c(B01003_001E, B03001_003E, B05002_013E))

#### Saving Census Data ------------------------------------------------------>
write.csv(clean_census, "cleaned_census_data.csv")


### Trying to Merge Using Fuzzy Join -- didn't work, missing values

# final_287g_data <- fuzzy_left_join(
#   fulL_287g_data,
#   clean_census,
#   by = c("GEOID" = "GEOID", "Year_Signed" = "year",  "Last_Year" = "year"),
#   match_fun = list(`==`, `>=`, `<=`)
# )

census_wide <- clean_census %>%
  pivot_wider(id_cols = c("GEOID", "NAME"),
    names_from = year,
              values_from = c(total_juris_pop, percent_latino, percent_foreign),
              names_sep = "_")
names(full_287g_clean)
names(census_wide)

clean_287g <- full_287g_clean %>% select(-c(geometry.x, state_code.x, Type_Jurisdiction))

merged_287g_data <- clean_287g %>%
  left_join(census_wide, by = "GEOID")

#
merged_data_287gINDs <- merged_287g_data %>%                                    ### System --> Below 5% of latino/foreign pop --> -.10
  mutate(                                                                       ### System --> Between 5-10% of latino/foreign pop --> -.25
    # Define type multiplier: Warrant gets reduced penalty                      ### System --> Between 10-25% of latino/foreign pop --> -.5
    type_multiplier = case_when(                                                ### System --> Above 25% of latino/foreign pop --> -1
      Type_287g == "Warrant" ~ 0.95,                                            ### System --> If a "warrant type" multiplied by .25 
      TRUE ~ 1
    ),
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

full_287g_census <- merged_data_287gINDs %>% select(-c(geometry))               ## Version WITHOUT geometry data

write.csv(full_287g_census, "full_data_287g_census_inds.csv")                   ## Saving ---> 

###### Summarizing across states to add to climate index -----------------------



state_exp_summary <- full_287g_census %>%
  group_by(State) %>%
  summarise(
    total_exp_lat_2010 = sum(exp_lat_2010, na.rm = TRUE),
    total_exp_lat_2016 = sum(exp_lat_2016, na.rm = TRUE),
    total_exp_lat_2020 = sum(exp_lat_2020, na.rm = TRUE),
    total_exp_lat_2025 = sum(exp_lat_2025, na.rm = TRUE),
    total_exp_for_2010 = sum(exp_for_2010, na.rm = TRUE),
    total_exp_for_2016 = sum(exp_for_2016, na.rm = TRUE),
    total_exp_for_2020 = sum(exp_lat_2020, na.rm = TRUE),
    total_exp_for_2025 = sum(exp_lat_2025, na.rm = TRUE)
  )

write.csv(state_exp_summary, "states_287g_sums.csv")


