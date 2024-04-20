# Google API
creds <- read_json("creds.json") 
register_google(key = creds$secret)

# Cleaning locations
location <- unique(text$loca)
location <- location[!is.na(location) & location != ""]
location <- gsub("[^\x01-\x7F]", "", location)
location <- iconv(location, to = "UTF-8")

# Geocoding
loca_code <- geocode(location, output = "all")

# Fetching state information
state <- data.frame(k_id = 1:length(location))
state$address <- location
state$state <- NA
  
for (i in 1:length(location)) {
  results_available <- tryCatch({
    !is.null(loca_code[[i]]$results) && length(loca_code[[i]]$results) >= 1 &&
      !is.null(loca_code[[i]]$results[[1]]$address_components)
  }, error = function(e) FALSE)  
  
  if (results_available) {
    short_name3_available <- tryCatch({
      length(loca_code[[i]]$results[[1]]$address_components) >= 3 &&
        !is.null(loca_code[[i]]$results[[1]]$address_components[[3]]$short_name) &&
        nchar(loca_code[[i]]$results[[1]]$address_components[[3]]$short_name) > 0
    }, error = function(e) FALSE)
    
    short_name1_available <- tryCatch({
      length(loca_code[[i]]$results[[1]]$address_components) >= 1 &&
        !is.null(loca_code[[i]]$results[[1]]$address_components[[1]]$short_name) &&
        nchar(loca_code[[i]]$results[[1]]$address_components[[1]]$short_name) > 0
    }, error = function(e) FALSE)
    
    if (short_name3_available) {
      state$state[i] <- loca_code[[i]]$results[[1]]$address_components[[3]]$short_name
    } else if (short_name1_available) {
      state$state[i] <- loca_code[[i]]$results[[1]]$address_components[[1]]$short_name
    }
  }
} # These lines were partially made by GPT


# Editing misclassifications
missc <- c("7", "AB", "AE", "AU", "AW", "BB", "BC", "BE", "BG", "BR", "CH", "CN", 
           "CR", "CV", "CZ", "DA", "ES", "ET", "FI", "FR", "FS", "GB", "GR", "GU",
           "HR", "HT", "IQ", "IT", "JE", "JP", "KP", "LU", "LV", "MX", "NB", "NG",
           "NL", "NO", "NP", "NS", "NZ", "ON", "PF", "PH", "PT", "RJ", "RM", "RP",
           "RU", "SA", "SE", "SI", "SK", "TR", "TS", "TW", "UA", "UP", "US", "ZA",
           "ZH", "MO")
state$state <- ifelse(state$state %in% missc, NA, state$state)

state <- state %>%
  mutate(state = recode(state,
                        "5th Ave" = "NY",
                        "Ada County" = "WY",
                        "Adirondack Mountains" = "NY",
                        "Airport Cir" = "FL",
                        "Albany County" = "NY",
                        "Alcona County" = "MI",
                        "Allegheny County" = "PA",
                        "Allen County" = "IN",
                        "Atsion Rd" = "NJ",
                        "Auglaize County" = "OH",
                        "Austin" = "TX",
                        "Barnstable County" = "MA",
                        "Baton Rouge" = "LA",
                        "Bay County" = "MI",
                        "Bay Minette" = "AL",
                        "Berks County" = "PA",
                        "Berkshire County" ="MA",
                        "Bexar County" = "TX",
                        "Blair County" = "PA",
                        "Blue Mountain Rd" = "FL",
                        "Bond County" = "IL",
                        "Boone County" = "IN",
                        "Brevard County" = "FL",
                        "Broadway St" = "WL",
                        "Brockton Ave" = "CA",
                        "Bronx County" = "NY",
                        "Bucks County" = "PA",
                        "Cape Girardeau County" = "MO",
                        "Cedar City" = "UT",
                        "Cole County" = "MO",
                        "Dallas County" = "TX",
                        "Fairfax County" = "VA",
                        "GE" = "GA",
                        "Gloucester County" = "NJ",
                        "Great Smoky Mountains" = "NC",
                        "Hammond" = "LA",
                        "Harris County" = "TX",
                        "Ingham County" = "MI",
                        "Kendall County" = "IL",
                        "Lancaster County" = "PA",
                        "Lorain County" = "OH",
                        "Los Angeles County" = "CA",
                        "Manhattan" = "NY",
                        "McDonald County" = "MO",
                        "McHenry County" = "IL",
                        "Medina County" = "OH",
                        "Nassau County" = "NY",
                        "New York" = "NY",
                        "New York County" = "NY",
                        "Northwest Washington" = "DC",
                        "Oakland County" = "MI",
                        "Olympia" = "WA",
                        "Ozark Mountains" = "MO",
                        "Palm Beach" = "FL",
                        "Philadelphia" = "PA",
                        "Philadelphia County" = "PA",
                        "Plymouth County" = "MA",
                        "Queens County" = "NY",
                        "Ross County" = "OH",
                        "San Diego County" = "CA",
                        "Southside Historic District" = "TN",
                        "Sugarloaf Township" = "PA",
                        "Tioga County" = "PA",
                        "Tippecanoe County" = "IN",
                        "Vanderburgh County" = "IN",
                        "Washington" = "DC",
                        "Wayne County" = "IN",
                        "West Pasco Industrial Park" = "FL"))

state$state <- ifelse(nchar(state$state) >= 3, NA, state$state)

# Merging
text$state <- NA
text$state <- ifelse(text$loca %in% state$address, state$state[match(text$loca, state$address)], NA)
text <- text %>% 
  select(id, time, state, loca, word, name, lang, follower, everything())

df$state <- NA
df$state <- ifelse(df$loca %in% state$address, state$state[match(df$loca, state$address)], NA)
df <- df %>% 
  select(id, time, state, loca, truth, name, lang, follower, everything())

# Additional location classification (modification continuing)
text <- text %>%
  mutate(state = case_when(
    loca == "Upstate New York" ~ "NY",
    TRUE ~ state  
  ))

df <- df %>%
  mutate(state = case_when(
    loca == "Upstate New York" ~ "NY",
    TRUE ~ state  
  ))

# Political party by states (2020)
dem <-c("AZ", "CA", "CO", "CT", "DE", "GA", "HI", "IL", "MD", "MA", "MI", "NV",
        "NH", "NJ", "NM", "NY", "OR", "PA", "RI", "VT", "VA", "WA", "ME", "MN", "DC")
rep <-c("AK", "AL", "AR", "FL", "ID", "IN", "IA", "KS", "KY", "LA", "MS", "MO",
        "MT", "NE", "NC", "ND", "OH", "OK", "SC", "SD", "TN", "TX", "UT", "WV", "WY")
text$party <- NA
text <- text %>%
  select(id, time, state, loca, party, everything()) %>% 
  mutate(party = case_when(
    state %in% dem ~ "DM",
    state %in% rep ~ "RP",
    TRUE ~ party
  ))

df$party <- NA
df <- df %>%
  select(id, time, state, loca, party, everything()) %>% 
  mutate(party = case_when(
    state %in% dem ~ "DM",
    state %in% rep ~ "RP",
    TRUE ~ party
  ))


                        