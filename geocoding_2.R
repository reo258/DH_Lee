### Googles API
creds <- read_json("creds.json") 
register_google(key = creds$secret)

# Geocoding
location <- unique(text$loca)
location <- location[!is.na(location) & location != ""]
location <- gsub("[^\x01-\x7F]", "", location)
location <- iconv(location, to = "UTF-8")

loca_code1 <- geocode(location[1:1000]) 
loca_code2 <- geocode(location[1001:2000]) 
loca_code3 <- geocode(location[2001:3000]) 
loca_code4 <- geocode(location[3001:4000]) 
loca_code5 <- geocode(location[4001:4613]) 