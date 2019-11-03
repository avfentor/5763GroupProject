
# setup -------------------------------------------------------------------
library(owmr)
library(leaflet)

# store API key in an environment variable called OWM_API_KEY
Sys.setenv(OWM_API_KEY = 'e366d11329936ebfaaf4cf08af0ff523')

# Access to weather API ---------------------------------------------------

owm_data = find_city('London, uk', units = 'metric') %>%
  owmr_as_tibble()
map = leaflet() %>%
  addTiles() %>%
  add_weather(owm_data,
              template = '<b>{{name}}</b>, {{temp}}°C',
              icon = owm_data$weather_icon)
map
