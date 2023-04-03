library(tidyverse)
library(readr)
library(USAboundaries)
library(sf)
library(leaflet)
library(RColorBrewer)
library(leaflet.extras2)
library(geojsonsf)
library(lubridate)

# upload data
wwp <- read_csv('wwp_data.csv')

# remove data that does not include locations
location_data <- wwp[!is.na(wwp$Places),]

# rename columns
colnames(location_data) <- c("id", "document_type","parent_id", "parent_name", "uuid", "page",
                             "website_url", "short_url", "image_url", "original_transcript",
                             "text_transcript", "people", "all_places", "dates", "topics")
# filter data to only include journals
journal <- location_data %>%
  filter(document_type == "Journals")


############# adding dates to the data #############

sorted_data <- journal[order(journal$id),]

# appending the id to the end of every text script
sorted_data$text_transcript <- paste0(sorted_data$text_transcript, ' %%%%', sorted_data$id, '%%%% ')


######## code from Trey and Emma 

# This paste turns it into one long string 
n_full_text <- paste(sorted_data$`text_transcript`, collapse = "")
# This is the pattern that removes all posible ways that WW wrote his dates
pattern_trey <- "(([J|F|M|A|J|S|O|N|D][a-z]{2,8}\\s){1,2}\\d{1,2}(th|st|rd|nd)?,?(\\s\\d{4})?|[J|F|M|A|J|S|O|N|D][a-z]{3,8}\\s\\d{1,2}(th|st|rd|nd)?) ~"
# Removing of the dates
matches <- str_extract_all(n_full_text, pattern=pattern_trey) %>% unlist()
# Emma wrote this, it adds an NA in the beginning where there is not a date
matches2 <- append(matches, NA, after = 0)
# This then resplits the data by date as opposed by page
text <- strsplit(n_full_text, split = pattern_trey) %>% unlist()
# This creates our database which is huge for us
papers <- data.frame(
  date = matches2,
  text = text
)

# separating the ids from the text
# some of the days include multiple pages which why there are a lot of columns
papers2 <- papers %>%
  separate(text, c("text", "id", "extra", "id2", "extra2", "id3", "extra3", "id4", "extra4",
                   "id5", "extra5", "id6", "extra6", "id7", "extra7", "id8", "extra8", 
                   "id9", "extra9", "id10", "extra10", "id11", "extra11", "id12", "extra12",
                   "id13", "extra13", "id14", "extra14", "id15", "extra15",
                   "id16", "extra16", "id17", "extra17", "id18", "extra18", 
                   "id19", "extra19", "id20", "extra20", "id21", "extra21", "id22", "extra22",
                   "id23", "extra23", "id24", "extra24", "id25", "extra25",
                   "id26", "extra26", "id27", "extra27", "id28", "extra28", 
                   "id29", "extra29", "id30", "extra30", "id31", "extra31", "id32", "extra32",
                   "id33", "extra33", "id34", "extra34"), "%%%%") 

papers2 <- papers2 %>%
  subset(select = -c(extra, extra2, extra3, extra4, extra5, extra6, extra7, extra8,
                     extra9, extra10, extra11, extra12, extra13, extra14, extra15, extra16,
                     extra17, extra18, extra19, extra20, extra21, extra22, extra23, extra24,
                     extra25, extra26, extra27, extra28, extra29, extra30, extra31, extra32,
                     extra33, extra34))%>%
  pivot_longer(cols=c(3:36), names_to='drop_column', values_to='id') %>%
  drop_na()

# removing na rows, dropping extra text, and making the date column as a date variable
papers3 <- papers2 %>% 
  subset(select = -c(drop_column)) %>%
  mutate(day = mdy(date)) %>%
  select(id, day)

journal$id <- as.character(journal$id)

# joining the date table with the rest of the data by id
data_dates <- journal %>% inner_join(papers3, by='id')

################ wrangling places column
###########################################################################

# separating all places into separate columns
sep_places <- data_dates %>%
  separate(all_places, c("place1", "place2", "place3", "place4", "place5", "place6", 
                         "place7", "place8", "place9", "place10", "place11", "place12", "place13", "place14", "place15", "place16", 
                         "place17", "place18", "place19", "place20", "place21", "place22", "place23", "place24", "place25", "place26", 
                         "place27"), "[|]")

# using pivot_longer to make each location it's own row
sep_places <- sep_places %>%
  pivot_longer(cols=c(13:39),
               names_to='place_n',
               values_to='location') %>%
  drop_na(location)

# dropping all columns that do not include a county
format <- sep_places %>%
  mutate(format = grepl("[A-Za-z ]+(,)+[A-Za-z ]+(,)+[A-Z a-z]+$", location)) %>%
  subset(format != 'FALSE')

# separating location column into city, county, and state columns
data <- format %>%
  separate(location, c('city', 'county_name', 'state_name'), sep=',') %>%
  mutate(county_yn = grepl('County', county_name)) %>%
  subset(county_yn) %>%
  select(city, county_name, state_name, short_url, 
         text_transcript, day)

# removing extra spaces from county and state columns
data$county_name <- trimws(data$county_name, which = c("left"))
data$state_name <- trimws(data$state_name, which = c("left"))

# removing territory from state column
data <- data %>% mutate(state_name = str_remove_all(state_name, " Territory"))

data$county_name <- str_replace(data$county_name, "Great Salt Lake County", "Salt Lake County")
data$city <- str_replace(data$city, "Great Salt Lake City", "Salt Lake City")

########### adding city coordinates to the data
############################################################################


cities <- read_csv("uscities.csv") %>%
  select(city, state_name, lat, lng)

# creating a geometry point out of the latitude and longitude
cities$point <- st_geometry(st_as_sf(cities,coords = c("lng","lat")))

# joining city data with location data
city_data <- data %>% inner_join(cities, by = c("city", "state_name"))

## creating a url for every city
city_data['state_url'] <- city_data['state_name']
city_data['city_url'] <- city_data['city']
city_data$state_url <- gsub(" ", "+", city_data$state_url)
city_data$city_url <- gsub(" ", "+", city_data$city_url)
city_data['search_url'] <- paste0("https://wilfordwoodruffpapers.org/places?search=",city_data$city_url, "+", city_data$state_url)

# grouping data by city and state
group_data <- city_data %>% 
  group_by(city, state_name)

# creating a count column fro how many times every county is mentioned
count_data <- transform(group_data,city_frequency=ave(seq(nrow(group_data)),search_url,FUN=length))


############################################################################
################################## GRAPH ###################################
############################################################################
############################################################################

# converting data frame into an sf points object
count_data <- sf::st_as_sf(count_data)

# making the 'day' column into an as.POSIXct object type
data2 <- count_data
data2 <- sf::st_as_sf(data2)
data2 <- st_cast(data2, "POINT")
data2 <- data2[order(data2$day), ]
data2$day = as.POSIXct(
  seq.POSIXt(as.POSIXct(min(data2$day)), as.POSIXct(max(data2$day)), length.out = nrow(data2)))
data2$day = as.Date(data2$day)

data3 <- data2 %>%
  select(day, point)

# creating a color pallette and bins for number of mentions
mybins <- c(0,2,5,10,50,100,200,Inf)
mypalette <- colorBin(palette="YlGnBu", domain=data2$city_frequency, na.color="transparent", bins=mybins)

# creating leaflet graph
leaflet() %>%
  addProviderTiles('CartoDB.Positron') %>%
  setView(-98.5795, 39.8283, zoom = 3) %>%
  addTimeslider(data = data3, fillOpacity = 1, popup = ~paste("<b>", "<a href=", data2$search_url, ">", data2$city, 
                                                              "</a>", ",", "</b>", data2$state_name, "<br>Number of Mentions:",
                                                              data2$city_frequency),
                color = ~mypalette(data2$city_frequency), radius = 5, weight = 5,
                options = timesliderOptions(position = "topright", timeAttribute = "day",
                                            showAllOnStart = TRUE, alwaysShowDate = TRUE)) %>%
  addLegend(data=data2, pal=mypalette, values=~city_frequency, opacity=0.9, title = "Mentions", 
            position = "bottomleft")

