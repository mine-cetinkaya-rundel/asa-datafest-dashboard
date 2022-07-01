# load packages -----------------------------------------------------
library(tidyverse)
library(leaflet)
library(shiny)
library(shinythemes)
library(here)
library(praise)
library(usethis)
library(wordcloud)
library(shinyWidgets)
library(shinydashboard)
library(scales)
library(tm)
library(slam)
library(httr)    
set_config(use_proxy(url="10.3.100.207",port=8080))


# load data ---------------------------------------------------------

datafest <- read.csv("data/datafest.csv")
past_prompts <- read.csv("data/past_winners/past_prompts.csv")
updated_datafest <- read.csv("data/updated_datafest.csv")
datafest_titles <- read.csv("data/update_titles.csv")
names(datafest_titles) <- gsub("_", " ", names(datafest_titles), useBytes = TRUE) 
datafest_titles <- datafest_titles %>%
  mutate(
    Slides = paste0("<a href='", Slides, "'>", as.character(icon("file-powerpoint", lib = "font-awesome")), "</a>"
    )
  )
major_df <- updated_datafest %>%
  dplyr::select(host,year,major_dist) %>%
  na.omit()

#max and min years
max_year <- max(updated_datafest$year)
min_year <- min(updated_datafest$year)

#updated
# get data for universities page
universities_df <- updated_datafest %>%
  dplyr::select(host, year, num_part)

#Map

# set map bounds ----------------------------------------------------
left <- floor(min(updated_datafest$lon))
right <- ceiling(max(updated_datafest$lon))
bottom <- floor(min(updated_datafest$lat))
top <- ceiling(max(updated_datafest$lat))


# set colors --------------------------------------------------------
href_color <- "#9966CC"
marker_color <- "darkseagreen"
part_color <- "#CC9966"

states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
country <- countries[countries$name %in% updated_datafest$country,]
recent <- updated_datafest %>% 
  filter(year == max(year))
write.csv(recent, "data/recent.csv")
num_part <- updated_datafest %>% 
  filter(!is.na(num_part)) %>% 
  group_by(year, state) %>% 
  summarise(num = sum(num_part))
max_part <- as.numeric(max(num_part$num))

country$density = NA
states <- rbind(states,country)

participants <- recent %>%
  filter(!is.na(state)) %>% 
  mutate(state = case_when(country == "Germany" ~ "Germany",
                           country == "Canada" ~ "Canada",
                           country == "Australia" ~ "Australia",
                           state == "Minnessota"~ "Minnesota",
                           TRUE ~ state)) %>%
  dplyr::select(state, num_part) %>%
  dplyr::rename(name = state) 

states$num_par=0
for (i in 1:nrow(states)) {
  for (j in 1:nrow(participants)) {
    if (states$name[i] == participants$name[j]) {
      if (!is.na(participants$num_part[j])) {
        states$num_par[i] = states$num_par[i] + participants$num_part[j]
      }
    }
  }
}



leaflet() %>%
  addProviderTiles("CartoDB.Voyager") %>% 
  fitBounds(left, bottom, right, top) %>% 
  #addControl(h1(input$year), position = "topright") %>% 
  addPolygons(
    data = states,
    fillColor = ~pal(num_par),
    weight = 1,
    opacity = 1,
    color = "lightgray",
    dashArray = "",
    fillOpacity = 1,
    highlightOptions = highlightOptions(
      weight = 3,
      color = "lightgray",
      dashArray = "2",
      fillOpacity = 0.9,
      bringToFront = FALSE),
    label = labels,
    labelOptions = labelOptions(
      style = list("font-weight" = "normal",
                   padding = "3px 8px",
                   "color" = "#999999"),
      textsize = "10px",
      direction = "auto")) %>% 
  addLegend(pal = pal, values = bins, opacity = 0.7, title = "Number of Participants",
            position = "bottomright") %>% 
  addCircleMarkers(lng = recent$lon, lat = recent$lat,
                   radius = log(recent$num_part)/2,
                   fillColor = marker_color,
                   color = marker_color,
                   weight = 3,
                   fillOpacity = 0.5,
                   popup = popups)


bins <- c(0, 10, 20, 40, 70, 100, 200, 400, max_part)
pal <- colorBin("Blues", domain = states$num_par, bins = bins)

labels <- sprintf(
  "<strong>%s</strong>",
  states$name
) %>% lapply(htmltools::HTML)

host_text <- paste0(
  "<b><a href='", datafest$url, "' style='color:", href_color, "'>", datafest$host, "</a></b>"
)

other_inst_text <- paste0(
  ifelse(is.na(datafest$other_inst),
         "",
         paste0("<br>", "with participation from ", datafest$other_inst))
)

part_text <- paste0(
  "<font color=", part_color,">", datafest$num_part, " participants</font>"
)

popups <- paste0(
  host_text, other_inst_text, "<br>" , part_text
)

#updated
# calculate total participants for each year ------------------------
part_count <- updated_datafest %>%
  group_by(year) %>%
  summarise(tot_part = sum(num_part, na.rm = TRUE))

#make main and max according to min and max of inputted college, change year scale?
# min_tot_part <- min(part_count$tot_part)
# max_tot_part <- max(part_count$tot_part)

#updated
# calculate total countries participating for each year ------------------------
df_yes <- updated_datafest[updated_datafest$df == "Yes", ]
country_count <- df_yes %>%
  group_by(year) %>%
  summarise(tot_country = n_distinct(country))

#updated
# calculate total hosts participating for each year ------------------------
host_count <- df_yes %>%
  group_by(year) %>%
  summarise(tot_host = n_distinct(host))

## calculate DataSource list for each year ----------------------
year <- c("2011","2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022")
#"2018","2019","2020","2021","2022")
source_data <- c("LAPD","Kiva.com","eHarmony","GridPoint","Edmunds.com","Ticketmaster", "Expedia","Indeed", "Canadian National Women's Rugby Team","COVID-19 Virtual Data Challenge","Rocky Mountain Poison and Drug Safety","Play2Prevent Lab")
#"Indeed","Candadian National Women's Rugby Team","Covid-19 (Virtual Data Challenge)","Rocky Mountain Posion and Drug Safety","Play2Prevent Lab")
datasource <- data.frame(year, source_data)

# ## Subset dataframe to Year Country, State, City, Majors, Participating institutions
# country_hosts_df <- subset(datafest,
#                            df =="Yes",
#                            select= c("year","host","country","state","city","other_inst"))

