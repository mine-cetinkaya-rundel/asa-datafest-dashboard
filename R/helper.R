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
year <- unique(updated_datafest$year)
year <- as.POSIXct(as.character(year), format = "%Y")
year <- format(year, "%Y")
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

canada <-  geojsonio::geojson_read("https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/canada.geojson", what = "sp")
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
country <- countries[countries$name %in% updated_datafest$country,]
country <- country[country$name!="Canada",]
recent <- updated_datafest %>% 
  filter(year == max_year)
write.csv(recent, "data/recent.csv")
num_part <- updated_datafest %>% 
  filter(!is.na(num_part)) %>% 
  group_by(year, state) %>% 
  summarise(num = sum(num_part))
max_part <- as.numeric(max(num_part$num))

canada$id <- canada$cartodb_id
canada <- canada[,!(names(canada) %in% c("cartodb_id", "created_at", "updated_at"))]
canada$density = NA
country$density = NA
states <- rbind(states, canada, country)

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
source_data <- c("LAPD","Kiva.com","eHarmony","GridPoint","Edmunds.com","Ticketmaster", "Expedia","Indeed", "Canadian National Women's Rugby Team","COVID-19 Virtual Data Challenge","Rocky Mountain Poison and Drug Safety","Play2Prevent Lab")
#"Indeed","Candadian National Women's Rugby Team","Covid-19 (Virtual Data Challenge)","Rocky Mountain Posion and Drug Safety","Play2Prevent Lab")
datasource <- data.frame(year, source_data)

# ## Subset dataframe to Year Country, State, City, Majors, Participating institutions
# country_hosts_df <- subset(datafest,
#                            df =="Yes",
#                            select= c("year","host","country","state","city","other_inst"))

