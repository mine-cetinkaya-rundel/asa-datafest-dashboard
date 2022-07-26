# load packages -----------------------------------------------------
library(tidyverse)
library(leaflet)
library(geojsonio)
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
past_prompts <- read.csv("data/past_prompts.csv")
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
year  <- unique(updated_datafest$year)
max_year <- max(year)
min_year <- min(year)

# get data for hosts page
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

#import shape files for Canada and US - States and Countries across the world
canada <-  geojsonio::geojson_read("https://raw.githubusercontent.com/codeforgermany/click_that_hood/main/public/data/canada.geojson", what = "sp")
states <- geojsonio::geojson_read("https://rstudio.github.io/leaflet/json/us-states.geojson", what = "sp")
countries <- geojsonio::geojson_read("https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json", what = "sp")
country <- countries[countries$name %in% updated_datafest$country,]
country <- country[country$name!="Canada",]

num_part <- updated_datafest %>% 
  filter(!is.na(num_part)) %>% 
  group_by(year, state) %>% 
  summarise(num = sum(num_part))
max_part <- as.numeric(max(num_part$num))

recent <- updated_datafest %>% 
  filter(year == max(year))

states <- states[,!names(states)=="density"]
canada$id <- canada$cartodb_id
canada <- canada[,!(names(canada) %in% c("cartodb_id", "created_at", "updated_at"))]
states <- rbind(states, canada, country)
states$num_par=0

bins <- c(0, 10, 20, 40, 80, 100, 200, 300, 400, max_part)

participants <- recent %>%
  filter(!is.na(state)) %>% 
  mutate(state = case_when(country == "Germany" ~ "Germany",
                           country == "Australia" ~ "Australia",
                           state == "Minnessota"~ "Minnesota",
                           TRUE ~ state)) %>%
  dplyr::select(state, num_part) %>%
  dplyr::rename(name = state) 

for (i in 1:nrow(states)) {
  for (j in 1:nrow(participants)) {
    if (states$name[i] == participants$name[j]) {
      if (!is.na(participants$num_part[j])) {
        states$num_par[i] = states$num_par[i] + participants$num_part[j]
      }
    }
  }
}

pal <- colorBin("Blues", domain = states$num_par, bins = bins)

#Tiles

# calculate total participants for each year ------------------------
part_count <- updated_datafest %>%
  group_by(year) %>%
  summarise(tot_part = sum(num_part, na.rm = TRUE))

# calculate total countries participating for each year ------------------------
df_yes <- updated_datafest[updated_datafest$df == "Yes", ]
country_count <- df_yes %>%
  group_by(year) %>%
  summarise(tot_country = n_distinct(country))

# calculate total hosts participating for each year ------------------------
host_count <- df_yes %>%
  group_by(year) %>%
  summarise(tot_host = n_distinct(host))

# DataSource list for each year ----------------------
source_data <- c("LAPD","Kiva.com","eHarmony","GridPoint","Edmunds.com","Ticketmaster", "Expedia","Indeed", "Canadian National Women's Rugby Team","COVID-19 Virtual Data Challenge","Rocky Mountain Poison and Drug Safety","Play2Prevent Lab")
datasource <- data.frame(year, source_data)

# wordcloud
all_majors <- major_df$major_dist
all_majors <- unlist(strsplit(all_majors, "[;]|[,]"))
all_majors <- gsub('[[:punct:]]+' , '' , all_majors)
all_majors <- gsub('[[:digit:]]+', '', all_majors)
all_majors <- str_trim(all_majors)
all_majors <- str_squish(all_majors)

library(ggwordcloud)
words<-data.frame(na.omit(all_majors))
par(mar = rep(0, 4))
set.seed(1)
names(words) = "word"
sizes <- words %>% 
  group_by(word) %>% 
  summarise(n())
names(sizes) = c("word","num")
words$size = NA
words <- unique(words)
for (i in 1:nrow(words)) {
  for (j in 1:nrow(sizes)) {
    if (words$word[i]==sizes$word[j]) {
      words$size[i] = sizes$num[j]
    }
  }
}
