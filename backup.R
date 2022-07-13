participants <- recent %>%
  mutate(state = case_when(country == "Germany"~ "Germany",
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

bins <- c(0, 10, 20, 40, 80, 100, 200, 300, 400, max_part)
pal <- colorBin("Blues", domain = states$num_par, bins = bins)

labels <- sprintf(
  "<strong>%s</strong>",
  states$name
) %>% lapply(htmltools::HTML)

host_text <- paste0(
  "<b><a href='", recent$url, "' style='color:", href_color, "'>", recent$host, "</a></b>"
)

other_inst_text <- paste0(
  ifelse(is.na(recent$other_inst),
         "",
         paste0("<br>", "with participation from ", recent$other_inst))
)

part_text <- paste0(
  "<font color=", part_color,">", recent$num_part, " participants</font>"
)

popups <- paste0(
  host_text, other_inst_text, "<br>" , part_text
)


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





#updated
# calculate total participants for each year ------------------------
part_count <- recent %>%
  group_by(year) %>%
  summarise(tot_part = sum(num_part, na.rm = TRUE))

#make main and max according to min and max of inputted college, change year scale?
# min_tot_part <- min(part_count$tot_part)
# max_tot_part <- max(part_count$tot_part)