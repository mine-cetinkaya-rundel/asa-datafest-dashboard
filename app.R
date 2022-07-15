# load helpers ------------------------------------------------------
source("R/helper.R", local = TRUE)
#--------------------------------------------------------------------
header <- dashboardHeader(title = "ASA DataFest over the years", titleWidth = "350px")

sidebar <- dashboardSidebar(width = 140,collapsed = FALSE,
                            sidebarMenuOutput("home"),
                            sidebarMenuOutput("host"),
                            sidebarMenuOutput("winner"))

body <- dashboardBody(
  
  fluidPage(
    title = NULL, width = 12,
    id = "tabset1",height = "250px",
    tabItems(
      tabItem(tabName = "home",
              fluidRow(
                infoBoxOutput("ParticipantsTile", width = 3),
                infoBoxOutput("HostsTile", width = 3),
                infoBoxOutput("CountryTile", width = 3),
                infoBoxOutput("DataTile", width = 3)
              ),
              fluidRow(box(width = 12,
                           sliderTextInput("year",
                                       "Year",
                                       choices = year,
                                       selected = max_year, 
                                       width = "100%",
                                       animate = animationOptions(interval = 1500),
                                       grid = year))),
              br(),
              fluidRow(h4("This map represents the geographic distribution of DataFest participants over the years. Click on the points to find out more about each event.")),
              
              fluidRow(leafletOutput("map")),
              br(),
              fluidRow(h4("This word cloud represents the different majors of participants at DataFest since its inception.")),
              fluidRow(box(width = 12, plotOutput("wordcloud", width = "100%", height = "400px"))),
              fluidRow(h4("This app is designed to compile and visualize metadata from ",
                 tags$a(href = "http://www.amstat.org/education/datafest/", "ASA DataFest"),
                 "over the years.",
                 "If your institution does not appear on the list, email",
                 tags$a(href = "mailto:mc301@duke.edu", " mc301@duke.edu"),
                 "."))
      ),
      
       tabItem(tabName = "host",
               fluidRow(
                 box(width = 3,
                     selectInput("college", "College",
                                 choices = sort(unique(pull(updated_datafest, "host"))),
                                 selected=sort(unique(pull(updated_datafest, "host")))[10])),
                 box(width = 9,
                     sliderTextInput("uni_year",
                                     "Year",
                                     choices = year,
                                     selected = max_year, 
                                     width = "100%",
                                     animate = animationOptions(interval = 1500),
                                     
                                     grid = T)
                 ),
                 textOutput("start_year"),
                 tags$head(tags$style("#start_year{color: #000000;
                                  font-size: 20px;
             font-style: bold; text-align: left;
             }")),
                 br(),
               ),
               fluidRow(box(
                 plotOutput("line", height = "400px"),width = 9),
                 box(solidHeader = TRUE,
                     title = p("Details",
                               style = "font-size:22px;
                                 margin-bottom: 0.2em;
                                 color: #005e97"),
                     hr(style = "margin-top: 0.1em; border-top: 1px solid"),
                     textOutput("country"),
                     tags$head(tags$style("#country{color: #001833;
                                  font-size: 18px;
             font-family:'Trebuchet MS', sans-serif; font-style: bold;
             }")),
                     br(),
                     textOutput("state"),
                     tags$head(tags$style("#state{color: #001833;
                                  font-size: 18px;
             font-family:'Trebuchet MS', sans-serif;font-style: bold;
             }")),
                     br(),
                     textOutput("city"),
                     tags$head(tags$style("#city{color: #001833;
                                  font-size: 18px;
             font-family:'Trebuchet MS', sans-serif;font-style: bold;
             }")),
                     br(),
                     textOutput("other_inst"),
                     tags$head(tags$style("#other_inst{color: #001833;
                                  font-size: 18px;
             font-family:'Trebuchet MS', sans-serif;font-style: bold;
             }")),
                     br(),
                     textOutput("uni_proportion"),
                     tags$head(tags$style("#uni_proportion{color: #001833;
                                  font-size: 18px;
             font-family:'Trebuchet MS', sans-serif;font-style: bold;
             }")),
                     width = 3, height = "420px"),
                 #p("major distribution"),
                 # textOutput("major_distribution")
               ),
               #fluidRow(textOutput("major_distribution")),
               textOutput("missing_year"),
               tags$head(tags$style("#missing_year{color: #000000;
                                  font-size: 15px;
             font-style: oblique; text-align: left;
             }")),
               br(),
               plotOutput("wordcloud_host", width = "100%", height = "400px"),
               br(),
               fluidRow(textOutput("wordcloud_caption")),
       ),
      
      tabItem(tabName = "winner",
              fluidRow(
                box(
                  selectInput("year_choice",
                                     "year",
                                     choices = sort(c(unique(datafest_titles$Year))),
                                     selected = "2022",
                  ),
                  
                  pickerInput("host_choice",
                              "Host University",
                              choices = c(unique(datafest_titles$Host)),
                              selected = c(datafest_titles$Host),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),
                  
                  pickerInput("award_choice",
                              "Award",
                              choices = c(sort(na.omit(unique(datafest_titles$Awards)))),
                              selected = c(sort(na.omit(unique(datafest_titles$Awards)))),
                              options = list(`actions-box` = TRUE),
                              multiple = TRUE),
                  actionButton(inputId = "search", label = "Search"),
                  width = 3
            ),
                
                box(
                  solidHeader = TRUE,
                  title = p(paste0("Data Description")),
                  textOutput("provider"),
                  br(),
                  textOutput("prompt"),
                  tags$head(tags$style("#state{color: #001833;
                                 font-size: 18px;
            font-family:'Trebuchet MS', sans-serif;font-style: bold;
            }")),
                  width = 9
                  ),
                
                box(
                tableOutput("titles"),
                width = 9)
              )
              
      )
    )
  )
)



ui <- dashboardPage(
  header,
  sidebar,
  body
)

# Preview the UI in the console
server <- function(input, output, session) {
  
  output$start_year <- renderText({
    year_start = updated_datafest %>%
      filter(host == input$college & df == "Yes") %>%
      dplyr::select(year)
    min_year = min(year_start[[1]])
    paste(input$college, "first participated in Datafest in the year ",min_year)
  })
  
  output$missing_year <- renderText({
    # year_start = updated_datafest %>%
    #   filter(host == input$college & df == "Yes") %>%
    #   dplyr::select(year)
    # min_year = min(year_start[[1]])
    # 
    # year_miss = updated_datafest %>%
    #   filter(host == "Duke University" & is.na(num_part)) %>%
    #   dplyr::select(year_miss)
    paste("Note: Participation data for", input$college,"is only available for the visualized years")})
  
  output$country <- renderText({
    loc_country = updated_datafest %>%
      filter(host == input$college) %>%
      dplyr::select(country)
    country = loc_country[[1]][1]
    paste("Country: ",country)
  })
  
  output$state <- renderText({
    loc_state = updated_datafest %>%
      filter(host == input$college) %>%
      dplyr::select(state)
    state = loc_state[[1]][1]
    paste("State:", state)
  })
  
  output$city <- renderText({
    loc_city = updated_datafest %>%
      filter(host == input$college) %>%
      dplyr::select(city)
    city = loc_city[[1]][1]
    paste("City:", city)
  })
  
  output$uni_proportion <- renderText({
    part = updated_datafest %>%
      filter(year == input$uni_year) %>%
      dplyr::select(num_part) %>%
      drop_na()
    totpart = sum(part$num_part)
    host =  updated_datafest %>%
      filter(host == input$college, year == input$uni_year) %>%
      dplyr::select(num_part) %>%
      drop_na()
    uni = host$num_part
    uni_prop = percent(uni/totpart, accuracy = 0.01)
    if (is_empty(uni_prop)){
      uni_prop = percent(0)
    }
    paste("Proportion of total participants in ", input$uni_year, ": ", uni_prop)
  })
  output$other_inst <- renderText({
    inst = updated_datafest %>%
      filter(host == input$college & df == "Yes" & year == input$uni_year) %>%
      dplyr::select(other_inst)
    coll = inst[[1]][1]
    if (is.na(coll)){
      coll = "No other institutions"
    }
    paste("Other Participating Institutions: ", coll)
  })
  
  # Create tab items in sidebar
  output$home <- renderMenu({
    sidebarMenu(
      menuItem("Home", tabName = "home",icon = icon("home")))})
  
  output$host <- renderMenu({
    sidebarMenu(
      menuItem("Hosts", tabName = "host",icon = icon("university")))})
  
  output$winner <- renderMenu({
    sidebarMenu(
      menuItem("Winners", tabName = "winner",icon = icon("award")))})
  
  d <- reactive({
    filter(updated_datafest, year == input$year & df == "Yes")
  })

  output$map <- renderLeaflet({
    
    host_text <- paste0(
      "<b><a href='", d()$url, "' style='color:", href_color, "'>", d()$host, "</a></b>"
    )
    
    other_inst_text <- paste0(
      ifelse(is.na(d()$other_inst),
             "",
             paste0("<br>", "with participation from ", d()$other_inst))
    )
    
    part_text <- paste0(
      "<font color=", part_color,">", d()$num_part, " participants</font>"
    )
    
    popups <- paste0(
      host_text, other_inst_text, "<br>" , part_text
    )
    
    participants <- d() %>%
      mutate(state = case_when(country == "Germany"~ "Germany",
                               country == "Australia" ~ "Australia",
                               state == "Minnessota"~ "Minnesota",
                               TRUE ~ state)) %>%
      dplyr::select(state, num_part) %>%
      dplyr::rename(name = state)
    
    # calculate total participants in each state
    states$num_par=0
    #if (nrow(participants!=0)) {
      for (i in 1:nrow(states)) {
        for (j in 1:nrow(participants)) {
          #if(!is.na(states$name[i]) & !is.na(participants$name[j])){
            if (states$name[i] == participants$name[j]) {
              if (!is.na(participants$num_part[j])) {
                states$num_par[i] = states$num_par[i] + participants$num_part[j]
              }
            }
          #}
        }
      }
    #}
    
    pal <- colorBin("Blues", domain = states$num_par, bins = bins)
    
    leaflet() %>%
      clearControls() %>% 
      clearMarkers() %>% 
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
      addCircleMarkers(lng = d()$lon, lat = d()$lat,
                       radius = log(d()$num_part)/2,
                       fillColor = marker_color,
                       color = marker_color,
                       weight = 3,
                       fillOpacity = 0.5,
                       popup = popups)
    
  })
  
  ## Add Tile graphics
  output$ParticipantsTile <- renderInfoBox({
    participant_tile <- part_count[part_count$year == input$year, ]$tot_part
    infoBox(
      "Participants", paste0(participant_tile), icon = icon("users"),
      color = "red", fill = TRUE, width = 2.5
    )
  })
  
  output$HostsTile <- renderInfoBox({
    hosts <- host_count[host_count$year == input$year, ]$tot_host
    infoBox(
      "Instituitions", paste0(hosts), icon = icon("university"),
      color = "yellow", fill = TRUE, width = 2.5
    )
  })
  
  output$CountryTile <- renderInfoBox({
    countries <- country_count[host_count$year == input$year, ]$tot_country
    infoBox(
      "Countries", paste0(countries), icon = icon("globe"),
      color = "blue", fill = TRUE, width = 2.5
    )
  })
  
  output$DataTile <- renderInfoBox({
    company <-datasource[datasource$year == input$year, ]$source_data
    infoBox(
      "Source Data", paste0(company), icon = icon("file-upload", library = "font-awesome"),
      color = "green", fill = TRUE, width = 2.5
    )
  })
  
  #use df of individual university
  output$line <- renderPlot({
    sel_part_count <- filter(universities_df, year <= input$uni_year, host == input$college)
    min_tot_part <- min(sel_part_count$num_part)
    uni_max <- filter(universities_df, host == input$college)$num_part
    max_tot_part <- max(uni_max)
    
    ggplot(sel_part_count, aes(x = year, y = num_part)) +
      geom_line(color = "#005e97", size=1.25) +
      geom_point(color = "#005e97",size = 1.5) +
      scale_x_continuous("Year",
                         limits = c(min_year, max_year),
                         breaks = c(min_year:max_year)) +
      scale_y_continuous("",
                         limits = c(0, max_tot_part)) +
      labs(title = "DataFest participants over time",
           subtitle = "Number of participants for each year") +
      geom_text(aes(label = num_part, x = year, y = num_part), 
                position = position_dodge(width = 0.8), vjust = 1.5, color = "#404040", size = 5) +
      theme(panel.grid.major = element_line(color="lightgray"),
            panel.grid.minor = element_line(color="lightgray"),
            panel.background = element_rect(fill="#EBEBEB"),
            plot.background = element_rect(fill="white"),
            axis.line = element_line(colour = "darkgray")) +
      theme(plot.title = element_text(color = "#005e97", size = 20),
            plot.subtitle = element_text(size = 15),
            #plot.caption = element_text(color = "aquamarine4", size = 20, face = "italic"),
            axis.text.x = element_text(size = 15),
            axis.text.y = element_text(size = 15)) +
      theme_minimal()
  }, bg="transparent")
  
  
    #print the competition goal for the selected year on winners tab
  source_text <- eventReactive(input$search, {
    text <- datasource %>% 
    filter(year == input$year_choice)
    datasource <- paste0("Data Provider: ", text$source_data[1])
    paste(datasource)})
  
  output$provider <- renderText({
    source_text()
  })
  
    prompts <- eventReactive(input$search,{
      text <- past_prompts %>% 
        filter(year == input$year_choice)
      word <- text$goal[1]
      paste(word)})
    
    
    output$prompt <- renderText({
      prompts()
    })
    
    #reactive past winners table
    titles_subset <- eventReactive(input$search, {
    
    ifelse(
      is.null(input$award_choice),
      award <- (na.omit(unique(datafest_titles$Awards))),
      award <- input$award_choice)

    ifelse(
      is.null(input$year_choice),
      year_title <- unique(datafest$year),
      year_title <- input$year_choice)

    ifelse(
      is.null(input$host_choice),
      host_title <- unique(datafest$host),
      host_title <- input$host_choice)
    
    table <- filter(
      datafest_titles,
      Awards %in% award,
      Year %in% year_title,
      Host %in% host_title)
    
    return(table %>% dplyr::select("Awards", "Host", "Year", "Title", "Team", "Slides"))
  })
  
  output$titles <- renderTable(
    {titles_subset()}, 
    sanitize.text.function = function(x) x,
    hover = TRUE,
    striped = TRUE,
    digits = 0
  )
  
library(tm)
library(slam) 
  #Adding Word Cloud
output$wordcloud <- renderPlot({
  all_majors <- major_df$major_dist
  all_majors <- unlist(strsplit(all_majors, "[;]"))
  all_majors <- gsub('[[:punct:]]+' , '' , all_majors)
  all_majors <- gsub('[[:digit:]]+', '', all_majors)
  all_majors <- str_trim(all_majors)
  all_majors <- str_squish(all_majors)
  wordcloud::wordcloud(words = all_majors, rot.per=0.3,scale = c(6,0.75), colors = brewer.pal(8, "Dark2"), min.freq = 1)
})

  output$wordcloud_host <- renderPlot({
    majors <- filter(updated_datafest,host == input$college)
    majors <- majors$major_dist

    majors <- unlist(strsplit(majors, "[;]"))
    majors <- gsub('[[:punct:]]+' , '' , majors)
    majors <- gsub('[[:digit:]]+', '', majors)
    majors <- str_trim(majors)
    majors <- str_squish(majors)

    if (all(is.na(majors))){
      majors <- c("None")
    }

    wordcloud::wordcloud(words = na.omit(majors), rot.per=0.3,scale = c(6,0.75),colors=brewer.pal(8, "Dark2"),min.freq = 1)
  })

  output$wordcloud_caption <- renderText({
    paste("This word cloud represents the different majors of participants across all years up to ", input$uni_year, ".")
  })
 }

############
shinyApp(ui = ui, server = server)
