# load helpers ------------------------------------------------------
source("R/helper.R", local = TRUE)
#--------------------------------------------------------------------

#Create header for Dashboard
header <- dashboardHeader(title = "ASA DataFest over the years", titleWidth = "350px")

#Create sidebar to host all the tabs
sidebar <- dashboardSidebar(width = 140,collapsed = FALSE,
                            sidebarMenuOutput("home"),
                            sidebarMenuOutput("host"),
                            sidebarMenuOutput("winner"))

#Create body of the dashboard 
body <- dashboardBody(
  fluidPage(
    title = NULL, width = 12,
    id = "tabset1",height = "250px",
    #Create tabs for navigation
    tabItems(
      #Create first tab page
      tabItem(tabName = "home",
              #Create row for tiles
              fluidRow(
                infoBoxOutput("ParticipantsTile", width = 3),
                infoBoxOutput("HostsTile", width = 3),
                infoBoxOutput("CountryTile", width = 3),
                infoBoxOutput("DataTile", width = 3)
              ),
              fluidRow(h6("Move the slider to see how DataFest has grown over the years.")),
              #Create row for slider
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
      #Create second tab page
      tabItem(tabName = "host",
              tags$style(type="text/css",
                         ".shiny-output-error { visibility: hidden; }",
                         ".shiny-output-error:before { visibility: hidden; }"
              ),
              fluidRow(
                #Create uni dropdown
                box(width = 3,
                    selectInput("college", "College",
                                choices = sort(unique(pull(updated_datafest, "host"))),
                                selected="Duke University")),
                #Create slider on host page
                box(width = 9,
                    sliderTextInput("uni_year",
                                    "Year",
                                    choices = year,
                                    selected = max_year, 
                                    width = "100%",
                                    animate = animationOptions(interval = 1500),
                                    
                                    grid = T)
                ),
                #create text for university start year above line graph
                textOutput("start_year"),
                tags$head(tags$style("#start_year{color: #000000;
                                  font-size: 16px;
             font-style: bold; text-align: left;
             }")),
                br(),
              ),
              fluidRow(box(
                #Create Line Chart
                plotOutput("line", height = "400px"),width = 9),
                #Create Uni specific details box
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
              ),
              textOutput("missing_year"),
              tags$head(tags$style("#missing_year{color: #000000;
                                  font-size: 15px;
             font-style: oblique; text-align: left;
             }")),
              br(),
              #Create majors wordcloud
              fluidRow(
                plotOutput("wordcloud_host")
              )  ,
              fluidRow(h4("This app is designed to compile and visualize metadata from ",
                          tags$a(href = "http://www.amstat.org/education/datafest/", "ASA DataFest"),
                          "over the years.",
                          "If your institution does not appear on the list, email",
                          tags$a(href = "mailto:mc301@duke.edu", " mc301@duke.edu"),
                          "."))
              
      ),
      #Create third tab page
      tabItem(tabName = "winner",
              fluidRow(
                #Create drop downs for Uni, Year and Award choice 
                box(
                  selectInput("year_choice",
                              "Year",
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
                  #Create action button for table 
                  actionButton(inputId = "search", label = "Search"),
                  width = 3
                ),
                #Create box for year wise Data Provider and Goals
                box(
                  solidHeader = TRUE,
                  title = p("Goal for the year",style = "font-size:20px;"),
                  textOutput("prompt"),
                  tags$head(tags$style("#prompt{color: #001833;
                                 font-size: 14px;
            font-family:'Trebuchet MS', sans-serif;font-style: bold;
            }")),
                  br(),
                  #Add year wise dataprovider
                  textOutput("provider"),
                  tags$head(tags$style("#provider{color: #001833;
                                 font-size: 15px;
            font-family:'Trebuchet MS', sans-serif;font-style: bold;
            }")),
                  width = 9
                ),
                
                box(
                  #Add instructions on how to use table
                  p("Select the inputs from the left panel and click on the \"search\" button to see the winners"),
                  tableOutput("titles"),
                  width = 9)
              )
              
      )
    )
  )
)


#Create the ui for the dashboard by combining the header, sidebar and body code above
ui <- dashboardPage(
  header,
  sidebar,
  body
)

#Add server code to incorporate the functionality in the dashboard
server <- function(input, output, session) {
  
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
  
  #Home Tab
  
  ## Add Tiles
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
  
  
  #Map
  
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
    
    pal <- colorBin("Blues", domain = states$num_par, bins = bins)
    
    labels <- sprintf(
      "<strong>%s</strong>",
      states$name
    ) %>% lapply(htmltools::HTML)
    
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
  
  #Add wordcloud
  library(tm)
  library(slam) 
  # Word Cloud
  output$wordcloud <- renderPlot({
    set.seed(1)
    ggplot(words, aes(label = word, color = word, size = size)) +
      geom_text_wordcloud(max_steps = 1, grid_margin = 1,eccentricity = 0.6) +
      #scale_size_area(max_size = 11) +
      scale_size(range = c(4,10)) +
      theme_void()
  })
  
  #Hosts tab
  
  #Year for each uni first participate in datafest
  output$start_year <- renderText({
    year_start = updated_datafest %>%
      filter(host == input$college & df == "Yes") %>%
      dplyr::select(year)
    min_year = min(year_start[[1]])
    paste(input$college, "first participated in Datafest in the year ",min_year)
  })
  #Text for data not available
  output$missing_year <- renderText({
    paste("Note: Participation data for", input$college,"is only available for the visualized years")})
  
  #Details tab calculation
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
  
  #Line chart
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
  
  library(tm)
  library(slam) 
  #Adding Word Cloud
  
  output$wordcloud_host <- renderPlot({
    majors <- filter(updated_datafest,host == input$college)
    majors <- majors$major_dist
    majors <- unlist(strsplit(majors, "[;]|[,]"))
    majors <- gsub('[[:punct:]]+' , '' , majors)
    majors <- gsub('[[:digit:]]+', '', majors)
    majors <- str_trim(majors)
    majors <- str_squish(majors)
    
    # if (all(is.na(majors))){
    #  return (" ")
    # }
    layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
    text(x=0.5, y=0.5, paste("This word cloud represents the different majors of participants across all years up to", input$uni_year))
    word<-data.frame(unique(na.omit(majors)))
    names(word) = "word"
    word$angle <- sample(c(0, 90), nrow(word), replace = TRUE)
    set.seed(9)
    ggplot(word, aes(label = word, color = word, size = 80, angle = angle)) +
      geom_text_wordcloud(max_steps = 1, grid_margin = 1) +
      scale_size(range = c(3,9)) +
      theme_void()
  })
  
  
  
  output$wordcloud_caption <- renderText({
    paste("This word cloud represents the different majors of participants across all years up to ", input$uni_year, ".")
  })
  
  #Winners Tab
  
  #print the competition data provider for the selected year on winners tab
  output$provider <- renderText({
    text <- datasource %>% 
      filter(year == input$year_choice)
    datasource <- paste0("Data Provider: ", text$source_data[1])
    paste(datasource)
    
  })
  
  #print the competition goal for the selected year on winners tab
  output$prompt <- renderText({
    text <- past_prompts %>% 
      filter(year == input$year_choice)
    word <- text$goal[1]
    paste(word)
    
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
  
}


############
shinyApp(ui = ui, server = server)
