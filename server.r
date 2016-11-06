

# function to capitalize first letter of each word in a string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(text = s, first = 1, last = 1)), 
        #tolower(substring(text = s, first = 2)),
        substring(text = s, first = 2),
        sep = "", 
        collapse = " ")
}

shinyServer(function(input, output, session) {
  
  # sort out how to limit filters based on selections already made
  # first deal with Continent
  observeEvent(input$continent, {
    if(input$continent == "All"){
      selectedChoices <- allChoices
    } else {
      selectedChoices <- allChoices %>% filter(allChoices$Continent == input$continent)
    }
    
    updateSelectInput(session, 
                      inputId = "region",
                      choices = c("All", sort(selectedChoices$RegionName)))
    updateSelectInput(session, 
                      inputId = "country",
                      choices = c("All", sort(selectedChoices$Ctry)))
  })
  
  
  observeEvent(input$region, {
    #if(input$continent == "All"){
      if(input$region == "All"){
        selectedChoices <- allChoices
      } else {
        selectedChoices <- allChoices %>% filter(allChoices$RegionName == input$region)
      }
      
#     } else {
#       if(input$region == "All"){
#         selectedChoices <- allChoices %>% filter(allChoices$Continent == input$continent)
#       } else {
#         selectedChoices <- allChoices %>% filter(allChoices$Continent == input$continent & allChoices$RegionName == input$region)
#       }
#     }
    
    updateSelectInput(session, 
                      inputId = "country",
                      choices = c("All", sort(selectedChoices$Ctry)))
  })
  
  # initialize map
  output$jpMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(provider = "Stamen.TonerLite", group = "Stamen Toner Lite") %>%
      setView(lng = 80.85937, lat = 25.48295, zoom = 2)
    
  })
  
  # filter data
  peopleGroupsFiltered <- eventReactive(input$action, {
    peopleGroupsDT_Filter <- peopleGroupsDT
    
    if(input$continent != "All"){
      peopleGroupsDT_Filter <- peopleGroupsDT_Filter %>% filter(Continent == input$continent)
    }
    
    if(input$region != "All"){
      peopleGroupsDT_Filter <- peopleGroupsDT_Filter %>% filter(RegionName == input$region)
    }
    
    if(input$country != "All"){
      peopleGroupsDT_Filter <- peopleGroupsDT_Filter %>% filter(Ctry == input$country)
    }
    
    peopleGroupsDT_Filter <- dplyr::sample_n(peopleGroupsDT_Filter, 1000, replace = TRUE)
    
  }, ignoreNULL = FALSE)
  
  # map the data
  observeEvent(peopleGroupsFiltered(), {
    popup <- paste0("<b>People Group: </b>", peopleGroupsFiltered()$PeopNameAcrossCountries,
                    "<br> <b>Location Details: </b>", peopleGroupsFiltered()$LocationInCountry,
                    "<br> <b>Population: </b>", format(peopleGroupsFiltered()$Population, big.mark = ','))
    
    rad <- function (y) log(y, base = 4)
    
    colorVec <- c("#dd7d1b", "#f9b625", "#666062", "#3eb1c8", "#007398")
    labelVec <- c("Unreached", "Formerly or falsely reached", "Minimally reached", "Partially reached", "Significantly reached")
    
    pal <- colorFactor(palette = colorVec, levels = labelVec)
    
    range_latitude <- max(peopleGroupsFiltered()$Latitude) - min(peopleGroupsFiltered()$Latitude)
    range_longitude <- max(peopleGroupsFiltered()$Longitude) - min(peopleGroupsFiltered()$Longitude)
    zoom_level <- zoom_range %>% filter(Degree > max(range_longitude, range_latitude)/2)
    zoom_level <- max(zoom_level$Level, 2)
    
    geomean <- geosphere::geomean(cbind(peopleGroupsFiltered()$Longitude,peopleGroupsFiltered()$Latitude))
    
    #browser()
    leafletProxy("jpMap", data = peopleGroupsFiltered()) %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(popup = popup,
                       fillOpacity = 1,
                       lng = ~Longitude, 
                       lat = ~Latitude, 
                       layerId = ~PeopleID3,
                       stroke = FALSE, 
                       color = ~pal(JPScaleDefs),
                       radius = ~rad(Population)) %>%
      setView(lng = geomean[1], lat = geomean[2], zoom = zoom_level) %>%
      addLegend(position = "bottomleft", title = "Progress Level", values = ~eval(lm), colors = colorVec, labels = labelVec,
                opacity = 1)
    
  })
  
  # update DataTable
      observeEvent(c(peopleGroupsFiltered(), input$jpMap_marker_click), {
        nullTableText <- "<h4>Click on a location for additional detail.</h4>"
        
        output$uiTable <- renderUI(HTML(nullTableText))
        
        if(!is.null(input$jpMap_marker_click)){
          ### Create DT
          
          clickedPeopleGroupId <- input$jpMap_marker_click$id
          peopleGroupsFilteredTable <- peopleGroupsFiltered() %>% filter(PeopleID3 == clickedPeopleGroupId)
          
          # condition on whether there is data, construct message if not
          if(nrow(peopleGroupsFilteredTable) != 0){
            
            jpTable <- unique(peopleGroupsFilteredTable) %>% select(one_of('PeopNameAcrossCountries','PeopleCluster','PrimaryLanguageName','Ctry','CtryLiteracyRate','PrimaryReligion','PercentAdherents','PercentEvangelical'))
            names(jpTable) <- c('People Group','People Cluster','Language','Country','Literacy Rate','Primary Religion','% Adherents','% Evangelical')
            
            output$uiTable <- renderUI({ DT::dataTableOutput("table", width = 360) })
            output$table <- DT::renderDataTable(jpTable, 
                                                colnames = unlist(lapply(names(jpTable), simpleCap)),
                                                options = list(dom = 'ftipr',
                                                               order = list(list(1, 'desc')),
                                                               pageLength = 5,
                                                               scrollX = TRUE))
          } else{
            output$uiTable <- renderUI(HTML(nullTableText))
          }
        } else {
          output$uiTable <- renderUI(HTML(nullTableText))
        }
        
      })
  
  
  # update people group name on hover
  defaultText <- as.character("<h4><br><b>Hover over a location for people group names.</b></h4><hr>")
  
  observeEvent(input$jpMap_marker_mouseout, {
    output$people <- renderPrint({
      HTML(defaultText)
    })
  }, ignoreNULL = FALSE)
  
  
  observeEvent(input$jpMap_marker_mouseover, {
    output$people <- renderPrint({
      HTML(paste0("<h4><br><b>People Group: </b>", as.character(unique(peopleGroupsFiltered()[peopleGroupsFiltered()$PeopleID3 == input$jpMap_marker_mouseover$id,]$PeopNameAcrossCountries)), "<br><br></h4><hr>"))
    })
  }, ignoreNULL = TRUE)
  
  
  
})