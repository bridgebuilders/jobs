library(shiny)
library(DT)
library(leaflet)
library(data.table)
library(RColorBrewer)

# simple function to capitalize first letter of each word in a string
simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(text = s, first = 1, last = 1)), 
        #tolower(substring(text = s, first = 2)),
        substring(text = s, first = 2),
        sep = "", 
        collapse = " ")
}

shinyServer(function(input, output, session) {
  
  load(file = 'jp_people_groups.rda')
  
  # sort out how to limit filters based on selections already made
  # first deal with Continent
  observeEvent(input$continent, {
    
    allChoices <- unique(peopleGroupsDT[, .(Continent, RegionName, Ctry)])
    
    if(input$continent == "All"){
      selectedChoices <- allChoices
    } else {
      selectedChoices <- allChoices[allChoices$Continent == input$continent]
    }
    
    updateSelectInput(session, 
                      inputId = "region",
                      choices = c("All", sort(selectedChoices$RegionName)))
    updateSelectInput(session, 
                      inputId = "country",
                      choices = c("All", sort(selectedChoices$Ctry)))
  })
  
  observeEvent(input$region, {
    
    allChoices <- unique(peopleGroupsDT[, .(Continent, RegionName, Ctry)])
    
    if(input$region == "All"){
      selectedChoices <- allChoices
    } else {
      selectedChoices <- allChoices[allChoices$RegionName == input$region]
    }
    
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
  peopleGroupsFiltered <- eventReactive(c(input$continent, input$region, input$country), {
    
    if(input$continent != "All"){
      peopleGroupsDT <- peopleGroupsDT[Continent == input$continent,]
    }
    
    if(input$region != "All"){
      peopleGroupsDT <- peopleGroupsDT[RegionName == input$region,]
    }
    
    if(input$country != "All"){
      peopleGroupsDT <- peopleGroupsDT[Ctry == input$country,]
    }
    
    peopleGroupsDT
    
  }, ignoreNULL = FALSE)
  
  # map the data
  observeEvent(peopleGroupsFiltered(), {
    popup <- paste0("<b>People Group: </b>", peopleGroupsFiltered()$PeopNameAcrossCountries,
                    "<br> <b>Location Details: </b>", peopleGroupsFiltered()$LocationInCountry,
                    "<br> <b>Population: </b>", format(peopleGroupsFiltered()$Population, big.mark = ','))
    
    rad <- function (y) log(y, base = 4)
    #rad <- function (y) y^2*100 # use for addCircles, since circles radii measured in meters, not pixels
    
    colorVec <- c("#dd7d1b", "#f9b625", "#666062", "#3eb1c8", "#007398")
    labelVec <- c("Unreached", "Formerly or falsely reached", "Minimally reached", "Partially reached", "Significantly reached")
    
    pal <- colorFactor(palette = colorVec,
                       levels = labelVec
    )
    
    
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
      addLegend(position = "bottomleft", title = "Progress Level", values = ~eval(lm), colors = colorVec, labels = labelVec,
                opacity = 1
      )
    
  })
  
  # update DataTable
    observeEvent(c(peopleGroupsFiltered(), input$jpMap_marker_click), {
      nullTableText <- "<h4>Click on a location for additional detail.</h4>"
      
      if(!is.null(input$jpMap_marker_click)){
        ### Create DT
        x <- peopleGroupsFiltered()[peopleGroupsFiltered()$PeopleID3 == input$jpMap_marker_click$id]
        
        # condition on whether there is data, construct message if not
        if(nrow(x) != 0){
          
          jpTable <- x[, .('People Group' = PeopNameAcrossCountries, 'People Cluster' = PeopleCluster, 'Language' = PrimaryLanguageName, 'Literacy Rate' = CtryLiteracyRate, 'Primary Religion' = PrimaryReligion, '% Adherents' = PercentAdherents, '% Evangelical' = PercentEvangelical, '% Buddhism' = PCBuddhism, '% Ethnic Religiosn' = PCEthnicReligions, '% Hinduism' = PCHinduism, '% Islam' = PCIslam, '% Non Religios' = PCNonReligious, '% Other' = PCOtherSmall, '% Unknown' = PCUnknown)]
          
          output$uiTable <- renderUI({ DT::dataTableOutput("table", width = 450) })
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
    defaultText <- as.character("<h4><br><b>Hover over a location for people group names.</b><br></h4><hr>")
    
    observeEvent(input$jpMap_marker_mouseout, {
      output$people <- renderPrint({
        HTML(defaultText)
      })
    }, ignoreNULL = FALSE)
    
    observeEvent(input$jpMap_marker_mouseover, {
      output$people <- renderPrint({
        HTML(paste0("<h4><br><b>People Group: </b>", as.character(unique(peopleGroupsFiltered()[peopleGroupsFiltered()$PeopleID3 == input$jpMap_marker_mouseover$id,]$PeopNameAcrossCountries)), "<br></h4><hr>"))
      })
    }, ignoreNULL = TRUE)
    

  
  })