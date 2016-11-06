
shinyUI(bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  tags$head(includeCSS("panelStyle.css")),
  
  leafletOutput("jpMap", width = "100%", height = "100%"),
  
  absolutePanel(bottom = 5, right = 125, 
                width = 200,
                fixed = TRUE, 
                draggable = FALSE,
                
                checkboxInput('showPanel', HTML("<b> Show info panel? </b>"), TRUE)
  ),
  
  conditionalPanel(condition = 'input.showPanel',
                   absolutePanel(HTML("<h3><center><b> Bridge Builders </b></center></h3>"), 
                                 id = "panel", 
                                 class = "panel panel-default", 
                                 top = 45, right = 10, width = 400, height = "auto", 
                                 fixed = TRUE, 
                                 draggable = FALSE,
                                 
                                 tabsetPanel(id = "tabsetMain",
                                             type = "pills",
                                             tabPanel(title = "Home", value = "inputs",
                                                      htmlOutput("people"),
                                                      p(),
                                                      selectInput(inputId = "continent", 
                                                                  label = HTML("<h4>Select a continent:</h4>"), 
                                                                  choices = c("All", sort(allChoices$Continent)), 
                                                                  selected = "All", 
                                                                  multiple = FALSE),
                                                      selectInput(inputId = "region", 
                                                                  label = HTML("<h4>Select a region:</h4>"), 
                                                                  choices = c("All", sort(allChoices$RegionName)), 
                                                                  selected = "All", 
                                                                  multiple = FALSE),
                                                      selectInput(inputId = "country", 
                                                                  label = HTML("<h4>Select a country:</h4>"), 
                                                                  choices = c("All", sort(allChoices$Ctry)), 
                                                                  selected = "All", 
                                                                  multiple = FALSE),
                                                      actionButton(inputId = "action", "Update Data")
                                             ),
                                             tabPanel(title = "Analyze", 
                                                      value = "analyze",
                                                      HTML("<hr>"),
                                                      tabsetPanel("tabsetAnalyze",
                                                                  type = "pills",
                                                                  tabPanel(title = "Table", value = "table",
                                                                           p(),
                                                                           uiOutput("uiTable")))
                                                      ),
                                             tabPanel("About", 
                                                      HTML("<h4>This map uses people group data from the Joshua Project.</h4>")
                                             )
                                             
                                 )
                   )
  )
  
))