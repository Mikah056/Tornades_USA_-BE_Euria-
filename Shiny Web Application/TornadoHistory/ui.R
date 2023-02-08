
navbarPage("Tornado Tracker",
           tabPanel("Map",
              fluidPage(theme = shinytheme("flatly")),
                    tags$head(
                      tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                    pageWithSidebar(
                      headerPanel(''),
                      sidebarPanel(width = 3,
                                   checkboxGroupInput(inputId = "Echelle",
                                                      label = "Filter by Fujita scale", choices = c("EF0","EF1","EF2","EF3","EF4","EF5"), 
                                                      selected = c("EF3"="EF3","EF4"="EF4","EF5"="EF5"),inline=FALSE),
  
                  ##### Bout de code copie dans Masters Pokemon #####        
                
                                            p(tags$b("Filter by years", style = "font-size: 102%")),
                                            fluidRow(column(5, align = 'center',
                                                            selectInput(inputId = 'year_inf',
                                                                        label = NULL,
                                                                        choices = 1990:2022, 
                                                                        selected = 2001)),
                                                     column(2, style = 'margin-top: 7px', align = 'center', p("to")),
                                                     column(5, align = 'center',
                                                            selectInput(inputId = 'year_sup',
                                                                        label = NULL,
                                                                        choices = 1990:2022, 
                                                                        selected = 2015))),
                
                  ##### Fin du bout copie ####
                  
                  selectInput('tri_mois', 'Filter by months',choices=month.name,selected=c("April","May"),multiple=TRUE),
                  
                  
                  checkboxGroupInput(inputId = "MapDetails",
                                     label = "Display the :", choices = c("Starting points","Tornadoe's trajectories","Frequencies by state"), 
                                     selected = c("Starting points"="Starting points"), inline = FALSE),
                  
                  
                  
                             submitButton("Update filters")
                      ),
                  
                  
                      mainPanel(
                        leafletOutput("mymap", width = 1200, height=595)
                        
                      )
                    )),
           
           
           
           tabPanel("Stat_desc"),
           
           
           
           
           tabPanel("Data",
                    p(a("Romain AKAKPO", target="_blank"),style = "font-size:25px"),
                    p(a("Chaymae GRAOUI", target="_blank"),style = "font-size:25px"),
                    p(a("Hakim MOUNDE", target="_blank"),style = "font-size:25px"),
)

)



