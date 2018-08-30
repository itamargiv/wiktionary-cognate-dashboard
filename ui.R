### ---------------------------------------------------------------------------
### --- Wiktionary Cognate Dashboard
### --- Script: ui.R, v. Beta 0.1
### --- WMDE 2018.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### ---
### --- Wiktionary: Cognate Dashboard Update is free software: 
### --- you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- Wiktionary: Cognate Dashboard Update is distributed in the 
### --- hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with Wiktionary: Cognate Dashboard Update. 
### --- If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup

### --- general
library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(visNetwork)

# - options
options(warn = -1)

### --- User Interface w. {shinydashboard}

shinyUI(
  
  ### --- dashboardPage
  ### --------------------------------
  
  dashboardPage(skin = "black",
                
                ### --- dashboarHeader
                ### --------------------------------
                
                dashboardHeader(
                  # - Title
                  title = "Wiktionary Cognate Dashboard",
                  titleWidth = 400
                ), 
                ### ---- END dashboardHeader
                
                ### --- dashboardSidebar
                ### --------------------------------
                
                dashboardSidebar(
                  sidebarMenu(
                    id = "tabsWCD",
                    menuItem(text = "My Wiktionary", 
                             tabName = "mywiktionary", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome"),
                             selected = TRUE
                    ),
                    menuItem(text = "Hubs", 
                             tabName = "hubs", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome")
                    ),
                    menuItem(text = "Anti-Hubs", 
                             tabName = "antihubs", 
                             icon = icon("bar-chart", class = NULL, lib = "font-awesome")
                    ),
                    menuItem(text = "Links Dataset", 
                             tabName = "overview", 
                             icon = icon("barcode")
                    ),
                    menuItem(text = "I miss you", 
                             tabName = "missing", 
                             icon = icon("barcode")
                    ),
                    menuItem(text = "Compare", 
                             tabName = "compare",
                             icon = icon("barcode")),
                    menuItem(text = "Most Popular", 
                             tabName = "mostpopular",
                             icon = icon("barcode"))
                  )
                ),
                ### --- END dashboardSidebar
                
                ### --- dashboardBody
                ### --------------------------------
                
                dashboardBody(
                  # - session time-out trick:
                  tags$script(inactivity),
                  # - style
                  tags$head(tags$style(HTML('.content-wrapper, .right-side {
                                              background-color: #ffffff;
                                            }'))),
                  tags$style(type="text/css",
                             ".shiny-output-error { visibility: hidden; }",
                             ".shiny-output-error:before { visibility: hidden; }"
                  ),
                  tabItems(
                    
                    ### --- TAB: My Wiktionary
                    ### --------------------------------
                    
                    tabItem(tabName = "mywiktionary",
                            fluidRow(
                              column(width = 6,
                                     htmlOutput('instructions_MyWiktionary')
                                     ),
                              column(width = 2,
                                     selectizeInput("selectInstructions",
                                                    "Language of instructions:",
                                                    multiple = F,
                                                    choices = NULL,
                                                    selected = 'en')
                                     ),
                              column(width = 4,
                                     HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                          target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                          target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                          target = "_blank">GitHub</a></p>'),
                                     htmlOutput('updateString')
                                     )
                                     ),
                            fluidRow(
                              column(width = 3, 
                                     selectizeInput("selectMyWiktionary",
                                                    "Select Wiktionary:",
                                                    multiple = F,
                                                    choices = NULL,
                                                    selected = 'enwiktionary')
                              )
                            ),
                            fluidRow(
                              column(width = 6,
                                     fluidRow(
                                       column(width = 12, 
                                              withSpinner(plotOutput('top25projects',
                                                                     width = "100%",
                                                                     height = "500px")),
                                              hr())
                                     ),
                                     fluidRow(
                                       column(width = 12, 
                                              withSpinner(plotOutput('bottom25projects',
                                                                     width = "100%",
                                                                     height = "500px")))
                                     )
                              ),
                              column(width = 6,
                                     h4('Wiktionary Links Dataset'),
                                     downloadButton('download_projectDataset',
                                                    'Download (csv)'),
                                     hr(),
                                     withSpinner(DT::dataTableOutput('projectDT', width = "100%"))
                              )
                            ),
                            fluidRow(
                              column(width = 1,
                                     br(),
                                     img(src = 'WiktionaryLogo.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                     HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                     br(),
                                     br()
                                     )
                              )
                            ),
                    ### --- END TAB: My Wiktionary
                    
                    ### --- TAB: Hubs
                    ### --------------------------------
                    
                    tabItem(tabName = "hubs",
                            fluidRow(
                              column(width = 6,
                                     htmlOutput('instructions_Hubs')
                              ),
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                          target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                          target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                          target = "_blank">GitHub</a></p>')
                              )
                                     ),
                            fluidRow(
                              column(width = 12, 
                                     h3("Hubs")
                              )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     withSpinner(visNetwork::visNetworkOutput('hubs', width = "100%", height = 850))
                              )
                            ),
                            fluidRow(
                              column(width = 1,
                                     br(),
                                     img(src = 'WiktionaryLogo.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                     HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                     br(),
                                     br()
                                     )
                              )
                            ),
                    ### --- END TAB: Hubs
                    
                    ### --- TAB: Anti-Hubs
                    ### --------------------------------
                    
                    tabItem(tabName = "antihubs",
                            fluidRow(
                              column(width = 6,
                                     htmlOutput('instructions_AntiHubs')
                                     ),
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                          target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                          target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                          target = "_blank">GitHub</a></p>')
                              )
                                     ),
                            fluidRow(
                              column(width = 12, 
                                     h3("Anti-Hubs")
                              )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     withSpinner(visNetwork::visNetworkOutput('antihubs', width = "100%", height = 850))
                              )
                            ),
                            fluidRow(
                              column(width = 1,
                                     br(),
                                     img(src = 'WiktionaryLogo.png',
                                         align = "left")
                              ),
                              column(width = 11,
                                     hr(),
                                     HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                     HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                     br(),
                                     br()
                              )
                            )
                                     ),
                    ### --- END TAB: Anti-Hubs
                    
                    ### --- TAB: Links dataset
                    ### --------------------------------
                    
                    tabItem(tabName = "overview",
                            fluidRow(
                              column(width = 6,
                                     htmlOutput('instructions_LinksDataset')
                                     ),
                              column(width = 6,
                                     HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                          target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                          target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                          target = "_blank">GitHub</a></p>')
                              )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     h3("Links Dataset")
                                     )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     downloadButton('download_overviewDT',
                                                    'Download Links Dataset (csv)'),
                                     hr()
                              )
                            ),
                            fluidRow(
                              column(width = 12, 
                                     withSpinner(DT::dataTableOutput('overviewDT', width = "60%"))
                                     )
                            ),
                            fluidRow(
                              column(width = 1,
                                     br(),
                                     img(src = 'WiktionaryLogo.png',
                                         align = "left")
                                      ),
                              column(width = 11,
                                     hr(),
                                     HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                     HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                     br(),
                                     br()
                                     )
                              )
                            ),
                  ### --- END TAB: Overview
                  
                  ### --- TAB: I Miss you
                  ### --------------------------------
                  
                  tabItem(tabName = "missing",
                          fluidRow(
                            column(width = 6,
                                   htmlOutput('instructions_IMissYou')
                            ),
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                          target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                          target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                          target = "_blank">GitHub</a></p>')
                            )
                                   ),
                          fluidRow(
                            column(width = 12, 
                                   h3("I miss you")
                            )
                          ),
                          fluidRow(
                            column(width = 3, 
                                   selectizeInput("selectMyWiktionaryMiss",
                                                  "Select Wiktionary:",
                                                  multiple = F,
                                                  choices = NULL,
                                                  selected = 'en')
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   downloadButton('download_projectDTMiss',
                                                  'Download (csv)'),
                                   hr(),
                                   withSpinner(DT::dataTableOutput('projectDTMiss', width = "35%"))
                            )
                          ),
                          fluidRow(
                            column(width = 1,
                                   br(),
                                   img(src = 'WiktionaryLogo.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                                   )
                            )
                          ),
                  
                  ### --- TAB: Compare
                  ### --------------------------------
                  
                  tabItem(tabName = "compare",
                          fluidRow(
                            column(width = 6,
                                   htmlOutput('instructions_Compare')
                                   ),
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                          target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                          target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                          target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                          target = "_blank">GitHub</a></p>')
                            )
                            ),
                          fluidRow(
                            column(width = 12, 
                                   h3("Compare")
                            )
                          ),
                          fluidRow(
                            column(width = 3, 
                                   selectizeInput("selectMyWiktionaryEntries_Source",
                                                  "Select Source:",
                                                  multiple = F,
                                                  choices = NULL,
                                                  selected = 'enwiktionary')
                            ),
                            column(width = 3, 
                                   selectizeInput("selectMyWiktionaryEntries_Target",
                                                  "Select Target:",
                                                  multiple = F,
                                                  choices = NULL,
                                                  selected = "frwiktionary")
                            )
                          ),
                          fluidRow(
                            column(width = 3,
                                   actionButton("generateCompare", "Generate")
                                   ),
                            column(width = 9, 
                                   downloadButton('download_compareDataSetDT',
                                                  'Download (csv)'),
                                   hr(),
                                   withSpinner(DT::dataTableOutput('compareDataSetDT', width = "35%"))
                            )
                          ),
                          fluidRow(
                            column(width = 1,
                                   br(),
                                   img(src = 'WiktionaryLogo.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                            )
                          )
                          ),
                  
                  ### --- TAB: Most Popular
                  ### --------------------------------
                  
                  tabItem(tabName = "mostpopular",
                          fluidRow(
                            column(width = 6,
                                   htmlOutput('instructions_MostPopular')
                            ),
                            column(width = 6,
                                   HTML('<p style="font-size:80%;"align="right"><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard" 
                                        target= "_blank">Documentation</a><br><a href = "https://meta.wikimedia.org/wiki/Wiktionary_Cognate_Dashboard/Interface" 
                                        target = "_blank">Help translating the interface</a><br><a href = "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/" 
                                        target = "_blank">Public datasets</a><br><a href = "https://github.com/wikimedia/analytics-wmde-WiktionaryCognateDashboard" 
                                        target = "_blank">GitHub</a></p>')
                                   )
                                   ),
                          fluidRow(
                            column(width = 12, 
                                   h3("Most Popular Entres")
                            )
                          ),
                          fluidRow(
                            column(width = 12, 
                                   downloadButton('download_mostPopularDT',
                                                  'Download (csv)'),
                                   hr(),
                                   withSpinner(DT::dataTableOutput('mostPopularDT', width = "35%"))
                            )
                          ),
                          fluidRow(
                            column(width = 1,
                                   br(),
                                   img(src = 'WiktionaryLogo.png',
                                       align = "left")
                            ),
                            column(width = 11,
                                   hr(),
                                   HTML('<b>Wiktionary Cognate Dashboard :: WMDE 2018</b><br>'),
                                   HTML('<b>Contact:</b> Goran S. Milovanovic, Data Scientist, WMDE<br><b>e-mail:</b> goran.milovanovic_ext@wikimedia.de
                          <br><b>IRC:</b> goransm'),
                                   br(),
                                   br()
                            )
                          )
                                   )
                  
                  
                ) ### --- END tabItems
                
                ) ### --- END dashboardBody
                
                ) ### --- dashboardPage
  
  ) # END shinyUI

