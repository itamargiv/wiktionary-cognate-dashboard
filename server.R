### ---------------------------------------------------------------------------
### --- Wiktionary Cognate Dashboard
### --- Script: server.R, v. Beta 0.1
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
library(data.table)
library(dplyr)
library(tidyr)
library(stringr)
library(urltools)
library(DT)
library(ggplot2)
library(ggrepel)
library(scales)
library(visNetwork)

### --- Server (Session) Scope
### --------------------------------
setwd('/srv/shiny-server/Wiktionary_CognateDashboard/data')

### --- Update string:
updateString <- readLines('cognateUpdateString.txt')
updateString <- paste0(updateString, ' UTC')

### --- Fetch data: cognateLinksDT.csv
wiktionariesDT <- read.csv('cognateLinksDT.csv',
                           header = T,
                           row.names = 1,
                           stringsAsFactors = F)
wiktionariesDT <- wiktionariesDT[!duplicated(wiktionariesDT), ]
colnames(wiktionariesDT) <- c("Source", "Target", "Num. Links")
# wiktionariesDT$Source <- gsub("wiktionary", "", wiktionariesDT$Source, fixed = T)
# wiktionariesDT$Target <- gsub("wiktionary", "", wiktionariesDT$Target, fixed = T)
wiktionariesDT$Target <- paste0("> ", wiktionariesDT$Target)
wiktionariesDT <- arrange(wiktionariesDT, desc(`Num. Links`))

### --- Fetch data: {visNetwork} datasets
# - nodes
nodes <- read.csv('nodes.csv',
                  header = T,
                  row.names = 1,
                  stringsAsFactors = F)
nodes$label <- gsub("wiktionary", "", nodes$label, fixed = T)
nodes$color <- NULL
nodes$shape <- 'square'
# - edges
edges <- read.csv('edges.csv',
                  header = T,
                  row.names = 1,
                  stringsAsFactors = F)
edges <- filter(edges, width > 2)
edges$width <- (edges$width - 2)/2
edges$smooth <- F
# - n_edges
n_edges <- read.csv('n_edges.csv',
                    header = T,
                    row.names = 1,
                    stringsAsFactors = F)
n_edges <- filter(n_edges, width > 2)
n_edges$width <- (n_edges$width - 2)/2
n_edges$smooth <- F
# - produce hubs nodes
hubnodes <- nodes
hubnodes$value <- sapply(hubnodes$id, function(x) {
  round(log(length(which(edges$to == x)) + 1)) + 1
})
# - produce antihubs nodes
antihubnodes <- nodes
antihubnodes$value <- sapply(antihubnodes$id, function(x) {
  round(log(length(which(n_edges$to == x)) + 1)) + 1
})

### --- Fetch data: most popular entries
mostPopular <- fread('mostPopularEntries.csv')
mostPopular$V1 <- NULL
colnames(mostPopular) <- c('Entry', 'In how many Wiktionaries?')

### --- Fetch data: missing entries per project
setwd('/srv/shiny-server/Wiktionary_CognateDashboard/data/projectData')
lF <- list.files()
lF <- lF[which(grepl("missing", lF, fixed = T))]
missingEntries <- list()
for (i in 1:length(lF)) {
  missingEntries[[i]] <- fread(lF[i])
  missingEntries[[i]]$project <- gsub("_missing.csv", "", lF[i], fixed = T)
}
missingEntries <- rbindlist(missingEntries)
missingEntries$V1 <- NULL

### --- Fetch data: cmpFiles (for comparison of Wiktionaries, Compare tab)
cmpFiles <- list.files()
cmpFiles <- cmpFiles[which(grepl("searchvector", cmpFiles, fixed = T))]

### --- Fetch data: instructions
setwd('/srv/shiny-server/Wiktionary_CognateDashboard/data/instructions')
lInstructions <- list.files()
lInstructions <- gsub("_wcd_inst.txt", "", lInstructions, fixed = T)

### --- Back to '/srv/shiny-server/Wiktionary_CognateDashboard/data/projectData'
### --- for comparison datasets
setwd('/srv/shiny-server/Wiktionary_CognateDashboard/data/projectData')


### --- shinyServer
shinyServer(function(input, output, session) {
  
  ### ----------------------------------
  ### --- GENERAL: Tab Instructions 
  ### ----------------------------------
  
  ### --- SELECT: update select 'selectInstructions'
  updateSelectizeInput(session,
                       'selectInstructions',
                       choices = unique(lInstructions),
                       selected = 'en',
                       server = TRUE)
  
  ### --- FETCH: instructions file
  instructionsFile <- eventReactive(input$selectInstructions, {
    if (!is.null(input$selectInstructions) | !(input$selectInstructions == "")) {
      filename <- paste0("/srv/shiny-server/Wiktionary_CognateDashboard/data/instructions/",
                         input$selectInstructions, 
                         "_wcd_inst.txt")
      r <- readLines(filename)
      r <- r[-which(r == "")]
      return(r)}
    else {
      return(rep(" ", 5))
    }
  }, 
  ignoreNULL = TRUE)
  
  ### ----------------------------------
  ### --- GENERAL: Update String
  ### ----------------------------------
  
  output$updateString <- renderText({
    paste0('<p style="font-size:80%;"align="right"><b>', updateString, '</b></p>')
  })
  
  ### ----------------------------------
  ### --- TAB: Links Dataset
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_LinksDataset <- renderText({
    instructionsFile()[4]
  })
   
  ### --- output$overviewDT
  output$overviewDT <- DT::renderDataTable({
    datatable(wiktionariesDT, 
              filter = 'top',
              options = list(
                pageLength = 200,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$download_overviewDT
  output$download_overviewDT <- downloadHandler(
    filename = function() {
      'Wiktionary_LinksDataset.csv'},
    content = function(file) {
      write.csv(wiktionariesDT,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### ----------------------------------
  ### --- TAB: My Wiktionary
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_MyWiktionary <- renderText({
    instructionsFile()[1]
  })
  
  ### --- SELECT: update select 'selectMyWiktionary'
  updateSelectizeInput(session,
                       'selectMyWiktionary',
                       choices = unique(wiktionariesDT$Source),
                       selected = 'enwiktionary',
                       server = TRUE)
  
  ### --- projectDataset
  projectDataset <- reactive({
    wiktionariesDT %>% 
      filter(Source == input$selectMyWiktionary) %>% 
      arrange(desc(`Num. Links`))
  })
  
  ### --- output$download_overviewDT
  output$download_projectDataset <- downloadHandler(
    filename = function() {
      'Wiktionary_SpecificLinksDataset.csv'},
    content = function(file) {
      write.csv(projectDataset(),
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### --- output$projectDT
  output$projectDT <- DT::renderDataTable({
    datatable(projectDataset(),
              options = list(
                pageLength = 25,
                width = '100%',
                columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- OUTPUT output$top25projects
  output$top25projects <- renderPlot({
    # - plotFrame
    plotFrame <- projectDataset() %>% 
      arrange(desc(`Num. Links`)) %>% 
      head(25)
    plotFrame$Target <- gsub("> ", "", plotFrame$Target)
    plotFrame$Target <- gsub("wiktionary", "", plotFrame$Target)
    plotFrame$Target <- factor(plotFrame$Target, 
                               levels = plotFrame$Target[order(-plotFrame$`Num. Links`)])
    # - ggplot2
    ggplot(plotFrame, aes(x = Target,
                            y = `Num. Links`,
                            label = Target)) +
      geom_line(size = .25, color = "#4c8cff", group = 1) + 
      geom_point(size = 1.5, color = "#4c8cff") + 
      geom_point(size = 1, color = "white") + 
      geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
      ylab("Number of Links") + xlab("Language") + 
      scale_y_continuous(labels = comma) + 
      ggtitle("Most shared links with...") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) + 
      theme(axis.text.y = element_text(size = 12, hjust = 1)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) + 
      theme(plot.title = element_text(hjust = 0.5)) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- OUTPUT output$bottom25projects
  output$bottom25projects <- renderPlot({
    # - plotFrame
    plotFrame <- projectDataset() %>% 
      arrange(desc(`Num. Links`)) %>% 
      tail(25)
    plotFrame$Target <- gsub("> ", "", plotFrame$Target)
    plotFrame$Target <- gsub("wiktionary", "", plotFrame$Target)
    plotFrame$Target <- factor(plotFrame$Target, 
                               levels = plotFrame$Target[order(-plotFrame$`Num. Links`)])
    # - ggplot2
    ggplot(plotFrame, aes(x = Target,
                          y = `Num. Links`,
                          label = Target)) +
      geom_line(size = .25, color = "red", group = 1) + 
      geom_point(size = 1.5, color = "red") + 
      geom_point(size = 1, color = "white") + 
      geom_label_repel(size = 3, segment.size = .25, show.legend = FALSE) +
      ylab("Number of Links") + xlab("Language") + 
      scale_y_continuous(labels = comma) + 
      ggtitle("Least shared links with...") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, size = 12, hjust = 1)) + 
      theme(axis.text.y = element_text(size = 12, hjust = 1)) +
      theme(axis.title.x = element_text(size = 12)) +
      theme(axis.title.y = element_text(size = 12)) + 
      theme(plot.title = element_text(hjust = 0.5)) %>% 
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### ----------------------------------
  ### --- TAB: Hubs
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_Hubs <- renderText({
    instructionsFile()[2]
  })
  
  # - output$hubs
  output$hubs <- renderVisNetwork({
    visNetwork(nodes = hubnodes,
               edges = edges,
               width = "100%",
               height = "100%") %>%
      visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNodes(scaling = list(label = list(enabled = T))) %>% 
      visOptions(highlightNearest = TRUE, selectedBy = "label") %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
    })
  
  ### ----------------------------------
  ### --- TAB: Anti-Hubs
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_AntiHubs <- renderText({
    instructionsFile()[3]
  })
  
  # - output$antihubs
  output$antihubs <- renderVisNetwork({
    visNetwork(nodes = antihubnodes,
               edges = n_edges,
               width = "100%",
               height = "100%") %>%
      visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNodes(scaling = list(label = list(enabled = T))) %>% 
      visOptions(highlightNearest = TRUE, selectedBy = "label") %>%
      withProgress(message = 'Generating plot',
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### ----------------------------------
  ### --- TAB: I miss you
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_IMissYou <- renderText({
    instructionsFile()[5]
  })
  
  ### --- SELECT: update select 'selectMyWiktionaryMiss'
  updateSelectizeInput(session,
                       'selectMyWiktionaryMiss',
                       choices = unique(missingEntries$project),
                       selected = 'enwiktionary',
                       server = TRUE)
  
  ### --- projectDatasetMiss
  projectDatasetMiss <- reactive({
    mEntries <- missingEntries %>%
      filter(project == input$selectMyWiktionaryMiss) %>%
      select(-project)
    colnames(mEntries) <- c('Entry', 'In how many Wiktionaries:')
    mEntries
  })
  
  ### --- output$projectDTMiss
  output$projectDTMiss <- DT::renderDataTable({
    datatable(projectDatasetMiss(),
              options = list(pageLength = 100,
                             width = '100%',
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$download_overviewDT
  output$download_projectDTMiss <- downloadHandler(
    filename = function() {
      'IMissYouDataset.csv'},
    content = function(file) {
      write.csv(projectDatasetMiss(),
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### ----------------------------------
  ### --- TAB: Compare
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_Compare <- renderText({
    instructionsFile()[6]
  })
  
  ### --- SELECT: update select 'selectMyWiktionaryEntries_Source'
  updateSelectizeInput(session,
                       'selectMyWiktionaryEntries_Source',
                       choices = unique(gsub("_searchvector.Rds", "", cmpFiles, fixed = T)),
                       selected = 'enwiktionary',
                       server = TRUE)
  
  ### --- SELECT: update select 'selectMyWiktionaryEntries_Target'
  updateSelectizeInput(session,
                       'selectMyWiktionaryEntries_Target',
                       choices = unique(gsub("_searchvector.Rds", "", cmpFiles, fixed = T)),
                       selected = 'frwiktionary',
                       server = TRUE)


  ### --- reactive compareDataset
  compareDataSet <- 
    eventReactive(input$generateCompare,
                  {
                    withProgress(message = 'Generating Data Set', detail = "Loading Source", value = 0, {
                      
                      # - get Source Wiktionary
                      sWiktionary <- paste0(isolate(input$selectMyWiktionaryEntries_Source), 
                                            "_searchvector.Rds")
                      sWiktionary <- readRDS(sWiktionary)
                      sourceWiktionary <- sWiktionary$search
                      incProgress(0.2, detail = "Loading Target.")
                      
                      # - get Target Wiktionary
                      tWiktionary <- paste0(isolate(input$selectMyWiktionaryEntries_Target), 
                                            "_searchvector.Rds")
                      tWiktionary <- readRDS(tWiktionary)
                      targetWiktionary <- tWiktionary$search
                      incProgress(0.2, detail = "Comparing Wiktionaries.")
                      
                      ### --- compareDataSet
                      if (is.null(targetWiktionary) | is.null(sourceWiktionary)) {
                        cmpData <- NULL
                        rm(sWiktionary); rm(tWiktionary); rm(sourceWiktionary); rm(targetWiktionary) 
                      } else {
                        ### --- load searchEntries.Rds
                        searchEntries <- readRDS('/srv/shiny-server/Wiktionary_CognateDashboard/data/searchEntries.Rds')
                        incProgress(0.2, detail = "Loading Entries.")
                        wIx <- which(sourceWiktionary - targetWiktionary == -1)
                        cmpData <- data.frame(entry = searchEntries$entry[wIx],
                                              stringsAsFactors = F)
                        incProgress(0.2, detail = "Sorting result.")
                        cmpData <- arrange(cmpData, entry)
                        rm(sWiktionary); rm(tWiktionary); rm(sourceWiktionary); rm(targetWiktionary); rm(searchEntries)
                      }
                    })
                    
                    ### --- output$download_compareDataSetDT
                    output$download_compareDataSetDT <- downloadHandler(
                      filename = function() {
                        'CompareDataset.csv'},
                      content = function(file) {
                        write.csv(compareDataSet(),
                                  file,
                                  quote = FALSE,
                                  row.names = FALSE)
                      },
                      contentType = "text/csv"
                    )
                    
                    return(cmpData)
                    
                  })
  
  ### --- output$compareDataSetDT
  output$compareDataSetDT <- DT::renderDataTable({
    
    if (input$selectMyWiktionaryEntries_Source != input$selectMyWiktionaryEntries_Target) {
    
      prefix <- gsub("wiktionary$", "", input$selectMyWiktionaryEntries_Target)
      prefix <- paste0("https://", prefix, ".wiktionary.org/wiki/")
      cD <- compareDataSet()
      entryLinks <- url_encode(paste0(prefix, cD$entry))
      cD$entry <- paste0('<a href = "', 
                         entryLinks,
                         '" target = "_blank">',
                         cD$entry, 
                         "</a>")

      datatable(cD,
                escape = F,
                options = list(
                  ordering = F,
                  pageLength = 100,
                  width = '100%',
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
                  ),
                rownames = FALSE)
      
    } else {
      
      cD <- data.frame(entry = "Source and Target are identical.")
      
      datatable(cD,
                escape = F,
                options = list(
                  dom = 't', 
                  ordering = F,
                  pageLength = 100,
                  width = '100%',
                  columnDefs = list(list(className = 'dt-center', targets = "_all"))
                ),
                rownames = FALSE)
      
    }
    
    })
  
  ### --- output$download_compareDataSetDT
  output$download_compareDataSetDT <- downloadHandler(
    filename = function() {
      'CompareDataset.csv'},
    content = function(file) {
      write.csv(data.frame(Entry = ""),
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  ### ----------------------------------
  ### --- TAB: Most Popular
  ### ----------------------------------
  
  ### --- instructions
  output$instructions_MostPopular <- renderText({
    instructionsFile()[7]
  })
  
  ### --- output$mostPopularDT
  output$mostPopularDT <- DT::renderDataTable({
    datatable(mostPopular,
              options = list(ordering = F,
                             pageLength = 100,
                             width = '100%',
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = 'Generating data',
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$download_mostPopularDT
  output$download_mostPopularDT <- downloadHandler(
    filename = function() {
      'MostPopularEntries_Dataset.csv'},
    content = function(file) {
      write.csv(mostPopular,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
  }) ### --- END shinyServer






