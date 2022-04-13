#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @import magrittr
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic 
  
  # - shared data directory
  dataDir <- "_wiktionary_data/"
  projectDataDir <- "_wiktionary_data/projectData/"
  instructionsDir <- "_wiktionary_data/instructions/"
  
  ### --- Update string:
  updateString <- readLines(paste0(dataDir, "cognateUpdateString.txt"))
  updateString <- paste0(updateString, " UTC")
  
  ### --- Fetch data: cognateLinksDT.csv
  wiktionariesDT <- read.csv(paste0(dataDir,"cognateLinksDT.csv"),
                             header = T,
                             row.names = 1,
                             stringsAsFactors = F)
  wiktionariesDT <- wiktionariesDT[!duplicated(wiktionariesDT), ]
  colnames(wiktionariesDT) <- c("Source", "Target", "Num. Links")
  # wiktionariesDT$Source <- gsub("wiktionary", "", wiktionariesDT$Source, fixed = T)
  # wiktionariesDT$Target <- gsub("wiktionary", "", wiktionariesDT$Target, fixed = T)
  wiktionariesDT$Target <- paste0("> ", wiktionariesDT$Target)
  wiktionariesDT <- dplyr::arrange(wiktionariesDT, 
                                   dplyr::desc(`Num. Links`))
  
  ### --- Fetch data: {visNetwork} datasets
  # - nodes
  nodes <- read.csv(paste0(dataDir,"nodes.csv"),
                    header = T,
                    row.names = 1,
                    stringsAsFactors = F)
  nodes$label <- gsub("wiktionary", "", nodes$label, fixed = T)
  nodes$color <- NULL
  nodes$shape <- "square"
  # - edges
  edges <- read.csv(paste0(dataDir,"edges.csv"),
                    header = T,
                    row.names = 1,
                    stringsAsFactors = F)
  edges <- dplyr::filter(edges, width > 2)
  edges$width <- (edges$width - 2)/2
  edges$smooth <- F
  # - n_edges
  n_edges <- read.csv(paste0(dataDir,"n_edges.csv"),
                      header = T,
                      row.names = 1,
                      stringsAsFactors = F)
  n_edges <- dplyr::filter(n_edges, width > 2)
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
  mostPopular <- data.table::fread(paste0(dataDir, "mostPopularEntries.csv"))
  mostPopular$V1 <- NULL
  colnames(mostPopular) <- c("Entry", "In how many Wiktionaries?")
  
  lF <- list.files(projectDataDir)
  lF <- lF[which(grepl("missing", lF, fixed = T))]
  missingEntries <- list()
  for (i in 1:length(lF)) {
    missingEntries[[i]] <- data.table::fread(paste0(projectDataDir, lF[i]))
    missingEntries[[i]]$project <- gsub("_missing.csv", "", lF[i], fixed = T)
  }
  missingEntries <- data.table::rbindlist(missingEntries)
  missingEntries$V1 <- NULL
  
  ### --- Fetch data: cmpFiles (for comparison of Wiktionaries, Compare tab)
  cmpFiles <- list.files(projectDataDir)
  cmpFiles <- cmpFiles[which(grepl("searchvector", cmpFiles, fixed = T))]
  
  lInstructions <- list.files(instructionsDir)
  lInstructions <- gsub("_wcd_inst.txt", "", lInstructions, fixed = T)
  
  ### ----------------------------------
  ### --- GENERAL: Tab Instructions 
  ### ----------------------------------
  
  ### --- SELECT: update select 'selectInstructions'
  updateSelectizeInput(session,
                       "selectInstructions",
                       choices = unique(lInstructions),
                       selected = "en",
                       server = TRUE)
  
  ### --- FETCH: instructions file
  instructionsFile <- eventReactive(input$selectInstructions, {
    if (!is.null(input$selectInstructions) | !(input$selectInstructions == "")) {
      filename <- 
        paste0(instructionsDir,
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
    paste0('<p style="font-size:80%;"align="right"><b>', 
           updateString, '</b></p>')
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
    DT::datatable(wiktionariesDT,
                  filter = 'top',
                  options = list(
                    pageLength = 200,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', 
                                           targets = "_all"))
                    ),
                  rownames = FALSE
                  )
    }) %>% withProgress(message = "Generating data",
                        min = 0,
                        max = 1,
                        value = 1, {incProgress(amount = 1)})
  
  ### --- output$download_overviewDT
  output$download_overviewDT <- downloadHandler(
    filename = function() {
      "Wiktionary_LinksDataset.csv"},
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
                       "selectMyWiktionary",
                       choices = unique(wiktionariesDT$Source),
                       selected = "enwiktionary",
                       server = TRUE)
  
  ### --- projectDataset
  projectDataset <- reactive({
    wiktionariesDT %>% 
      dplyr::filter(Source == input$selectMyWiktionary) %>% 
      dplyr::arrange(dplyr::desc(`Num. Links`))
  })
  
  ### --- output$download_overviewDT
  output$download_projectDataset <- downloadHandler(
    filename = function() {
      "Wiktionary_SpecificLinksDataset.csv"},
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
    DT::datatable(projectDataset(),
                  options = list(
                    pageLength = 25,
                    width = '100%',
                    columnDefs = list(list(className = 'dt-center', 
                                           targets = "_all"))
                    ),
                  rownames = FALSE
                  )
    }) %>% withProgress(message = "Generating data",
                        min = 0,
                        max = 1,
                        value = 1, {incProgress(amount = 1)})
  
  ### --- OUTPUT output$top25projects
  output$top25projects <- renderPlot({
    # - plotFrame
    plotFrame <- projectDataset() %>% 
      dplyr::arrange(dplyr::desc(`Num. Links`)) %>% 
      head(25)
    plotFrame$Target <- gsub("> ", "", plotFrame$Target)
    plotFrame$Target <- gsub("wiktionary", "", plotFrame$Target)
    plotFrame$Target <- factor(plotFrame$Target, 
                               levels = 
                                 plotFrame$Target[order(-plotFrame$`Num. Links`)])
    # - ggplot2
    ggplot2::ggplot(plotFrame, 
                    ggplot2::aes(x = Target,
                                 y = `Num. Links`,
                                 label = Target)) +
      ggplot2::geom_line(size = .25, color = "#4c8cff", group = 1) +
      ggplot2::geom_point(size = 1.5, color = "#4c8cff") +
      ggplot2::geom_point(size = 1, color = "white") + 
      ggrepel::geom_label_repel(size = 3, 
                                segment.size = .25, 
                                show.legend = FALSE) +
      ggplot2::ylab("Number of Links") + 
      ggplot2::xlab("Language") + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::ggtitle("Most shared links with...") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = 
                       ggplot2::element_text(angle = 90, size = 12, hjust = 1)) + 
      ggplot2::theme(axis.text.y = 
                       ggplot2::element_text(size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = 
                       ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = 
                       ggplot2::element_text(size = 12)) + 
      ggplot2::theme(plot.title = 
                       ggplot2::element_text(hjust = 0.5)) %>% 
      withProgress(message = "Generating plot",
                   min = 0,
                   max = 1,
                   value = 1, {incProgress(amount = 0)})
  })
  
  ### --- OUTPUT output$bottom25projects
  output$bottom25projects <- renderPlot({
    # - plotFrame
    plotFrame <- projectDataset() %>% 
      dplyr::arrange(dplyr::desc(`Num. Links`)) %>% 
      tail(25)
    plotFrame$Target <- gsub("> ", "", plotFrame$Target)
    plotFrame$Target <- gsub("wiktionary", "", plotFrame$Target)
    plotFrame$Target <- factor(plotFrame$Target, 
                               levels = 
                                 plotFrame$Target[order(-plotFrame$`Num. Links`)])
    # - ggplot2
    ggplot2::ggplot(plotFrame, 
                    ggplot2::aes(x = Target,
                                 y = `Num. Links`,
                                 label = Target)) +
      ggplot2::geom_line(size = .25, color = "red", group = 1) + 
      ggplot2::geom_point(size = 1.5, color = "red") + 
      ggplot2::geom_point(size = 1, color = "white") + 
      ggrepel::geom_label_repel(size = 3, 
                                segment.size = .25, 
                                show.legend = FALSE) +
      ggplot2::ylab("Number of Links") + 
      ggplot2::xlab("Language") + 
      ggplot2::scale_y_continuous(labels = scales::comma) + 
      ggplot2::ggtitle("Least shared links with...") +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = 
                       ggplot2::element_text(angle = 90, size = 12, hjust = 1)) + 
      ggplot2::theme(axis.text.y = 
                       ggplot2::element_text(size = 12, hjust = 1)) +
      ggplot2::theme(axis.title.x = 
                       ggplot2::element_text(size = 12)) +
      ggplot2::theme(axis.title.y = 
                       ggplot2::element_text(size = 12)) + 
      ggplot2::theme(plot.title = 
                       ggplot2::element_text(hjust = 0.5)) %>% 
      withProgress(message = "Generating plot",
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
  output$hubs <- visNetwork::renderVisNetwork({
    visNetwork::visNetwork(nodes = hubnodes,
               edges = edges,
               width = "100%",
               height = "100%") %>%
      visNetwork::visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visNetwork::visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNetwork::visNodes(scaling = list(label = list(enabled = T))) %>% 
      visNetwork::visOptions(highlightNearest = TRUE, selectedBy = "label") %>%
      withProgress(message = "Generating plot",
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
  output$antihubs <- visNetwork::renderVisNetwork({
    visNetwork::visNetwork(nodes = antihubnodes,
               edges = n_edges,
               width = "100%",
               height = "100%") %>%
      visNetwork::visEvents(type = "once",
                startStabilizing = "function() {this.moveTo({scale:0.65})}") %>%
      visNetwork::visPhysics(enabled = T, maxVelocity = 1) %>% 
      visNetwork::visNodes(scaling = list(label = list(enabled = T))) %>% 
      visNetwork::visOptions(highlightNearest = TRUE, selectedBy = "label") %>%
      withProgress(message = "Generating plot",
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
                       "selectMyWiktionaryMiss",
                       choices = unique(missingEntries$project),
                       selected = "enwiktionary",
                       server = TRUE)
  
  ### --- projectDatasetMiss
  projectDatasetMiss <- reactive({
    mEntries <- missingEntries %>%
      dplyr::filter(project == input$selectMyWiktionaryMiss) %>%
      dplyr::select(-project)
    colnames(mEntries) <- c('Entry', 'In how many Wiktionaries:')
    mEntries
  })
  
  ### --- output$projectDTMiss
  output$projectDTMiss <- DT::renderDataTable({
    DT::datatable(projectDatasetMiss(),
              options = list(pageLength = 100,
                             width = '100%',
                             columnDefs = list(list(className = 'dt-center', targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = "Generating data",
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
                       "selectMyWiktionaryEntries_Source",
                       choices = unique(gsub("_searchvector.Rds",
                                             "", 
                                             cmpFiles, 
                                             fixed = T)),
                       selected = "enwiktionary",
                       server = TRUE)
  
  ### --- SELECT: update select 'selectMyWiktionaryEntries_Target'
  updateSelectizeInput(session,
                       "selectMyWiktionaryEntries_Target",
                       choices = unique(gsub("_searchvector.Rds", 
                                             "", 
                                             cmpFiles, 
                                             fixed = T)),
                       selected = "frwiktionary",
                       server = TRUE)
  
  
  ### --- reactive compareDataset
  compareDataSet <- 
    eventReactive(input$generateCompare,
                  {
                    withProgress(message = "Generating Data Set", 
                                 detail = "Loading Source", 
                                 value = 0, {
                      
                      # - get Source Wiktionary
                      sWiktionary <- paste0(projectDataDir, 
                                            isolate(input$selectMyWiktionaryEntries_Source), 
                                            "_searchvector.Rds")
                      
                      sWiktionary <- readRDS(sWiktionary)
                      sourceWiktionary <- sWiktionary$search
                      incProgress(0.2, detail = "Loading Target.")
                      
                      # - get Target Wiktionary
                      tWiktionary <- paste0(projectDataDir, 
                                            isolate(input$selectMyWiktionaryEntries_Target), 
                                            "_searchvector.Rds")
                      
                      tWiktionary <- readRDS(tWiktionary)
                      targetWiktionary <- tWiktionary$search
                      incProgress(0.2, detail = "Comparing Wiktionaries.")
                      
                      ### --- compareDataSet
                      if (is.null(targetWiktionary) | is.null(sourceWiktionary)) {
                        cmpData <- NULL
                        rm(sWiktionary)
                        rm(tWiktionary)
                        rm(sourceWiktionary)
                        rm(targetWiktionary) 
                      } else {
                        ### --- load searchEntries.Rds
                        searchEntries <- readRDS(paste0(dataDir, "searchEntries.Rds"))
                        incProgress(0.2, detail = "Loading Entries.")
                        wIx <- which(sourceWiktionary - targetWiktionary == -1)
                        cmpData <- data.frame(entry = searchEntries$entry[wIx],
                                              stringsAsFactors = F)
                        incProgress(0.2, detail = "Sorting result.")
                        cmpData <- dplyr::arrange(cmpData, entry)
                        rm(sWiktionary)
                        rm(tWiktionary)
                        rm(sourceWiktionary)
                        rm(targetWiktionary)
                        rm(searchEntries)
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
      
      DT::datatable(cD,
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
      
      DT::datatable(cD,
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
      "CompareDataset.csv"},
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
    DT::datatable(mostPopular,
              options = list(ordering = F,
                             pageLength = 100,
                             width = '100%',
                             columnDefs = list(list(className = 'dt-center', 
                                                    targets = "_all"))
              ),
              rownames = FALSE
    )
  }) %>% withProgress(message = "Generating data",
                      min = 0,
                      max = 1,
                      value = 1, {incProgress(amount = 1)})
  
  ### --- output$download_mostPopularDT
  output$download_mostPopularDT <- downloadHandler(
    filename = function() {
      "MostPopularEntries_Dataset.csv"},
    content = function(file) {
      write.csv(mostPopular,
                file,
                quote = FALSE,
                row.names = FALSE)
    },
    contentType = "text/csv"
  )
  
}
