#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Project: Wiktionary Cognate Dashboard
### --- Version 1.0.0
### --- Script: wiktionarycognaterevolver.R
### --- January 2022.
### --- Author: Goran S. Milovanovic, Data Scientist, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Description: data update for the Wiktionary Cognate Dashboard
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------

### --- Directory Tree
# - shared data directory
dataDir <- "_wiktionary_data/"
projectDataDir <- "_wiktionary_data/projectData/"
instructionsDir <- "_wiktionary_data/instructions/"

### --- Daemon
infinite = 0
repeat {
  
  # - toReport:
  print(paste0("Wiktionary Cognate copy files started on: ", 
               as.character(Sys.time())))
  
  # - get update string:
  destfile = paste0(dataDir, "cognateUpdateString.txt")
  URL <- 
    "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/cognateUpdateString.txt"
  h <- curl::new_handle()
  newUpdateString <- curl::curl_fetch_memory(URL, handle = h)
  newUpdateString <- rawToChar(newUpdateString$content)
  newUpdateString <- gsub("\\n", "", newUpdateString)
  
  # - existing update string:
  existingUpdateString <- readLines(destfile)
  
  # - to report:
  print(paste0("Current update: ", newUpdateString))
  print(paste0("Existing update: ", existingUpdateString))
  
  # - compare with stored update string, update if necessary:
  
  # - toReport:
  print("Comparing update timestamps for the Wiktionary Cognate update now.")
  
  if (newUpdateString != existingUpdateString) {
    
    # - toReport:
    print(paste0("Wiktionary Cognate download started on: ", 
                 as.character(Sys.time())))
    
    # - write new update string to productionUpdateStringPath
    writeLines(newUpdateString, destfile)
    
    # - touch remoteUpdateDirectory
    # - list files:
    url <- "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/"
    page <- as.character(httr::GET(url))
    links <- stringr::str_extract_all(page, "<a href=.+>.+</a>")
    links <- sapply(links, function(x) {stringr::str_extract_all(x, ">.+<")})
    links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
    links <- links[3:length(links)]
    links <- links[!grepl("/", links, fixed = T)]
    # - clean up README.txt
    wREADME <- which(grepl("README", links))
    if (length(wREADME) > 0) {links <- links[-wREADME]}
    # - download files:
    h <- curl::new_handle()
    for (i in 1:length(links)) {
      curl::curl_download(paste0(url, links[i]),
                          handle = h,
                          destfile = paste0(dataDir, links[i])
      )
    }
    
    # - touch remoteProjectDataDir
    # - list files:
    url <- "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/projectData/"
    page <- as.character(httr::GET(url))
    links <- stringr::str_extract_all(page, "<a href=.+>.+</a>")
    links <- sapply(links, function(x) {stringr::str_extract_all(x, ">.+<")})
    links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
    links <- links[3:length(links)]
    links <- links[!grepl("/", links, fixed = T)]
    links <- links[grepl("missing|Rds", links)]
    h <- curl::new_handle()
    # - download files:
    for (i in 1:length(links)) {
      # - download:
      curl::curl_download(paste0(url, links[i]),
                          handle = h,
                          destfile = paste0(projectDataDir, links[i])
      )
    }
    
    # - touch remote instructions directory
    # - list files:
    url <- "https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/instructions/"
    page <- as.character(httr::GET(url))
    links <- stringr::str_extract_all(page, "<a href=.+>.+</a>")
    links <- sapply(links, function(x) {stringr::str_extract_all(x, ">.+<")})
    links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
    links <- links[3:length(links)]
    links <- links[!grepl("/", links, fixed = T)]
    links <- links[grepl("txt", links)]
    h <- curl::new_handle()
    # - download files:
    for (i in 1:length(links)) {
      # - download:
      curl::curl_download(paste0(url, links[i]),
                          handle = h,
                          destfile = paste0(instructionsDir, links[i])
      )
    }
    
    # - toReport:
    print(paste0("Wiktionary Cognate update completed on: ", 
                 as.character(Sys.time())))
    
    
  } else {
    
    # - toReport:
    print(paste0("Wiktionary Cognate not run: update strings are matched. At: ", 
                 as.character(Sys.time())))
    
  }
  
  if (infinite == 1) {
    break
  }
  
  # - check every hour:
  Sys.sleep(60*60)
  
}