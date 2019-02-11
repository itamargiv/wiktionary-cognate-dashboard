#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Wiktionary Cognate Dashboard, v2.0 Beta 0.1
### --- Script: Wiktionary_CognateDashboard_UpdateCloudVPS.R, v. Beta 0.1
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- Wiktionary_CognateDashboard_UpdateCloudVPS.R
### --- runs on crontab from wmde-dashboards.eqiad.wmflabs
### --- hourly checking for changes on 
## ---  https://analytics.wikimedia.org/datasets/wmde-analytics-engineering/Wiktionary/
### --- which maps /srv/published-datasets/ from stat1007.
### ---------------------------------------------------------------------------

### ---------------------------------------------------------------------------
### --- LICENSE:
### ---------------------------------------------------------------------------
### --- GPL v2
### --- This file is part of Wiktionary Cognate Dashboard
### ---
### --- Wiktionary Cognate Dashboard is free software: you can redistribute it and/or modify
### --- it under the terms of the GNU General Public License as published by
### --- the Free Software Foundation, either version 2 of the License, or
### --- (at your option) any later version.
### ---
### --- Wiktionary Cognate Dashboard is distributed in the hope that it will be useful,
### --- but WITHOUT ANY WARRANTY; without even the implied warranty of
### --- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
### --- GNU General Public License for more details.
### ---
### --- You should have received a copy of the GNU General Public License
### --- along with Wiktionary Cognate Dashboard. If not, see <http://www.gnu.org/licenses/>.
### ---------------------------------------------------------------------------

### --- Setup
library(httr)
library(curl)
library(stringr)
library(XML)

### ---------------------------------------------------------------------------
### --- 5. Wiktionary Dashboards: Wiktionary Cognate Dashboard
### --- affected dashboards: Wiktionary Cognate Dashboard
### ---------------------------------------------------------------------------

# - toReport:
print(paste0("Wiktionary Cognate update started on: ", as.character(Sys.time())))

### --- Config File
### --- Read WDCM paramereters
# - fPath: where the scripts is run from?
fPath <- as.character(commandArgs(trailingOnly = FALSE)[4])
fPath <- gsub("--file=", "", fPath, fixed = T)
fPath <- unlist(strsplit(fPath, split = "/", fixed = T))
fPath <- paste(
  paste(fPath[1:length(fPath) - 1], collapse = "/"),
  "/",
  sep = "")
params <- xmlParse(paste0(fPath, 'config_Wiktionary_CognateDashboard_UpdateCloudVPS.xml'))
params <- xmlToList(params)

# - get update string:
destfile = params$localUpdateStringPath
URL <- params$remoteUpdateStringPath
h <- new_handle()
handle_setopt(h,
              copypostfields = "Wiktionary Cognate Dashboard");
handle_setheaders(h,
                  "Cache-Control" = "no-cache"
)
newUpdateString <- curl_fetch_memory(URL, handle = h)
newUpdateString <- rawToChar(newUpdateString$content)
newUpdateString <- gsub("\\n", "", newUpdateString)

# - existing update string:
existingUpdateString <- readLines(params$productionUpdateStringPath)

# - to report:
print(paste0("Current update: ", newUpdateString))
print(paste0("Existing update: ", existingUpdateString))

# - compare with stored update string, update if necessary:

# - toReport:
print("Comparing update timestamps for the Wiktionary Cognate update now.")

if (newUpdateString != existingUpdateString) {
  
  # - toReport:
  print(paste0("Wiktionary Cognate download started on: ", as.character(Sys.time())))
  
  # - copy update string to productionUpdateStringPath
  system(command = paste0('sudo cp ', params$localUpdateStringPath, ' ', params$productionUpdateStringPath, 
         wait = T))
  
  # - touch remoteUpdateDirectory
  # - list files:
  url <- params$remoteUpdateDirectory
  page <- as.character(GET(url))
  links <- str_extract_all(page, "<a href=.+>.+</a>")
  links <- sapply(links, function(x) {str_extract_all(x, ">.+<")})
  links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
  links <- links[3:length(links)]
  links <- links[!grepl("/", links, fixed = T)]
  # - clean up README.txt
  wREADME <- which(grepl("README", links))
  if (length(wREADME) > 0) {links <- links[-wREADME]}
  # - download files:
  h <- new_handle()
  handle_setopt(h,
                copypostfields = "Wiktionary Cognate Dashboard");
  handle_setheaders(h,
                    "Cache-Control" = "no-cache"
  )
  for (i in 1:length(links)) {
    curl_download(paste0(url, links[i]),
                  handle = h, 
                  destfile = paste0(params$localUpdateDirectory, links[i])
    )
  }
  
  # - touch remoteProjectDataDir
  # - list files:
  url <- params$remoteProjectDataDir
  page <- as.character(GET(url))
  links <- str_extract_all(page, "<a href=.+>.+</a>")
  links <- sapply(links, function(x) {str_extract_all(x, ">.+<")})
  links <- sapply(links, function(x) {gsub('^>|"|<$|>|<', "", x)})
  links <- links[3:length(links)]
  links <- links[!grepl("/", links, fixed = T)]
  links <- links[grepl("missing|Rds", links)]
  h <- new_handle()
  handle_setopt(h,
                copypostfields = "Wiktionary Cognate Dashboard");
  handle_setheaders(h,
                    "Cache-Control" = "no-cache"
  )
  # - download files:
  for (i in 1:length(links)) {
    # - download:
    curl_download(paste0(url, links[i]),
                  handle = h, 
                  destfile = paste0(params$localProjectDataDir, links[i])
    )
  }
  
  # - toReport:
  print(paste0("Wiktionary Cognate copy files started on: ", as.character(Sys.time())))
  
  # - migrate to /srv/shiny-server/
  system(command = paste0(
           'sudo cp -r ', 
           params$localUpdateDirectory, 
           '* ', 
           params$productionUpdateDirectory
           ), wait = T)
  
  # - toReport:
  print(paste0("Wiktionary Cognate update completed on: ", as.character(Sys.time())))
  
  
} else {
  
  # - toReport:
  print(paste0("Wiktionary Cognate not run: update strings are matched. At: ", as.character(Sys.time())))
  
}







