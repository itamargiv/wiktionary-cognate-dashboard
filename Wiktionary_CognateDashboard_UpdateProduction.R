#!/usr/bin/env Rscript

### ---------------------------------------------------------------------------
### --- Wiktionary: Cognate Dashboard Update
### --- Script: Wiktionary_CognateDashboard_UpdateProduction.R, v. Beta 0.1
### --- Author: Goran S. Milovanovic, Data Analyst, WMDE
### --- Developed under the contract between Goran Milovanovic PR Data Kolektiv
### --- and WMDE.
### --- Contact: goran.milovanovic_ext@wikimedia.de
### ---------------------------------------------------------------------------
### --- DESCRIPTION:
### --- This is the update engine for the Wiktionary Cognate Dashboard
### --- http://wdcm.wmflabs.org/Wiktionary_CognateDashboard/
### ---------------------------------------------------------------------------
### --- RUN FROM: /home/goransm/RScripts/WDCM_R
### --- nohup Rscript Wiktionary_CognateDashboard_UpdateProduction.R &
### --- on crontab stat 1005, user goransm:
### --- # Wiktionary: Cognate Dashboard Update (every 6 hours)
### --- 0 */6 * * * export USER=goransm && nice -10 Rscript 
### --- /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/Wiktionary_CognateDashboard_UpdateProduction.R
### ---  >> /home/goransm/RScripts/W$

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

### ---------------------------------------------------------------------------
### --- Setup
library(data.table)
library(dplyr)
library(tidyr)
library(parallelDist)
library(mclust)
library(RColorBrewer)
library(plotrix)
library(bit64)
setwd('/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard')

# - toReport
generalT1 <- Sys.time()
print(paste0("Initiate Cognate Wiktionary Dashboard update on: ", generalT1))

### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- A. Matrix of the number of interwikis between 
### --- each possible pair of Wiktionaries
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------


### ---------------------------------------------------------------------------
### --- Export tables from cognate_wiktionary
### ---------------------------------------------------------------------------

### --- Export cognate_wiktionary.cognate_pages
# - toReport
print("Export cognate_wiktionary.cognate_pages now.")
sqlLogIn <- 'mysql --defaults-file=/etc/mysql/conf.d/analytics-research-client.cnf -h analytics-store.eqiad.wmnet -A -e'
query <- '"USE cognate_wiktionary; SELECT cgpa_site, cgpa_title FROM cognate_pages WHERE cgpa_namespace = 0;"'
outFile <- '> /srv/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/cognate_pages0.tsv'
sQuery <- paste(sqlLogIn, 
                query, 
                outFile, 
                sep = " ")
system(sQuery, 
       wait = T)

### --- Export cognate_wiktionary.cognate_sites
# - toReport
print("Export cognate_wiktionary.cognate_sites now.")
sqlLogIn <- 'mysql --defaults-file=/etc/mysql/conf.d/analytics-research-client.cnf -h analytics-store.eqiad.wmnet -A -e'
query <- '"USE cognate_wiktionary; SELECT * FROM cognate_sites;"'
outFile <- '> /srv/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/cognate_sites.tsv'
sQuery <- paste(sqlLogIn, 
                query, 
                outFile, 
                sep = " ")
system(sQuery, 
       wait = T)

### --- Export cognate_wiktionary.cognate_titles
# - toReport
print("Export cognate_wiktionary.cognate_titles now.")
sqlLogIn <- 'mysql --defaults-file=/etc/mysql/conf.d/analytics-research-client.cnf -h analytics-store.eqiad.wmnet -A -e'
query <- '"USE cognate_wiktionary; SELECT * FROM cognate_titles;"'
outFile <- '> /srv/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/cognate_titles.tsv'
sQuery <- paste(sqlLogIn, 
                query, 
                outFile, 
                sep = " ")
system(sQuery, 
       wait = T)

### ---------------------------------------------------------------------------
### --- Step 2: Create InterWiktionary Links Table
### ---------------------------------------------------------------------------

# - toReport
print("Create InterWiktionary Links Table.")

# - load cognate_pages0.tsv
cognatePages <- fread('cognate_pages0.tsv', 
                      sep = "\t", 
                      quote = "")
# - auto left_join to produce project pairs
cognatePages$cgpa_site <- as.character(cognatePages$cgpa_site)
cognatePages$cgpa_title <- as.character(cognatePages$cgpa_title)
cognatePages <- left_join(cognatePages, cognatePages, 
                          by = "cgpa_title")
# - the title key is not needed anymore
cognatePages$cgpa_title <- NULL
# - wrangle a bit to produce a project x project matrix
cognatePages <- cognatePages %>% 
  group_by(cgpa_site.x, cgpa_site.y) %>% 
  summarise(links = n()) %>%
  spread(key = cgpa_site.y,
         value = links,
         fill = 0)
# - this will go to rownames():
cognatePages$cgpa_site.x <- NULL
# - no self-links:
diag(cognatePages) <- 0
# - eliminate Wiktionaries w. no links:
wZeroLinks <- which(rowSums(cognatePages) == 0)
if (length(wZeroLinks) > 0) {
  cognatePages <- cognatePages[-wZeroLinks, -wZeroLinks]
}
# - provide proper row names for a symmetric links matrix:
rownames(cognatePages) <- colnames(cognatePages)
# - load cognate_sites.tsv for project names
cognateSites <- fread('cognate_sites.tsv',
                      sep = "\t", quote = "")
rownames(cognatePages) <- sapply(rownames(cognatePages), 
                                 function(x) {
                                   cognateSites$cgsi_dbname[which(as.character(cognateSites$cgsi_key) %in% x)]
                                 })
colnames(cognatePages) <- rownames(cognatePages)
# - store cognatePages as cognateLinks.csv
write.csv(cognatePages, 'cognateLinks.csv')

### ---------------------------------------------------------------------------
### --- Step 3: Analytical Tables from the InterWiktionary Links Table
### ---------------------------------------------------------------------------

# - toReport
print("Produce analytical tables from the InterWiktionary links table.")

### --- DT representation of the InterWiktionary Links Matrix
cognatePages$sourceProject <- rownames(cognatePages)
cognateLinksDT <- as.data.frame(cognatePages) %>%
  gather(key = 'targetProject', 
         value = 'Links',
         -sourceProject)
cognateLinksDT <- filter(cognateLinksDT, 
                         Links > 0)
colnames(cognateLinksDT) <- c('Source Wiktionary', 'Target Wiktionary', 'Links')
# - store cognateLinksDT as cognateLinksDT.csv
write.csv(cognateLinksDT, 'cognateLinksDT.csv')

### --- Cluster Analysis of InterWiktionary Links Matrix
### --- And {visNetwork} representation: Hubs
# - toReport
print("Cluster Analysis of InterWiktionary Links Matrix.")
cP <- as.data.frame(cognatePages)
cP$sourceProject <- NULL
# - prepare for {mclust}
cP <- scale(cP, center = T, scale = T)
rownames(cP) <- rownames(cognatePages)
colnames(cP) <- rownames(cognatePages)
# - {mclust}
bic <- mclustBIC(cP)
model <- Mclust(cP, x = bic)
# - clusters membership
projectMembership <- model$classification
projectMembership <- data.frame(Wiktionary = names(projectMembership), 
                                Cluster = projectMembership, 
                                stringsAsFactors = F)
# - store clusters membership
write.csv(projectMembership, 'projectMembership.csv')
# - project distance matrix
projectDist <- as.matrix(parDist(model$z, method = "hellinger"))
rownames(projectDist) <- rownames(cP)
colnames(projectDist) <- colnames(cP)
# - store project distance matrix
write.csv(projectDist, 'projectDist.csv')
# - prepare {visNetwork} structures
# - toReport
print("Prepare {visNetwork} structures.")
nodeColors <- brewer.pal(length(unique(projectMembership$Cluster
                                       )), "Set3")
nodeColors <- unname(sapply(nodeColors, color.id))
nodeColors <- sapply(projectMembership$Cluster, function(x) {
  nodeColors[x]
})
nodes <- data.frame(id = 1:dim(projectMembership)[1], 
                    label = projectMembership$Wiktionary, 
                    group = paste0("Cluster ", projectMembership$Cluster), 
                    shape = 'circle',
                    color = nodeColors,
                    shadow = T)
# - store nodes
write.csv(nodes, "nodes.csv")

edgesFrom1 <- nodes$id[which(rownames(projectDist) %in% nodes$label)]
edgesFrom2 <- edgesFrom1
edgesFrom3 <- edgesFrom1
edgesFrom4 <- edgesFrom1
edgesFrom5 <- edgesFrom1
cP <- as.data.frame(cognatePages)
cP$sourceProject <- NULL
edgesTo1 <- numeric()
edgesTo2 <- numeric()
edgesTo3 <- numeric()
edgesTo4 <- numeric()
edgesTo5 <- numeric()
for (i in 1:dim(cP)[1]) {
  s <- cP[i, ]
  s <- s[-i]
  s <- names(sort(s, decreasing = T)[1:5])
  edgesTo1[i] <- nodes$id[which(nodes$label == s[1])]
  edgesTo2[i] <- nodes$id[which(nodes$label == s[2])]
  edgesTo3[i] <- nodes$id[which(nodes$label == s[3])]
  edgesTo4[i] <- nodes$id[which(nodes$label == s[4])]
  edgesTo5[i] <- nodes$id[which(nodes$label == s[5])]
}
# - create {visNetwork} egdes for Hubs
edges <- data.frame(
  from = c(edgesFrom1, edgesFrom2, edgesFrom3, edgesFrom4, edgesFrom5),
  to = c(edgesTo1, edgesTo2, edgesTo3, edgesTo4, edgesTo5), 
  width = c(rep(5, length(edgesTo1)), rep(4, length(edgesTo2)), 
            rep(3, length(edgesTo3)), rep(2, length(edgesTo4)), 
            rep(1, length(edgesTo5))),
  shadow = T, 
  smooth = T,
  arrows = "to"
)
# - store "hub" edges
write.csv(edges, "edges.csv")

### --- InterWiktionary Links Matrix
### --- And {visNetwork} representation: who does not connect? (Anti-Hubs)
# - toReport
print("Prepare {visNetwork} structures for Anti-Hubs.")
n_edgesTo1 <- numeric()
n_edgesTo2 <- numeric()
n_edgesTo3 <- numeric()
n_edgesTo4 <- numeric()
n_edgesTo5 <- numeric()
for (i in 1:dim(cP)[1]) {
  s <- cP[i, ]
  s <- s[-i]
  s <- names(sort(s, decreasing = F)[1:5])
  n_edgesTo1[i] <- nodes$id[which(nodes$label == s[1])]
  n_edgesTo2[i] <- nodes$id[which(nodes$label == s[2])]
  n_edgesTo3[i] <- nodes$id[which(nodes$label == s[3])]
  n_edgesTo4[i] <- nodes$id[which(nodes$label == s[4])]
  n_edgesTo5[i] <- nodes$id[which(nodes$label == s[5])]
}
# - create {visNetwork} n_egdes for Anti-Hubs
n_edges <- data.frame(
  from = c(edgesFrom1, edgesFrom2, edgesFrom3, edgesFrom4, edgesFrom5),
  to = c(n_edgesTo1, n_edgesTo2, n_edgesTo3, n_edgesTo4, n_edgesTo5), 
  width = c(rep(5, length(n_edgesTo1)), rep(4, length(n_edgesTo2)), 
            rep(3, length(n_edgesTo3)), rep(2, length(n_edgesTo4)), 
            rep(1, length(n_edgesTo5))),
  shadow = T, 
  smooth = T,
  arrows = "to"
)
# - store "anti-hub" edges
write.csv(n_edges, "n_edges.csv")


### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------
### --- B1. Most interlinked entries not having a page on your own wiki
### --- SQL Approach
### ---------------------------------------------------------------------------
### ---------------------------------------------------------------------------

### --- clean up
rm(list = ls()); gc()

# - toReport
print("Finding missing entries from Wiktionaries.")

### --- select all wiktionaries
# - show all databases
mySqlArgs <- 
  '--defaults-file=/etc/mysql/conf.d/analytics-research-client.cnf -h analytics-store.eqiad.wmnet -A'
mySqlInput <- '"SHOW DATABASES;" > /srv/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/wiktionary_databases.tsv'
# - command:
mySqlCommand <- paste0("mysql ", mySqlArgs, " -e ", mySqlInput, collapse = "")
system(command = mySqlCommand, wait = TRUE)
# - get databases
clients <- read.table('wiktionary_databases.tsv', 
                      header = T, 
                      check.names = F, 
                      stringsAsFactors = F, 
                      sep = "\t")
# - select client projects
wClients <- which(grepl("wiktionary$", clients$Database))
clients <- clients$Database[wClients]

### --- SQL iterate across clients, fetch data, and produce the dataset
# - toReport
print("SQL iterate across clients, fetch data, and produce the dataset.")
wiktionaryEntries <- list()
tStart <- Sys.time()
c <- 0
for (i in 1:length(clients)) {
  # - report
  print(paste0("Fetching now project ", i, "/", length(clients), ": ", clients[i], "."))
  entries <- tryCatch(
    {
      mySqlArgs <- 
        '--defaults-file=/etc/mysql/conf.d/analytics-research-client.cnf -h analytics-store.eqiad.wmnet -A'
      mySqlInput <- paste0('"USE ', 
                           clients[i], 
                           '; SELECT page_title FROM page WHERE page_namespace = 0;" > /srv/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/currentWiktionary.tsv')
      # - command:
      mySqlCommand <- paste0("mysql ", mySqlArgs, " -e ", mySqlInput, collapse = "")
      system(command = mySqlCommand, wait = TRUE)
      fread('currentWiktionary.tsv', sep = "\t", quote = "")
      },
    error = function(condtition) {
      return(NULL)
    }
  )
  if (!is.null(entries)) {
    c <- c + 1
    entries <- entries[!duplicated(entries), ] 
    entries$project <- clients[i]
    entries <- entries[!(grepl("main_page", entries$page_title, ignore.case = T)), ]
    if (dim(entries)[1] >= 1) {
      write.csv(entries, paste0("projectData/", clients[i], "_entries.csv"))
    }
    wiktionaryEntries[[c]] <- entries
    rm(entries)
    print(paste0("Total time elapsed: ", Sys.time() - tStart))
  } else {
    rm(entries)
    print("NULL.")
  }
  print("Sleep for 5 seconds.")
  Sys.sleep(5)
}
wiktionaryEntries <- rbindlist(wiktionaryEntries)
# - remove instances of 'Main_Page' and 'main_Page'
wiktionaryEntries <- wiktionaryEntries[!(grepl("main_page", wiktionaryEntries$page_title, ignore.case = T)), ]
wiktionaryEntries <- as.data.table(wiktionaryEntries)

### --- entry counts
cognateEntries <- wiktionaryEntries[ , `:=`(entryCount = .N) , by = page_title]
cognateEntries <- cognateEntries[, c(1, 3)]
cognateEntries <- cognateEntries[!duplicated(cognateEntries), ]

# - store entities for search vector comparisons
searchEntries <- data.frame(entry = cognateEntries$page_title, 
                            stringsAsFactors = F)
write.csv(searchEntries, "searchEntries.csv")
saveRDS(searchEntries, 'searchEntries.Rds')

# - complete cognateEntries by arranging and filtering by entryCount > 2
cognateEntries <- cognateEntries %>% 
  arrange(desc(entryCount)) %>% 
  filter(entryCount > 2)

### --- find missing entries w. counts per project
# - toReport
print("Iterate across projects.")
wiktionaryEntries <- as.data.frame(wiktionaryEntries)
projects <- unique(wiktionaryEntries$project)
for (i in 1:length(projects)) {
  # - report
  print(paste0("Inspecting now project ", i, "/", length(projects), ": ", projects[i], "."))
  pEntries <- wiktionaryEntries$page_title[which(wiktionaryEntries$project == projects[i])]
  # - produce project search vector and save as .Rds
  sVec <- as.integer(searchEntries$entry %chin% pEntries)
  sVec <- data.frame(search = sVec)
  saveRDS(sVec, paste0("projectData/", projects[i], "_searchvector.Rds"))
  # - continue w. finding missing entries
  pEntries <- unique(wiktionaryEntries$page_title[which(!(wiktionaryEntries$page_title %in% pEntries))])
  projectFile <- cognateEntries %>% 
    filter(cognateEntries$page_title %in% pEntries) %>% 
    head(1000)
  write.csv(projectFile, paste0("projectData/", projects[i], "_missing.csv"))
}
# - clean
rm(sVec); rm(pEntries); rm(wiktionaryEntries); rm(cognateEntries); gc()

### ------------------------------------------------------------------
### --- Migrate data to:
### --- /srv/published-datasets/wmde-analytics-engineering/Wiktionary/
### ------------------------------------------------------------------

# - copy to: /srv/published-datasets/wmde-analytics-engineering/Wiktionary/
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/projectMembership.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/projectDist.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/nodes.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/n_edges.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/edges.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/cognateLinksDT.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/cognateLinks.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)
system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/searchEntries.csv /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)

system(command = 
         'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/searchEntries.Rds /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', 
       wait = T)


# - copy to: /srv/published-datasets/wmde-analytics-engineering/Wiktionary/projectData
system(command = 'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/projectData/* /srv/published-datasets/wmde-analytics-engineering/Wiktionary/projectData', wait = T)

# - toReport
print(paste0("Update completed on: ", Sys.time()))

# - update string
write(paste0("Last updated on: ", Sys.time()), "cognateUpdateString.txt")
# - copy update string to: /srv/published-datasets/wmde-analytics-engineering/Wiktionary/
system(command = 'cp /home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/cognateUpdateString.txt /srv/published-datasets/wmde-analytics-engineering/Wiktionary/', wait = T)




