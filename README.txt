This is the description of the following public data sets:

Wiktionary Cognate Dashboard data sets, found in /srv/published-datasets/wmde-analytics-engineering/Wiktionary, hosted by WMDE on stat1005

The Wiktionary Cognate Dashboard  is found on: http://wdcm.wmflabs.org/Wiktionary_CognateDashboard/

Author: Goran S. Milovanovic, Data Scientist, WMDE
Contact: goran.milovanovic_ext@wikimedia.de

Description: this is a set of update files for the Wiktionary Cognate Dashboard (hosted on CloudVPS). 
The update is obtained by querying (1) the cognate_wiktionary databse from analytics-store.eqiad.wmnet, (2) the page tables from the relevant wiktionary databases, also from analytics-store.eqiad.wmnet.

NOTE: SQL calls are orchestrated from within an R script which provides additional data wrangling procedures and statistical modeling (see: /srv/home/goransm/RScripts/Wiktionary/Wiktionary_CognateDashboard/Wiktionary_CognateDashboard_UpdateProduction.R)

Files:

--- cognateLinksDT.csv
-- Description: main data file, encompasses pairs of Wiktionaries and the number of shared Cognate links in a pair
-- Fields:
- SourceWiktionary: the Wiktionary project under consideration (e.g. "enwiktionary")
- TargetWiktionary: the Wiktionary with which the SourceWiktionary shares the number of Cognate links given in the Links field
- Links: the number of shared links

--- nodes.csv
-- Description: first (nodes) component of the data structure needed for Dashboard network visualizations with {visNetwork} in R
-- Fields:
- id
- label (e.g. "enwiktionary", "zhwiktionary", etc)
- group (cluster from the BIC optimal solution obtained from {mclust} in R)
- shape (visualization parameter)
- color (visualization parameter, corresponds to group)
- shadow (visualization parameter)

--- edges.csv
-- Description: second (edges) component of the data structure needed for Dashboard network visualizations with {visNetwork} in R for Dashboard Hubs Tab
-- Fields:
- from (id of the 'from' node)
- to (id of the 'to' node)
- width (visualization parameter)
- shadow (visualization parameter)
- arrows(visualization parameter)

--- n_edges.csv
-- Description: second (edges) component of the data structure needed for Dashboard network visualizations with {visNetwork} in R for Dashboard Anti-Hubs Tab
-- Fields:
- from (id of the 'from' node)
- to (id of the 'to' node)
- width (visualization parameter)
- shadow (visualization parameter)
- arrows(visualization parameter)

Folders:

---- /projectData

Files in /projectData:
-- Description: A set of files with of the same structure that describe the 1,000 missing entries from each Wiktionary alongside the number of Wiktionaries having the respective items.
-- Fields:
- page_title - a Wiktionary entry
- entryCount - the number of Wiktionaries having the respective entry

-- NOTES:

(1) Feedback should be sent to: goran.milovanovic_ext@wikimedia.de
(2) The data set is produced by Goran S. Milovanovic working as a contractor for WMDE, via a contract established between his Data Kolektiv, Belgrade and WMDE, Berlin.
