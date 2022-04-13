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

FROM rocker/r-ver:4.0.3

RUN apt-get update && apt-get install -y  git-core libcairo2-dev libcurl4-openssl-dev libgit2-dev libssh2-1-dev libssl-dev libxml2-dev make pandoc pandoc-citeproc zlib1g-dev && rm -rf /var/lib/apt/lists/*
RUN echo "options(repos = c(CRAN = 'https://cran.rstudio.com/'), download.file.method = 'libcurl')" >> /usr/local/lib/R/etc/Rprofile.site
RUN Rscript -e 'install.packages("curl")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("httr")'

RUN mkdir /app
RUN mkdir /app/_wiktionary_data
RUN mkdir /app/_wiktionary_data/projectData
RUN mkdir /app/_wiktionary_data/instructions
COPY wiktionarycognaterevolver.R /app
WORKDIR /app

CMD ["Rscript", "wiktionarycognaterevolver.R"]
