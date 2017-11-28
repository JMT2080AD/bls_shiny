rm(list = ls())

library(blsAPI)
library(rjson)
library(ggplot2)
library(data.table)

## read in regKey
con <- file("./.regKey")
regKey <- readLines(con)

##QCEW
## file check
fileorder <- c("seasonal",
               "area",
               "datatype",
               "size",
               "ownership",
               "industry")

files <- lapply(paste0("./lookup_tables/qcew/",
                       paste0(fileorder,"_titles.csv")),
                fread, colClasses ="character")

ViewSubsetQcew <- function(val){
    results <- c(files[[1]][seasonal      == val[1], seasonal_text ,],
                 files[[2]][area_fips     == val[2], area_title    ,],
                 files[[3]][data_type     == val[3], title         ,],
                 files[[4]][size_code     == val[4], size_title    ,],
                 files[[5]][own_code      == val[5], own_title     ,],
                 files[[6]][industry_code == val[6], industry_title,])
    return(results)
}

Indust <- function(val){
    return(gsub("^[0-9]+ |^NAICS [0-9]+", "", files[[6]][industry_code == val, industry_title,]))
}

payloadInit <- function(area, industry){
    iden <- c(seasonal  = "U",
              area      = area,
              datatype  = "4",
              size      = "0",
              ownership = "5")
    paste0(paste(c("EN", iden), collapse = ""), industry)
}

retrievePayload <- function(payVec, regKey){
    iden <- list('seriesid' = payVec,
                 'startyear' = 2006,
                 'endyear' = 2016,
                 'catalog' = FALSE,
                 'calculations' = TRUE,
                 'annualaverage' = TRUE,
                 'registrationKey' = regKey)
    iden <- blsAPI(iden, api.version = 2)
    return(iden)
}

## industries
industries <- c(10, 101, 1012, 1013, 102, 1021, 1022, 1023, 1024, 1025, 1026, 1027)
areas <- files[[2]][grepl(", Washington", area_title) | area_title == "Washington -- Statewide",]$area_fips

## all construction building jobs annual wage
payloads <- sapply(industries, function(industry){
    sapply(areas, function(area, industry){
        payloadInit(area, industry)
    }, industry = industry)
})

## dataAll <- lapply(payloads, retrievePayload, regKey = regKey)
## saveRDS(dataAll, "./data_output/payload.rds")

dataAll <- readRDS("./data_output/payload.rds")
dataAll[grepl("REQUEST_NOT_PROCESSED", dataAll)] <- NULL

dataAll <- lapply(dataAll,
                  gsub,
                  pattern = ".*\"Results\":\\{\n\"series\":\n\\[",
                  replacement = "")

dataAll <- lapply(dataAll,
                  gsub,
                  pattern = "\\]\n\\}\\}$",
                  replacement = "")

dataAll <- gsub(",$", "", paste0(paste0("    ", dataAll, ","), collapse = "\n"))
dataAll <- paste0("{\"Results\":{\n\"series\":[\n", dataAll, "]\n}}", collapse = "")

con <- file("./data_output/payload.json")
write(dataAll, con) 
close(con)

