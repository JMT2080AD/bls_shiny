rm(list = ls())

library(rjson)
library(data.table)
library(parallel)

payload <- fromJSON(file = "./data_output/payload.json")
area <- fread("./lookup_tables/qcew/area_titles.csv")
indu <- fread("./lookup_tables/qcew/industry_titles.csv")

parseSeries <- function(series){
    sid <- series$seriesID
    dat <- series$data
    dat <- lapply(dat, "[", c("year", "period", "periodName", "value"))
    dat <- rbindlist(lapply(dat, data.frame))
    if(nrow(dat) > 0){
        return(dat[,seriesID:=sid])
    }
    return(NA)
}

cl <- makeCluster(detectCores(), type = "FORK")
dat <- parLapply(cl, payload$Results$series, parseSeries)
stopCluster(cl)

dat[is.na(dat)] <- NULL
dat <- rbindlist(dat)

## get fips from series ID, add county name
dat[,area_fips:=substring(seriesID, 4, 8)]
dat[,industry_code:=gsub(".*405", "", seriesID)]
dat <- area[dat, on = "area_fips"]
dat <- indu[dat, on = "industry_code"]
areas <- area[grepl(", Washington", area_title) | area_title == "Washington -- Statewide",]$area_fips

## set column data types
dat[,':='(year = as.numeric(as.character(year)),
          period = as.character(period),
          periodName = as.character(periodName),
          value = as.numeric(as.character(value)))]

## write out rds
saveRDS(dat, "./data_output/washington_qcew.rds")
