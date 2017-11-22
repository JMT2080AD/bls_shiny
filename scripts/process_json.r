library(rjson)

payload <- fromJSON(file = "./data_output/payload.json")

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

dat <- lapply(payload$Results$series, parseSeries)
dat[is.na(dat)] <- NULL
dat <- rbindlist(dat)

saveRDS(dat, "./data_output/washington_qcew.rds")
