# Aggregate safegraph poi visits to county by category visities.
library(data.table)
library(bit64)
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
aggregatecounty <- function(files) {
  dt <- data.table()
  columns <- c("safegraph_place_id",
               "date_range_start",
               "poi_cbg",
               "top_category",
               "visits_by_day")
  for (f in files) {
    cat("Loading ",f,"\n")
    load(f)
    monthdt <- monthdt[, ..columns]
    dt <- rbind(dt, monthdt)
  }
  dt[,paste("day",0:6,sep="") := tstrsplit(gsub("\\[|\\]","",visits_by_day), ",", type.convert=TRUE)]

  load(paste(rootdir,"data/homepanel.Rda",sep="/"))
  hp$ds <- as.Date(hp$date_range_start)
  dt$ds <- as.Date(dt$date_range_start)
  dt[,countyfips:=poi_cbg %/% 10000000]
  hp <- hp[,c("countyfips","devices_residing","ds")]
  dt <- merge(dt, hp, by=c("countyfips", "ds"), all.x=TRUE, allow.cartesian=TRUE)
  dt <- melt(dt, measure=patterns("day\\d"),
             variable.name="day", value.name="visits")
  dt[ , date := ds + as.numeric(gsub("day(\\d)","\\1",day)) ]
  countydt <- dt[,list(visits=sum(visits, na.rm=TRUE),
                       devices_residing=mean(devices_residing, na.rm=TRUE)),
                 by=list(countyfips, top_category, date)]
  cat(summary(countydt$devices_residing),"\n")
  return(countydt)
}

files <- dir(paste(rootdir, "data",sep="/"), pattern="schoolpatterns.+\\.Rda",
             full.names=TRUE)
school <- aggregatecounty(files)

files <- dir(paste(rootdir, "data",sep="/"), pattern="elderpatterns.+\\.Rda",
             full.names=TRUE)
elder <- aggregatecounty(files)

files <- dir(paste(rootdir, "data",sep="/"), pattern="gympatterns.+\\.Rda",
             full.names=TRUE)
gym <- aggregatecounty(files)

files <- dir(paste(rootdir, "data",sep="/"), pattern="brcpatterns.+\\.Rda",
             full.names=TRUE)
brc <- aggregatecounty(files)

sg <- rbind(elder, school, gym, brc)
setorder(sg, countyfips, date, top_category)
save(sg, file=paste(rootdir,"data/sg-county.Rda",sep="/"))
