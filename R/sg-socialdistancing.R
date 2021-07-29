library(SafeGraphR)
library(data.table)
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
recreate  <- FALSE


updatesafegraph <- function(keyfile, path) {
  if (!file.exists(keyfile)) {
    error("safegraph key information not found. Go to
http://catalog.safegraph.io/ , get an AWS ID and Key and save them in a text file named \"data/safegraph.key\". The file should have the id string on the first line and the key on the second.") }
  foo <- read.delim(keyfile,header=FALSE)
  safegraph_aws(path=path,dataset="distancing", key=foo[1,1],
                secret=foo[2,1], prefix="202")

}

datadir <- paste(rootdir,"data/distancing",sep="/")
if (!dir.exists(datadir)) dir.create(datadir)
if (downloaddata) {
  keyfile <- paste(rootdir,"data/safegraph.key", sep="/")
  updatesafegraph(keyfile, datadir)
}


mergefiles <- function(files) {
  dt  <-  data.table()
  for (f in files) {
    cat("Working on ",f,"\n")
    new  <- fread(f,
                  select = c("origin_census_block_group",
                             "date_range_start",   "date_range_end",
                             "device_count",
                             "completely_home_device_count",
                             "full_time_work_behavior_devices",
                             "part_time_work_behavior_devices"))
    #,"mean_home_dwell_time", "mean_non_home_dwell_time")
    new$date <- median(as.Date(new$date_range_start))
    new[,c("date_range_start","date_range_end"):=NULL]
    new[,c('state_fips','county_fips') := fips_from_cbg(origin_census_block_group)]
    cty <- new[, lapply(.SD,sum), by=.(county_fips,state_fips, date)]
    cty[,origin_census_block_group := NULL]
    dt <- rbind(dt, cty)
  }
  return(dt)
}

if (recreate) {
  sd <- mergefiles(dir(datadir, recursive=TRUE, full.name=TRUE))
} else {
  file=paste(rootdir,"data/sg-distancing.Rda",sep="/")
  load(file)
  dates <- unique(sd$date)
  allfiles <- dir(datadir, recursive=TRUE, full.name=TRUE)
  filedate <- function(f) {
    m <- regexec("/(\\d\\d\\d\\d-\\d\\d-\\d\\d)-",f)
    regmatches(f,m)[[1]][2]
  }
  newfiles <- allfiles[!(as.Date(sapply(allfiles, FUN=filedate)) %in% dates)]
  newsd  <- mergefiles(newfiles)
  sd <- rbind(sd, newsd)
}

save(sd, file=paste(rootdir,"data/sg-distancing.Rda",sep="/"))
