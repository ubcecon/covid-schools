## Load home panel files, rbind them, and aggregate from CBG to county
# Merge home panel summaries
library(data.table)

rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
loadcountyhomepanel <- function(datadir, pattern=".*home[-|_]panel[-|_]summary\\.csv") {
  filesall <- list.files(datadir, pattern=pattern, full.names=TRUE, recursive=TRUE)
  files <- c(
      # use backfill from 2020-11-30 and earlier
      filesall[grepl("backfill/2020",filesall)],
      filesall[grepl("summary/2020/12/(16|30)",filesall)],
      filesall[grepl("summary/2021",filesall)])
  dt <- data.table()
  for (f in files) {
    cat(f,"\n")
    new  <- fread(f)
    new[,countyfips:=census_block_group %/% 10000000]
    newc <- new[,list(devices_residing=sum(number_devices_residing)),
                by=list(countyfips,date_range_start, date_range_end)]
    dt <- rbind(dt, newc)
    cat(".\n")
  }
  return(dt)
}

mfile <- sprintf("%s/data/homepanel.Rda",rootdir)
recreate <- TRUE
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
if (!file.exists(mfile) || recreate) {
  hp <- loadcountyhomepanel(paste(rootdir,"data",sep="/"))
  save(hp, file=mfile)
} else {
  warning(sprintf("homepanel.Rda already exists"))
}
