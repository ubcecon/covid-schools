#' Download and clean data on schools from SafeGraph
#'

# library(remotes)
# remotes::install_github('SafeGraphInc/SafeGraphR')
library(SafeGraphR)

updatesafegraph <- function(keyfile) {
  if (!file.exists(keyfile)) {
    error("safegraph key information not found. Go to
http://catalog.safegraph.io/ , get an AWS ID and Key and save them in a text file named \"data/safegraph.key\". The file should have the id string on the first line and the key on the second.") }
  foo <- read.delim("safegraph.key",header=FALSE)
  safegraph_aws(dataset="weekly-new", key=foo[1,1],
                secret=foo[2,1])
  safegraph_aws(dataset="core", key=foo[1,1],
                secret=foo[2,1])
  #safegraph_aws(dataset="weekly", key=foo[1,1],
  #              secret=foo[2,1], prefix="main-file/202")
  #safegraph_aws(dataset="weekly", key=foo[1,1],
  #              secret=foo[2,1], prefix="home-summary-file/202")
  #safegraph_aws(dataset="weekly", key=foo[1,1],
  #              secret=foo[2,1], prefix="normalization-stats/202")
}



rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
original.wd <- getwd()
downloaddata  <- TRUE

if (downloaddata) {
  tryCatch(
  {
    setwd(paste(rootdir,"data",sep="/"))
    keyfile <- paste(rootdir,"data/safegraph.key", sep="/")
    updatesafegraph(keyfile)
  },
  finally= {
    setwd(original.wd)
  } )
}


################################################################################
library(data.table)
library(bit64)

schoolcats <- c("Colleges, Universities, and Professional Schools",
                "Elementary and Secondary Schools")
eldercats <- c("Nursing Care Facilities (Skilled Nursing Facilities)",
               "Continuing Care Retirement Communities and Assisted Living Facilities for the Elderly")

extractcategories <- function(categories, prefix, recreate=FALSE, subcategories=NULL) {

  datadir <- paste(rootdir,"data/2020/09/",sep="/")

  extractpois  <- function(categories, datadir, subcategories) {
    files <- list.files(datadir)
    files <- files[grep("core.+\\.csv\\.gz",list.files(datadir))]
    poi <- data.table()
    for (f in files) {
      new <- fread(paste(datadir,f,sep="/"),
                   select=c("safegraph_place_id","top_category","naics_code",
                            "sub_category"))
      if (is.null(subcategories)) {
        new <- new[top_category %in% categories]
      } else {
        new <- new[top_category %in% categories &
                   sub_category %in% subcategories]
      }
      poi <- rbind(poi,new)
    }
    return(poi)
  }

  poi <- extractpois(categories, datadir, subcategories)


  loadweek <- function(datadir, poi) {
    files <- list.files(datadir)
    files <- files[grep(".*patterns.*\\.csv\\.gz",list.files(datadir))]
    week <- data.table()
    setkey(poi, safegraph_place_id)
    for (f in files) {
      new <- fread(paste(datadir, f, sep="/"),
                 drop=c("related_same_day_brand","related_same_week_brand",
                        "safegraph_brand_ids","brands","iso_country_code"))
      setkey(new, safegraph_place_id)
      new <- new[poi, nomatch=0]
      week <- rbind(week,new)
    }
    return(week)
  }

  for (year in 2020:2021) {
    if (year<=2020) {
      # use backfilled data when available
      path <- paste(rootdir,"data/patterns_backfill/2020/12/14/21/",year,sep="/")
      months <- sprintf("%02d",1:12) #dir(path)
    } else {
      path <- paste(rootdir,"data/patterns",year,sep="/")
      months <- dir(path)
    }
    for (mm in months) {
      mfile <- sprintf("%s/data/%spatterns_%4d_%s.Rda",rootdir, prefix, year, mm)
      if (!file.exists(mfile) || recreate || (mm==max(months) && year==2021)) {
        monthdt <- data.table()
        cat(sprintf("Loading weekly patterns data for month %s", mm),"\n")
        if (year==2020 && mm=="12") {
          path <- paste(rootdir,"data/patterns",year,sep="/")
          days <- dir(paste(path,mm,sep="/"))
          days <- days[as.numeric(days)>=16]
        } else {
          days <- dir(paste(path,mm, sep="/"))
        }
        for (dd in days) {
          xx <- dir(paste(path,mm,dd, sep="/"))
          if (any(grepl(".csv.gz",xx))) {
            datadir <- paste(path,mm,dd,sep="/")
          } else {
            datadir <- paste(path,mm,dd,xx, sep="/")
          }
          startt <- Sys.time()
          monthdt <- rbind(monthdt, loadweek(datadir, poi), fill=TRUE)
          endt <- Sys.time()
          cat("Loading ", datadir, " took", endt-startt, "\n")
        }
        save(monthdt, file=mfile)
      } else {
        warning(sprintf("Patterns data for %4d/%s already exists in %s.",year, mm, mfile))
      }
    }
  }

}


extractcategories(schoolcats, "school", recreate=TRUE)
extractcategories(eldercats, "elder", recreate=TRUE)
extractcategories("Other Amusement and Recreation Industries", "gym",
                  recreate=TRUE,
                  subcategories="Fitness and Recreational Sports Centers")
extractcategories(c("Drinking Places (Alcoholic Beverages)",
                    "Restaurants and Other Eating Places",
                    "Religious Organizations"),
                  "brc", recreate=TRUE)
