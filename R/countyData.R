library(bit64)
library(data.table)


#' Download and merge together various sources of US-county level Covid
#' related data.
#' @param redo If FALSE, will not redownload and recreate the data if
#' data already exists on disk. If TRUE, it will.
countyData <- function(redo=FALSE) {

  rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  datafile <- paste(rootdir,"data/covidCounty.Rda", sep="/")

  if (!redo && file.exists(datafile)) {
    cat("Loading data last updated on ", as.character(file.mtime(datafile)), "\n")
    cat("Call countyData(redo=TRUE) to update data.\n")
    load(datafile)
    return(covidCounty)
  }

  ## Data on state cases, deaths, etc from NYTimes
  nyt  <- read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", stringsAsFactors=FALSE)
  nyt$date <- as.Date(nyt$date)
  nyt <- subset(nyt, !is.na(fips))

  ################################################################################
  ## County Mask Mandate info from Wright, Chawla, Chen, & Farmer
  library(readxl)
  if (!(file.exists("mm.xlsx"))) {
    download.file("https://drive.google.com/uc?export=download&id=1qVIhPaBQ-apdDjOaKV2eA9SgZNkLMLAm",destfile="mm.xlsx")
  } else {
    warning("mm.xlsx exists. Not redownloading mask mandate data.")
  }

  mask <- data.frame(read_excel("mm.xlsx", col_names=FALSE))
  names(mask) <- c("state.fips", # 1
                   "state",      # 2
                   "fips",       # 3
                   "county",     # 4
                   "start_county", # 5
                   "end_county",   # 6
                   "scope_county", # 7
                   "source_county", #8
                   "note1_county", # 9
                   "note2_county", # 10
                   "code1", # 11  (this is some stata encoding of a date, I think)
                   "start_state", # 12
                   "end_state",  # 13
                   "scope_state", # 14
                   "source_state", # 15
                   "code2",
                   "code3",
                   "code4",
                   "start_mask") # min(start_county, start_state)

  for (v in grep("start|end",names(mask))) {
    # let's enter dates in 3 different formats for some reason ...
    if (is.character(mask[,v])) {
      if (any(grepl("/",mask[,v])))
      mask[,v] <- as.Date(mask[,v],format="%m/%d/%y")
      else if (any(grepl("-",mask[,v])))
        mask[,v] <- as.Date(mask[,v],format="%m-%d-%y")
    } else
      mask[,v] <- as.Date(mask[,v])
  }
  stopifnot(all(pmin(mask$start_county, mask$start_state) == mask$start_mask, na.rm=TRUE))

  ## Corrections and additions to mask data

  #unique(mask$state[!is.na(mask$end_state)])
  # only AL, CO, DC, KY, LA, MS, NC, & RI  have end dates. I checked on
  # 2020-09-10, and all state orders were extended at least to the present.


  #unique(mask$state[is.na(mask$start_state)])
  # Check against
  # https://www.aarp.org/health/healthy-living/info-2020/states-mask-mandates-coronavirus.html
  # Following mandates added more recently
  mask[mask$state=="Mississippi","start_state"] <- as.Date("2020-08-04")
  mask[mask$state=="Montana","start_state"] <- as.Date("2020-07-15") # https://covid19.mt.gov/Portals/223/Documents/Mask%20Directive%20FINAL.pdf?ver=2020-07-15-140109-633
  mask[mask$state=="Vermont","start_state"] <- as.Date("2020-08-01")
  mask[mask$state=="Wisconsin","start_state"] <- as.Date("2020-08-01")
  # Texas mandated on July 2, but counties with < 20 cases can get
  # exemption. County level dates appear to be accurate.

  for (v in grep("_",names(mask))) {
    n <- names(mask)[v]
    names(mask)[v] <- paste("mask",n,sep="_")
  }
  mask$mask_start <- pmin(mask$mask_start_county, mask$mask_start_state, na.rm=TRUE)
  mask <- mask[,!(names(mask) %in%
                  c("state","code1","code2","code3","code4","county"))]

  covidCounty <- merge(mask, nyt, by=c("fips"), all=TRUE)
  ################################################################################

  # Data from Yu group on county characteristics etc
  yu <- read.csv("https://raw.githubusercontent.com/Yu-Group/covid19-severity-prediction/master/data/county_data_abridged.csv",stringsAsFactors=FALSE)

  # more up to date policy data with rollback dates
  yup <-
    read.csv("https://raw.githubusercontent.com/JieYingWu/COVID-19_US_County-level_Summaries/master/data/interventions.csv", stringsAsFactors=FALSE)

  # convert dates from Gregorian ordinals (days since 0001-01-01, starting from 1)
  origindate = "0000-12-31"
  stopifnot(as.Date(737446, origin=origindate)=="2020-01-22")

  policyvars <- names(yup)[4:length(yup)]
    #c("stay.at.home","X.50.gatherings","X.500.gatherings","public.schools",
    #  "restaurant.dine.in","entertainment.gym","federal.guidelines",
    #  "foreign.travel.ban")
  for (v in policyvars) {
    yup[,v] <- as.Date(yup[,v], origin=origindate)
  }


  yu <- yu[,!(names(yu) %in% policyvars)]
  yu$FIPS <- as.numeric(yu$countyFIPS)
  yu <- subset(yu, !is.na(yu$FIPS)) # just two rows of all NA

  #df <- merge(yu,yup, by="FIPS", all=TRUE)
  # There are 25 FIPS codes in the yu data not in yup. These all
  # appear to be related to changes in FIPS codes over time, see e.g.
  # https://www.cdc.gov/nchs/data/oae/Country_Geography.pdf
  # and
  # https://www.ddorn.net/data/FIPS_County_Code_Changes.pdf
  # We drop these observations
  #
  # The yup data also has state level codes. We drop these as well
  df <- merge(yu, yup, by="FIPS", all=FALSE)

  tmp <- merge(covidCounty, df, by.x="fips", by.y="FIPS", all.x=TRUE)

  # fix missing state fips
  x <- floor(tmp$fips/1000)
  stopifnot(all(x==tmp$state.fips, na.rm=TRUE))
  tmp$state.fips <- x

  # drop redundant variables
  covidCounty <- tmp[, !(names(tmp) %in%
                         c("countyFIPS","STATE","AREA_NAME","COUNTYFP","STATEFP","CountyName",
                           "StateName","State"))]

  ################################################################################

  ## Google Mobility Reports
  # URL <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"
  # getOption('timeout')
  # options(timeout=400)
  # download.file(URL, destfile = "./google.csv")
  # gmr <- read.csv("google.csv", stringsAsFactors=FALSE)
  gmr <- read.csv("https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv", stringsAsFactors=FALSE)
  gmr <- subset(gmr, country_region_code=="US")
  #gmr <- read.csv("2020_US_Region_Mobility_Report.csv", stringsAsFactors=FALSE)
  gmr <- subset(gmr, sub_region_2!="") # get county level data
  gmr$date <- as.Date(gmr$date, "%Y-%m-%d")
  drops <-
    c("country_region_code","country_region","sub_region_1","sub_region_2",
      "iso_3166_2_code","metro_area")
  gmr <- gmr[, !(names(gmr) %in% drops)]

  tmp <- merge(covidCounty, gmr, by.x=c("fips","date"),
               by.y=c("census_fips_code","date"), all.x=TRUE)

  covidCounty <- subset(tmp, !is.na(tmp$date))
  ################################################################################

  ## School reopening
  # District reopening information from edweek,
  # https://www.edweek.org/ew/section/multimedia/school-districts-reopening-plans-a-snapshot.html
  # fill out form to download file
  file <- paste(rootdir, "data/District_Reopening_Data.xlsx",sep="/")
  schools <- data.frame(read_excel(file, sheet=2))
  warning(paste("School reopening data last updated on",
                max(schools$Last.verified),
                "\nGoto https://www.edweek.org/ew/section/multimedia/school-districts-reopening-plans-a-snapshot.html to download more recent data"))


  # link between school districts and counties from census
  url <-
    "https://www2.census.gov/programs-surveys/saipe/guidance-geographies/districts-counties/sdlist-20.xls"
  file <- "sdlist-20.xls"
  if (!(file.exists(file))) {
    download.file(url, destfile=file)
  } else {
    warning(paste(file,"exists. Not redownloading."))
  }
  sd2c <- data.frame(read_excel(file, skip=2))
  sd2c$NCES <- paste(sd2c$State.FIPS,sd2c$District.ID.Number, sep="")
  sd2c$County.FIPS <- gsub("[^0-9.-]", "", sd2c$County.FIPS) # Some mistaken letters in a few rows

  # Fix some apparent data entry errors. Some school districts have
  # the wrong first two digits in NCES for their state
  schools$stfips.orig <- substr(schools$NCES,1,2)
  schools$District.ID <- substr(schools$NCES,3,7)
  stfips <- unique(sd2c[,c("State.Postal.Code","State.FIPS")])
  stmp <- merge(schools, stfips, by.x="State", by.y="State.Postal.Code", all.x=TRUE)
  stmp$State.FIPS[is.na(stmp$State.FIPS)] <-
    stmp$stfips.orig[is.na(stmp$State.FIPS)]
  stmp$NCES.orig <- stmp$NCES
  stmp$NCES <- paste(stmp$State.FIPS, stmp$District.ID, sep="")
  schools <- stmp[, !(names(stmp) %in% c("State.FIPS"))]

  # Remaing manually checked corrections
  schools$NCES[schools$District=="Detroit Public Schools"] <-
    "2612000"
  schools$NCES[schools$District=="Addison Northwest Unified School District"]<-
    "5099902"
  schools$NCES[schools$District=="Addison Central Unified School District"]<-
    "5099903"
  schools$NCES[schools$District=="Champlain Valley Unified School District"]<-
    "5099914"
  schools$NCES[schools$District=="MT. Abraham Unified School District"]<-
    "5099901"
  schools$NCES[schools$District=="North Colonie Csd"]<-
    "3600971"
  schools$NCES[schools$District=="Bob Hope School"]<-
    "4835400"
  schools$NCES[schools$District=="New York City"]<-
    "3620580"

  tmp <- merge(schools, sd2c, by="NCES",all.y=TRUE) #, all=TRUE)
  tmp$FIPS <- paste(tmp$State.FIPS, tmp$County.FIPS, sep="")

  # create enrollment weighted average strategy and plan
  library(data.table)
  tmp$one <- 1
  plans <- unique(tmp$District.reopening.plan)
  dt <- setDT(tmp)
  sc <- dt[, c(paste("start.",plans, sep=""),  paste("portion.",plans, sep="")) :=
               c(lapply(plans, function(p) weighted.mean(x=Start.date[District.reopening.plan==p],
                                                       w=Public.School.Enrollment[District.reopening.plan==p])),
                 lapply(plans, function(p)
                   sum(Public.School.Enrollment[District.reopening.plan==p],
                       na.rm=TRUE)/
                   sum(Public.School.Enrollment))),
           by=.(FIPS)]
  usc <- unique(setDF(sc)[,c("FIPS",paste("start.",plans, sep=""),
                             paste("portion.",plans, sep=""))])
  usc$school.reopen.info <- TRUE
  #                 .SDcols=c("Start.date")]
  usc$fips <- as.integer(usc$FIPS)
  for  (v in grep("start\\.",names(usc))) {
    usc[,v] <- as.Date(usc[,v])
  }

  covidCounty <- merge(covidCounty, usc, by="fips", all.x=TRUE)

  # visits to school & nursing homes from safegraph
  if (!file.exists(paste(rootdir,"data/sg-county.Rda",sep="/"))) {
    source(paste(rootdir,"R/sg-tocounty.R",sep="/"))
  }
  load(paste(rootdir,"data/sg-county.Rda",sep="/"))
  wsg <- dcast(sg, countyfips + date + devices_residing ~ top_category, value.var = "visits")
  setnames(wsg, old=c("Colleges, Universities, and Professional Schools",
                      "Continuing Care Retirement Communities and Assisted Living Facilities for the Elderly",
                      "Elementary and Secondary Schools",
                      "Nursing Care Facilities (Skilled Nursing Facilities)",
                      "Other Amusement and Recreation Industries",
                      "Drinking Places (Alcoholic Beverages)",
                      "Religious Organizations",
                      "Restaurants and Other Eating Places"
                      ),
           new = c("college_visits",
                   "assistedliving_visits",
                   "school_visits",
                   "nursing_visits",
                   "gym_visits",
                   "bar_visits",
                   "church_visits",
                   "restaurant_visits")
           )
  for (i in names(wsg)[grep("_visits",names(wsg))]) {
    wsg[is.na(get(i)), (i):=0]
  }

  covidCounty <- data.table(covidCounty)
  cat(max(covidCounty$date),"\n")
  cat(max(wsg$date),"\n")
  covidCounty <- merge(covidCounty, wsg, by.x=c("fips","date"), by.y=c("countyfips","date"),
                       all=TRUE)
  cat(max(covidCounty$date),"\n")

  # safegraph social distancing measures
  load(paste(rootdir,"data/sg-distancing.Rda",sep="/"))
  sd$fips <- as.numeric(sd$fips)
  covidCounty <- merge(covidCounty, sd, by=c("fips","date"), all.x=TRUE)

  cdc <- cdcCaseData()
  countyage <- aggregateToCounty(cdc)

  tmp <- merge(covidCounty, countyage,  by.x=c("fips","date"),
               by.y=c("county_fips_code","cdc_case_earliest_dt"),
               all.x=TRUE)
  cat(max(tmp$date),"\n")
  # balance the panel
  tmp <- setDT(tmp, key = c("fips", "date"))[CJ(fips, date, unique=TRUE)]
  tmp[,newcases := cases - shift(cases), by=fips]
  setnames(tmp, "cases_cdc","newcases_cdc")
  tmp[is.na(newcases_cdc), newcases_cdc := 0]
  tmp[,cases_cdc := cumsum(newcases_cdc), by=fips]
  cat(max(tmp$date),"\n")

  covidCounty <- tmp
  save(covidCounty, file=datafile)
  return(covidCounty)
}


rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
#' Load data.table of CDC restricted use case data
cdcCaseData <- function(infile=c(paste(rootdir,
                                       "data/COVID_Cases_Restricted_Detailed_12312020_Part_1.csv.zip",
                                       sep="/"),
                                 paste(rootdir,
                                       "data/COVID_Cases_Restricted_Detailed_12312020_Part_2.csv.zip",
                                       sep="/"))
                                 ) {
  if (!all(file.exists(infile))) {
    error(sprintf("%s not found. Obtain it from the CDC restricted use git repo", infile))
  }
  dt1 <- data.table::fread(cmd=sprintf("unzip -p %s",infile[1]))
  dt2 <- data.table::fread(cmd=sprintf("unzip -p %s",infile[2]))
  names(dt2) <- names(dt1)
  dt  <- rbind(dt1, dt2)
  cols <-  c(names(dt)[grep("_yn", names(dt))],"current_status","race_ethnicity_combined","sex","age_group")
  dt[,(cols):=lapply(.SD, as.factor),.SDcols=cols]
  return(dt)
}

#' aggregate cdc case data to county level
aggregateToCounty <- function(cases) {
  countyage <- cases[,list(cases=.N),
                  by=list(cdc_case_earliest_dt, county_fips_code, age_group)]
  countyage[order(county_fips_code, age_group, cdc_case_earliest_dt)]
  countyage <- na.omit(countyage, cols=c("county_fips_code","cdc_case_earliest_dt"))
  wide <- dcast(countyage, cdc_case_earliest_dt + county_fips_code ~ age_group ,
                value.var="cases")
  for (i in seq(0,70,10)) {
    setnames(wide, sprintf("%d - %d Years", i, i+9),
             sprintf("cases%d_%d",i, i+9))
    wide[is.na(get(sprintf("cases%d_%d",i, i+9))),
         (sprintf("cases%d_%d",i, i+9)) := 0]
  }
  setnames(wide, "80+ Years","cases80plus")
  wide[is.na(cases80plus),cases80plus := 0]
  wide[,Unknown:=NULL]
  wide[,("NA"):=NULL]

  countytotal <- cases[,list(cases=.N),
                       by=list(cdc_case_earliest_dt, county_fips_code)]
  countytotal <- na.omit(countytotal, cols=c("county_fips_code","cdc_case_earliest_dt"))
  countytotal[order(county_fips_code, cdc_case_earliest_dt)]
  setnames(countytotal,"cases","cases_cdc")
  out <- merge(wide, countytotal, by=c("county_fips_code","cdc_case_earliest_dt"),
               all=TRUE)
  return(out)
}
