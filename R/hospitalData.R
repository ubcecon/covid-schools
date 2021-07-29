library(data.table)
#' Download and process "COVID-19 Reported Patient Impact and Hospital Capacity by Facility"
#' https://beta.healthdata.gov/Hospital/COVID-19-Reported-Patient-Impact-and-Hospital-Capa/anag-cw7u
#'
#' Important data notes:
#'
#' - "It does not include psychiatric, rehabilitation, Indian Health
#'    Service (IHS) facilities, U.S. Department of Veterans Affairs (VA)
#'    facilities, Defense Health Agency (DHA) facilities, and religious
#'    non-medical facilities."
#'
#' - "For a given entry, the term “collection_week” signifies the start
#'   of the period that is aggregated. For example, a “collection_week”
#'   of 2020-11-20 means the average/sum/coverage of the elements
#'   captured from that given facility starting and including Friday,
#'   November 20, 2020, and ending and including reports for Thursday,
#'   November 26, 2020."
#'
loadHospital <- function(downloaddata=FALSE,
                         rootdir=system("git rev-parse --show-toplevel", intern=TRUE)[1])
 {
  csvfile <- paste(rootdir,"data/hospital.csv", sep="/")
  if (downloaddata || !file.exists(csvfile)) {
    url  <- "https://beta.healthdata.gov/api/views/anag-cw7u/rows.csv?accessType=DOWNLOAD"
    download.file(url, destfile=csvfile)
  }
  hospital <- fread(csvfile)
  missingvalue <- -999999
  for(col in names(hospital)) {
    set(hospital, i=which(hospital[[col]]==missingvalue), j=col, value=NA)
  }
  hospital$collection_week <- as.Date(hospital$collection_week)
  return(hospital)
}


aggregateHospitalToCounty <- function(hospital) {
  admitvars <- names(hospital)[grep("previous_day_admission.+covid", names(hospital))]
  county <- hospital[ , lapply(.SD, function(x) sum(x, na.rm=TRUE)),
                     by=c("fips_code","collection_week"),
                     .SDcols=c("total_beds_7_day_avg",admitvars)]
  setnames(county, admitvars, gsub("previous_day_","",admitvars))
  nobs <- hospital[, lapply(.SD, function(x) sum(total_beds_7_day_avg[!is.na(x)], na.rm=TRUE)),
                   by=c("fips_code","collection_week"),
                 .SDcols=admitvars]
  setnames(nobs, admitvars, gsub("_7_day_sum","_beds",gsub("previous_day_","",admitvars)))
  county <- merge(county, nobs, by=c("fips_code","collection_week"))
  return(county)
}
