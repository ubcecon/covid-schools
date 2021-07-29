library(plm)
library(readxl)
library(tidyverse)
library(data.table)
library(dplyr)
library(zoo)


#' Create variables and do other data processing in preparation for
#' regressions and other analysis
#' @param df Dataframe of covid county data, as created by
#' countyData.R
#' @param difflength Lag length for differences for computing changes
#' and growth rates
#' @param policymalength Length of window for taking moving average of
#' policy indicators
dataprep <- function(df, difflength=7, policymalength=6, redo=FALSE) {

  rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]
  source(paste(rootdir,"R/utils.R", sep="/"))
  source(paste(rootdir,"R/varlabels.R", sep="/"))
  source(paste(rootdir,"R/hospitalData.R",sep="/"))

  # hospital <- loadHospital(downloaddata=redo, rootdir=rootdir) #  updating the hospital data
  # zipdf <- data.frame(read_excel(paste(rootdir,"data/ZIP_COUNTY_122020.xlsx",sep="/")))
  # zipdf <- zipdf[,1:2]
  # # drop territory and associate
  # state_name <- unique(hospital$state[which(hospital$fips_code>=60000)])
  # for (st in state_name){
  #   hospital <- hospital[which(hospital$state!=st),]
  # }
  # # there are many hospitals with missing county fips but zip codes are available
  # # use zip code information to fill in county fips when county fips == NA
  # zipdf$COUNTY <- as.integer(zipdf$COUNTY)
  # hospital$zip <- as.integer(hospital$zip)
  # state_name <- unique(hospital$state)
  # for (j in 1:length(state_name)){
  #   st <- state_name[j]
  #   if (any(is.na(hospital$fips_code[which(hospital$state==st)]))){
  #     # print(st)
  #     zip_name <- unique(hospital$zip[which(hospital$state==st&is.na(hospital$fips_code))])
  #     for (z in zip_name){
  #       idx <- which(zipdf$ZIP==z)
  #       if (length(idx)>0){
  #         hospital$fips_code[which(hospital$state==st&hospital$zip==z&is.na(hospital$fips_code))] <- zipdf$COUNTY[idx]
  #       }
  #     }
  #   }
  # }
  # hospital <- aggregateHospitalToCounty(hospital)
  # # rename variables
  # hospital <- hospital %>%
  #   rename(
  #     bed_h = total_beds_7_day_avg,
  #     adm_cfmd = admission_adult_covid_confirmed_7_day_sum,
  #     adm_cfmd_18 = `admission_adult_covid_confirmed_18-19_7_day_sum`,
  #     adm_cfmd_20 = `admission_adult_covid_confirmed_20-29_7_day_sum`,
  #     adm_cfmd_30 = `admission_adult_covid_confirmed_30-39_7_day_sum`,
  #     adm_cfmd_40 = `admission_adult_covid_confirmed_40-49_7_day_sum`,
  #     adm_cfmd_50 = `admission_adult_covid_confirmed_50-59_7_day_sum`,
  #     adm_cfmd_60 = `admission_adult_covid_confirmed_60-69_7_day_sum`,
  #     adm_cfmd_70 = `admission_adult_covid_confirmed_70-79_7_day_sum`,
  #     adm_cfmd_80 = `admission_adult_covid_confirmed_80+_7_day_sum`,
  #     adm_cfmd_unknown = admission_adult_covid_confirmed_unknown_7_day_sum,
  #     adm_cfmd_ped = admission_pediatric_covid_confirmed_7_day_sum,
  #     adm_susp = admission_adult_covid_suspected_7_day_sum,
  #     adm_susp_18 = `admission_adult_covid_suspected_18-19_7_day_sum`,
  #     adm_susp_20 = `admission_adult_covid_suspected_20-29_7_day_sum`,
  #     adm_susp_30 = `admission_adult_covid_suspected_30-39_7_day_sum`,
  #     adm_susp_40 = `admission_adult_covid_suspected_40-49_7_day_sum`,
  #     adm_susp_50 = `admission_adult_covid_suspected_50-59_7_day_sum`,
  #     adm_susp_60 = `admission_adult_covid_suspected_60-69_7_day_sum`,
  #     adm_susp_70 = `admission_adult_covid_suspected_70-79_7_day_sum`,
  #     adm_susp_80 = `admission_adult_covid_suspected_80+_7_day_sum`,
  #     adm_susp_unknown = admission_adult_covid_suspected_unknown_7_day_sum,
  #     adm_susp_ped = admission_pediatric_covid_suspected_7_day_sum)
  # # we focus on the sum of confirmed and suspeted admission by age group
  # hospital$adm <- hospital$adm_cfmd + hospital$adm_susp
  # vars <- c("18","20","30","40","50","60","70","80","unknown","ped")
  # for (v in vars){
  #   vnew <- paste0("adm_",v)
  #   v1 <- paste0("adm_cfmd_",v)
  #   v2 <- paste0("adm_susp_",v)
  #   hospital[,vnew] <- hospital[,..v1] + hospital[,..v2]
  # }
  # hospital$adm_total <- hospital$adm_80 + hospital$adm_70 + hospital$adm_60 + hospital$adm_50 + hospital$adm_40 +
  #                       hospital$adm_30 + hospital$adm_20 + hospital$adm_18 + hospital$adm_ped +  hospital$adm_unknown
  # # collection_week represents a given date are for the week **beginning** that date
  # # we define the ``date'' for which admissions are the 7 days sum in the last 7 days.
  # hospital$date <- as.Date(hospital$collection_week) + 7
  # hospital$fips <- hospital$fips_code
  # vars <- c("fips","date","adm","adm_total")
  # hospital <- hospital[,..vars]
  # sdf <- df[,c("fips","date")]
  # sdf <- merge(sdf,hospital,by=c("fips","date"), all.x=TRUE)
  # sdf$fips <- as.integer(sdf$fips)
  # # impute admissions
  # vars <- c("adm","adm_total")
  # for (v in vars){
  #   sdf[,v] <- lapply(sdf[,..v],function(x){as.double(x)})
  # }
  # fipsname <- unique(sdf$fips)
  # for (v in fipsname){
  #   if (any(!is.na(sdf$adm[sdf$fips==v]))) {
  #     sdf_j <- sdf[sdf$fips==v,]
  #     sdf_j$adm <- na.approx(sdf_j$adm, maxgap = 8, rule = 2)
  #     sdf_j$adm_total <- na.approx(sdf_j$adm_total, maxgap = 8, rule = 2)
  #     sdf[sdf$fips==v,] <- sdf_j
  #   }
  # }
  # df <- merge(df,sdf,by=c("fips","date"),all.x=TRUE)
  # rm(hospital,sdf,sdf_j)

  # # Merge vaccination data
  # # county-level
  # datafile <- paste(rootdir,"data/vaccinations.Rda", sep="/")
  # load(datafile)
  # df$date <- as.Date(df$date)
  # # california
  # ca  <- read.csv("https://raw.githubusercontent.com/datadesk/california-coronavirus-data/master/cdph-vaccination-county-totals.csv", stringsAsFactors=FALSE)
  # ca$date <-  as.Date(ca$date, format = "%Y-%m-%d")
  # ca$fips <- ca$fips + 6000
  # ca <- ca[,c("fips","date","doses_administered")]
  # vdf <- merge(v_df, ca, by=c("fips","date"),all=TRUE)
  # vdf$total_dose[which(vdf$fips<7000)] <- vdf$doses_administered[which(vdf$fips<7000)] # california's total dose
  # vdf <- vdf[,c("fips","date","doses_administered","one_dose", "one_dose_pct", "full_dose", "full_dose_pct","total_dose")]
  # df <- merge(df, vdf, by=c("fips","date"), all=TRUE)
  # # state-level
  # datafile <- paste(rootdir,"data/vst.Rda", sep="/")
  # load(datafile)
  # vst_df <- vst_df[,c("state.fips","date","one_dose_st", "one_dose_st_pct", "full_dose_st", "full_dose_st_pct","total_dose_st")]
  # df <- merge(df, vst_df, by=c("state.fips","date"), all=TRUE)
  # # df <- df[which(state.fips<60),]
  # # unique(df$state.fips)
  # rm(vdf,v_df,vst_df,ca)

  # Merging more data
  ## Covid Tracking project data on tests and hospitalizations at state-level
  ctp  <- read.csv("https://covidtracking.com/api/v1/states/daily.csv", stringsAsFactors=FALSE)
  # Convert date to  correct format
  year <- ctp$date %/% 1e4
  month <- (ctp$date %% 1e4)  %/% 100
  day  <-  (ctp$date %% 1e4) %% 100
  date <- as.Date(sprintf("%d-%d-%d", year, month, day))
  ctp$date <- date
  names(ctp)[names(ctp)=="positive"] <- "cases.ctp"
  names(ctp)[names(ctp)=="death"] <- "deaths.ctp"
  names(ctp)[names(ctp)=="recovered"] <- "recovered.ctp"
  names(ctp)[names(ctp)=="state"] <- "ST"
  names(ctp)[names(ctp)=="fips"] <- "state.fips"
  names(ctp)[names(ctp)=="totalTestResults"] <- "tests"
  #ctp <- ctp[,c("state.fips","date","cases.ctp","deaths.ctp","ST","tests")]
  ctp <- ctp[,c("state.fips","date","tests")]
  df <- merge(df, ctp[ctp$state.fips<60,], by=c("state.fips","date"), all=TRUE)

  # ## Trump voting share in 2016
  # vote <- read.csv(paste(rootdir,"data/countypres_2000-2016.csv", sep="/"))
  # vote <- subset(vote,vote$party=="republican" & vote$year == 2016)
  # vote$trump <- vote$candidatevotes/vote$totalvotes
  # vote <- vote[,c("FIPS","trump")]
  # df <- merge(df, vote, by.x="fips", by.y="FIPS")

  ## mask wearing data from NYT
  # nyt  <- read.csv(https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv)
  # nyt <- read.csv(paste(rootdir,"data/mask-use-by-county.csv", sep="/"))
  # df <- merge(df, nyt, by.x="fips", by.y="COUNTYFP", all.x=TRUE)


  ################################################################################
  # District reopening information from https://www.mchdata.com ,
  schools <- read.csv(paste(rootdir,"data/covid-school-data.csv", sep="/"))
  schools$OpenDate <- as.Date(schools$OpenDate,format ="%m/%d/%Y")
  # delete Distriction observation with NCES == NA or Enrollment = NA
  # schools <- subset(schools, !is.na(schools$DistrictNCES) & !is.na(Enrollment) )
  # schools <- subset(schools, !is.na(schools$DistrictNCES) & !is.na(Enrollment) )

  # # The following is a possible alternative data source for county-level information
  # edge <- data.frame(read_excel(paste(rootdir,"data/EDGE_GEOCODE_PUBLICLEA_1819.xlsx",sep="/")))
  # edge$DistrictNCES <- as.integer(edge$LEAID)
  # edge <- subset(edge,edge$STFIP<60)
  # tmp <- merge(schools, edge, by="DistrictNCES",all.y=TRUE)
  # tmp <- subset(tmp,tmp$STFIP<60)
  # length(unique(tmp$DistrictNCES))   # 14703
  # print(length(unique(tmp$STFIP))) # 51
  # length(unique(tmp$CNTY)) # 3124 # CNTY does not seem to be the same as county fips
  # tmp$FIPS <- tmp$CNTY

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
  sd2c$FIPS <- paste(sd2c$State.FIPS, sd2c$County.FIPS, sep="")

  # Some states are missing because of miscoding of District NCES as follows:
  # tmp2 <- merge(schools, sd2c, by.x="DistrictNCES", by.y="NCES")
  # setdiff(unique(sd2c$State.Postal.Code), unique(tmp2$State.Postal.Code))
  #[1] "AL" "AK" "AZ" "AR" "CA" "CO" "CT"
  miss  <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT")
  schools$NCES <- as.character(schools$DistrictNCES)
  for (v in miss){
    tmp3 <- subset(schools,schools$PhysicalState==v)
    schools$NCES[schools$PhysicalState==v] <- paste("0",as.character(tmp3$NCES),sep = "")
  }
  tmp <- merge(schools, sd2c, by="NCES",all.y=TRUE) # ``HI'' has one school district for multiple counties and so need to keep county information.
  # length(unique(tmp$FIPS)) = 3142
  # length(unique(tmp$State.FIPS)) = 51
  # length(unique(tmp$NCES)) = 13183


  # create enrollment weighted average strategy and plan for each county
  library(data.table)
  tmp$one <- 1
  plans <- unique(tmp$TeachingMethod)
  sports <- unique(tmp$SportsParticipation)
  staffmask <- unique(tmp$StaffMaskPolicy)
  studentmask <- unique(tmp$StudentMaskPolicy)
  online <- unique(tmp$OnlineInstructionIncrease)
  homeschool <- unique(tmp$ParentOptOutClassroomTeaching)
  # regroup some categories
  tmp$TeachingMethod[which(tmp$TeachingMethod=='Other'|tmp$TeachingMethod=='Pending')] <- 'Unknown'
  tmp$TeachingMethod[which(tmp$TeachingMethod== 'On Premises')] <- 'Full'
  tmp$TeachingMethod[which(tmp$TeachingMethod== 'Online Only')] <- 'Remote'
  tmp$SportsParticipation[which(tmp$SportsParticipation=='Pending')] <- 'Unknown'
  tmp$StaffMaskPolicy[which(tmp$StaffMaskPolicy=='Pending')] <- 'Unknown'
  tmp$StaffMaskPolicy[which(tmp$StaffMaskPolicy=='Required for all staff')] <- 'Yes'
  tmp$StaffMaskPolicy[which(tmp$StaffMaskPolicy=='Not required')] <- 'No'
  tmp$StudentMaskPolicy[which(tmp$StudentMaskPolicy=='Pending')] <- 'Unknown'
  tmp$StudentMaskPolicy[which(tmp$StudentMaskPolicy=='Required for high school students only')] <- 'Yes'
  tmp$StudentMaskPolicy[which(tmp$StudentMaskPolicy=='Required for middle/high school students only')] <- 'Yes'
  tmp$StudentMaskPolicy[which(tmp$StudentMaskPolicy=='Not required')] <- 'No'
  tmp$OnlineInstructionIncrease[which(tmp$OnlineInstructionIncrease=='Pending')] <- 'Unknown'
  #tmp$OnlineInstructionIncrease[which(tmp$OnlineInstructionIncrease=='NA')] <- 'Unknown'
  tmp$ParentOptOutClassroomTeaching[which(tmp$ParentOptOutClassroomTeaching=='Pending')] <- 'Unknown'
  #tmp$ParentOptOutClassroomTeaching[which(tmp$ParentOptOutClassroomTeaching=='NA')] <- 'Unknown'

  # tmp$TeachingMethod.Mask <- tmp$TeachingMethod
  # tmp$TeachingMethod.Mask[which(tmp$StudentMaskPolicy=='Yes' & tmp$TeachingMethod=='Full')] <- 'Full.Yes'
  # tmp$TeachingMethod.Mask[which(tmp$StudentMaskPolicy=='Yes' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Yes'
  # tmp$TeachingMethod.Mask[which(tmp$StudentMaskPolicy=='No' & tmp$TeachingMethod=='Full')] <- 'Full.No'
  # tmp$TeachingMethod.Mask[which(tmp$StudentMaskPolicy=='No' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.No'
  # tmp$TeachingMethod.Mask[which(tmp$StudentMaskPolicy=='Unknown' & tmp$TeachingMethod=='Full')] <- 'Full.Unknown'
  # tmp$TeachingMethod.Mask[which(tmp$StudentMaskPolicy=='Unknown' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Unknown'
  # tmp$TeachingMethod.Sports <- tmp$TeachingMethod
  # tmp$TeachingMethod.Sports[which(tmp$SportsParticipation=='Yes' & tmp$TeachingMethod=='Full')] <- 'Full.Yes'
  # tmp$TeachingMethod.Sports[which(tmp$SportsParticipation=='Yes' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Yes'
  # tmp$TeachingMethod.Sports[which(tmp$SportsParticipation=='No' & tmp$TeachingMethod=='Full')] <- 'Full.No'
  # tmp$TeachingMethod.Sports[which(tmp$SportsParticipation=='No' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.No'
  # tmp$TeachingMethod.Sports[which(tmp$SportsParticipation=='Unknown' & tmp$TeachingMethod=='Full')] <- 'Full.Unknown'
  # tmp$TeachingMethod.Sports[which(tmp$SportsParticipation=='Unknown' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Unknown'
  # tmp$TeachingMethod.Staff <- tmp$TeachingMethod
  # tmp$TeachingMethod.Staff[which(tmp$StaffMaskPolicy=='Yes' & tmp$TeachingMethod=='Full')] <- 'Full.Yes'
  # tmp$TeachingMethod.Staff[which(tmp$StaffMaskPolicy=='Yes' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Yes'
  # tmp$TeachingMethod.Staff[which(tmp$StaffMaskPolicy=='No' & tmp$TeachingMethod=='Full')] <- 'Full.No'
  # tmp$TeachingMethod.Staff[which(tmp$StaffMaskPolicy=='No' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.No'
  # tmp$TeachingMethod.Staff[which(tmp$StaffMaskPolicy=='Unknown' & tmp$TeachingMethod=='Full')] <- 'Full.Unknown'
  # tmp$TeachingMethod.Staff[which(tmp$StaffMaskPolicy=='Unknown' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Unknown'
  # tmp$TeachingMethod.Online <- tmp$TeachingMethod
  # tmp$TeachingMethod.Online[which(tmp$OnlineInstructionIncrease=='Yes' & tmp$TeachingMethod=='Full')]  <- 'Full.Yes'
  # tmp$TeachingMethod.Online[which(tmp$OnlineInstructionIncrease=='Yes' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Yes'
  # tmp$TeachingMethod.Online[which(tmp$OnlineInstructionIncrease=='No' & tmp$TeachingMethod=='Full')]  <- 'Full.No'
  # tmp$TeachingMethod.Online[which(tmp$OnlineInstructionIncrease=='No' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.No'
  # tmp$TeachingMethod.Online[which(tmp$OnlineInstructionIncrease=='Unknown' & tmp$TeachingMethod=='Full')]  <- 'Full.Unknown'
  # tmp$TeachingMethod.Online[which(tmp$OnlineInstructionIncrease=='Unknown' & tmp$TeachingMethod=='Hybrid')] <- 'Hybrid.Unknown'


  plans <- unique(tmp$TeachingMethod)
  # plans.mask <- unique(tmp$TeachingMethod.Mask)
  # plans.staff <- unique(tmp$TeachingMethod.Staff)
  # plans.sports <- unique(tmp$TeachingMethod.Sports)
  # plans.online <- unique(tmp$TeachingMethod.Online)
  sports <- unique(tmp$SportsParticipation)
  staffmask <- unique(tmp$StaffMaskPolicy)
  studentmask <- unique(tmp$StudentMaskPolicy)
  online <- unique(tmp$OnlineInstructionIncrease)
  homeschool <- unique(tmp$ParentOptOutClassroomTeaching)

  dt <- setDT(tmp)
  sc <- dt[, c(paste("start.",plans, sep=""),paste("start2.",plans, sep=""), paste("portion.",plans, sep=""),
               # paste("start.mask.",plans.mask, sep=""),  paste("portion.mask.",plans.mask, sep=""),
               # paste("start.staff.",plans.staff, sep=""),  paste("portion.staff.",plans.staff, sep=""),
               # paste("start.sports.",plans.sports, sep=""),  paste("portion.sports.",plans.sports, sep=""),
               # paste("start.online.",plans.online, sep=""),  paste("portion.online.",plans.online, sep=""),
               paste("sports.",sports, sep=""), paste("staffmask.",staffmask, sep=""),
               paste("studentmask.",studentmask, sep=""), paste("online.",online, sep=""), paste("homeschool.",homeschool, sep="")) :=
             c(lapply(plans, function(p) weighted.mean(x=OpenDate[TeachingMethod==p],
                                                       w=Enrollment[TeachingMethod==p], na.rm=FALSE)),
               lapply(plans, function(p) mean(x=OpenDate[TeachingMethod==p],na.rm=FALSE)),
               lapply(plans, function(p)
                 sum(Enrollment[TeachingMethod==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
               # lapply(plans.mask, function(p) weighted.mean(x=OpenDate[TeachingMethod.Mask==p],
               #                                         w=Enrollment[TeachingMethod.Mask==p], na.rm=FALSE)),
               # lapply(plans.mask, function(p)
               #   sum(Enrollment[TeachingMethod.Mask==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
               # lapply(plans.staff, function(p) weighted.mean(x=OpenDate[TeachingMethod.Staff==p],
               #                                         w=Enrollment[TeachingMethod.Staff==p], na.rm=FALSE)),
               # lapply(plans.staff, function(p)
               #   sum(Enrollment[TeachingMethod.Staff==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
               # lapply(plans.sports, function(p) weighted.mean(x=OpenDate[TeachingMethod.Sports==p],
               #                                         w=Enrollment[TeachingMethod.Sports==p], na.rm=FALSE)),
               # lapply(plans.sports, function(p)
               #   sum(Enrollment[TeachingMethod.Sports==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
               # lapply(plans.online, function(p) weighted.mean(x=OpenDate[TeachingMethod.Online==p],
               #                                         w=Enrollment[TeachingMethod.Online==p], na.rm=FALSE)),
               # lapply(plans.online, function(p)
               #   sum(Enrollment[TeachingMethod.Online==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
               lapply(sports, function(p)
                 sum(Enrollment[SportsParticipation==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
                lapply(staffmask, function(p)
                  sum(Enrollment[StaffMaskPolicy==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
                lapply(studentmask, function(p)
                  sum(Enrollment[StudentMaskPolicy==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
               lapply(online, function(p)
                 sum(Enrollment[OnlineInstructionIncrease==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE)),
           lapply(homeschool, function(p)
             sum(Enrollment[ParentOptOutClassroomTeaching==p], na.rm=FALSE)/ sum(Enrollment, na.rm=FALSE))),
           by=.(FIPS)]
  usc <- unique(setDF(sc)[,c("FIPS",paste("start.",plans, sep=""), paste("start2.",plans, sep=""), paste("portion.",plans, sep=""),
                             # paste("start.mask.",plans.mask, sep=""), paste("portion.mask.",plans.mask, sep=""),
                             # paste("start.staff.",plans.staff, sep=""), paste("portion.staff.",plans.staff, sep=""),
                             # paste("start.sports.",plans.sports, sep=""), paste("portion.sports.",plans.sports, sep=""),
                             # paste("start.online.",plans.online, sep=""), paste("portion.online.",plans.online, sep=""),
                             paste("sports.",sports, sep=""),
                             paste("staffmask.",staffmask, sep=""),
                             paste("studentmask.",studentmask, sep=""),
                             paste("online.",online, sep=""),
                             paste("homeschool.",homeschool, sep="")
                             )])
  usc$school.reopen.info <- TRUE
  #                 .SDcols=c("Start.date")]
  usc$fips <- as.integer(usc$FIPS)
  for  (v in grep("start\\.",names(usc))) {
    usc[,v] <- as.Date(usc[,v])
  }
  df <- merge(df, usc, by="fips", all.y=TRUE)
  length(unique(df$fips))

  ## restrict the sample period
  startdate  <- "2020-02-01"
  #enddate  <- "2020-12-16"
  df <- subset(df, df$date>=as.Date(startdate))
  #df <- subset(df, df$date<=as.Date(enddate))

  # ## Replace names for school opening variables for EdWeek
  # df$start.Full2 <- df$`start.Full in-person reopening available for all students`
  # df$start.Remote2 <- df$`start.Remote learning only`
  # df$start.Hybrid2 <- df$`start.Hybrid/Partial`
  # df$portion.Full2 <- df$`portion.Full in-person reopening available for all students`
  # df$portion.Remote2 <- df$`portion.Remote learning only`
  # df$portion.Hybrid2 <- df$`portion.Hybrid/Partial`

  ## pdata.frame
  df$t <- as.numeric(df$date)
  df <- df[order(df$fips, df$date),]
  df <- pdata.frame(df,index=c("fips","t"), stringsAsFactors=FALSE)
  df <- df[order(df$fips, df$date),]

  dlogd <- function(x, id, t, lag=L, minval=0.0, b=0, g=0) {
    dx  <- paneldiff(x, id, t, lag=lag)
    dx[dx<minval] <- minval
    dlogdx <- log(dx + exp(g)*b) - panellag(log(dx+b),id,t,lag=lag)
    dlogdx[!is.finite(dlogdx)] <- NA
    return(dlogdx)
  }

  L  <- difflength
  df$dcases <- paneldiff(df$cases,df$fips, df$date, lag=L)
  df$ddeath <- paneldiff(df$deaths,df$fips, df$date, lag=L)
  df$dcases <- sapply(df$dcases, function(x) max(x, exp(-1)))
  df$ddeath <- sapply(df$ddeath, function(x) max(x, exp(-1)))

  df$dlogtests <- dlogd(df$tests, df$fips, df$date, lag=L,
                        minval=exp(-1))


  df$dcase_capita <- 1000*df$dcases/df$PopulationEstimate2018
  df$ddeath_capita <- 1000*df$ddeath/df$PopulationEstimate2018

  df$case_capita <- 1000*df$cases/df$PopulationEstimate2018
  df$death_capita <- 1000*df$deaths/df$PopulationEstimate2018

  df$dcase_capita_day <- 1000*paneldiff(df$cases,df$fips, df$date, lag=1)/df$PopulationEstimate2018
  df$ddeath_capita_day <- 1000*paneldiff(df$deaths,df$fips, df$date, lag=1)/df$PopulationEstimate2018

  # # hospital admission
  # df$adm_capita <- 1000*df$adm/df$PopulationEstimate2018
  # df$adm_total_capita <- 1000*df$adm_total/df$PopulationEstimate2018


  df$logdc <- log(sapply(df$dcases, function(x) max(x, exp(-1))))
  df$dlogdc <- dlogd(df$cases, df$fips, df$date, lag=L,
                   minval=exp(-1))
  df$logdd <- log(sapply(df$ddeath, function(x) max(x, exp(-1))))
  df$dlogdd <-  dlogd(df$deaths, df$fips, df$date, lag=L,
                      minval=exp(-1))
  # log difference over two or three weeks
  df$d2logdc <- dlogd(df$cases, df$fips, df$date, lag=(2*L),
                     minval=exp(-1))
  df$d2logdd <- dlogd(df$deaths, df$fips, df$date, lag=(2*L),
                      minval=exp(-1))
  df$d3logdc <- dlogd(df$cases, df$fips, df$date, lag=(3*L),
                      minval=exp(-1))
  df$d3logdd <- dlogd(df$deaths, df$fips, df$date, lag=(3*L),
                      minval=exp(-1))

  # alternatie measure of case/death growth
  df$logdc2 <- log(sapply(df$dcases, function(x) max(x, 1)))
  df$dlogdc2 <- dlogd(df$cases, df$fips, df$date, lag=L,
                      minval=1)


  df$logdd2 <- log(sapply(df$ddeath, function(x) max(x, 1)))
  df$dlogdd2 <-  dlogd(df$deaths, df$fips, df$date, lag=L,
                      minval=1)
  df$d3logdd2 <- dlogd(df$deaths, df$fips, df$date, lag=(3*L),
                      minval=1)

  startdates <- c("mask_start",
                  "public.schools",
                  "stay.at.home",
                  "X.50.gatherings",
                  "X.500.gatherings",
                  "restaurant.dine.in",
                  "entertainment.gym",
                  "start.Full",
                  "start.Remote",
                  "start.Hybrid",
                  "start.Unknown")
  enddates <- c(NA, NA, paste(startdates[3:7], ".rollback", sep=""), NA,NA,NA,NA)
  stopifnot(length(enddates)==length(startdates))
  pvars <- c("pmask", "pschoolclose", "pshelter", "pgather50", "pgather500",
             "prestaurant", "pentertainment",
             "pschoolfull", "pschoolremote", "pschoolhybrid", "pschoolunknown")
  for(i in 1:length(startdates)) df[,pvars[i]] <-
                                   smoothedpolicy(df,startdates[i],enddatevar=enddates[i],type="ma",bw=policymalength,
                                                  id="fips")
  haveschool=apply(df, 1, function(row) any(!is.na(row[startdates[8:11]])))
  portionnames  <- sub("start","portion",startdates[grep("start\\.",startdates)])
  p <- c("pschoolfull", "pschoolremote", "pschoolhybrid", "pschoolunknown")
  for (i in 1:length(p)) {
    df[,p[i]] <- df[,p[i]]*df[,portionnames[i]]
    df[!haveschool,p[i]] <- NA
  }

  # startdates <- c("start.mask.Full.No","start.mask.Full.Yes","start.mask.Full.Unknown","start.mask.Remote",
  #                 "start.mask.Hybrid.No","start.mask.Hybrid.Yes","start.mask.Hybrid.Unknown","start.mask.Unknown",
  #                 "start.staff.Full.No","start.staff.Full.Yes","start.staff.Full.Unknown","start.staff.Remote",
  #                 "start.staff.Hybrid.No","start.staff.Hybrid.Yes","start.staff.Hybrid.Unknown","start.staff.Unknown",
  #                 "start.sports.Full.No","start.sports.Full.Yes","start.sports.Full.Unknown","start.sports.Remote",
  #                 "start.sports.Hybrid.No","start.sports.Hybrid.Yes","start.sports.Hybrid.Unknown","start.sports.Unknown",
  #                 "start.online.Full.No","start.online.Full.Yes","start.online.Full.Unknown","start.online.Remote",
  #                 "start.online.Hybrid.No","start.online.Hybrid.Yes","start.online.Hybrid.Unknown","start.online.Unknown")
  # enddates <- rep(NA,32)
  # pvars <- c("ps.full.mask.no","ps.full.mask.yes","ps.full.mask.unknown", "ps.remote.mask",
  #            "ps.hybrid.mask.no","ps.hybrid.mask.yes","ps.hybrid.mask.unknown", "ps.unknown.mask",
  #            "ps.full.staff.no","ps.full.staff.yes","ps.full.staff.unknown", "ps.remote.staff",
  #            "ps.hybrid.staff.no","ps.hybrid.staff.yes","ps.hybrid.staff.unknown", "ps.unknown.staff",
  #            "ps.full.sports.no","ps.full.sports.yes","ps.full.sports.unknown", "ps.remote.sports",
  #            "ps.hybrid.sports.no","ps.hybrid.sports.yes","ps.hybrid.sports.unknown", "ps.unknown.sports",
  #            "ps.full.online.no","ps.full.online.yes","ps.full.online.unknown", "ps.remote.online",
  #            "ps.hybrid.online.no","ps.hybrid.online.yes","ps.hybrid.online.unknown", "ps.unknown.online")
  # for(i in 1:length(startdates)) df[,pvars[i]] <-
  #   smoothedpolicy(df,startdates[i],enddatevar=enddates[i],type="ma",bw=policymalength,
  #                  id="fips")
  # # haveschool=apply(df, 1, function(row) any(!is.na(row[startdates])))
  # portionnames  <- sub("start","portion",startdates[grep("start\\.",startdates)])
  # p <- pvars
  # for (i in 1:length(p)) {
  #   df[,p[i]] <- df[,p[i]]*df[,portionnames[i]]
  #   df[!haveschool,p[i]] <- NA
  # }

  # df$ps.full.index.no <- (df$ps.full.mask.no + df$ps.full.staff.no + df$ps.full.sports.yes + df$ps.full.online.no)/4
  # df$ps.full.index.yes <- (df$ps.full.mask.yes + df$ps.full.staff.yes + df$ps.full.sports.no + df$ps.full.online.yes)/4
  # df$ps.full.index.unknown <- (df$ps.full.mask.unknown + df$ps.full.staff.unknown + df$ps.full.sports.yes + df$ps.full.online.unknown)/4
  # df$ps.hybrid.index.no <- (df$ps.hybrid.mask.no + df$ps.hybrid.staff.no + df$ps.hybrid.sports.yes + df$ps.hybrid.online.no)/4
  # df$ps.hybrid.index.yes <- (df$ps.hybrid.mask.yes + df$ps.hybrid.staff.yes + df$ps.hybrid.sports.no + df$ps.hybrid.online.yes)/4
  # df$ps.hybrid.index.unknown <- (df$ps.hybrid.mask.unknown + df$ps.hybrid.staff.unknown + df$ps.hybrid.sports.yes + df$ps.hybrid.online.unknown)/4

  df$month <- as.factor(months.Date(df$date))

  ## Social distancing measures
  # newnames <- c("residential","transit","workplaces","retail","grocery", "parks")
  # idx <- sapply(newnames,function(x) grep(paste(x,"_.+_baseline",sep=""), names(df)))
  # for(v in newnames) {
  #   df[,v] <- panelma(df[,idx[v]], df$fips, df$date, len=policymalength)
  # }

  newnames <- c("college","assistedliving","school","nursing","gym","church","bar","restaurant")
  idx <- sapply(newnames,function(x) grep(paste(x,"_visits",sep=""), names(df)))
  for(v in newnames) {
    df[,v] <- panelma(df[,idx[v]]/df$devices_residing, df$fips, df$date, len=policymalength)
  }


  newnames <- c("college_ma","school_ma")
  idx <- c("college_visits","school_visits")
  for(i in 1:length(newnames)) {
    df[,newnames[i]] <- panelma(df[,idx[i]]/df$devices_residing, df$fips, df$date, len=28)
  }

  idx <- c("completely_home_device_count","full_time_work_behavior_devices","part_time_work_behavior_devices")
  newnames <- c("home","fullwork","partwork")
  for(i in 1:length(newnames)) {
    df[,newnames[i]] <- panelma(df[,idx[i]]/df$devices_residing, df$fips, df$date, len=policymalength)
  }

  df$school_day <- df$school_visits/df$devices_residing
  df$fullwork_day <- df$full_time_work_behavior_devices/df$devices_residing
  df$home_day <- df$completely_home_device_count/df$devices_residing


  # # identify the school opening status of October by looking at the vistis to school in October
  # sdf <- subset(df, df$date==as.Date("2020-10-01"))
  # sdf <- sdf %>% mutate(
  #   pfull =  case_when(pschoolfull<=0.5 ~ 0, pschoolfull>0.5 ~ 1),
  #   phybrid =  case_when(pschoolhybrid<=0.5 ~ 0, pschoolhybrid>0.5 ~ 1),
  #   premote =  case_when(pschoolremote<=0.5 ~ 0, pschoolremote>0.5 ~ 1)
  # )
  # sdf$med <- median(sdf$school[sdf$premote==1],na.rm=TRUE)
  # sdf <- sdf %>% mutate(
  #   premote_low = case_when(premote == 0 | (premote == 1 & school >= med) ~ 0,
  #                           premote == 1 & school < med ~ 1),
  #   premote_high = case_when(premote == 0 | (premote == 1 & school < med) ~ 0,
  #                            premote == 1 & school >= med ~ 1)
  # )
  # sdf$punknown <- 1- sdf$pfull - sdf$phybrid - sdf$premote
  # sdf <- sdf[,c("pfull","phybrid","premote","premote_low","premote_high","punknown","fips")]
  # df <- merge(sdf, df, by="fips")



  # add google measures on Sept 1 --- this is for identifying counties with missing Google mobility variables in September
  # this is because Google mobility measures are missing for many counties around Sept 1
  # e.g., subsetting the data with subset(sdf,!is.na(sdf$residential_sept)) will roughly give balanced panel for Google vars
  # sdf2 <- subset(df,df$date==as.Date("2020-09-01"))
  # oldnames <- c("residential","transit","workplaces","retail","grocery", "parks")
  # newnames <- c("residential_sept","transit_sept","workplaces_sept","retail_sept","grocery_sept", "parks_sept")
  # sdf2 <- sdf2[,c("fips",oldnames)]
  # for(i in 1:length(oldnames)) {
  #   names(sdf2)[names(sdf2) == oldnames[i]] <- newnames[i]
  #   print(length(unique(sdf[,newnames[i]])))
  # }
  # df <- merge(df, sdf2, by="fips")
  # remove(sdf2)

  # identify counties with high visits to schools or colleges in Feb
  sdf <- subset(df, df$date==as.Date("2020-02-29"))
  sdf$school_feb <- sdf$school_ma
  sdf$college_feb <- sdf$college_ma
  sdf <- sdf[,c("school_feb","college_feb","fips")]
  df <- merge(sdf, df, by="fips")

  sdf <- subset(sdf, df$school_visits_feb>0.01)
  q <- quantile(sdf$school,probs=c(0.25,0.5,0.75),na.rm=TRUE)
  print(q)
  df$sc_l <- 0*df$school
  df$sc_m <- 0*df$school
  df$sc_h <- 0*df$school
  df$sc_l[df$school<=q[1]] <- 1
  df$sc_m[(df$school>q[1]) & (df$school<=q[3])] <- 1
  df$sc_h[(df$school>q[3])] <- 1
  p <- c("sc_l","sc_m","sc_h")
  newnames <- c("pschoollow", "pschoolmiddle", "pschoolhigh")
  for (i in 1:length(p)) {
    df[,newnames[i]] <- panelma(df[,p[i]],df$fips, df$date, len=policymalength)
  }
  #
  sdf <- subset(sdf, df$college_visits_feb>0.01)
  q <- quantile(sdf$college,probs=c(0.25,0.5,0.75),na.rm=TRUE)
  print(q)
  df$cl_l <- 0*df$college
  df$cl_m <- 0*df$college
  df$cl_h <- 0*df$college
  df$cl_l[df$college<=q[1]] <- 1
  df$cl_m[(df$college>q[1]) & (df$college<=q[3])] <- 1
  df$cl_h[(df$college>q[3])] <- 1
  p <- c("cl_l","cl_m","cl_h")
  newnames <- c("pcollegelow", "pcollegemiddle", "pcollegehigh")
  for (i in 1:length(p)) {
    df[,newnames[i]] <- panelma(df[,p[i]],df$fips, df$date, len=policymalength)
  }

  df$edu <- df$High.School.Graduation.Rate

  p <- c(
    "PopFmle5.92010"                  ,
    "PopFmle10.142010"                ,
    "PopFmle15.192010"                ,
    #"PopFmle20.242010"                ,
    "PopMale5.92010"                  ,
    "PopMale10.142010"                ,
    "PopMale15.192010"
    #"PopMale20.242010"                ,
  )
  df$pop15 <- 0
  for (i in 1:length(p)) {
    df$pop15 <- df$pop15 + df[,p[i]]
  }
  df$pop15 <- df$pop15/df$PopulationEstimate2018 # populaiton share below 20


  # drop variables that are not used in analysis to save memory
  v <- c("entertainment.gym"  ,
  "entertainment.gym.rollback",
  "federal.guidelines"        ,
  "foreign.travel.ban"        ,
  "FracMale2017"             ,
  "HeartDiseaseMortality"     ,
  "HPSAServedPop"             ,
  "HPSAUnderservedPop"        ,
  "mask_end_county"           ,
  "mask_end_state"            ,
  "mask_note1_county"         ,
  "mask_note2_county"         ,
  "mask_scope_county"         ,
  "mask_scope_state"          ,
  "mask_source_county"        ,
  "mask_source_state"         ,
  "mask_start"                ,
  "mask_start_county"         ,
  "mask_start_mask"           ,
  "mask_start_state"          ,
  "Mask.Always"               ,
  "Mask.Frequently"           ,
  "Mask.Never"                ,
  "Mask.Rarely"               ,
  "Mask.Sometimes"            ,
  "MedicareEnrollment.AgedTot2017",
  "X..Adults.with.Obesity"        ,
  "X..Children.in.Poverty"        ,
  "X..Fair.or.Poor.Health"        ,
  "X..Severe.Housing.Problems"    ,
  "X..Single.Parent.Households"   ,
  "X..Unemployed"                 ,
  "X..Uninsured"                  ,
  "X..Vaccinated"                 ,
  "X.50.gatherings"               ,
  "X.50.gatherings.rollback"      ,
  "X.500.gatherings"              ,
  "X.500.gatherings.rollback"     ,
  "X.EligibleforMedicare2018"     ,
  "X.FTEHospitalTotal2017"        ,
  "X.Hospitals"                   ,
  "X.HospParticipatinginNetwork2017",
  "X.ICU_beds"                      ,
  "X3.YrDiabetes2015.17"            ,
  "X3.YrMortalityAge.1Year2015.17"  ,
  "X3.YrMortalityAge1.4Years2015.17",
  "X3.YrMortalityAge15.24Years2015.17",
  "X3.YrMortalityAge25.34Years2015.17",
  "X3.YrMortalityAge35.44Years2015.17",
  "X3.YrMortalityAge45.54Years2015.17",
  "X3.YrMortalityAge5.14Years2015.17" ,
  "X3.YrMortalityAge55.64Years2015.17",
  "X3.YrMortalityAge65.74Years2015.17",
  "X3.YrMortalityAge75.84Years2015.17",
  "X3.YrMortalityAge85.Years2015.17",
  "POP_LATITUDE"                    ,
  "POP_LONGITUDE"                   ,
  "PopFmle.52010"                   ,
  "PopFmle.842010"                  ,
  "PopFmle10.142010"                ,
  "PopFmle15.192010"                ,
  "PopFmle20.242010"                ,
  "PopFmle25.292010"                ,
  "PopFmle30.342010"                ,
  "PopFmle35.442010"                ,
  "PopFmle45.542010"                ,
  "PopFmle5.92010"                  ,
  "PopFmle55.592010"                ,
  "PopFmle60.642010"                ,
  "PopFmle65.742010"                ,
  "PopFmle75.842010"                ,
  "PopMale.52010"                   ,
  "PopMale.842010"                  ,
  "PopMale10.142010"                ,
  "PopMale15.192010"                ,
  "PopMale20.242010"                ,
  "PopMale25.292010"                ,
  "PopMale30.342010"                ,
  "PopMale35.442010"                ,
  "PopMale45.542010"                ,
  "PopMale5.92010"                  ,
  "PopMale55.592010"                ,
  "PopMale60.642010"                ,
  "PopMale65.742010"                ,
  "PopMale75.842010"                ,
  "PopTotalFemale2017"              ,
  "PopTotalMale2017"                ,
  # "PopulationEstimate65.2017",
  "mortality2015.17Estimated",
  "RespMortalityRate2014",
  "Rural.UrbanContinuumCode2013",
  "StrokeMortality"        ,
   "SVIPercentile",
  "TotalM.D..s.TotNon.FedandFed2017" )

  exvars <- names(df) %in% v
  df <- df[!exvars]

  # identify the school opening status of October by looking at the
  # visits to school in October
  ### Sample selection ###
  sdf$pschoolfull[sdf$portion.Unknown>0.5] <- NA
  sdf$pschoolhybrid[sdf$portion.Unknown>0.5] <- NA
  sdf$pschoolremote[sdf$portion.Unknown>0.5] <- NA
  sdf$staffmask.No[sdf$staffmask.Unknown>0.5] <- NA
  sdf <- data.table(subset(df, df$date==as.Date("2020-10-01")))
  sdf[,pfull :=  1*(pschoolfull>0.5)]
  sdf[,phybrid :=  1*(pschoolhybrid>0.5)]
  sdf[,premote :=  1*(pschoolremote>0.5)]
  sdf[,pfull_no :=  1*(pschoolfull>0.5)*(staffmask.No>0)]
  sdf[,pfull_yes :=  1*(pschoolfull>0.5)*(staffmask.No==0)]
  sdf[,phybrid_no :=  1*(pschoolhybrid>0.5)*(staffmask.No>0)]
  sdf[,phybrid_yes :=  1*(pschoolhybrid>0.5)*(staffmask.No==0)]
  med <- median(sdf$school[sdf$premote==1],na.rm=TRUE)
  sdf[,premote_low := 1*(premote & (school<med))]
  sdf[,premote_high := 1*(premote & (school>=med))]
  sdf[,punknown := 1 - pfull - phybrid - premote]
  sdf <- sdf[,c("pfull","pfull_no","pfull_yes","phybrid","phybrid_no","phybrid_yes","premote","premote_low","premote_high","punknown","fips")]
  df <- merge(sdf, df, by="fips",all=TRUE)
  rm(sdf)



  return(df)
}
