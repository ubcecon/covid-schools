varlabels <- data.frame(t(matrix(c(
  "dlogdc",
  "Case Growth Rates",
  "log(cases[t]-cases[t-7]) - log(cases[t-7] - cases[t-14])",
  "NYT",
  
  "dlogdd", "Death Growth Rates",
  "log(deaths[t]-deaths[t-7]) - log(deaths[t-7] - deaths[t-14])",
  "NYT",
  
  "dlogtests", "Test Growth Rates",
  "log(tests from t to t-7) - log(tests from t-7 to t-14)",
  "Covid Tracking Project",
  
  "testrate", "tests per 1000", "tests in past week per 1000 people",
  "Covid Tracking Project",
  
  "logdc", "log(Cases)",
  "log(cases[t]-cases[t-7])", "NYT",
    
  "logdd", "log(Deaths)",
  "log(deaths[t]-deaths[t-7])", "NYT",
    
  "dcase_capita", "Weekly Cases per 1000", "Cases per 1000", "NYT",
  
  "ddeath_capita", "Weekly Deaths per 1000", "Cases per 1000", "NYT",
  
  "testratedc", "$\\frac{\\Delta{D}}{\\Delta{C}} T_{it}$",
  "tests in past week time deaths in past week divided by cases in past week",
  "NYT, Covid Tracking Project",
  
  "workplaces", "Workplaces",
  "workplaces percent change from baseline",
  "Google",
  
  "residential", "Residential",
  "residential percent change from baseline",
  "Google",
  
  "retail", "Retail",
  "retail and recreation percent change from baseline",
  "Google",
  
  "grocery", "Grocery",
  "grocery and pharmacy percent change from baseline",
  "Google",
  
  "transit", "Transit",
  "transit percent change from baseline",
  "Google",
   
  "church", "Church Vists",
  "visits to gyms / # of devices", "safegraph",
  
  "gym", "Rec. Facilities Visits",
  "visits to gyms / # of devices", "safegraph",
  
  
  "bar", "Bar Visits",
  "visits to bars / # of devices", "safegraph",
  
  
  "restaurant", "Restaurant Visits",
  "visits to restaurants / # of devices", "safegraph",
  
  
  "college", "College Visits",
  "visits to colleges / # of devices", "safegraph",
  
  "school", "K-12 School Visits",
  "visits to K-12 schools / # of devices", "safegraph",
  
  
  
  "workplaces_percent_change_from_baseline", "workplaces intensity",
  "workplaces percent change from baseline",
  "Google",
  
  "residential_percent_change_from_baseline", "residential intensity",
  "residential percent change from baseline",
  "Google",
  
  "retail_and_recreation_percent_change_from_baseline", "retail intensity",
  "retail and recreation percent change from baseline",
  "Google",
  
  "grocery_and_pharmacy_percent_change_from_baseline", "grocery intensity",
  "grocery and pharmacy percent change from baseline",
  "Google",
  
  "transit_stations_percent_change_from_baseline", "transit intensity",
  "transit percent change from baseline",
  "Google",
  
  "parks_percent_change_from_baseline", "parks intensity",
  "parks percent change from baseline",
  "Google",
  
  "zero", "", "", "",
  
  "(0.000)", "", "", "",
  
  "fips","County FIPS", "", "",
  
  "date","Date", "", "",
  
  "state.fips", "State FIPS", "", "",
  
  "mask_start_county", "Date county mask mandate began",
  "", "Wright et al",
  
  "mask_end_county", "Date county mask mandate ended",
  "", "Wright et al",
  
  "mask_start_state", "Date state mask mandate began",
  "", "Wright et al",
  
  "mask_end_state", "Date state mask mandate ended",
  "", "Wright et al",
  
  "mask_start", "Earliest of county or state mask mandate", "", "",
  
  "lat", "lat",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "lon", "lon",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "POP_LATITUDE", "POP_LATITUDE",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "POP_LONGITUDE", "POP_LONGITUDE",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "CensusRegionName", "CensusRegionName",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "CensusDivisionName", "CensusDivisionName",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "Rural.UrbanContinuumCode2013", "Rural.UrbanContinuumCode2013",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "pop", "Population in 2018 (millions)",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopulationEstimate2018", "Population in 2018",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopTotalMale2017", "PopTotalMale2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopTotalFemale2017", "PopTotalFemale2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "FracMale2017", "FracMale2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopulationEstimate65.2017", "PopulationEstimate65.2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopulationDensityperSqMile2010",
  "PopulationDensityperSqMile2010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "CensusPopulation2010", "CensusPopulation2010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "MedianAge2010", "MedianAge2010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.EligibleforMedicare2018", "X.EligibleforMedicare2018",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "MedicareEnrollment.AgedTot2017",
  "MedicareEnrollment.AgedTot2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrDiabetes2015.17", "X3.YrDiabetes2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "DiabetesPercentage", "DiabetesPercentage",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "HeartDiseaseMortality", "HeartDiseaseMortality",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "StrokeMortality", "StrokeMortality",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "Smokers_Percentage", "Smokers_Percentage",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "RespMortalityRate2014", "RespMortalityRate2014",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.FTEHospitalTotal2017", "X.FTEHospitalTotal2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "TotalM.D..s.TotNon.FedandFed2017",
  "TotalM.D..s.TotNon.FedandFed2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.HospParticipatinginNetwork2017",
  "X.HospParticipatinginNetwork2017",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.Hospitals", "X.Hospitals",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.ICU_beds", "X.ICU_beds",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "dem_to_rep_ratio", "dem_to_rep_ratio",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale.52010", "PopMale.52010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle.52010", "PopFmle.52010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale5.92010", "PopMale5.92010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle5.92010", "PopFmle5.92010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale10.142010", "PopMale10.142010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle10.142010", "PopFmle10.142010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale15.192010", "PopMale15.192010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle15.192010", "PopFmle15.192010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale20.242010", "PopMale20.242010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle20.242010", "PopFmle20.242010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale25.292010", "PopMale25.292010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle25.292010", "PopFmle25.292010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale30.342010", "PopMale30.342010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle30.342010", "PopFmle30.342010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale35.442010", "PopMale35.442010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle35.442010", "PopFmle35.442010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale45.542010", "PopMale45.542010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle45.542010", "PopFmle45.542010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale55.592010", "PopMale55.592010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle55.592010", "PopFmle55.592010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale60.642010", "PopMale60.642010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle60.642010", "PopFmle60.642010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale65.742010", "PopMale65.742010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle65.742010", "PopFmle65.742010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale75.842010", "PopMale75.842010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle75.842010", "PopFmle75.842010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopMale.842010", "PopMale.842010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "PopFmle.842010", "PopFmle.842010",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge.1Year2015.17",
  "X3.YrMortalityAge.1Year2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge1.4Years2015.17",
  "X3.YrMortalityAge1.4Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge5.14Years2015.17",
  "X3.YrMortalityAge5.14Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge15.24Years2015.17",
  "X3.YrMortalityAge15.24Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge25.34Years2015.17",
  "X3.YrMortalityAge25.34Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge35.44Years2015.17",
  "X3.YrMortalityAge35.44Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge45.54Years2015.17",
  "X3.YrMortalityAge45.54Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge55.64Years2015.17",
  "X3.YrMortalityAge55.64Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge65.74Years2015.17",
  "X3.YrMortalityAge65.74Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge75.84Years2015.17",
  "X3.YrMortalityAge75.84Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X3.YrMortalityAge85.Years2015.17",
  "X3.YrMortalityAge85.Years2015.17",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "mortality2015.17Estimated", "mortality2015.17Estimated",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "SVIPercentile", "SVIPercentile",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "HPSAShortage", "HPSAShortage",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "HPSAServedPop", "HPSAServedPop",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "HPSAUnderservedPop", "HPSAUnderservedPop",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "stay.at.home", "stay.at.home",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.50.gatherings", "X.50.gatherings",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.500.gatherings", "X.500.gatherings",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "public.schools", "public.schools",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "restaurant.dine.in", "restaurant.dine.in",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "entertainment.gym", "entertainment.gym",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "federal.guidelines", "federal.guidelines",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "foreign.travel.ban", "foreign.travel.ban",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "stay.at.home.rollback", "stay.at.home.rollback",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.50.gatherings.rollback", "X.50.gatherings.rollback",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "X.500.gatherings.rollback", "X.500.gatherings.rollback",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "restaurant.dine.in.rollback", "restaurant.dine.in.rollback",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "entertainment.gym.rollback", "entertainment.gym.rollback",
  "see https://github.com/Yu-Group/covid19-severity-prediction/blob/master/data/list_of_columns.md",
  "Yu group",
  
  "pmask", "Mandatory mask", "", "",
  
  "pshelter", "Stay at home", "", "",
  
  "pgather50", "Ban gatherings", "", "",
  
  "pgather500", "Ban gatherings >500", "", "",
  
  "pschool", "Close schools", "", "",
   
  "fullwork", "Full-time Workplace Visits", "", "",
   
  "partwork", "Part-time Workplace Visits", "", "",
  
  "home", "Staying Home Devices", "", "",
  
  "pschoolfull", "Open K-12 In-person", "", "",
  
  "pschoolhybrid", "Open K-12 Hybrid", "", "",
  
  "pschoolremote", "Open K-12 Remote", "", "",
  
  "staff_dum", "No-Mask for Staffs", "", "",
  
  "school_staff_dum", "K-12 Visits $\times$ No-Mask", "", "",
  
  "full_staff_dum", "K-12 In-person $\times$ No-Mask", "", "",
  
  "hybrid_staff_dum", "K-12 Hybrid $\times$ No-Mask", "", "",
  
  "pschoolhigh", "High K-12 visits", "", "",
  
  "pschoolmiddle", "Middle K-12 visits", "", "",
  
  "pschoollow", "Low K-12 visits", "", "",
  
  "pcollegehigh", "High College visits", "", "",
  
  "pcollegemiddle", "Middle College visits", "", "",
  
  "pcollegelow", "Low College visits", "", "",
  
  "prestaurant", "Close dine-in restaurants", "", "",
  
  "pgather", "Ban gatherings >50", "", "",
  
  "pentertainment", "Close gyms and entertainment venues", "", "",
  
  
  "studentmask.No", "No mask for students", "", "",
  
  "staffmask.No", "No mask for staffs", "", "",
  
  "sports.Yes", "Yes sports for students", "", "",
  
  "online.No", "No online instruction", "", "",
  
  "spindex.No", "School policy index", "", "",
  
  "school_studentmask.No",  "K-12 Visits $\times$ No-Mask", "", "",
  
  "full_studentmask.No",  "K-12 In-person $\times$ No-Mask", "", "",
  
  "hybrid_studentmask.No", "K-12 Hybrid $\times$ No-Mask", "", "",
  
  "school_spindex.No",  "K-12 Visits $\times$ No-Mitigation", "", "",
  
  "full_spindex.No", "K-12 In-person $\times$ No-Mitigation", "", "",
  
  "hybrid_spindex.No", "K-12 In-person $\times$ No-Mitigation", "", ""
), nrow=4)), stringsAsFactors=FALSE)
names(varlabels) <- c("Variable","Label","Description","Source")


#' Returns the variable label for a variable name or array of variable names
getlabel <- function(v) sapply(v, function(x)
  varlabels[varlabels$Variable==x,"Label"])

#' Replace variable names with variable labels in text tbl
relabel <- function(tbl) {
  vars <- varlabels$Variable
  vars <- vars[order(nchar(vars), decreasing=TRUE)]
  for (v in vars) {
    tbl <- gsub(v, getlabel(v), tbl, fixed=TRUE)
  }
  tbl
}


