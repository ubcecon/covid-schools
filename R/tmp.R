library(lfe)
library(stargazer)
library(knitr)
library(ggplot2)
library(ggthemes)
library(data.table)
detectCores()



sgtype <- opts_knit$get("rmarkdown.pandoc.to")
sgstyle <- 'default'
colors <-  scale_color_solarized
colors_fill <- scale_fill_solarized
rootdir <- system("git rev-parse --show-toplevel", intern=TRUE)[1]

source(paste(rootdir,"R/countyData.R",sep="/"))
source(paste(rootdir,"R/varlabels.R", sep="/"))
source(paste(rootdir,"R/dataprep.R",sep="/"))
source(paste(rootdir,"R/utils.R", sep="/"))
countyvars <-
  c("PopulationEstimate2018",
    "PopulationDensityperSqMile2010",
    "Smokers_Percentage",
  "Social.Association.Rate",
    "DiabetesPercentage",
    "MedianAge2010",
    "dem_to_rep_ratio",
    "HPSAShortage")
L.d <- 21
L.c <- 14
source(paste(rootdir,"rmd/generatetables.R",sep="/"))
library(devtools)
#devtools::install_github("bcallaway11/did")
library(did)

if (FALSE) {
  df <- countyData(redo=FALSE)
  df <- dataprep(df)

  save(df, file="tmp.Rdata")
} else {
  load("tmp.Rdata")
}
df <- subset(df, df$date>=as.Date("2020-02-01"))
startdate  <- "2020-04-01"
enddate  <- "2021-01-01"
sdf <- subset(df, df$date>=as.Date(startdate))
sdf <- subset(sdf, sdf$date<=as.Date(enddate))
sdf <- subset(sdf, sdf$fips!=0)
length(unique(sdf$fips))
unique(sdf$date)
sdf$pgather <- sdf$pgather50


# identify the school opening status of October by looking at the
# visits to school in October
sdf2 <- subset(sdf, sdf$date==as.Date("2020-10-01"))
sdf2 <- sdf2 %>% mutate(
    pfull =  case_when(pschoolfull<=0.5 ~ 0, pschoolfull>0.5 ~ 1),
    phybrid =  case_when(pschoolhybrid<=0.5 ~ 0, pschoolhybrid>0.5 ~ 1),
    premote =  case_when(pschoolremote<=0.5 ~ 0, pschoolremote>0.5 ~ 1)
)
sdf2$med <- median(sdf2$school[sdf2$premote==1],na.rm=TRUE)
sdf2 <- sdf2 %>% mutate(
  premote_low = case_when(premote == 0 | (premote == 1 & school >= med) ~ 0,
                          premote == 1 & school < med ~ 1),
  premote_high = case_when(premote == 0 | (premote == 1 & school < med) ~ 0,
                          premote == 1 & school >= med ~ 1)
)
sdf2$punknown <- 1- sdf2$pfull - sdf2$phybrid - sdf2$premote
sdf2 <- sdf2[,c("pfull","phybrid","premote","premote_low","premote_high","punknown","fips")]
sdf <- merge(sdf2, sdf, by="fips")

tdf <- sdf[,grep("start.",names(sdf))]
sdf$start.first <- as.Date(apply(tdf, 1, function(x) min(x, na.rm=TRUE)))
sdf$event_week <- as.numeric(floor(difftime(sdf$date, sdf$start.first, units="weeks")))
rm(tdf)



library(did)
library(data.table)
sdf$week <- as.numeric(floor(difftime(sdf$date,min(sdf$date), units="weeks")))
sdf$nfips <- as.numeric(sdf$fips)
DT <- data.table(sdf, key="nfips,week")
# make the data weekly
udf <- DT[, n := rank(date, ties.method="first"), by = key(DT)][n == 1]
udf$start.full <- udf$pfull*as.numeric(floor(difftime(udf$start.first,min(sdf$date), units="weeks")))
udf <- subset(udf, !is.na(start.full))

# start date not constant for 3 counties. Replace with min
# To see which 3:
## udf[,list(vs=var(start.full)),by=nfips][vs>0]
udf[, start.full:= ifelse(start.full>0, min(start.full), start.full), by=nfips]
# only one county with very early & late starts
udf[week==20,list(.N), by="start.full"][N<2, start.full]
udf[start.full>=23,start.full:=23]
udf[start.full>0 & start.full<=17,start.full:=17]

## Incorrect DID event study
library(fixest)
reg <- feols(dcase_capita ~ i(pfull, event_week, ref=-1)
             #+ i(phybrid, event_week, -1)
             #+ i(event_week, ref=-1)
             | fips + week, data=subset(udf, pfull | premote), cluster="fips")
png("wrongdid.png")
coefplot(reg)
dev.off()

# Conditioning on group
out <- att_gt(yname="logdc", gname="start.full",
              idname="nfips",
              tname="week",
              xformla = ~1,
              data=subset(udf, pfull | premote),
              est_method="reg")

# Then aggregating correctly
es  <- aggte(out, type="dynamic")

summary(es)
#png("correctes.png")
ggdid(es)
#dev.off()


## Hybrid vs remote
udf$start.hybrid <- udf$phybrid*as.numeric(floor(difftime(udf$start.first,min(sdf$date), units="weeks")))
udf[, start.hybrid:= ifelse(start.hybrid>0, min(start.hybrid), start.hybrid), by=nfips]
#udf[week==20,list(.N), by="start.hybrid"]

outh <- att_gt(yname="dcase_capita", gname="start.hybrid",
              idname="nfips",
              tname="week",
              xformla = ~1,
              data=subset(udf, phybrid | premote),
              est_method="reg")
# Then aggregating correctly
esh  <- aggte(outh, type="dynamic")

summary(esh)
ggdid(esh)
