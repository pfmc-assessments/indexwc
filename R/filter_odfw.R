# ##### AT SEA OBSERVER CPUE DATA PREP/FILTERING

# # created by A. Whitman
# # updated 06/21/2021

# # last update to exclude 2020 due to sample size.

# # purpose: explore drift level data for the At Sea charter observer CPUE index

# # load some libraries  - not sure if I'll need all these.

# #load.libraries
# load.libraries<-function(){
#   library(reshape2)
#   library(pROC)
#   library(ggplot2)
#   library(RODBC)
#   library(MASS)
#   library(rstanarm)
#   library(HDInterval)
#   library(ggmap)
#   library(maptools)
#   library(maps)
#   library(mapdata)
#   library(dplyr)
#   library(tidyr)
#   library(data.table)
#   library(lme4)
#   library(lattice)
#   library(DHARMa)
#   library(fitdistrplus)
# }

# load.libraries()

# setwd("C:/Users/DaubleAl/Desktop/2021 Stock Assessment Planning/03_Lingcod/At Sea CPUE")

# dat <- read.csv("ORAtSea_lingcod.csv", header = TRUE)
# names(dat)
# summary(dat)

# #fixing all the character columns
# dat[,c(8:12,14,16:19)]<-sapply(dat[,c(8:12,14,16:19)],as.numeric)
# dat$GF_OpenDepth<-as.factor(dat$GF_OpenDepth)

# # removing zero catch drifts (per Mel's email)
# # TODO: talk to Ali about this filtering step. Is it actually removing
# # zero values or just NA values?
# dat<-dat[!is.na(dat$Total_lingcod),]
# summary(dat)

# # adding a p/a variable for lingcod
# dat$ling.pa<-ifelse(dat$Total_lingcod==0,0,1)

# # adding a megaregion variable
# dat$Megaregion<-as.factor(ifelse(dat$Port<30,"North","South"))

# # positive rate of overall dataset
# dim(dat)[1] # total records
# length(which(dat$ling.pa==1)) # total positive drifts
# length(which(dat$ling.pa==1))/dim(dat)[1] # positive rate

# # positive rate by year without any filtering
# pos_rate_year<-dat |>
#   dplyr::group_by(Year)|>
#   dplyr::summarise(No.Drifts=n(),Pos.Drifts=round((sum(ling.pa)/n()),3)) |>
#   as.data.frame()
# pos_rate_year

# # ouch on 2020, might need to throw that year out, otherwise, annual rate
# # ranges from approx. 0.203 to 0.611

# #looking at potential filters
# # drift minutes, drifts without depth, depth bins, closed season or depths
# # look at drifts by megaregion and by depth bin, filter by percent midwater

# ### FILTER: DRIFT MINUTES
# # TODO: talk to Ali about the following removal of NA and negative values in
# # drift minutes as a filter rather than just data cleaning. I do not think that
# # values with NA or negative values should even be in the data data passed to
# # the filtering. Can we separate filters by logical filters and cleaning for
# # bs?
# summary(dat$DriftMinutes)
# # remove NAs and negative drifts
# dat<-dat[!is.na(dat$DriftMinutes),]
# dat<-dat[dat$DriftMinutes>0,]

# quantile(dat$DriftMinutes,0.95) # 235 minutes seems unreasonable for a drift, 95 percentile is 28 mins
# dat<-dat[dat$DriftMinutes<=28,]

# #'
# #'
# filter_odfw_drift_quantile <- function(x, percent = 0.95, verbose = FALSE) {
#   limit <- stats::quantile(x, percent)
#   remove <- which(x > limit)
#   if (verbose) {
#     message(
#       glue::glue("
#         ODFW Drift Quantile: 95% percentile was {round(limit, 2)} and
#         {length(remove)} rows were removed, removals are summarized below.
#       ")
#     )
#     print(summary(x[remove]))
#   }
#   return(x > limit)
# }
# test |>
#   dplyr::mutate(filter_drift = filter_odfw_drift_quantile(DriftMinutes))
# # positive rate of overall dataset
# dim(dat)[1] # total records
# length(which(dat$ling.pa==1)) # total positive drifts
# length(which(dat$ling.pa==1))/dim(dat)[1] # positive rate

# ### FILTER: DEPTHS

# summary(dat$SDEPTH_fm)
# # remove the NAs
# dat<-dat[!is.na(dat$SDEPTH_fm),]
# histogram(dat$SDEPTH_fm)
# quantile(dat$SDEPTH_fm,0.99) #99 percentile is 60, so I will cut it off there
# dat<-dat[dat$SDEPTH_fm<=60,]

# # positive rate of overall dataset
# dim(dat)[1] # total records
# length(which(dat$ling.pa==1)) # total positive drifts
# length(which(dat$ling.pa==1))/dim(dat)[1] # positive rate

# #creating a depth bin from the start depth in FMs
# dat<- dat |>
#   mutate(DepthBin=cut(dat$SDEPTH_fm,breaks = c(seq(0,60,by = 5)))) |>
#   as.data.frame()

# # look at drifts by depth bin
# drifts_by_depth<-dat |>
#   group_by(DepthBin) |>
#   summarise(drifts=n())
# drifts_by_depth
# histogram(dat$DepthBin)

# ### FILTER: PERCENT MIDWATER FISH

# summary(dat$percent_midwater)
# quantile(dat$percent_midwater)  # so 50% is 0.75 midwater, and 75% is 1.0! this would be a pretty heavy filter
# histogram(dat$percent_midwater) # lots of zeros and lots of 1.0, no clear break in between
# length(which(dat$percent_midwater<=0.75))

# #just to check, are there lingcod caught on any 100% midwater trips?
# length(which(dat$percent_midwater==1.0 & dat$ling.pa==1)) # yay

# # if I filter out all the 1.0's, what do the quantiles look like then?
# dat_test<-dat[dat$percent_midwater<1.0,]
# dim(dat_test)[1]
# quantile(dat_test$percent_midwater,0.99) # that's 0.92 percent midwater, so I think that's reasonable
# # it's also similar to the last round (which was 95% midwater)

# dat<-dat[dat$percent_midwater<=0.92,]

# # positive rate of overall dataset
# dim(dat)[1] # total records
# length(which(dat$ling.pa==1)) # total positive drifts
# length(which(dat$ling.pa==1))/dim(dat)[1] # positive rate

# #### CHECKING OPEN DEPTHS
# summary(dat$GF_OpenDepth)

# # so lingcod is not a LL species, so those get recategorized
# levels(dat$GF_OpenDepth)[levels(dat$GF_OpenDepth)=="<30 fm & >40 fm (LL)"] <- "<30 fm"
# levels(dat$GF_OpenDepth)[levels(dat$GF_OpenDepth)=="<40 fm & >40 fm (LL)"] <- "<40 fm"
# histogram(dat$GF_OpenDepth)
# table(dat$GF_OpenDepth,dat$ling.pa > 0) # a few trips outside of 40fm

# #checking if fishing in legal depths
# #add a legal depth from the GF_OpenDepth
# dat$legal.dep<-as.numeric(ifelse(dat$GF_OpenDepth=="All Depth",1000,substring(dat$GF_OpenDepth,2,3)))
# dat$dep.diff<-dat$legal.dep-dat$SDEPTH_fm # so if it's negative, they were fishing in depths that were deeper than legal
# length(which(dat$dep.diff<0))

# #### FILTER: FISHING OUTSIDE LEGAL DEPTHS

# # remove drifts outside legal depths, with a buffer
# dat<-dat[dat$dep.diff>-5,]

# # positive rate of overall dataset
# dim(dat)[1] # total records
# length(which(dat$ling.pa==1)) # total positive drifts
# length(which(dat$ling.pa==1))/dim(dat)[1] # positive rate

# # positive rate by year without any filtering
# pos_rate_year<-dat |>
#   dplyr::group_by(Year)|>
#   dplyr::summarise(No.Drifts=n(),Pos.Drifts=sum(ling.pa),Pos.Perc=round((sum(ling.pa)/n()),3)) |>
#   as.data.frame()
# pos_rate_year

# ### FILTER - REMOVE 2020 BECAUSE OF SAMPLE SIZE

# dat<-dat[dat$Year %in% c(2001,2003:2019),]

# # positive rate of overall dataset
# dim(dat)[1] # total records
# length(which(dat$ling.pa==1)) # total positive drifts
# length(which(dat$ling.pa==1))/dim(dat)[1] # positive rate

# pos_rate_year<-dat |>
#   dplyr::group_by(Year)|>
#   dplyr::summarise(No.Drifts=n(),Pos.Drifts=sum(ling.pa),Pos.Perc=round((sum(ling.pa)/n()),3)) |>
#   as.data.frame()
# pos_rate_year

# # okay, I think that's it for filters.

# # exporting final dataset and positive rate table

# #write.csv(dat,"Final_filtered_ORAtSea_updated.csv",row.names = F)
# #write.csv(pos_rate_year,"PositivesByYear_updated.csv",row.names = F)
