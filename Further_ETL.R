##7.10.2018####
##Join for lapse period##

lapse2 <- read.csv("C:/Users/mdunning/Documents/Data/LAPSE/lapse_stuff.csv")
names(lapse)[names(lapse) == 'new_mbrshp_sid'] <- 'MBRSHP_SID'


View(lapse)

##GOOD JOIN for exp. date on njit5
NJIT5 <- left_join(NJIT5, lapse, by = 'MBRSHP_SID')

rr_123 <- read.csv("C:/Users/mdunning/Documents/Data/LAPSE/GEOCODE_RR.csv")


NJIT_Data4 <- left_join(NJIT_Data3, rr_123, by = 'GEOCODE')
NJIT5 <- NJIT_Data4
##Read in club services data and append
club_services <- read.csv("C:/Users/mdunning/Documents/Data/club_services.csv")
names(club_services)[names(club_services) == 'club'] <- 'CLUB'


NJIT6 <- left_join(NJIT6, club_services, by = "CLUB")

date <- as.Date(15705, origin = "1970-01-01")

date1<- as.Date(16467, origin = "1970-01-01")
date1
diff <- date1 - date




###Coercing Dates
lapse1 <- as.Date(lapse$max_mbrshp_exp_dt, origin = "1970-01-01") - as.Date(NJIT_Data4$MBRSHP_RNWL_DT, "%m/%d/%Y")
lapse1

lapse$max_mbrshp_exp_dt <- as.Date(lapse$max_mbrshp_exp_dt, origin = "1970-01-01")

NJIT5$MBRSHP_EXP_DT <- as.Date(NJIT5$MBRSHP_EXP_DT, "%m/%d/%Y")


NJIT5$max_mbrshp_exp_dt <- as.Date(NJIT5$max_mbrshp_exp_dt, "%m/%d/%Y")
NJIT7 <- NJIT6[ , -which(names(NJIT6) %in% c("max_mbrshp_exp_dt", "LAPSE"))]
NJIT5$MBRSHP_RNWL_DT <- as.Date(NJIT5$MBRSHP_RNWL_DT, "%m/%d/%Y")

###Mutate to calc lapse
NJIT6 <- mutate(NJIT5, 
                LAPSE = NJIT5$max_mbrshp_exp_dt - )



NJIT8 <- NJIT7[ , -which(names(NJIT7) %in% c("TIRE_CENTER_IND"))]

###Changing Service indicators
NJIT6$OPTICAL_IND <- ifelse(NJIT6$OPTICAL_IND %in% c("X"), 1, 0)
NJIT6$GAS_IND <- ifelse(NJIT6$GAS_IND %in% c("X"), 1,0)
NJIT6$FULL_SERVICE_DELI <- ifelse(NJIT6$FULL_SERVICE_DELI %in% c("X"), 1,0)
NJIT6$CELL_PHONE <- ifelse(NJIT6$CELL_PHONE %in% c("X"), 1,0)
NJIT6$TIRE_CENTER_IND <- ifelse(NJIT6$TIRE_CENTER_IND %in% c("X"), 1,0)

tire_center <- subset(club_services, select = c("TIRE_CENTER_IND", "CLUB"))
NJIT6 <- left_join(NJIT6, tire_center, by = 'CLUB')

min_date <- as.Date("08/20/2017", origin = "%m/%d/%Y")

###Filtering out max_exp dates that are greater than 8/20

lapse_filtered <- filter(lapse2, max_mbrshp_exp_dt )



write.csv(NJIT8, "NJIT_Data2")



NJIT8 <- NJIT7[ , -which(names(NJIT7) %in% c("PTA_Club", 
                                              "MBRSHP_RNWL_DT", 
                                              "MBRSHP_EXP_DT", "MBRSHP_SUB_TYPE", "MBRSHP_EXP_DT",
                                              "MBRSHP_ENR_DT",
                                              "MBRSHP_RNWL_DT", 
                                             "CAR_RTE",
                                             "ORIG_CAR_RTE",
                                             "avg_resp",
                                             "OPTICAL_IND",
                                             "GAS_IND",
                                             "FULL_SERVICE_DELI",
                                             "CELL_PHONE",
                                             "TIRE_CENTER_IND"))]

NJIT9 <- NJIT8[ ,-which(names(NJIT8) %in% c("3yrRR_Decile"))]



##Adding good lapse period to master table
lapse3 <- read.csv("C:/Users/mdunning/Documents/Data/LAPSE/MATT_LAPSE_TABLE_711.csv")

##This file has key on MBRSHP_NBR not SID so going back to MERGED_FALL_17_TABLES to retrieve keys

ppd <- read.csv("C:/Users/mdunning/Documents/Data/past_paid.csv")

names(ppd)[names(ppd) == 'SALES'] <- 'PAST_SALES'
NJIT8 <- left_join(NJIT8, ppd, by = 'MBRSHP_SID')
##Appended Past sales
write.csv(NJIT8, "LTV_Table.csv")
getwd()
setwd("C:/Users/mdunning/Documents/Data")

##Fix to Date issues -- merge good_enr_date with NJIT8 and drop old enr_date column due to issues
good_enr_date <- subset(another_merged_df7, select = c(REC_NBR, MBRSHP_ENR_DT))

###Reading lapse data

good_exp <- read.csv("C:/Users/mdunning/Documents/Data/MATT_LAPSE_TABLE_711_TRIPS.csv")

names(good_exps)[names(good_exps) == 'TRIPS'] <- 'PAST_TRIPS'


##Reading in extra FALLMAP aquisition data

Extra_FallMAP_Data <- read.csv("C:/Users/mdunning/Documents/Data/FALLMAP17_PREP1/FALLMAP17_PREP1.csv")

##Appending
names(Extra_FallMAP_Data)[names(Extra_FallMAP_Data) == 'X3yrRR_Decile.y'] <- 'X3yrRR_Decile'

NJIT9 <- left_join(NJIT8, Extra_FallMAP_Data, by = "GEOCODE")

NJIT9 <- NJIT9[ , -which(names(NJIT9) %in% c("X3yrRR_Decile.x"))]


##Readin in master table


LTV_Table <- read.csv("C:/Users/mdunning/Documents/Data/LTV_Table.csv")
View(head(LTV_Table))


write.csv(NJIT8, "NJIT.csv")


##Attempting to Join


?substring
NJIT9 <- NJIT8 %>%
  mutate(Zip = substring(GEOCODE, 1, 5), Car_Rte = substring(GEOCODE, 6, 9))

NJIT10 <- left_join(NJIT9, Extra_FallMAP_Data, by = c("Zip" = "Zip_Cd", "Car_Rte" = "Car_Rte"))
 NJIT8
NJIT9 <- as.double(NJIT9$Zip)
Extra_FallMAP_Data$Zip_Cd <-  as.numeric(Extra_FallMAP_Data$Zip_Cd)



