#library(readr)

install.packages("dplyr")
library(dplyr)
install.packages("stringr")
library(stringr)
merged <- MERGED_FALL_17_TABLES

table(merged)

##Check for duplicates
n_occur <- data.frame(table(merged$))
n_occur[n_occur$Freq > 1,]
merged[merged$`FULL NAME` %in% n_occur$Var1[n_occur$Freq > 1],]
## Recode name to binary
merged$`FULL NAME` <- ifelse(merged$`FULL NAME` %in% c("OUR FRIENDS AT"), 0, 1)
##change column name
names(merged)[names(merged) == 'FULL NAME'] <- 'HAS NAME'
##Select columns
merged_tidy <- subset(merged, select = c("REC_NBR", 
                                         "SOURCE",
                                         "GEOCODE",
                                         "ORIG_CAR_RTE",
                                         "CLUB",
                                         "HAS NAME",
                                         "DSF",
                                         "3yrRR_Decile",
                                         "BJs_Dist1",
                                         "COMP_Dist1",
                                         "Med_Income",
                                         "Med_HV",
                                         "Med_Age",
                                         "LowRR_Flag",
                                         "PTA_Flag",
                                         "PTA_Club",
                                         "MBRSHP_SID",
                                         "MBRSHP_SUB_TYPE",
                                         "MBRSHP_EXP_DT",
                                         "MBRSHP_ENR_DT",
                                         "MBRSHP_RNWL_DT",
                                         "MBRSHP_FEE_INC",
                                         "MKT_CD",
                                         "PAID",
                                         "TYPE",
                                         "MBRSHP_STAT_CD",
                                         "CLUB_OF_MBRSHP",
                                         "ZIPFLAG"
                                  ))

##merged$`FULL NAME` <- replace(merged$`FULL NAME`,merged$`FULL NAME` == "OUR FRIENDS AT", 0)
##merged$`FULL NAME` <- replace(merged$`FULL NAME`, merged$`FULL NAME` == ".+", 1)

##Random Sample
merged_sample <- sample_n(merged_tidy, size = 20)
##Describe Data
#install.packages("Hmisc")
#library(Hmisc)
#di <- describe(merged_tidy)
di
install.packages("pastecs")
library(pastecs)
summary_df <- stat.desc(merged_tidy)
summary_df
str(merged_tidy)
table(merged_tidy)
summary(merged_tidy)
##Write to csv
write.csv(merged_tidy, file = "MERGED_FALL17_TABLES.csv")

##Sale/Trip Join
sales_trips <- MERGED_FALL_17_TABLES_SALESTRIPS
merged_tidy_w_sales <- merge(merged_tidy, sales_trips)
##Select columns
merged_tidy_w_sale2 <- subset(merged_tidy_w_sale2, select = c("REC_NBR", 
                                         "SOURCE",
                                         "GEOCODE",
                                         "ORIG_CAR_RTE",
                                         "CLUB",
                                         "HAS NAME",
                                         "3yrRR_Decile",
                                         "BJs_Dist1",
                                         "COMP_Dist1",
                                         "Med_Income",
                                         "Med_HV",
                                         "Med_Age",
                                         "LowRR_Flag",
                                         "PTA_Flag",
                                         "PTA_Club",
                                         "MBRSHP_SID",
                                         "MBRSHP_SUB_TYPE",
                                         "MBRSHP_EXP_DT",
                                         "MBRSHP_ENR_DT",
                                         "MBRSHP_RNWL_DT",
                                         "MBRSHP_FEE_INC",
                                         "MKT_CD",
                                         "PAID",
                                         "TYPE",
                                         "MBRSHP_STAT_CD",
                                         "CLUB_OF_MBRSHP",
                                         "ZIPFLAG",
                                         "SALES",
                                         "TRIPS"
))
##Recode PTA Flag from Y/N to 1/0
merged_tidy_w_sale2$PTA_Flag <- ifelse(merged_tidy_w_sale2$PTA_Flag %in% c("N"), 0, 1)
##Recode Paid from Y/N to 1/0
merged_tidy_w_sale2$PAID <- ifelse(merged_tidy_w_sale2$PAID%in% c("N"), 0, 1)
##Recode ZIPFLAG fom Y/N to 1/0
merged_tidy_w_sale2$ZIPFLAG <- ifelse(merged_tidy_w_sale2$ZIPFLAG %in% c("N"), 0, 1)

##Write to csv
write.csv(merged_tidy_w_sale2, file = "MERGED_FALL17MAP_SALESTRIP_TABLES.csv")
contents <- contents(merged_tidy_w_sale2)
contents
##Random Sample
merged_sample <- sample_n(merged_tidy_w_sale2, size = 20)
#rm(merged_tidy_w_sales)
##Sample to csv
write.csv(merged_sample, file = "SAMPLE_MERGED_FALL17MAP_SALESTRIP_TABLES.csv")
##Merge Extra Columns with up to date dataframe
#extra_columns_merge <- merge(merged_tidy_w_sale2, EXTRA_COLUMNS)
extra_columns_merge <- left_join(merged_tidy_w_sale2, EXTRA_COLUMNS, by = "MBRSHP_SID")
##Rename
names(extra_columns_merge)[names(extra_columns_merge) == 'ORIG_CAR_RTE.y'] <- 'ORIG_CAR_RTE'
##Merge with more
another_merged_df <- left_join(extra_columns_merge, EXTRA_COLUMNS2, by = "MBRSHP_SID")
names(another_merged_df)[names(another_merged_df) == 'MBRSHP_SUB_TYPE.y'] <- 'MBRSHP_SUB_TYPE'
names(another_merged_df)[names(another_merged_df) == 'MBRSHP_SUB_TYPE.x'] <- 'MBRSHP_SUB_TYPE'


##(LAST merge before sales/gas is added in, adding htese in adds members who enrolled post 9/19/17)
another_merged_df2 <- subset(another_merged_df, select = c("REC_NBR", 
                                                              "SOURCE",
                                                              "GEOCODE",
                                                              "CLUB",
                                                              "HAS NAME",
                                                              "3yrRR_Decile",
                                                              "BJs_Dist1",
                                                              "COMP_Dist1",
                                                              "Med_Income",
                                                              "Med_HV",
                                                              "Med_Age",
                                                              "LowRR_Flag",
                                                              "PTA_Flag",
                                                              "PTA_Club",
                                                              "MBRSHP_SID",
                                                              "MBRSHP_SUB_TYPE",
                                                              "MBRSHP_EXP_DT",
                                                              "MBRSHP_ENR_DT",
                                                              "MBRSHP_RNWL_DT",
                                                              "MBRSHP_FEE_INC",
                                                              "MKT_CD",
                                                              "PAID",
                                                              "TYPE",
                                                              "MBRSHP_STAT_CD",
                                                              "CLUB_OF_MBRSHP",
                                                              "ZIPFLAG",
                                                              "SALES",
                                                              "TRIPS", 
                                                              "CAR_RTE",
                                                              "ORIG_CAR_RTE"
))
##Fixing NA issue with another_merged_df2 (most up to date before adding sales data)
##antoher_df7 contains a clean df without gas data added due to adding members to were enrolled post 9/19/2017
another_merged_df7 <- left_join(another_merged_df2, ignore, by = "MBRSHP_SID")
names(another_merged_df7)[names(another_merged_df7) == 'MKT_CD.y'] <- 'MKT_CD'
another_merged_df7 <- subset(another_merged_df7, select = c("REC_NBR", 
                                                            "SOURCE",
                                                            "GEOCODE",
                                                            "CLUB",
                                                            "HAS NAME",
                                                            "3yrRR_Decile",
                                                            "BJs_Dist1",
                                                            "COMP_Dist1",
                                                            "Med_Income",
                                                            "Med_HV",
                                                            "Med_Age",
                                                            "LowRR_Flag",
                                                            "PTA_Flag",
                                                            "PTA_Club",
                                                            "MBRSHP_SID",
                                                            "MBRSHP_SUB_TYPE",
                                                            "MBRSHP_EXP_DT",
                                                            "MBRSHP_ENR_DT",
                                                            "MBRSHP_RNWL_DT",
                                                            "MBRSHP_FEE_INC",
                                                            "MKT_CD",
                                                            "PAID",
                                                            "TYPE",
                                                            "MBRSHP_STAT_CD",
                                                            "CLUB_OF_MBRSHP",
                                                            "ZIPFLAG",
                                                            "SALES",
                                                            "TRIPS", 
                                                            "CAR_RTE",
                                                            "ORIG_CAR_RTE"
))
###FRIDAY WORK###Alsoi fixed NA issues with df above
##Adding Gas Sales and Trips by MBRSHIP_SID

##Again writing to CSV
another_merged_df3 <- merge(another_merged_df2,Gas_Sales_Trips_MBRSHP_SID, by = "MBRSHP_SID")
write.csv(another_merged_df2, file = "ANOTHER_MERGED_DF.csv")
merged_sample <- sample_n(another_merged_df2, size = 20)

##Merging With Gas Sales Data
another_merged_df4 <- left_join(another_merged_df2, Gas_Sales_Trips_MBRSHP_SID, by = "MBRSHP_SID")
#Correcting NA issue
another_merged_df5 <- left_join(another_merged_df4, ignore, by = "MBRSHP_SID")
##Renaming and dropping duplicate columns
names(another_merged_df5)[names(another_merged_df5) == 'MKT_CD.y'] <- 'MKT_CD'
##Dropping
another_merged_df6 <- subset(another_merged_df5, select = c("REC_NBR", 
                                                           "SOURCE",
                                                           "GEOCODE",
                                                           "CLUB",
                                                           "HAS NAME",
                                                           "3yrRR_Decile",
                                                           "BJs_Dist1",
                                                           "COMP_Dist1",
                                                           "Med_Income",
                                                           "Med_HV",
                                                           "Med_Age",
                                                           "LowRR_Flag",
                                                           "PTA_Flag",
                                                           "PTA_Club",
                                                           "MBRSHP_SID",
                                                           "MBRSHP_SUB_TYPE",
                                                           "MBRSHP_EXP_DT",
                                                           "MBRSHP_ENR_DT",
                                                           "MBRSHP_RNWL_DT",
                                                           "MBRSHP_FEE_INC",
                                                           "MKT_CD",
                                                           "PAID",
                                                           "TYPE",
                                                           "MBRSHP_STAT_CD",
                                                           "CLUB_OF_MBRSHP",
                                                           "ZIPFLAG",
                                                           "SALES",
                                                           "TRIPS", 
                                                           "CAR_RTE",
                                                           "ORIG_CAR_RTE",
                                                           "GAS_SALES",
                                                           "GAS_TRIPS"
))
##Random Sample of 20
merged_sample <- sample_n(another_merged_df6, size = 20)
##Writing most recent to csv
write.csv(another_merged_df6, file = "MERGED_FALLMAP17_GAS_SALES_TABLE.csv")
##Writing CSV without GAS_SALES data
write.csv(another_merged_df7, file = "MERGED_FALLMAP17_TABLE_NO_GAS.csv")


##filter out merged FALLMAP and GAS_SALEs data fro enrollees in range 8/20-9/19

another_merged_df8 <- another_merged_df7 %>%
  filter(another_merged_df6$MBRSHP_ENR_DT >= "2017-08-20")


###7.3.18## Read in cleaned dataset that was shown to Rong in meeting 7/2, tasked to dropped MKT_CD, add in RR, AGI info, etc.

##Reading in csv most recent w/ gas sales

##df1 <- read.csv("C:/Users/mdunning/Documents/Data/MERGED_FALLMAP17_TABLES(FINAL)/MERGED_FALLMAP17_GAS_SALES_TABLE.CSV", sep = ',')

##Dropping MKT_CD
df1 <- subset(df1, select = c("Obs",
                              "REC_NBR", 
                              "SOURCE_PPD_HH",
                              "SOURCE_0",
                              "SOURCE_TA",
                              "GEOCODE",
                              "CLUB",
                              "HAS.NAME_HH",
                              "X3yrRR_Decile",
                              "BJs_Dist1_CR",
                              "COMP_Dist1",
                              "Med_Income_CR",
                              "Med_HV_CR",
                              "Med_Age_CR",
                              "LowRR_Flag",
                              "PTA_Flag",
                              "PTA_Club",
                              "MBRSHP_SID",
                              "MBRSHP_EXP_DT",
                              "MBRSHP_ENR_DT",
                              "MBRSHP_RNWL_DT",
                              "MBRSHP_FEE_INC",
                              "Total.MFI",
                              "PAID",
                              "TYPE_IC",
                              "TYPE_BUS",
                              "MBRSHP_STAT_CD_AM",
                              "MBRSHP_STAT_CD_CR",
                              "MBRSHP_STAT_VC",
                              "MBRSHP_STAT_IM",
                              "MBRSHP_STAT_MR",
                              "CLUB_OF_MBRSHP",
                              "SALES",
                              "TRIPS", 
                              "CAR_RTE",
                              "ORIG_CAR_RTE",
                              "GAS_SALES",
                              "GAS_TRIPS"
))


df_IRS <- read.csv("C:/Users/mdunning/Documents/Data/IRS_Data/14zpallagi.csv", sep = ',')

table(df_IRS$zipcode)
?head

sample <- sample(df_IRS, 20)
sample
sample
df2_IRS <- read.csv("C:/Users/mdunning/Documents/Data/IRS_Data/14zpalloagi.csv", sep = ',')
df2_50samp <- head(df2_IRS, 50)
View(df2_50samp)

##Average AGI Per Zip
##IRS data from 14zpalloagi.csv computing average AGI per zip and appending column to new df
df3_IRS <- mutate(df2_IRS,
                  AVG_AGI_ZIP = round(df2_IRS$A00100 / df2_IRS$N1))
##Apending ZIP to most recent master to enable merging with IRS data df1 is most recent master
df2_w_ZIP <- left_join(df1, select(MERGED_FALL_17_TABLES_SALESTRIPS, c("MBRSHP_SID","ZIP")), by = "MBRSHP_SID") 

##Merge df2_w_ZIP(df1 combined w ZIP info) with df3_IRS(ZIP info with AVG_ZIP_AGI), goal is to get AVG_ZIP_AGI appended to df1 (Master)
##Drop zip after merge
##change zip column name in df2_w_zpi to ZIPCODE to match with other table
names(df2_w_ZIP)[names(df2_w_ZIP) == 'ZIP'] <- 'ZIPCODE'

df2 <- left_join(df2_w_ZIP, select(df3_IRS, c("ZIPCODE", "AVG_AGI_ZIP")), by = "ZIPCODE")
##Dropping ZIPCODE
df2 <- subset(df2, select = c("Obs",
                              "REC_NBR", 
                              "SOURCE_PPD_HH",
                              "SOURCE_0",
                              "SOURCE_TA",
                              "GEOCODE",
                              "CLUB",
                              "HAS.NAME_HH",
                              "X3yrRR_Decile",
                              "BJs_Dist1_CR",
                              "COMP_Dist1",
                              "Med_Income_CR",
                              "Med_HV_CR",
                              "Med_Age_CR",
                              "LowRR_Flag",
                              "PTA_Flag",
                              "PTA_Club",
                              "MBRSHP_SID",
                              "MBRSHP_EXP_DT",
                              "MBRSHP_ENR_DT",
                              "MBRSHP_RNWL_DT",
                              "MBRSHP_FEE_INC",
                              "Total.MFI",
                              "PAID",
                              "TYPE_IC",
                              "TYPE_BUS",
                              "MBRSHP_STAT_CD_AM",
                              "MBRSHP_STAT_CD_CR",
                              "MBRSHP_STAT_VC",
                              "MBRSHP_STAT_IM",
                              "MBRSHP_STAT_MR",
                              "CLUB_OF_MBRSHP",
                              "SALES",
                              "TRIPS", 
                              "CAR_RTE",
                              "ORIG_CAR_RTE",
                              "GAS_SALES",
                              "GAS_TRIPS",
                              "AVG_AGI_ZIP"
))

club_data <- read.csv("C:/Users/mdunning/Documents/Data/Club_Level_Data/trips_sales_club_level_aug_to_june.csv", sep = ',')
View(club_data)
##Merge Club level data with df2
names(club_data)[names(club_data) == 'SITE_NBR'] <- 'CLUB'
names(club_data)[names(club_data) == 'trips'] <- 'CLUB_TRIPS'
names(club_data)[names(club_data) == 'sales'] <- 'CLUB_SALES'

df3 <- left_join(df2, club_data, by = "CLUB")

###Loading ZIP_RR data
RR_data <- read.csv("C:/Users/mdunning/Documents/Data/ZIP_RR2.csv", sep = ',')
View(RR_data)
names(RR_data)[names(RR_data) == 'ZIP_CD'] <- 'ZIPCODE'
names(RR_data)[names(RR_data) == 'avg_resp)'] <- 'avg_resp'
df2_w_ZIP <- left_join(df2_w_ZIP, RR_data, by = "ZIPCODE")
df5 <- left_join(df4, select(df2_w_ZIP, c("MBRSHP_SID", "avg_resp")), by = "MBRSHP_SID")
write.csv(df6, file = "NJIT_Data.csv")
names(df5)[names(df5) == 'avg_resp.y'] <- 'avg_resp'

df7 <- subset(df7, select = c("Obs",
                              "REC_NBR", 
                              "SOURCE_PPD_HH",
                              "SOURCE_0",
                              "SOURCE_TA",
                              "GEOCODE",
                              "CLUB",
                              "HAS.NAME_HH",
                              "X3yrRR_Decile",
                              "BJs_Dist1_CR",
                              "COMP_Dist1",
                              "Med_Income_CR",
                              "Med_HV_CR",
                              "Med_Age_CR",
                              "LowRR_Flag",
                              "PTA_Flag",
                              "PTA_Club",
                              "MBRSHP_SID",
                              "MBRSHP_EXP_DT",
                              "MBRSHP_ENR_DT",
                              "MBRSHP_RNWL_DT",
                              "MBRSHP_FEE_INC",
                              "PAID",
                              "TYPE_IC",
                              "TYPE_BUS",
                              "MBRSHP_STAT_CD_AM",
                              "MBRSHP_STAT_CD_CR",
                              "MBRSHP_STAT_VC",
                              "MBRSHP_STAT_IM",
                              "MBRSHP_STAT_MR",
                              "CLUB_OF_MBRSHP",
                              "SALES",
                              "TRIPS", 
                              "CAR_RTE",
                              "ORIG_CAR_RTE",
                              "GAS_SALES",
                              "GAS_TRIPS",
                              "AVG_AGI_ZIP",
                              "CLUB_TRIPS",
                              "CLUB_SALES",
                              "CLUB_SIZE_CTGRY",
                              "avg_resp",
                              "TOTMFI"
))

##Are CAR_RTE and ORIG_CAR_RTE equivalent?
all(df6$CAR_RTE == df6$ORIG_CAR_RTE)
##FALSE
df6$CAR_RTE[!(df6$CAR_RTE %in% df6$ORIG_CAR_RTE)]
##596 mismatches
##Club of mbrshp vs club
all(df6$CLUB_OF_MBRSHP == df6$CLUB)


##Adding Total MFI
Total_MFI <- read.csv("C:/Users/mdunning/Documents/Data/Engagement/Total_MFI.csv")
df7 <- left_join(df6, Total_MFI, by = "MBRSHP_SID")

library(Hmisc)
all(df7$TOTMFI == df7$MBRSHP_FEE_INC)

sum(is.na(df7$MBRSHP_FEE_INC))
sum(df7$TOTMFI==df7$MBRSHP_FEE_INC)

#Dropping NAs 
df8 <- subset(df7, !is.na(df7$REC_NBR))


check2 <-merge(select(df8, "MBRSHP_SID", "MBRSHP_FEE_INC"), select(df8, "MBRSHP_SID","TOTMFI"), by = "MBRSHP_SID")

NJIT_data <- subset(NJIT_data, select = c("Obs",
                              "REC_NBR",
                              "MBRSHP_SID",
                              "GEOCODE",
                              "CLUB",
                              "CLUB_TRIPS",
                              "CLUB_SALES",
                              "CLUB_SIZE_CTGRY_S",
                              "CLUB_SIZE_CTGRY_L",
                              "HAS.NAME_HH",
                              "SALES",
                              "TRIPS",
                              "GAS_SALES",
                              "GAS_TRIPS",
                              "X3yrRR_Decile",
                              "BJs_Dist1_CR",
                              "COMP_Dist1",
                              "Med_Income_CR",
                              "Med_HV_CR",
                              "Med_Age_CR",
                              "PTA_Flag",
                              "PTA_Club",
                              "MBRSHP_EXP_DT",
                              "MBRSHP_ENR_DT",
                              "MBRSHP_RNWL_DT",
                              "MBRSHP_FEE_INC",
                              "PAID", 
                              "SOURCE_PPD_HH",
                              "SOURCE_0",
                              "SOURCE_TA",
                              "TYPE_IC",
                              "TYPE_BUS",
                              "MBRSHP_STAT_CD_AM",
                              "MBRSHP_STAT_CD_CR",
                              "MBRSHP_STAT_VC",
                              "MBRSHP_STAT_IM",
                              "MBRSHP_STAT_MR",
                              "CLUB_OF_MBRSHP", 
                              "CAR_RTE",
                              "ORIG_CAR_RTE",
                              "AVG_AGI_ZIP",
                              "avg_resp",
                              "Total_MFI"
))
install.packages("memisc")
library(memisc)
codebook <- codebook(df8)
write.csv(vec1, "column_names.csv")


sample <- sample_n(df8, size = 100)
write.csv(sample, "sample.csv")


tot_mfi_fixed <- read.csv("C:/Users/mdunning/Documents/Data/Engagement/QUERY_FOR_TOTMFI.csv")

names(tot_mfi_fixed)[names(tot_mfi_fixed) == 'Totmfi'] <- 'Total_MFI'
df9 <- left_join(df8, tot_mfi_fixed, by = "MBRSHP_SID")

write.csv(NJIT_data, "NJIT_Data.csv")

NJIT_data <- read.csv("C:/Users/mdunning/Documents/NJIT_Data.csv")


##Re-reading in IRS data



New_IRS_Data <- read.csv("C:/Users/mdunning/Documents/Data/IRS_Data/Derived_IRS_Data.csv")

rm(New_IRS_Data)
##Extracting Zip codes and Rec_NBR to join with NJIT_Data to allow for merge with IRS data by zip
zipcodes <- subset(df2_w_ZIP, select = c("REC_NBR", "ZIPCODE"))


NJIT_Data_W_zip <- left_join(NJIT_data, zipcodes, by = "REC_NBR")

NJIT_Data_w_IRS_Data <- left_join(NJIT_Data_W_zip, New_IRS_Data, by = "ZIPCODE")

##Writing to CSV
write.csv(NJIT_Data2, "NJIT_w_IRS_Data.csv")
##Dropping column named ZIPCODE
NJIT_Data2 <- NJIT_Data_w_IRS_Data[ , -which(names(NJIT_Data_w_IRS_Data) %in% c("ZIPCODE"))]
