# EJ at 10 Analysis
# 2019-01-17
# Katherine Wolf



#### clear workspace ####
rm(list=ls())



#### set this to the folder with your files in it! ####
setwd("C:/analysis/ej_10/")



#### load needed R packages ####
library(data.table)
library(ggplot2)
library(dplyr)
library(psych)
library(bit64)
library(scales)
library(forcats)
library(reshape2)
library(totalcensus)



#### process census data ####
race_ethnicity_DEC_00_SF3_P007 <- 
  fread(file = "DEC_00_SF3_P007_with_ann.csv")
poverty_DEC_00_SF3_P088 <- 
  fread(file = "DEC_00_SF3_P088_with_ann.csv")

# View(race_ethnicity_DEC_00_SF3_P007)
# View(poverty_DEC_00_SF3_P088)

colnames(race_ethnicity_DEC_00_SF3_P007)
colnames(poverty_DEC_00_SF3_P088)

names(race_ethnicity_DEC_00_SF3_P007) <- c("GEOID","GEOID2","GEO_label","total","not_Hispanic_or_Latino","non_HL_white","non_HL_Black","non_HL_AIAN","non_HL_Asian","non_HL_NHOPI","non_HL_other","non_HL_2_plus","HL","HL_white","HL_Black","HL_AIAN","HL_Asian","HL_NHOPI","HL_other","HL_2_plus")
# View(race_ethnicity_DEC_00_SF3_P007)

names(poverty_DEC_00_SF3_P088) <- c("GEOID","GEOID2","GEO_label","total","under_50","from_50_to_75","from_75_to_99","from_100_to_124","from_125_to_149","from_150_to_174","from_175_to_184","from_185_to_199","from_200_up")
# View(poverty_DEC_00_SF3_P088)

# remove label rows
race_ethnicity_DEC_00_SF3_P007 <- race_ethnicity_DEC_00_SF3_P007[-c(1),]
poverty_DEC_00_SF3_P088 <- poverty_DEC_00_SF3_P088[-c(1),]
# View(race_ethnicity_DEC_00_SF3_P007)
# View(poverty_DEC_00_SF3_P088)

# make rows into numbers
poverty_DEC_00_SF3_P088$total <- as.integer(poverty_DEC_00_SF3_P088$total)
poverty_DEC_00_SF3_P088$under_50 <- as.integer(poverty_DEC_00_SF3_P088$under_50)
poverty_DEC_00_SF3_P088$from_50_to_75 <- as.integer(poverty_DEC_00_SF3_P088$from_50_to_75)
poverty_DEC_00_SF3_P088$from_75_to_99 <- as.integer(poverty_DEC_00_SF3_P088$from_75_to_99)
poverty_DEC_00_SF3_P088$from_100_to_124 <- as.integer(poverty_DEC_00_SF3_P088$from_100_to_124)
poverty_DEC_00_SF3_P088$from_125_to_149 <- as.integer(poverty_DEC_00_SF3_P088$from_125_to_149)
poverty_DEC_00_SF3_P088$from_150_to_174 <- as.integer(poverty_DEC_00_SF3_P088$from_150_to_174)
poverty_DEC_00_SF3_P088$from_175_to_184 <- as.integer(poverty_DEC_00_SF3_P088$from_175_to_184)
poverty_DEC_00_SF3_P088$from_185_to_199 <- as.integer(poverty_DEC_00_SF3_P088$from_185_to_199)
poverty_DEC_00_SF3_P088$from_200_up <- as.integer(poverty_DEC_00_SF3_P088$from_200_up)

names(race_ethnicity_DEC_00_SF3_P007) <- c("GEOID","GEOID2","GEO_label","total","not_Hispanic_or_Latino","non_HL_white","non_HL_Black","non_HL_AIAN","non_HL_Asian","non_HL_NHOPI","non_HL_other","non_HL_2_plus","HL","HL_white","HL_Black","HL_AIAN","HL_Asian","HL_NHOPI","HL_other","HL_2_plus")

race_ethnicity_DEC_00_SF3_P007$total <- as.integer(race_ethnicity_DEC_00_SF3_P007$total)
race_ethnicity_DEC_00_SF3_P007$not_Hispanic_or_Latino <- as.integer(race_ethnicity_DEC_00_SF3_P007$not_Hispanic_or_Latino)
race_ethnicity_DEC_00_SF3_P007$non_HL_white <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_white)
race_ethnicity_DEC_00_SF3_P007$non_HL_Black <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_Black)
race_ethnicity_DEC_00_SF3_P007$non_HL_AIAN <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_AIAN)
race_ethnicity_DEC_00_SF3_P007$non_HL_Asian <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_Asian)
race_ethnicity_DEC_00_SF3_P007$non_HL_NHOPI <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_NHOPI)
race_ethnicity_DEC_00_SF3_P007$non_HL_other <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_other)
race_ethnicity_DEC_00_SF3_P007$non_HL_2_plus <- as.integer(race_ethnicity_DEC_00_SF3_P007$non_HL_2_plus)
race_ethnicity_DEC_00_SF3_P007$HL <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL)
race_ethnicity_DEC_00_SF3_P007$HL_white <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_white)
race_ethnicity_DEC_00_SF3_P007$HL_Black <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_Black)
race_ethnicity_DEC_00_SF3_P007$HL_AIAN <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_AIAN)
race_ethnicity_DEC_00_SF3_P007$HL_Asian <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_Asian)
race_ethnicity_DEC_00_SF3_P007$HL_NHOPI <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_NHOPI)
race_ethnicity_DEC_00_SF3_P007$HL_other <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_other)
race_ethnicity_DEC_00_SF3_P007$HL_2_plus <- as.integer(race_ethnicity_DEC_00_SF3_P007$HL_2_plus)

# make poverty total under 200%
poverty_DEC_00_SF3_P088$below_200 <- 
  rowSums(poverty_DEC_00_SF3_P088[, c("under_50",
                                      "from_50_to_75",
                                      "from_75_to_99",
                                      "from_100_to_124",
                                      "from_125_to_149",
                                      "from_150_to_174",
                                      "from_175_to_184",
                                      "from_185_to_199")])
# View(poverty_DEC_00_SF3_P088)

# make fraction over and under 200%
poverty_DEC_00_SF3_P088$fraction_below_200 <- 
  round(poverty_DEC_00_SF3_P088$below_200/
          poverty_DEC_00_SF3_P088$total, 
        digits = 5)
poverty_DEC_00_SF3_P088$fraction_below_200[
  is.nan(poverty_DEC_00_SF3_P088$fraction_below_200)] = 0
# View(poverty_DEC_00_SF3_P088)

# make flag for those with 30% under 200%
poverty_DEC_00_SF3_P088$over_30_under_200 <- "no"
poverty_DEC_00_SF3_P088 <-
  within(poverty_DEC_00_SF3_P088, 
         over_30_under_200[fraction_below_200 >= .3]  <-  "yes")

# 2000 race/ethnicity data percentages

# percent white
race_ethnicity_DEC_00_SF3_P007$percent_white <- 
  round(race_ethnicity_DEC_00_SF3_P007$non_HL_white/
          race_ethnicity_DEC_00_SF3_P007$total, 
        digits = 5) # white
race_ethnicity_DEC_00_SF3_P007$percent_white[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_white)] = 0

# African American
race_ethnicity_DEC_00_SF3_P007$percent_Black <- 
  round(race_ethnicity_DEC_00_SF3_P007$non_HL_Black/
  race_ethnicity_DEC_00_SF3_P007$total, 
  digits = 5)
race_ethnicity_DEC_00_SF3_P007$percent_Black[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_Black)] = 0

# Hispanic/Latinx
race_ethnicity_DEC_00_SF3_P007$percent_HL <- 
  round(race_ethnicity_DEC_00_SF3_P007$HL/
  race_ethnicity_DEC_00_SF3_P007$total, 
  digits = 5)
race_ethnicity_DEC_00_SF3_P007$percent_HL[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_HL)] = 0

# American Indian/Alaska Native
race_ethnicity_DEC_00_SF3_P007$percent_AIAN <- 
  round(race_ethnicity_DEC_00_SF3_P007$non_HL_AIAN/
  race_ethnicity_DEC_00_SF3_P007$total, 
  digits = 5)
race_ethnicity_DEC_00_SF3_P007$percent_AIAN[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_AIAN)] = 0

# Asian
race_ethnicity_DEC_00_SF3_P007$percent_Asian <- 
  round(race_ethnicity_DEC_00_SF3_P007$non_HL_Asian/
  race_ethnicity_DEC_00_SF3_P007$total, 
  digits = 5) 
race_ethnicity_DEC_00_SF3_P007$percent_Asian[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_Asian)] = 0

# Native Hawaiian/Other Pacific Islander
race_ethnicity_DEC_00_SF3_P007$percent_NHOPI <- 
  round(race_ethnicity_DEC_00_SF3_P007$non_HL_NHOPI/
  race_ethnicity_DEC_00_SF3_P007$total, 
  digits = 5)
race_ethnicity_DEC_00_SF3_P007$percent_NHOPI[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_NHOPI)] = 0

# other
race_ethnicity_DEC_00_SF3_P007$percent_other <- 
  round(race_ethnicity_DEC_00_SF3_P007$non_HL_other/
  race_ethnicity_DEC_00_SF3_P007$total, 
  digits = 5)
race_ethnicity_DEC_00_SF3_P007$percent_other[
  is.nan(race_ethnicity_DEC_00_SF3_P007$percent_other)] = 0

# remove duplicate data from race/ethnicity
race_ethnicity_DEC_00_SF3_P007$GEOID <- NULL
race_ethnicity_DEC_00_SF3_P007$GEO_label <- NULL

# merge poverty and race/ethnicity data
ArcGIS_data_CT_EJ_10 <- 
  merge(poverty_DEC_00_SF3_P088, 
        race_ethnicity_DEC_00_SF3_P007, 
        by = "GEOID2")
# View(ArcGIS_data_CT_EJ_10)

# fix GEOID to be a character
ArcGIS_data_CT_EJ_10$GEOID2 <- 
  as.character(ArcGIS_data_CT_EJ_10$GEOID2)

# 
str(ArcGIS_data_CT_EJ_10)

# make CSV of the ArcGIS data
write.csv(ArcGIS_data_CT_EJ_10, 
          file="ArcGIS_data_CT_EJ_10.csv")



#### 2010 US Census data ####

# race categories

# make vector of race variable labels
census_2010_race_variables <- 
  paste("P00", 50001:50017, sep = "")

# make vector of poverty variable labels
acs_2008_2012_poverty_variables <- 
  paste("C17002_", sprintf('%0.3d', 1:8), sep = "")
acs_2008_2012_poverty_variables

# pull the actual variables
census_2010_via_totalcensus <- 
  read_decennial(year = 2010, 
                 states = "CT", 
                 table_contents = census_2010_race_variables,
                 summary_level = "block group", 
                 geo_headers = 
                   c("STATE", 
                     "COUNTY", 
                     "TRACT", 
                     "BLKGRP"),
                 show_progress = TRUE)

# add GEOID
census_2010_via_totalcensus <- 
  census_2010_via_totalcensus %>% 
  mutate(GEOID = 
           paste0(SUMLEV, 
                  "00US", 
                  STATE, 
                  COUNTY, 
                  TRACT, 
                  BLKGRP))
  
# pull the actual variables
acs_2008_2012_via_totalcensus <- 
  read_acs5year(year = 2012, 
                states = "CT", 
                table_contents = acs_2008_2012_poverty_variables, 
                summary_level = "block group",
                show_progress = TRUE)

# View(census_2010_via_totalcensus)
# View(acs_2008_2012_via_totalcensus)
  
# join poverty and race tables
all_2010_data <- 
  full_join(census_2010_via_totalcensus, 
            acs_2008_2012_via_totalcensus, 
            by = c("GEOID", "lon", "lat", "SUMLEV", "GEOCOMP"))
# View(all_2010_data)



#### make percentages ####
all_2010_data <- 
  all_2010_data %>% 
  mutate(
    pct_hisp = P0050010/P0050001
  )

all_2010_data <- 
  all_2010_data %>% 
  mutate(
    pct_nonwhite = (P0050001 - P0050003)/P0050001
  )

all_2010_data <- 
  all_2010_data %>% 
  mutate(
    pct_black = P0050004/P0050001
  )

all_2010_data <- 
  all_2010_data %>% 
  mutate(
    pct_pov = (C17002_002 + C17002_003)/C17002_001
  )

all_2010_data <- 
  all_2010_data %>% 
  mutate(
    pct_200_pov = (C17002_002 + 
                     C17002_003 + 
                     C17002_004 + 
                     C17002_005 + 
                     C17002_006 + 
                     C17002_007) / C17002_001
  )

# record long GEOID for posterity
all_2010_data <- 
  all_2010_data %>% 
  mutate(
    GEOID_LONG = GEOID)

# make short GEOID for posterity and to match other files
all_2010_data <- 
  all_2010_data %>% 
  mutate(
    GEOID = gsub("15000US", "", GEOID))
# View(all_2010_data)

save(
  all_2010_data,
  file = "all_2010_data.rdata"
)

write.csv(
  all_2010_data,
  file = "all_2010_data.csv"
)






#### TRI release analysis ####
tri_releases_2017_block_groups_raw <- 
  fread("tri_releases_2017_block_groups.txt")

tri_releases_2016_block_groups_raw <-  
  fread("tri_releases_2016_block_groups.txt")

tri_releases_2008_block_groups_raw <-   
  fread("tri_releases_2008_block_groups.txt")

tri_2017_bgs_working <-   
  tri_releases_2017_block_groups_raw

tri_2016_bgs_working <-   
  tri_releases_2016_block_groups_raw

tri_2008_bgs_working <- 
  tri_releases_2008_block_groups_raw
  


#### fix chem_id_issues ####
tri_2016_chem_and_cas <- 
  tri_2016_bgs_working[,c("CHEMICAL", "CAS___COMP")]

cas_numbers_of_added_tri_chemicals_since_2008 <- "81492|3296900|110009|556525|7879593152|91236|75525|77098|116143|509148|75025|42397648|42397659|7496028|57835924|7783064|88722|104405|11066492|25154523|26543975|84852153|90481042|106945|3194556|25637994|7311275|9016459|20427843|26027383|26571119|27176938|27177055|27177088|27986363|37205871|51938251|68412544|127087870"

rows_of_chemicals_added_since_2008 <- 
  grep(cas_numbers_of_added_tri_chemicals_since_2008, 
       tri_2016_bgs_working$CAS___COMP)

# fix 2016 block groups
tri_2016_bgs_working$added_since_2008 <- 
  NA

tri_2016_bgs_working <- 
  within(tri_2016_bgs_working, 
         added_since_2008[as.numeric(rownames(tri_2016_bgs_working)) %in% 
                            rows_of_chemicals_added_since_2008] <- 'yes')

tri_2016_bgs_working$added_since_2008[
  is.na(
    tri_2016_bgs_working$added_since_2008)] <- 'no'

tri_2016_bgs_working <- 
  subset(tri_2016_bgs_working, 
         added_since_2008 == 'no')

# fix 2017 block groups
tri_2017_bgs_working$added_since_2008 <- 
  NA

tri_2017_bgs_working <- 
  within(tri_2017_bgs_working, 
         added_since_2008[as.numeric(rownames(tri_2017_bgs_working)) %in% rows_of_chemicals_added_since_2008] <- 'yes')

tri_2017_bgs_working$added_since_2008[
  is.na(
    tri_2017_bgs_working$added_since_2008)] <- 'no'

tri_2017_bgs_working <- 
  subset(tri_2017_bgs_working, 
         added_since_2008 == 'no')

# View(tri_2016_bgs_working)



#### make grams into pounds ####
tri_2008_bgs_working$TRUE_TOTAL_RELEASE_POUNDS <- NA
tri_2016_bgs_working$TRUE_TOTAL_RELEASE_POUNDS <- NA
tri_2017_bgs_working$TRUE_TOTAL_RELEASE_POUNDS <- NA

# gram conversion function (takes dataframe, returns revised dataframe)
grams_to_pounds <- function(x) {
  x$TRUE_TOTAL_RELEASE_POUNDS <- 
    ifelse(x$UNIT_OF_ME == "Grams", 
           x$TOTAL_RELE/453.592, 
           x$TOTAL_RELE) 
  return(x)
}


# convert grams and move pounds over to the true total release column
tri_2008_bgs_working <- grams_to_pounds(tri_2008_bgs_working)
tri_2016_bgs_working <- grams_to_pounds(tri_2016_bgs_working)
tri_2017_bgs_working <- grams_to_pounds(tri_2017_bgs_working)



#### count facilities ####

facilities_2017 <- 
  tri_2017_bgs_working[!duplicated(
    tri_2017_bgs_working$TRI_FACILI), ]

facilities_2016 <-   # n = 280
  tri_2016_bgs_working[!duplicated(
    tri_2016_bgs_working$TRI_FACILI), ]
# View(facilities_2016)

facilities_2008 <-  # n = 341
  tri_2008_bgs_working[!duplicated(
    tri_2008_bgs_working$TRI_FACILI), ]
# View(facilities_2008)

# get total releases
releases_by_bg_2017 <- 
  aggregate(TRUE_TOTAL_RELEASE_POUNDS ~ GEOID2_Tex, 
            tri_2017_bgs_working, 
            sum) 
colnames(releases_by_bg_2017)[
  colnames(releases_by_bg_2017) == "TRUE_TOTAL_RELEASE_POUNDS"] <- 
  "rel_n_2017"
# View(releases_by_bg_2016)

releases_by_bg_2016 <- 
  aggregate(TRUE_TOTAL_RELEASE_POUNDS ~ GEOID2_Tex, 
            tri_2016_bgs_working, 
            sum) 
colnames(releases_by_bg_2016)[
  colnames(releases_by_bg_2016) == "TRUE_TOTAL_RELEASE_POUNDS"] <- 
  "rel_n_2016"
# View(releases_by_bg_2016)

releases_by_bg_2008 <- 
  aggregate(TRUE_TOTAL_RELEASE_POUNDS ~ GEOID2_Tex, 
            tri_2008_bgs_working, 
            sum)
colnames(releases_by_bg_2008)[
  colnames(releases_by_bg_2008) == "TRUE_TOTAL_RELEASE_POUNDS"] <- 
  "rel_n_2008"
# View(releases_by_bg_2008)



# count facilties
facility_count_by_bg_2016 <- 
  facilities_2016 %>% 
  count(GEOID2_Tex)
colnames(facility_count_by_bg_2016)[
  colnames(facility_count_by_bg_2016) == "n"] <- 
  "fac_n_2016"
# View(facility_count_by_bg_2016)

facility_count_by_bg_2008 <- 
  facilities_2008 %>% 
  count(GEOID2_Tex)
colnames(facility_count_by_bg_2008)[
  colnames(facility_count_by_bg_2008) == "n"] <- 
  "fac_n_2008"
# View(facility_count_by_bg_2008)



# import full block group data 
bgs_all <- 
  fread("ct_census_bg_data_for_r.txt")
# # View(bgs_all)

bg_geoids_only <- 
  bgs_all[,c("GEOID2_Tex", 
             "GEOID")]

# merge prior files with block group data
all_bgs_tri_data <- 
  left_join(bg_geoids_only, 
            releases_by_bg_2008, 
            by = "GEOID2_Tex", 
            all.x = TRUE)

all_bgs_tri_data <- 
  left_join(all_bgs_tri_data, 
            releases_by_bg_2016, 
            by = "GEOID2_Tex")

all_bgs_tri_data <- 
  left_join(all_bgs_tri_data, 
            facility_count_by_bg_2008, 
            by = "GEOID2_Tex")

all_bgs_tri_data <- 
  left_join(all_bgs_tri_data, 
            facility_count_by_bg_2016, 
            by = "GEOID2_Tex")

# make NAs zeroes
all_bgs_tri_data[is.na(all_bgs_tri_data)] <- 0

# differences 
all_bgs_tri_data$rel_diff <- 
  all_bgs_tri_data$rel_n_2016 - 
  all_bgs_tri_data$rel_n_2008

all_bgs_tri_data$fac_diff <- 
  all_bgs_tri_data$fac_n_2016 - 
  all_bgs_tri_data$fac_n_2008

write.csv(all_bgs_tri_data, 
          file = "all_bgs_tri_data.csv")



#### import data back from assignment to covered areas
ej_covered_2008 <- 
  fread(file = "tri_2008_ej_covered.txt")
ej_covered_2016 <- 
  fread(file = "tri_2016_ej_covered.txt")

View(ej_covered_2016)

ej_covered_2008$covered_facility <- 0

ej_covered_2008 <- 
  within(ej_covered_2008, 
         covered_facility[!is.na(GEOID2_Tex)] <- 1)

ej_covered_2016$covered_facility <- 0

ej_covered_2016 <- 
  within(ej_covered_2016, 
         covered_facility[!is.na(GEOID2_Tex)] <- 1)

# fix grams to pounds again
ej_covered_2008 <- grams_to_pounds(ej_covered_2008)
ej_covered_2016 <- grams_to_pounds(ej_covered_2016)

# fix 2016 block groups
ej_covered_2016$added_since_2008 <- 
  NA

ej_covered_2016 <- 
  within(ej_covered_2016, 
         added_since_2008[as.numeric(rownames(ej_covered_2016)) %in% 
                            rows_of_chemicals_added_since_2008] <- 'yes')

ej_covered_2016$added_since_2008[
  is.na(
    ej_covered_2016$added_since_2008)] <- 'no'

ej_covered_2016 <- 
  subset(ej_covered_2016, 
         added_since_2008 == 'no')

write.csv(ej_covered_2016, 
          file = "ej_covered_2016.csv")

write.csv(ej_covered_2008, 
          file = "ej_covered_2008.csv")


releases_in_ej_2008 <- 
  aggregate(TRUE_TOTAL_RELEASE_POUNDS ~ covered_facility, ej_covered_2008, sum)
releases_in_ej_2008

releases_in_ej_2016 <- 
  aggregate(TRUE_TOTAL_RELEASE_POUNDS ~ covered_facility, ej_covered_2016, sum)
releases_in_ej_2016

facilities_2016 <-   # n = 280
  tri_2016_bgs_working[!duplicated(
    tri_2016_bgs_working$TRI_FACILI), ]
# View(facilities_2016)

facilities_2008 <-   # n = 341
  tri_2008_bgs_working[!duplicated(
    tri_2008_bgs_working$TRI_FACILI), ]
# View(facilities_2008)

ej_covered_facility_2008_no_dups <- 
  ej_covered_2008[!duplicated(
    ej_covered_2008$TRI_FACILI), ]

ej_covered_facility_2016_no_dups_or_new <- 
  ej_covered_2016[!duplicated(
    ej_covered_2016$TRI_FACILI), ]

facility_count_in_ej_areas_2008 <- 
  sum(ej_covered_facility_2008_no_dups$covered_facility)
facility_count_in_ej_areas_2008

facility_count_in_ej_areas_2016_no_new <- 
  sum(ej_covered_facility_2016_no_dups_or_new$covered_facility)
facility_count_in_ej_areas_2016_no_new

facilities_not_ej_2008 <- 
  nrow(ej_covered_facility_2008_no_dups) - 
  facility_count_in_ej_areas_2008
facilities_not_ej_2008

facilities_not_ej_2016 <- 
  nrow(ej_covered_facility_2016_no_dups_or_new) - 
  facility_count_in_ej_areas_2016_no_new
facilities_not_ej_2016

ej_block_group_assignments <- 
  fread(file = "block_group_assignments_ej.txt")



# add % in poverty
ej_block_group_assignments$pov_pct <- 
  (ej_block_group_assignments$under_5_51 + 
  ej_block_group_assignments$from_50__1 + 
  ej_block_group_assignments$from_75__1) / 
  ej_block_group_assignments$total_x_1

# add % below 200% of poverty
ej_block_group_assignments$pov_200_pct <- 
  ej_block_group_assignments$fraction_b

write.csv(ej_block_group_assignments, 
          file = "ej_block_group_assignments.csv")
View(ej_block_group_assignments)
  


# remove six bgs without population 
ej_block_groups_people <- 
  ej_block_group_assignments[which(ej_block_group_assignments$total_y > 0),]

density_non_white_ej_law <- 
  density(1 - ej_block_groups_people$percent_wh[which(
    ej_block_groups_people$ej_law == 1)], 
    na.rm = TRUE)
plot(density_non_white, 
     main = "% non-white ej law")

density_non_white_not_ej_law <- 
  density(1 - ej_block_groups_people$percent_wh[
    which(ej_block_groups_people$ej_law == 0)], 
    na.rm = TRUE)
plot(density_non_white_not_ej_law, 
     main = "% non-white not ej law")
  
# t-tests
nrow(ej_block_groups_people[which(ej_block_groups_people$ej_law == 0),])
nrow(ej_block_groups_people[which(ej_block_groups_people$ej_law == 1),])

t_test_percent_non_white <- 
  t.test(
    1 - ej_block_groups_people$percent_wh[ej_block_groups_people$ej_law == 0], 
    1 - ej_block_groups_people$percent_wh[ej_block_groups_people$ej_law == 1])
summary(t_test_percent_non_white)
t_test_percent_non_white

t_test_percent_200_poverty <- 
  t.test(ej_block_groups_people$fraction_b[ej_block_groups_people$ej_law == 0], 
         ej_block_groups_people$fraction_b[ej_block_groups_people$ej_law == 1])
summary(t_test_percent_200_poverty)
t_test_percent_200_poverty

# correlation between poverty and % nonwhite
cor((1 - ej_block_groups_people$percent_wh), 
    ej_block_groups_people$pov_pct, 
    method = c("spearman"), 
    use = "complete.obs")







