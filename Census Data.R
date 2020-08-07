#### CENSUS 2000 ####
setwd("~/Desktop/Demography") #SF1, SF3
setwd("~/Documents/R/ACS_2014") #ACS

#### LOCATIONS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

#### sf1 location ####
sf1_city_county <- subset(sf1_2000_geo, SUMLEV == 392)
austin_sf1_2000 <- subset(sf1_city_county, MSACMSA == 640)

sf1_place_2000 <- subset(sf1_2000_geo, SUMLEV == 160)
sf1_place_austin_2000 <- subset(sf1_place_2000, PLACE == 5000)
sf1_place_roundrock_2000 <- subset(sf1_place_2000, PLACE == 63500)

sf1_logrecno <- c(736417,736419,736420,736421,727536,734499)

#### sf3 location ####
sf3_city_county <- subset(sf3_2000_geo, SUMLEV == 392)
austin_sf3_2000 <- subset(sf3_city_county, MSACMSA == 640)

sf3_place_2000 <- subset(sf3_2000_geo, SUMLEV == 160)
sf3_place_austin_2000 <- subset(sf3_place_2000, PLACE == 5000)
sf3_place_roundrock_2000 <- subset(sf3_place_2000, PLACE == 63500)

sf3_logrecno <- c(76444,76446,76447,76448,67563,74526)

#### acs location ####
acs_2014_geo <- read.csv("g20141tx.csv", header = TRUE)
co_code <- c(21,55,209,453,491)
austin_2014 <- data.frame(matrix(data=0,nrow = 0, ncol = ncol(acs_2014_geo)))
for (i in co_code) {
  austin_2014 <- rbind(austin_2014, subset(acs_2014_geo, COUNTY == i))
}

acs_logrecno <- c(14,36,60,65,71,112)

#### SF1 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####

sf1_2000_geo_widths <- c(6,2,3,2,3,2,7,1,1,2,2,3,2,5,2,2,5,2,1,2,6,1,4,2,5,2,2,4,5,2,1,3,5,2,5,2,4,2,2,1,4,4,1,2,
                         1,5,2,1,1,2,2,2,2,3,3,6,1,3,5,5,2,14,14,90,1,1,9,9,9,10,2,1,5,5,5,6,5,5,5,15,5,5,7)
sf1_2000_geo_master <- read.fwf("2000_SF1/txgeo.uf1", widths = sf1_2000_geo_widths, header = FALSE)
sf1_2000_geo <- sf1_2000_geo_master
j <- sapply(sf1_2000_geo, as.character)
j[is.na(j)] <- ""
j <- as.data.frame(j)
names(j) <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO","REGION",
              "DIVISION","STATECE","STATE","COUNTY","COUNTYSC","COUSUB","COUSUBCC","COUSUBSC",
              "PLACE","PLACECC","PLACEDC","PLACESC","TRACT","BLKGRP","BLOCK","IUC","CONCIT","CONCITCC","CONCITSC",
              "AIANHH","AIANHHFP","AIANHHCC","AIHHTLI","AITSCE","AITS","AITSCC","ANRC","ANRCCC","MSACMSA","MASC",
              "CMSA","MACCI","PMSA","NECMA","NECMACCI","NECMASC","EXI","UA","UASC","UATYPE","UR","CD106","CD108",
              "CD109","CD110","SLDU","SLDL","VTD","VTDI","ZCTA3","ZCTA5","SUBMCD","SUBMCDCC","AREALAND",
              "AREAWATER","NAME","FUNCSTAT","GCUNI","POP100","HU100","INTPTLAT","INTPTLON","LSADC","PARTFLAG",
              "SDELM","SDSEC","SDUNI","TAZ","UGA","PUMA5","PUMA1","RESERVE2","MACC","UACP","RESERVED")
j[] <- lapply(j,as.character)
for (i in c(3:5,7:14,16:17,20:23,25:29,32:36,42:44,50,58,60:63,67:70,73:75,77,81)) {
  j[,i] <- as.numeric(j[,i])
}
j[is.na(j)] <- ""
sf1_2000_geo <- j
View(sf1_2000_geo)
#write.csv(sf1_2000_geo,"sf1_2000_geo.csv")
j <- read.csv("2000_SF1/sf1_2000_geo.csv", header = TRUE)
j[is.na(j)] <- ""
j[] <- lapply(j,as.character)
for (i in c(3:5,7:14,16:17,20:23,25:29,32:36,42:44,50,58,60:63,67:70,73:75,77,81)) {
  j[,i] <- as.numeric(j[,i])
}
j[is.na(j)] <- ""
sf1_2000_geo <- j

sf1_2000_1 <- read.csv("2000_SF1/tx00001.csv", header = FALSE)
sf1_2000_2 <- read.csv("2000_SF1/tx00002.csv", header = FALSE)
sf1_2000_37 <- read.csv("2000_SF1/tx00037.csv", header = FALSE)

age_by_sex_2000 <- sf1_2000_2[127:175]
names(age_by_sex_2000) <- c("total","male","m_under_5","m_5_9","m_10_14","m_15_17","m_18_19","m_20","m_21","m_22_24","m_25_29","m_30_34","m_35_39","m_40_44","m_45_49","m_50_54","m_55_59","m_60_61","m_62_64","m_65_66","m_67_69","m_70_74","m_75_79","m_80_84","m_over_85","female","f_under_5","f_5_9","f_10_14","f_15_19","f_20","f_21","f_22_24","f_25_29","f_30_34","f_35_39","f_40_44","f_45_49","f_50_54","f_55_59","f_60_61","f_62_64","f_65_66","f_67_69","f_70_74","f_75_79","f_80_84","f_over_85")

household_size_by_tenure_2000 <- sf1_2000_37[105:121]
names(household_size_by_tenure_2000) <- c("total","owner","o_1","o_2","o_3","o_4","o_5","o_6","o_7","renter","r_1","r_2","r_3","r_4","r_5","r_6","r_7")



#### total population ####
total_pop_2000 <- sf1_2000_1[6]
names(total_pop_2000) <- "total"

#### race ####
race_2000 <- sf1_2000_2[79:86]
names(race_2000) <- c("total","white","black","indian","asian","hawaian","other","two_or_more")

#### household size ####
cnt <- 1
house_size_2000 <- as.data.frame(matrix(data = 0,nrow = nrow(household_size_by_tenure_2000),ncol = 7))
for (i in 3:9) {
  house_size_2000[cnt] <- household_size_by_tenure_2000[i] + household_size_by_tenure_2000[i+8]
  cnt <- cnt + 1
}
names(house_size_2000) <- c("1_person","2_person","3_person","4_person","5_person","6_person","7_person")

#### tenure ####
tenure_2000 <- as.data.frame(cbind(household_size_by_tenure_2000$owner,household_size_by_tenure_2000$renter))
names(tenure_2000) <- c("owner","renter")

#### age ####
cnt <- 1
age_2000 <- as.data.frame(matrix(data = 0,nrow = nrow(household_size_by_tenure_2000),ncol = 23))
for (i in 3:25) {
  age_2000[cnt] <- age_by_sex_2000[i] + age_by_sex_2000[i+24]
  cnt <- cnt + 1
}
names(age_2000) <- c("under_5","5_9","10_14","15_17","18_19","20","21","22_24","25_29","30_34","35_39","40_44","45_49","50_54","55_59","60_61","62_64","65_66","67_69","70_74","75_79","80_84","over_85")

#### sex ####
sex_2000 <- as.data.frame(cbind(age_by_sex_2000$male,age_by_sex_2000$female))
names(sex_2000) <- c("male","female")

#### SF3 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
setwd("~/Desktop/Demography/")
sf3_2000_geo_widths <- c(6,2,3,2,3,2,7,1,1,2,2,3,2,5,2,2,5,2,1,2,6,1,4,2,5,2,2,4,5,2,1,3,5,2,5,2,4,2,2,1,4,4,1,2,
                         1,5,2,1,1,2,2,2,2,3,3,6,1,3,5,5,2,14,14,90,1,1,9,9,9,10,2,1,5,5,5,6,5,5,5,15,5,5,7)
sf3_2000_geo_master <- read.fwf("2000_SF3/txgeo.uf3", widths = sf3_2000_geo_widths, header = FALSE)
sf3_2000_geo <- sf3_2000_geo_master
j <- sapply(sf3_2000_geo, as.character)
j[is.na(j)] <- ""
j <- as.data.frame(j)
names(j) <- c("FILEID","STUSAB","SUMLEV","GEOCOMP","CHARITER","CIFSN","LOGRECNO","REGION",
              "DIVISION","STATECE","STATE","COUNTY","COUNTYSC","COUSUB","COUSUBCC","COUSUBSC",
              "PLACE","PLACECC","PLACEDC","PLACESC","TRACT","BLKGRP","BLOCK","IUC","CONCIT","CONCITCC","CONCITSC",
              "AIANHH","AIANHHFP","AIANHHCC","AIHHTLI","AITSCE","AITS","AITSCC","ANRC","ANRCCC","MSACMSA","MASC",
              "CMSA","MACCI","PMSA","NECMA","NECMACCI","NECMASC","EXI","UA","UASC","UATYPE","UR","CD106","CD108",
              "CD109","CD110","SLDU","SLDL","VTD","VTDI","ZCTA3","ZCTA5","SUBMCD","SUBMCDCC","AREALAND",
              "AREAWATER","NAME","FUNCSTAT","GCUNI","POP100","HU100","INTPTLAT","INTPTLON","LSADC","PARTFLAG",
              "SDELM","SDSEC","SDUNI","TAZ","UGA","PUMA5","PUMA1","RESERVE2","MACC","UACP","RESERVED")
j[] <- lapply(j,as.character)
for (i in c(3:5,7:14,16:17,20:23,25:29,32:36,42:44,50,58,60:63,67:70,73:75,77,81)) {
  j[,i] <- as.numeric(j[,i])
}
j[is.na(j)] <- ""
sf3_2000_geo <- j
View(sf3_2000_geo)

#write.csv(sf3_2000_geo,"sf3_2000_geo.csv")

j <- read.csv("2000_SF3/sf3_2000_geo.csv", header = TRUE)
j[] <- lapply(j,as.character)
for (i in c(3:5,7:14,16:17,20:23,25:29,32:36,42:44,50,58,60:63,67:70,73:75,77,81)) {
  j[,i] <- as.numeric(j[,i])
}
j[is.na(j)] <- ""
sf3_2000_geo <- j

sf3_2000_3 <- read.csv("2000_SF3/tx00003.csv", header = FALSE)
sf3_2000_7 <- read.csv("2000_SF3/tx00007.csv", header = FALSE)
sf3_2000_5 <- read.csv("2000_SF3/tx00005.csv", header = FALSE)
sf3_2000_58 <- read.csv("2000_SF3/tx00058.csv", header = FALSE)
sf3_2000_60 <- read.csv("2000_SF3/tx00060.csv", header = FALSE)
sf3_2000_59 <- read.csv("2000_SF3/tx00059.csv", header = FALSE)
sf3_2000_2 <- read.csv("2000_SF3/tx00002.csv", header = FALSE)

education_by_sex_2000 <- sf3_2000_3[207:241]
names(education_by_sex_2000) <- c("total_pop_over_25","male","m_none","m_nursery_4th","m_5th_6th","m_7th_8th","m_9th","m_10th","m_11th","m_12th","m_hsgrad","m_col_under_1","m_col","m_associate","m_bachelors","m_masters","m_professional","m_doctorate","female","f_none","f_nursery_4th","f_5th_6th","f_7th_8th","f_9th","f_10th","f_11th","f_12th","f_hsgrad","f_col_under_1","f_col","f_associate","f_bachelors","f_masters","f_professional","f_doctorate")

industry_by_sex_2000 <- sf3_2000_5[76:130]
names(industry_by_sex_2000) <- c("total_under_16","male","m_ag_hunt_mining","m_ag_hunt","m_mining","m_construction","m_manufacturing","m_wholesale","m_retail","m_trans_utils","m_trans","m_utils","m_info","m_fin_ins_real_rent","m_fin_ins","m_real_rent","m_pro_sci_man_admin_waste","m_pro_sci_tech","m_man","m_admin_waste","m_ed_health_soc","m_ed","m_health_soc","m_arts_ent_rec_food_lodge","m_arts_ent_rec","m_food_lodge","m_other","m_gov","female","f_ag_hunt_fining","f_ag_hunt","f_mining","f_construction","f_manufacturing","f_wholesale","f_retail","f_trans_utils","f_trans","f_utils","f_info","f_fin_ins_real_rent","f_fin_ins","f_real_rent","f_pro_sci_man_admin_waste","f_pro_sci_tech","f_man","f_admin_waste","f_ed_health_soc","f_ed","f_health_soc","f_arts_ent_rec_food_lodge","f_arts_ent_rec","f_food_lodge","f_other","f_gov")

length_by_tenure_2000 <- sf3_2000_58[122:136]
names(length_by_tenure_2000) <- c("total","owner","o99_00","o95_98","o90_94","o80_89","o70_79","o69","renter","r99_00","r95_98","r90_94","r80_89","r70_79","r69")

citizenship_by_birthplace_2000 <- sf3_2000_2[172:186]

#### rent ####
rent_2000 <- sf3_2000_59[203]
names(rent_2000) <- "median_rent"

#### value ####
value_2000 <- sf3_2000_60[252]
names(value_2000) <- "median_value"

#### length ####
cnt <- 1
length_2000 <- as.data.frame(matrix(data = 0,nrow = nrow(length_by_tenure_2000),ncol = 6))
for (i in 3:8) {
  length_2000[cnt] <- length_by_tenure_2000[i] + length_by_tenure_2000[i+7]
  cnt <- cnt + 1
}
names(length_2000) <- c("99_00","95_98","90_94","80_89","70_79","before_69")

#### citizenship ####
citizenship_2000 <- as.data.frame(cbind(citizenship_by_birthplace_2000[2] + citizenship_by_birthplace_2000[14], citizenship_by_birthplace_2000[15]))
names(citizenship_2000) <- c("citizen","non-citizen")

#### education ####
cnt <- 1
education_2000 <- as.data.frame(matrix(data = 0,nrow = nrow(education_by_sex_2000),ncol = 16))
for (i in 3:18) {
  education_2000[cnt] <- education_by_sex_2000[i] + education_by_sex_2000[i+17]
  cnt <- cnt + 1
}
names(education_2000) <- c("none","nursery_4th","5th_6th","7th_8th","9th","10th","11th","12th","hsgrad","col_under_1","col","associate","bachelors","masters","professional","doctorate")

#### income ####
income_2000 <- sf3_2000_7[31]
names(income_2000) <- "median_income"

#### occupation ####
cnt <- 1
industry_2000_big <- as.data.frame(matrix(data = 0,nrow = nrow(industry_by_sex_2000),ncol = 26))
for (i in 3:28) {
  industry_2000_big[cnt] <- industry_by_sex_2000[i] + industry_by_sex_2000[i+27]
  cnt <- cnt + 1
}
names(industry_2000_big) <- c("ag_hunt_mining","ag_hunt","mining","construction","manufacturing","wholesale","retail","trans_utils","trans","utils","info","fin_ins_real_rent","fin_ins","real_rent","pro_sci_man_admin_waste","pro_sci_tech","man","admin_waste","ed_health_soc","ed","health_soc","arts_ent_rec_food_lodge","arts_ent_rec","food_lodge","other","gov")
industry_2000 <- industry_2000_big[c(1,4,5,6,7,8,11,12,15,19,22,25,26)]

#### ACS 2014 ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
setwd("~/Documents/R/ACS_2014")

acs_fun <- function(name, file, seq_file){
  temp1 <- read.csv(file, header = FALSE)
  temp2 <- read.csv(seq_file, header = TRUE)
  names(temp1) <- names(temp2)
  return(temp1)
}

acs_2014_1 <- acs_fun(penis,"e20141tx0001000.csv","Seq1.csv")

acs_2014_2 <- acs_fun(penis,"e20141tx0002000.csv","Seq2.csv")
age_by_sex_2014 <- acs_2014_2[7:55]

acs_2014_4 <- acs_fun(penis,"e20141tx0004000.csv","Seq4.csv")

acs_2014_77 <- acs_fun(penis,"e20141tx0077000.csv","Seq77.csv")

acs_2014_55 <- acs_fun(penis,"e20141tx0055000.csv","Seq55.csv")
education_by_sex_2014 <- acs_2014_55[90:124]

acs_2014_77 <- acs_fun(penis,"e20141tx0077000.csv","Seq77.csv")

acs_2014_138 <- acs_fun(penis,"e20141tx0138000.csv","Seq138.csv")
household_size_by_tenure_2014 <- acs_2014_138[86:102]

acs_2014_10 <- acs_fun(penis,"e20141tx0010000.csv","Seq10.csv")
nativity_citizenship_2014 <- acs_2014_10[7:12]

acs_2014_140 <- acs_fun(penis,"e20141tx0140000.csv","Seq140.csv")
length_by_tenure_2014 <- acs_2014_140[66:80]
 
#acs_2014_111 <- acs_fun(penis,"e20141tx0111000.csv","Seq111.csv")
#industry_by_sex_2014 <- acs_2014_111[7:215]

acs_2014_33 <- acs_fun(penis,"e20141tx0033000.csv","Seq33.csv")
industry_by_trans_2014 <- acs_2014_33[7:111]

acs_2014_142 <- acs_fun(penis,"e20141tx0142000.csv","Seq142.csv")

acs_2014_141 <- acs_fun(penis,"e20141tx0141000.csv","Seq141.csv")

#### total population ####
total_pop_2014 <- acs_2014_2[7]
names(total_pop_2014) <- "total"

#### rent ####
rent_2014 <- acs_2014_141[95]
names(rent_2014) <- "median_rent"

#### value ####
value_2014 <- acs_2014_142[97]
names(value_2014) <- "median_value"

#### length ####
cnt <- 1
length_2014 <- as.data.frame(matrix(data = 0,nrow = nrow(length_by_tenure_2014),ncol = 6))
for (i in 3:8) {
  length_2014[cnt] <- length_by_tenure_2014[i] + length_by_tenure_2014[i+7]
  cnt <- cnt + 1
}
names(length_2014) <- c("after_10","00_09","90_99","80_89","70_79","before_69")

#### citizenship ####
citizenship_2014 <- as.data.frame(cbind(sum(nativity_citizenship_2014[2:5]),nativity_citizenship_2014[6]))
names(citizenship_2014) <- c("citizen","non-citizen")

#### age ####
cnt <- 1
age_2014 <- as.data.frame(matrix(data = 0,nrow = nrow(age_by_sex_2014),ncol = 23))
for (i in 3:25) {
  age_2014[cnt] <- age_by_sex_2014[i] + age_by_sex_2014[i+24]
  cnt <- cnt + 1
}
names(age_2014) <- c("under_5","5_9","10_14","15_17","18_19","20","21","22_24","25_29","30_34","35_39","40_44","45_49","50_54","55_59","60_61","62_64","65_66","67_69","70_74","75_79","80_84","over_85")

#### sex ####
sex_2014 <- as.data.frame(cbind(age_by_sex_2014[2],age_by_sex_2014[26]))
names(sex_2014) <- c("male","female")

#### race ####
race_2014 <- acs_2014_4[7:16]
names(race_2014) <- c("total","white","black","indian","asian","hawaian","other","two_or_more")

#### income ####
income_2014 <- acs_2014_77[177]
names(income_2014) <- "median_income"

#### education ####
cnt <- 1
education_2014 <- as.data.frame(matrix(data = 0,nrow = nrow(education_by_sex_2014),ncol = 16))
for (i in 3:18) {
  education_2014[cnt] <- education_by_sex_2014[i] + education_by_sex_2014[i+17]
  cnt <- cnt + 1
}
names(education_2014) <- c("none","nursery_4th","5th_6th","7th_8th","9th","10th","11th","12th","hsgrad","col_under_1","col","associate","bachelors","masters","professional","doctorate")

#### tenure ####
tenure_2014 <- acs_2014_138[12:13]
names(tenure_2014) <- c("owner","renter")

#### household size ####
cnt <- 1
house_size_2014 <- as.data.frame(matrix(data = 0,nrow = nrow(household_size_by_tenure_2014),ncol = 7))
for (i in 3:9) {
  house_size_2014[cnt] <- household_size_by_tenure_2014[i] + household_size_by_tenure_2014[i+8]
  cnt <- cnt + 1
}
names(house_size_2014) <- c("1_person","2_person","3_person","4_person","5_person","6_person","7_person")

#### industry ####
industry_2014 <- industry_by_trans_2014[2:14]
names(industry_2014) <- c("ag_hunt_mining","construction","manufacturing","wholesale","retail","trans_utils","info","fin_ins_real_rent","pro_sci_man_admin_waste","ed_health_soc","art_ent_rec_food_lodge","other","gov")


#### RESULTS ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ####
template <- data.frame(region = c("Bastrop County", "Hays County", "Travis County", "Williamson County", "Austin City", "Round Rock City"))
res_fun <- function (sf_2000,acs_2014,sf1val) {
  if (ncol(sf_2000) == 1) {
    if (sf1val == TRUE) {
      sf_temp <- cbind(template, sf_2000[sf1_logrecno,])
      names(sf_temp) <- c("region", names(sf_2000))
    } else {
      sf_temp <- cbind(template, sf_2000[sf3_logrecno,])
      names(sf_temp) <- c("region", names(sf_2000))
    }
    acs_temp <- cbind(template, acs_2014[acs_logrecno,])
    names(acs_temp) <- c("region", names(acs_2014))
    full_temp <- list(sf_temp, acs_temp)
    return(full_temp)
  } else {
    if (sf1val == TRUE) {
      sf_temp <- cbind(template, rowSums(sf_2000[sf1_logrecno,]), sf_2000[sf1_logrecno,])
      names(sf_temp) <- c("region","universe total",names(sf_2000))
    } else {
      sf_temp <- cbind(template, rowSums(sf_2000[sf3_logrecno,]), sf_2000[sf3_logrecno,])
      names(sf_temp) <- c("region","universe total",names(sf_2000))
    }
    acs_temp <- cbind(template, rowSums(acs_2014[acs_logrecno,]), acs_2014[acs_logrecno,])
    names(acs_temp) <- c("region","universe total", names(acs_2014))
    full_temp <- list(sf_temp, acs_temp)
    return(full_temp)
  }
}

#### total population ####
list_total_pop <- res_fun(total_pop_2000,total_pop_2014,sf1val = TRUE)
total_pop_res_2000 <- list_total_pop[[1]]
total_pop_res_2014 <- list_total_pop[[2]]

#### age ####
list_age <- res_fun(age_2000,age_2014,sf1val = TRUE)
age_res_2000 <- list_age[[1]]
age_res_2014 <- list_age[[2]]

#### race ####
list_race <- res_fun(race_2000,race_2014,sf1val = TRUE)
race_res_2000 <- list_race[[1]]
race_res_2014 <- list_race[[2]]

#### household size ####
list_house_size <- res_fun(house_size_2000,house_size_2014,sf1val = TRUE)
house_size_res_2000 <- list_house_size[[1]]
house_size_res_2014 <- list_house_size[[2]]

#### tenure ####
list_tenure <- res_fun(tenure_2000,tenure_2014,sf1val = TRUE)
tenure_res_2000 <- list_tenure[[1]]
tenure_res_2014 <- list_tenure[[2]]

#### sex ####
list_sex <- res_fun(sex_2000,sex_2014,sf1val = TRUE)
sex_res_2000 <- list_sex[[1]]
sex_res_2014 <- list_sex[[2]]

#### rent ####
list_rent <- res_fun(rent_2000,rent_2014,sf1val = FALSE)
rent_res_2000 <- list_rent[[1]]
rent_res_2014 <- list_rent[[2]]

#### income ####
list_income <- res_fun(income_2000,income_2014,sf1val = FALSE)
income_res_2000 <- list_income[[1]]
income_res_2014 <- list_income[[2]]

#### value ####
list_value <- res_fun(value_2000,value_2014,sf1val = FALSE)
value_res_2000 <- list_value[[1]]
value_res_2014 <- list_value[[2]]

#### length ####
list_length <- res_fun(length_2000,length_2014,sf1val = FALSE)
length_res_2000 <- list_length[[1]]
length_res_2014 <- list_length[[2]]

#### citizenship ####
list_citizenship <- res_fun(citizenship_2000,citizenship_2014,sf1val = FALSE)
citizenship_res_2000 <- list_citizenship[[1]]
citizenship_res_2014 <- list_citizenship[[2]]

#### education ####
list_education <- res_fun(education_2000,education_2014,sf1val = FALSE)
education_res_2000 <- list_education[[1]]
education_res_2014 <- list_education[[2]]

#### occupation ####
list_industry <- res_fun(industry_2000,industry_2014,sf1val = FALSE)
industry_res_2000 <- list_industry[[1]]
industry_res_2014 <- list_industry[[2]]


