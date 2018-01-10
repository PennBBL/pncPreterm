#################
### LOAD DATA ###
#################

##Demographic data (n=1629)
data.demo <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/demographics/n1601_demographics_go1_20161212.csv", header=TRUE) 

##Environment data (n=1601)
data.environ <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/environment/n1601_go1_environment_factor_scores_tymoore_20150909.csv", header=TRUE)

##Clinical data
#Screening diagnoses (n=1601)
data.diag <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_goassess_psych_summary_vars_20131014.csv", header=TRUE, na.strings=".")

#Psychosis (n=1601)
data.psy <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_diagnosis_dxpmr_20170509.csv", header=TRUE, na.strings=".")

#Suicidal ideation (n=1601)
data.suicidal <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_goassess_itemwise_smryvars_suicide_20170209.csv", header=TRUE, na.strings=".")

#Item level psychiatric interview (n=1601)
data.items <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_goassess_112_itemwise_vars_20161214.csv", header=TRUE, na.strings=".")

#Bifactors (n=1601)
data.bifactors <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_goassess_itemwise_bifactor_scores_20161219.csv", header=TRUE, na.strings=".")

#Correlated traits (n=1601)
data.corrTraits <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_goassess_itemwise_corrtraits_scores_20161219.csv", header=TRUE, na.strings=".")

#State trait anxiety data (n=1391)
data.stai <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/clinical/n1601_stai_pre_post_itemwise_smry_factors_20170131.csv", header=TRUE, na.strings=".")

##Cognitive scores
#Cognitive factor scores (n=1601)
data.cogFactors <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/cnb/n1601_cnb_factor_scores_tymoore_20151006.csv", header=TRUE, na.strings=".")

#14 cog tests (n=1601)
data.cogTests <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/cnb/n1601_cnb_zscores_all_fr_20161215.csv", header=TRUE, na.strings=".")

#WRAT scores (n=1601)
data.wrat <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/cnb/n1601_cnb_wrat_scores_20161215.csv", header=TRUE, na.strings=".")

##Exclusion data (n=1601)
#Health exclusion (use the new healthExcludev2 variable)
data.healthExclude <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/health/n1601_health_20170421.csv", header=TRUE, na.strings=".")

#T1 QA exclusion (n=1601)
data.t1QA <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/neuroimaging/t1struct/n1601_t1QaData_20170306.csv", header=TRUE, na.strings=".")

##Brain data
#Ravens NMF (n=1396)
data.ravensNMF <- read.csv("/data/jux/BBL/projects/pncPreterm/subjectData/NmfResults26Bases_bblids.csv", header=TRUE)

#JLF volume ROIs (n=1601)
data.volROIs <- read.csv("/data/jux/BBL/studies/pnc/pncDataFreeze/n1601_dataFreeze/neuroimaging/t1struct/n1601_jlfAntsCTIntersectionVol_20170412.csv", header=TRUE)

##Gestational age (n=)
data.ga <- read.csv("/data/jux/BBL/projects/pncPreterm/subjectData/gaData_final.csv", header=TRUE, na.strings="NA")

############################
#### TRANSFORM VARIABLES ###
############################

#Transform the age variable from months to years
data.demo$age <- (data.demo$ageAtScan1)/12

#Make age squared (demeaned)
data.demo$ageSq <- I(scale(data.demo$age, scale=FALSE, center=TRUE)^2)

#Recode male as 0 and female as 1
data.demo$sex[which(data.demo$sex==1)] <- 0
data.demo$sex[which(data.demo$sex==2)] <- 1
data.demo$sex <- as.factor(data.demo$sex)

#Make White (1) vs Non-white (0)
data.demo$white <- 0
data.demo$white[which(data.demo$race==1)] <- 1 
data.demo$white <- as.factor(data.demo$white)

#Make preterm variable
data.ga$preterm <- NA
data.ga$preterm[data.ga$ga < 37] <- 1
data.ga$preterm[data.ga$ga >= 37] <- 0

#Make preterm bins
data.ga$pretermBins <- "fullterm"
data.ga$pretermBins[data.ga$ga < 39] <- "earlyterm"
data.ga$pretermBins[data.ga$ga < 37] <- "latePreterm"
data.ga$pretermBins[data.ga$ga < 34] <- "moderatelyPreterm"
data.ga$pretermBins[data.ga$ga < 32] <- "veryPreterm"
data.ga$pretermBins[data.ga$ga < 28] <- "extremeleyPreterm"

#Remove sui001 and sui002 from data.items because they are redundant with data.suicidal
data.items$sui001 <- NULL
data.items$sui002 <- NULL

##################
### MERGE DATA ###
##################
dataMerge1 <-merge(data.demo,data.environ, by=c("bblid","scanid"), all=TRUE) 
dataMerge2 <-merge(dataMerge1,data.diag, by=c("bblid","scanid"), all=TRUE) 
dataMerge3 <-merge(dataMerge2,data.psy, by=c("bblid","scanid"), all=TRUE) 
dataMerge4 <-merge(dataMerge3,data.suicidal, by=c("bblid","scanid"), all=TRUE)
dataMerge5 <-merge(dataMerge4,data.items, by=c("bblid","scanid"), all=TRUE)
dataMerge6 <-merge(dataMerge5,data.bifactors, by=c("bblid","scanid"), all=TRUE)
dataMerge7 <-merge(dataMerge6,data.corrTraits, by=c("bblid","scanid"), all=TRUE)
dataMerge8 <-merge(dataMerge7,data.stai, by=c("bblid","scanid"), all=TRUE)
dataMerge9 <-merge(dataMerge8,data.cogFactors, by=c("bblid","scanid"), all=TRUE)
dataMerge10 <-merge(dataMerge9,data.cogTests, by=c("bblid","scanid"), all=TRUE)
dataMerge11 <-merge(dataMerge10,data.wrat, by=c("bblid","scanid"), all=TRUE)
dataMerge12 <-merge(dataMerge11,data.healthExclude, by=c("bblid","scanid"), all=TRUE)
dataMerge13 <-merge(dataMerge12,data.t1QA, by=c("bblid","scanid"), all=TRUE)
dataMerge14 <-merge(dataMerge13,data.ravensNMF, by=c("bblid","scanid"), all=TRUE)
dataMerge15 <-merge(dataMerge14,data.volROIs, by=c("bblid","scanid"), all=TRUE)
dataMerge16 <-merge(dataMerge15,data.ga, by="bblid", all=TRUE)

#Retain only the 1601 bblids (demographics has 1629)
data.n1601 <- dataMerge16[match(data.t1QA$bblid, dataMerge16$bblid, nomatch=0),] 

#Put bblids in ascending order
data.ordered <- data.n1601[order(data.n1601$bblid),]

#Count the number of subjects (should be 1601)
n <- nrow(data.ordered)

###########################
### SUBSET TO GA SAMPLE ###
###########################

#Remove those who are missing ga data
data.preterm <- data.ordered[!is.na(data.ordered$preterm),]

#Count the number of subjects (should be 345)
n_preterm <- nrow(data.preterm)

#################################
### APPLY EXCLUSIONS AND SAVE ### 
#################################
##Count the total number excluded for healthExcludev2=1 (1=Excludes those with medical rating 3/4, major incidental findings that distort anatomy, psychoactive medical medications)
#Included: n=303; Excluded: n=42, but medical.exclude (n=21) + incidental.exclude (n=8) + medicalMed.exclude (n=17) = 46, so 4 people were excluded on the basis of two or more of these criteria
data.final <- data.preterm
data.final$ACROSS.INCLUDE.health <- 1
data.final$ACROSS.INCLUDE.health[data.final$healthExcludev2==1] <- 0
health.include<-sum(data.final$ACROSS.INCLUDE.health)
health.exclude<-345-health.include

#Count the number excluded just medical rating 3/4 (GOAssess Medial History and CHOP EMR were used to define one summary rating for overall medical problems) (n=21)
data.final$ACROSS.INCLUDE.medical <- 1
data.final$ACROSS.INCLUDE.medical[data.final$medicalratingExclude==1] <- 0
medical.include<-sum(data.final$ACROSS.INCLUDE.medical)
medical.exclude<-345-medical.include

#Count the number excluded for just major incidental findings that distort anatomy (n=8)
data.final$ACROSS.INCLUDE.incidental <- 1
data.final$ACROSS.INCLUDE.incidental[data.final$incidentalFindingExclude==1] <- 0
incidental.include<-sum(data.final$ACROSS.INCLUDE.incidental)
incidental.exclude<-345-incidental.include

#Count the number excluded for just psychoactive medical medications (n=17)
data.final$ACROSS.INCLUDE.medicalMed <- 1
data.final$ACROSS.INCLUDE.medicalMed[data.final$psychoactiveMedMedicalv2==1] <- 0
medicalMed.include<-sum(data.final$ACROSS.INCLUDE.medicalMed)
medicalMed.exclude<-345-medicalMed.include

#Subset the data to just the  that pass healthExcludev2 (n=303)
data.subset <-data.final[which(data.final$ACROSS.INCLUDE.health == 1), ]

##Count the number excluded for failing to meet structural image quality assurance protocols
#Included: n=282; Excluded: n=63
data.subset$ACROSS.INCLUDE.QA <- 1
data.subset$ACROSS.INCLUDE.QA[data.subset$t1Exclude==1] <- 0
QA.include<-sum(data.subset$ACROSS.INCLUDE.QA)
QA.exclude<-345-QA.include

###Exclude those with ALL problems (health problems and problems with their t1 data) (included n=282)
data.exclude <- data.subset[which(data.subset$healthExcludev2==0 & data.subset$t1Exclude == 0 ),]

##################################
### Remove those missing medu1 ###
##################################

#Remove those who are missing maternal level of education data
data.final <- data.exclude[!is.na(data.exclude$medu1),]

#Count the number of subjects (should be 278)
n_final <- nrow(data.final)

####################
### Demographics ###
####################

#Demographics for the paper
meanAge<-mean(data.final$age)
sdAge<-sd(data.final$age)
rangeAge<-range(data.final$age)
genderTable<-table(data.final$sex)

#################
### Save Data ###
#################

#Save final dataset
saveRDS(data.final,"/data/jux/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.rds")

#Save the bblids and scanids for the final sample (n=)
IDs <- c("bblid", "scanid")
bblidsScanids <- data.final[IDs]

#Remove header
names(bblidsScanids) <- NULL

#Save list
write.csv(bblidsScanids, file="/data/jux/BBL/projects/pncPreterm/subjectData/n278_Prematurity_bblids_scanids.csv", row.names=FALSE)

############################
### SENSITIVITY ANALYSES ###
############################

#Count the number taking psychotropic psychiatric medications 
#Included: n=243; Excluded: n=35
data.final$ACROSS.INCLUDE.psychMeds <- 1
data.final$ACROSS.INCLUDE.psychMeds[data.final$psychoactiveMedPsychv2==1] <- 0
psychMeds.include<-sum(data.final$ACROSS.INCLUDE.psychMeds)
psychMeds.exclude<-278-psychMeds.include

#Exclude those who were on psychiatric medications (included n=243)
data.sensitivity <- data.final[which(data.final$ACROSS.INCLUDE.psychMeds==1),]

#Save sensitivity dataset
saveRDS(data.sensitivity,"/data/jux/BBL/projects/pncPreterm/subjectData/n243_Prematurity_NoPsyMeds.rds")
