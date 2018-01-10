###############################
#### Table 1: Demographics ####
###############################

###############################
### Load data and libraries ###
###############################

subjData <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Load libraries
library(plyr)

#################################
### Total sample demographics ###
#################################

#Total sample means
meanAge_total <- mean(subjData$age)

#Total sample sd
sdAge_total <- sd(subjData$age)

#Total number of males and females (0=male, 1=female)
sexTable_total <- table(subjData$sex)

#Total age range
rangeAge_total <- range(subjData$age)

#Total number of whites (0=non-white, 1=white)
whiteTable_total <- table(subjData$white)

########################
### Preterm vs Term ####
########################

#How many subjects in each group (0=term, 1=preterm)
TermVsPretermTable <- table(subjData$preterm) 

#Group means (0=term, 1=preterm)
meanSdGA <- ddply(subjData,~preterm,summarise,mean=mean(ga),sd=sd(ga))
meanSdAge <- ddply(subjData,~preterm,summarise,mean=mean(age),sd=sd(age))
meanSdMedu <- ddply(subjData,~preterm,summarise,mean=mean(medu1,na.rm=TRUE),sd=sd(medu1,na.rm=TRUE))

#Percentage of females (0=male, 1=female)
percentFemale <- ddply(subjData,~preterm,summarise,mean=mean(sex))

#Or to see the number of males and females in each group
sexTable <- table(Preterm = subjData$preterm, Sex = subjData$sex)

#Percentage white (0=nonwhite, 1=white)
percentWhite <- ddply(subjData,~preterm,summarise,mean=mean(white))

#Or to see the number of white and nonwhite in each group
raceTable <- table(Preterm = subjData$preterm, Race = subjData$white)

#T-tests to test for group differences on continuous variables (0=term, 1=preterm)
grpDiff_ga <- t.test(subjData$ga~subjData$preterm)
grpDiff_age <- t.test(subjData$age~subjData$preterm)
grpDiff_medu <- t.test(subjData$medu1~subjData$preterm, na.rm=TRUE)

#Chi-square tests to test for group differences on categorical variables
grpDiff_sex <- chisq.test(subjData$preterm, subjData$sex)
grpDiff_white <- chisq.test(subjData$preterm, subjData$white)

#How many with birth weight data (0=term, 1=preterm)
N_bw <- ddply(subjData,~preterm,summarise,length=length(bw[!is.na(bw)]))

#####################################################################################################################
### Postterm, late term, full term, early term, late preterm, moderately preterm, very preterm, extremely preterm ###
#####################################################################################################################

##How many subjects in each preterm or term bin (the order of these commands (starting with postterm) is critical to ensure non-overlapping groups)
#postterm
subjData$pretermBins <- "postTerm"

#late term
subjData$pretermBins[which(subjData$ga<42 )]<- "lateTerm"

#full term
subjData$pretermBins[which(subjData$ga<41 )]<- "fullTerm"

#early term
subjData$pretermBins[which(subjData$ga<39 )]<- "earlyTerm"

#late preterm
subjData$pretermBins[which(subjData$ga<37 )]<- "latePreterm"

#moderately preterm
subjData$pretermBins[which(subjData$ga<34 )]<- "moderatelyPreterm"

#very preterm
subjData$pretermBins[which(subjData$ga<32 )]<- "veryPreterm"

#extremely preterm
subjData$pretermBins[which(subjData$ga<28 )]<- "extremelyPreterm"

#Make the bins factors with a specific order (prevents table() from reordering the variables alphabetically)
subjData$pretermBins <- factor(subjData$pretermBins, levels=c("extremelyPreterm", "veryPreterm", "moderatelyPreterm", "latePreterm", "earlyTerm", "fullTerm", "lateTerm", "postTerm"))

#Frequencies of preterm bins
pretermBinsTable <- table(subjData$pretermBins)

################################################################################################
### Full term, early term, late preterm, moderately preterm, very preterm, extremely preterm ###
################################################################################################

##How many subjects in the each preterm and term bin (with postterm, late term, and full term collapsed into "full term")
#full term
subjData$pretermBins2 <- "fullTerm"

#early term
subjData$pretermBins2[which(subjData$ga<39 )]<- "earlyTerm"

#late preterm
subjData$pretermBins2[which(subjData$ga<37 )]<- "latePreterm"

#moderately preterm
subjData$pretermBins2[which(subjData$ga<34 )]<- "moderatelyPreterm"

#very preterm
subjData$pretermBins2[which(subjData$ga<32 )]<- "veryPreterm"

#extremely preterm
subjData$pretermBins2[which(subjData$ga<28 )]<- "extremelyPreterm"

#Make the bins factors with a specific order (prevents table() from reordering the variables alphabetically)
subjData$pretermBins2 <- factor(subjData$pretermBins2, levels=c("extremelyPreterm", "veryPreterm", "moderatelyPreterm", "latePreterm", "earlyTerm", "fullTerm"))

#Frequenceis of preterm bins
pretermBinsTable2 <- table(subjData$pretermBins2)

##Demographics
#Group means
meanSdGA2 <- ddply(subjData,~pretermBins2,summarise,mean=mean(ga),sd=sd(ga))
meanSdAge2 <- ddply(subjData,~pretermBins2,summarise,mean=mean(age),sd=sd(age))
meanSdMedu2 <- ddply(subjData,~pretermBins2,summarise,mean=mean(medu1,na.rm=TRUE),sd=sd(medu1,na.rm=TRUE))

#Percentage of females (0=male, 1=female)
percentFemale2 <- ddply(subjData,~pretermBins2,summarise,mean=mean(sex))

#Or to see the number of males and females in each group
sexTable2 <- table(PretermBinsTable2 = subjData$pretermBins2, Sex = subjData$sex)

#Percentage white (0=nonwhite, 1=white)
percentNonwhite2 <- ddply(subjData,~pretermBins2,summarise,mean=mean(white))

#Or to see the number of white and nonwhite in each group
raceTable2 <- table(PretermBinsTable2 = subjData$pretermBins2, Race = subjData$white)

#How many with birth weight data
N_bw2 <- ddply(subjData,~pretermBins2,summarise,length=length(bw[!is.na(bw)]))

#Mean and standard deviation for birthweight for each bin
meanSdBw <- ddply(subjData,~pretermBins2,summarise,mean=mean(bw[!is.na(bw)]),sd=sd(bw[!is.na(bw)]))

#########################
#### GA Correlations ####
#########################

#Gestational age is associated with birthweight
library(Hmisc)
gaBw_corr <- rcorr(subjData$ga, subjData$bw)

#Gestational age correlated with age at time of scan
gaAge_corr <- rcorr(subjData$ga, subjData$age)

#Gestational age correlated with maternal level of education
medu1_corr <- rcorr(subjData$ga, subjData$medu1)

################################
#### Mean differences in ga ####
################################

#Sex differences in ga (0=male, 1=female)
sexDiff_ga <- t.test(subjData$ga~subjData$sex)

#Race differences in ga (0=nonwhite, 1=white)
whiteDiff_ga <- t.test(subjData$ga~subjData$white)

#Mean ga for nonwhite and white
meanGA_white <- ddply(subjData,~white,summarise,mean=mean(ga),sd=sd(ga))

##################
#### AGE BINS ####
##################

#How many subjects were under age 13 years
subjData$under13 <- 0
subjData$under13[which(subjData$age < 13 )] <- 1
n_under13 <- sum(subjData$under13)

#Subset to only those under 13
preadol <- subjData[which(subjData$under13 == 1),]

#Number of subjects in each preterm bin
under13table <- table(preadol$pretermBins2)

#Do the preterm and full term groups differ on the number of subjects under 13 years
under13chiSq <- chisq.test(subjData$preterm, subjData$under13)

#How many subjects were over age 18 years
subjData$over18 <- 0
subjData$over18[which(subjData$age > 18 )] <- 1
n_over18 <- sum(subjData$over18)

#Subset to only those over 18
adults <- subjData[which(subjData$over18 == 1),]

#Number	of subjects in each preterm bin
over18table <- table(adults$pretermBins2)

#Do the preterm and full term groups differ on the number of subjects over 18 years
over18chiSq <- chisq.test(subjData$preterm, subjData$over18)

###################################
#### TIME BETWEEN COG AND SCAN ####
###################################

#cognitive testing came before scanning for all participants in this subset
time <- data.NMF$ageAtScan1 - data.NMF$ageAtCnb1
avgTime <- mean(time) #4.769784
