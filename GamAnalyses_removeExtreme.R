##########################################
#### GAM MODELS FOR PREMATURITY STUDY ####
##########################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

###################################
#### REMOVE LESS THAN 32 WEEKS ####
###################################

##Define those with gestational age of less than 32 weeks
data.NMF$exclude32 <- 1
data.NMF$exclude32[which(data.NMF$ga < 32 )] <- 0

##Remove those with ga<32 weeks
data.subset <- data.NMF[which(data.NMF$exclude32==1),]

######################################
#### OR REMOVE LESS THAN 28 WEEKS ####
######################################

##Define those with gestational age of less than 28 weeks
#data.NMF$exclude28 <- 1
#data.NMF$exclude28[which(data.NMF$ga < 28 )] <- 0

##Remove those with ga<28 weeks
#data.subset <- data.NMF[which(data.NMF$exclude28==1),]

######################################
#### OR REMOVE MORE THAN 39 WEEKS ####
######################################

##Define those with gestational age of more than 39 weeks
#data.NMF$exclude39 <- 1
#data.NMF$exclude39[which(data.NMF$ga >= 39 )] <- 0

##Remove those with ga>39 weeks
#data.subset <- data.NMF[which(data.NMF$exclude39==1),]

#############################################################
#### OR REMOVE LESS THAN 28 WEEKS AND MORE THAN 39 WEEKS ####
#############################################################

##Define those with gestational age of less than 28 weeks or more than 39 weeks
#data.NMF$excludeExtremes <- 1
#data.NMF$excludeExtremes[which(data.NMF$ga < 28 | data.NMF$ga >= 39 )] <- 0

##Remove those with ga<28 or ga>39 weeks
#data.subset <- data.NMF[which(data.NMF$excludeExtremes==1),]

################################################
#### OR REMOVE 2 SD ABOVE OR BELOW THE MEAN ####
################################################

#Calculate 1 SD
#ga_sd <- sd(data.NMF$ga)

#Find the mean
#ga_mean <- mean(data.NMF$ga)

#Calculate the mean plus and minus 2 SDs
#plus2SD <- ga_mean + 2*ga_sd
#minus2SD <- ga_mean - 2*ga_sd

##Define those with gestational age of less than or greater than 2 SDs from the mean 
#data.NMF$exclude2sd <- 1
#data.NMF$exclude2sd[which(data.NMF$ga > plus2SD | data.NMF$ga < minus2SD )] <- 0

##Remove those with ga less than or greater than 2 SDs from the	mean
#data.subset <- data.NMF[which(data.NMF$exclude2sd==1),]

######################
#### GAM ANALYSES ####
######################

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.subset)[grep("Nmf26",names(data.subset))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = data.subset)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#Pull p-values
p <- sapply(NmfModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p <- as.data.frame(p)

#Print original p-values to three decimal places
p_round <- round(p,3)

#FDR correct p-values
pfdr <- p.adjust(p[,1],method="fdr")

#Convert to data frame
pfdr <- as.data.frame(pfdr)

#To print fdr-corrected p-values to three decimal places
pfdr_round <- round(pfdr,3)

#List the NMF components that survive FDR correction
Nmf_fdr <- row.names(pfdr)[pfdr<0.05]

#Only FDR correct across the NMF components that survived correction in the original analyses
p_orig <- p[c(1, 2, 4, 7, 8, 10, 18, 19, 22, 23, 26),]
p_orig <- as.data.frame(p_orig)
pfdr_orig <- p.adjust(p_orig[,1],method="fdr")
pfdr_orig <- as.data.frame(pfdr_orig)
rownames(pfdr_orig) <- c(1, 2, 4, 7, 8, 10, 18, 19, 22, 23, 26)

################################
#### AVERAGE NMF COMPONENTS ####
################################

#Average across all 26 NMF components
data.subset$NMF26_totalBrain <- (data.subset$Nmf26C1 + data.subset$Nmf26C2 + data.subset$Nmf26C3 + data.subset$Nmf26C4 + data.subset$Nmf26C5 + data.subset$Nmf26C6 + data.subset$Nmf26C7 + data.subset$Nmf26C8 + data.subset$Nmf26C9 + data.subset$Nmf26C10 + data.subset$Nmf26C11 + data.subset$Nmf26C12 + data.subset$Nmf26C13 + data.subset$Nmf26C14 + data.subset$Nmf26C15 + data.subset$Nmf26C16 + data.subset$Nmf26C17 + data.subset$Nmf26C18 + data.subset$Nmf26C19 + data.subset$Nmf26C20 + data.subset$Nmf26C21 + data.subset$Nmf26C22 + data.subset$Nmf26C23 + data.subset$Nmf26C24 + data.subset$Nmf26C25 + data.subset$Nmf26C26)/26

totalBrain_gam <- gam(NMF26_totalBrain ~ s(age) + sex + medu1 + ga,  method="REML", data = data.subset)
