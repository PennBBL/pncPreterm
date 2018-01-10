##########################################
#### GAM MODELS FOR PREMATURITY STUDY ####
##########################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n282_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Remove those who are missing maternal level of education data
data.NMF <- data.NMF[!is.na(data.NMF$medu1),]

#Create an exclusion variable for those who are Early or Very Preterm
data.NMF$LateModeratePreterm <- 1
data.NMF$LateModeratePreterm[which(data.NMF$ga<32)]<- 0

#Remove those who are Early or Very Preterm
data.NMF <- data.NMF[which(data.NMF$LateModeratePreterm==1),]

####################
#### GAM MODELS ####
####################

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = data.NMF)
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

##############################
#### PARTIAL CORRELATIONS ####
##############################

#Load library
library(ppcor)

##All 26 NMF components
#Correlations controlling for age, sex, and medu1
NmfCorrs <- lapply(nmfComponents, function(z) {
  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$ga, c(data.NMF$age,data.NMF$sex,data.NMF$medu1), method = "pearson")
})

#Pull p-values
Corrs_p <- sapply(NmfCorrs, function(y) (y)$p.value)

#FDR correct p-values
Corrs_fdr <- p.adjust(Corrs_p,method="fdr")

#Convert to data frame
Corrs_fdr <- as.data.frame(Corrs_fdr)

#To print fdr-corrected p-values to three decimal places
Corrs_fdr_round <- round(Corrs_fdr,3)

#List the NMF components that survive FDR correction
Nmf_Corrs_fdr <- row.names(Corrs_fdr)[Corrs_fdr<0.05]


##Only the 11 significant components
nmfComponents11 <- c("Nmf26C1","Nmf26C2","Nmf26C4","Nmf26C7","Nmf26C8","Nmf26C10","Nmf26C18","Nmf26C19","Nmf26C22","Nmf26C23","Nmf26C26")

#Correlations controlling for age, sex, and medu1
NmfCorrs11 <- lapply(nmfComponents11, function(w) {
  pcor.test(substitute(data.NMF$i, list(i = as.name(w))), data.NMF$ga, c(data.NMF$age,data.NMF$sex,data.NMF$medu1), method = "pearson")
})

#Pull p-values
Corrs11_p <- sapply(NmfCorrs11, function(y) (y)$p.value)

#FDR correct p-values
Corrs11_fdr <- p.adjust(Corrs11_p,method="fdr")

#Convert to data frame
Corrs11_fdr <- as.data.frame(Corrs11_fdr)

#To print fdr-corrected p-values to three decimal places
Corrs11_fdr_round <- round(Corrs11_fdr,3)

