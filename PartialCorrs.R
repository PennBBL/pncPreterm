####################################################
#### PARTIAL CORRELATIONS FOR PREMATURITY STUDY ####
####################################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Create an exclusion variable for those who are Early or Very Preterm
data.NMF$LateModeratePreterm <- 1
data.NMF$LateModeratePreterm[which(data.NMF$ga<32)]<- 0

#Remove those who are Early or Very Preterm
data.NMF <- data.NMF[which(data.NMF$LateModeratePreterm==1),]

##############################
#### PARTIAL CORRELATIONS ####
##############################

#Load library
library(ppcor)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

# partial correlations
#library(ggm)
#pcor(c("ga", "Nmf26C1", "age", "ageSq", "sex", "medu1"), var(data.NMF))
# partial corr between a and b controlling for x, y, z

##All 26 NMF components
#Correlations controlling for age, age squared, sex, and medu1
NmfCorrs <- lapply(nmfComponents, function(z) {
  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$ga, c(data.NMF$age,data.NMF$ageSq,data.NMF$sex,data.NMF$medu1), method = "pearson")
})

#pcor.test(data.NMF$Nmf26C1, data.NMF$ga, c(data.NMF$age,data.NMF$ageSq,data.NMF$sex,data.NMF$medu1), method = "pearson")

#NmfCorrs <- lapply(nmfComponents, function(z) {
#  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$ga, c(data.NMF$age,data.NMF$ageSq,data.NMF$sex,data.NMF$medu1), method = "kendall")
#})

#NmfCorrs <- lapply(nmfComponents, function(z) {
#  pcor.test(substitute(data.NMF$i, list(i = as.name(z))), data.NMF$ga, c(data.NMF$age,data.NMF$ageSq,data.NMF$sex,data.NMF$medu1), method = "spearman")
#})

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
  pcor.test(substitute(data.NMF$i, list(i = as.name(w))), data.NMF$ga, c(data.NMF$age,data.NMF$ageSq,data.NMF$sex,data.NMF$medu1), method = "pearson")
})

#Pull p-values
Corrs11_p <- sapply(NmfCorrs11, function(y) (y)$p.value)

#FDR correct p-values
Corrs11_fdr <- p.adjust(Corrs11_p,method="fdr")

#Convert to data frame
Corrs11_fdr <- as.data.frame(Corrs11_fdr)

#To print fdr-corrected p-values to three decimal places
Corrs11_fdr_round <- round(Corrs11_fdr,3)

#Add row names
rownames(Corrs11_fdr_round) <- c(1, 2, 4, 7, 8, 10, 18, 19, 22, 23, 26)

#List the NMF components that survive FDR correction
Nmf_Corrs11_fdr <- row.names(Corrs11_fdr_round)[Corrs11_fdr_round<0.05]
