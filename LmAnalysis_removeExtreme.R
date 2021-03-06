#########################################
#### LM MODELS FOR PREMATURITY STUDY ####
#########################################

#Load data
data.NMF <- read.csv("/data/jux/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Create an exclusion variable for those who are Early or Very Preterm
data.NMF$LateModeratePreterm <- 1
data.NMF$LateModeratePreterm[which(data.NMF$ga<32)]<- 0

#Remove those who are Early or Very Preterm
#data.NMF <- data.NMF[which(data.NMF$LateModeratePreterm==1),]

#Or create an exclusion variable to remove those who are Extremely Preterm only
data.NMF$LateModerateVeryPreterm <- 1
data.NMF$LateModerateVeryPreterm[which(data.NMF$ga<28)]<- 0

#Remove those who are Extremely Preterm
data.NMF <- data.NMF[which(data.NMF$LateModerateVeryPreterm==1),]

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  lm(substitute(i ~ age + ageSq + sex + medu1 + ga, list(i = as.name(x))), data = data.NMF)
})

#Look at model summaries
models <- lapply(NmfModels, summary)

#Pull p-values
p <- sapply(NmfModels, function(v) summary(v)$coefficients[6,4])

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
