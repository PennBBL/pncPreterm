#########################################
#### LM MODELS FOR PREMATURITY STUDY ####
#########################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Load library
library(mgcv)

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
