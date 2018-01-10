##########################################
#### GAM MODELS FOR PREMATURITY STUDY ####
##########################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Make race2 a factor with three levels (White, African American, and Other)
data.NMF$race2 <- as.factor(data.NMF$race2)

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models with race 2 (white, african american, other)
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + race2 + medu1 + ga, list(i = as.name(x))), method="REML", data = data.NMF)
})

#OR Run gam models with white (white vs nonwhite)
#NmfModels <- lapply(nmfComponents, function(x) {
#  gam(substitute(i ~ s(age) + sex + white + medu1 + ga, list(i = as.name(x))), method="REML", data = data.NMF)
#})

#Look at model summaries
models <- lapply(NmfModels, summary)

#Pull p-values
p <- sapply(NmfModels, function(v) summary(v)$p.table[5,4])

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

