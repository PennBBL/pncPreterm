###############################
### Load data and libraries ###
###############################

subjData <- read.csv("/data/jux/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Load libraries
library(mgcv)

########################
#### Cognition = ga ####
########################

#Get cognition variable names
cogMeasures <- c("Overall_Accuracy","F1_Exec_Comp_Res_Accuracy","F2_Social_Cog_Accuracy","F3_Memory_Accuracy")

#Run gam models
cogModels <- lapply(cogMeasures, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
cogModels_ga <- lapply(cogModels, summary)

#Pull p-values
pcog <- sapply(cogModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
pcog <- as.data.frame(pcog)

#Print original p-values to three decimal places
pcog_round <- round(pcog,3)

#Add row names
rownames(pcog) <- cogMeasures

#List the variables that are significant
signif_cog <- row.names(pcog)[pcog<0.05]

#FDR correct p-values
pcog_fdr <- p.adjust(pcog[,1],method="fdr")

#Convert to data frame
pcog_fdr <- as.data.frame(pcog_fdr)

#To print fdr-corrected p-values to three decimal places
pcog_fdr_round <- round(pcog_fdr,3)


##############################
#### Cognition = ga * NMF ####
##############################

#Get NMF variable names
nmfComponents <- names(subjData)[grep("Nmf26",names(subjData))]

cogModels_gaNMF <- lapply(nmfComponents, function(x) {
  gam(substitute(F1_Exec_Comp_Res_Accuracy ~ s(age) + sex + medu1 + ga + i + ga*i, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
models_gaNMF <- lapply(cogModels_gaNMF, summary)

#Pull p-values
p_gaNMF <- sapply(cogModels_gaNMF, function(v) summary(v)$p.table[6,4])

#Convert to data frame
p_gaNMF <- as.data.frame(p_gaNMF)

#Print original p-values to three decimal places
p_gaNMF_round <- round(p_gaNMF,3)

#List the significant components
signif_gaNMF <- row.names(p_gaNMF)[p_gaNMF<0.05]


#########################
#### NMF = Cognition ####
#########################

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + F1_Exec_Comp_Res_Accuracy, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
NMFmodels_cog <- lapply(NmfModels, summary)

#Pull p-values
pNMF <- sapply(NmfModels, function(v) summary(v)$p.table[3,4])

#Convert to data frame
pNMF <- as.data.frame(pNMF)

#Print original p-values to three decimal places
pNMF_round <- round(pNMF,3)

#List the significant NMF components
signif_Nmf <- row.names(pNMF)[pNMF<0.05]

#FDR correct p-values
pNMF_fdr <- p.adjust(pNMF[,1],method="fdr")

#Convert to data frame
pNMF_fdr <- as.data.frame(pNMF_fdr)

#To print fdr-corrected p-values to three decimal places
pNMF_fdr_round <- round(pNMF_fdr,3)

#List the NMF components that survive FDR correction
signif_Nmf_fdr <- row.names(pNMF_fdr)[pNMF_fdr<0.05]

#Pull estimates (b values)
bNMF <- sapply(NmfModels, function(x) summary(x)$p.table[3,1])

#Convert to data frame
bNMF <- as.data.frame(bNMF)

#Print to two decimal places
bNMF_round <- round(bNMF,2)

#Pull standard errors (SE)
seNMF <- sapply(NmfModels, function(y) summary(y)$p.table[3,2])

#Convert to data frame
seNMF <- as.data.frame(seNMF)

#Print to two decimal places
seNMF_round <- round(seNMF,2)

#Pull t-values
tNMF <- sapply(NmfModels, function(y) summary(y)$p.table[3,3])

#Convert to data frame
tNMF <- as.data.frame(tNMF)

#Print to two decimal places
tNMF_round <- round(tNMF,2)


##############################
#### NMF = ga * Cognition ####
##############################

NmfModels2 <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga + F1_Exec_Comp_Res_Accuracy + ga*F1_Exec_Comp_Res_Accuracy, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
NMFmodels_gaCog <- lapply(NmfModels2, summary)


##############################
#### ga = NMF * Cognition ####
##############################

NmfModels3 <- lapply(nmfComponents, function(x) {
  gam(substitute(ga ~ s(age) + sex + medu1 + i + F1_Exec_Comp_Res_Accuracy + i*F1_Exec_Comp_Res_Accuracy, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
NMFmodels_NmfCog <- lapply(NmfModels3, summary)
