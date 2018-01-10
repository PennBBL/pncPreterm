###############################
### Load data and libraries ###
###############################

subjData <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

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


#########################
#### NMF = Cognition ####
#########################

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + F1_Exec_Comp_Res_Accuracy, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
NMFmodels_cog <- lapply(NmfModels, summary)

#Pull p-values
pNMF <- sapply(NmfModels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
pNMF <- as.data.frame(pNMF)

#Print original p-values to three decimal places
pNMF_round <- round(pNMF,3)

#FDR correct p-values
pNMF_fdr <- p.adjust(pNMF[,1],method="fdr")

#Convert to data frame
pNMF_fdr <- as.data.frame(pNMF_fdr)

#To print fdr-corrected p-values to three decimal places
pNMF_fdr_round <- round(pNMF_fdr,3)

#List the NMF components that survive FDR correction
Nmf_fdr <- row.names(pNMF_fdr)[pNMF_fdr<0.05]


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


#########################
#### JLF Volume ROIs ####
#########################

#Get ROI variable names
volROIs <- names(subjData)[grep("mprage_jlf_vol",names(subjData))]

ROImodels <- lapply(volROIs, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
ROImodels_ga <- lapply(ROImodels, summary)



#List the NMF components that survive FDR correction
#Nmf_fdr <- row.names(pNMF_fdr)[pNMF_fdr<0.05]
