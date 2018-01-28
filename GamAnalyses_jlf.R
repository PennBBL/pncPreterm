#################
### Load data ###
#################

subjData <- read.csv("/data/jux/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Load libraries
library(mgcv)

#########################
#### JLF Volume ROIs ####
#########################

#Get ROI variable names
volROIs_all <- subjData[c(grep("mprage_jlf_vol",names(subjData)))]

volROIs_short <- volROIs_all[,-grep("Vent|Brain_Stem|Cerebell|Cerebral_White_Matter|CSF|Lobe_WM",names(volROIs_all))]

volROIs <- names(volROIs_short)

ROImodels <- lapply(volROIs, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = subjData)
})

#Look at model summaries
ROImodels_ga <- lapply(ROImodels, summary)

#Pull p-values
pROI <- sapply(ROImodels, function(v) summary(v)$p.table[4,4])

#Convert to data frame
pROI <- as.data.frame(pROI)

#Print original p-values to three decimal places
pROI_round <- round(pROI,3)

#Add row names
rownames(pROI) <- volROIs

#List the significant uncorrected ROIs
signif_ROIs <- row.names(pROI)[pROI<0.05]

#FDR correct p-values
pROI_fdr <- p.adjust(pROI[,1],method="fdr")

#Convert to data frame
pROI_fdr <- as.data.frame(pROI_fdr)

#To print fdr-corrected p-values to three decimal places
pROI_fdr_round <- round(pROI_fdr,3)[pROI_fdr<0.05]

#Convert to data frame
p <- as.data.frame(pROI_fdr_round)

#Add row names
rownames(pROI_fdr) <- volROIs

#List the ROI components that survive FDR correction
signif_ROIs_fdr <- row.names(pROI_fdr)[pROI_fdr<0.05]

#Convert to data frame
ROI <- as.data.frame(signif_ROIs_fdr)

#Pull t-values
tROI <- sapply(ROImodels, function(x) summary(x)$p.table[4,3])

#Print to two decimal places
tROI_round <- round(tROI,2)[pROI_fdr<0.05]

#Convert to data frame
t <- as.data.frame(tROI_round)

#Combine ROI names, p values, and t values into one dataframe
combined <- cbind(ROI,p)
combined2 <- cbind(combined,t)

#Save as a .csv
write.csv(combined2, file="/data/jux/BBL/projects/pncPreterm/subjectData/pncPreterm_JLF_ROIs.csv", row.names=F, quote=F)
