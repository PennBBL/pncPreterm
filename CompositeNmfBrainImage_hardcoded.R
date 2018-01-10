#Create nifti images: 1) with all 26 NMF components on one brain and 2) with only FDR-significant NMF components showing an association with gestational age

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n282_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Load libraries
library(mgcv)
library(ANTsR)

###Run gam models and pull t-values
#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models
NmfModels <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + ga, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Pull t-values
t <- sapply(NmfModels, function(v) summary(v)$p.table[4,3])

#Convert to a data frame (rows correspond to component numbers)
t <- as.data.frame(t)

############################
#Read in merged images and mask
img<-antsImageRead('/data/joy/BBL/projects/pncPreterm/results/n282_26NmfComponentsMerged.nii.gz',4) #this 4d set of merged images for each comp (NOTE: each component needs to be merged in the correct order 1-26 for this script to work).
mask<-antsImageRead('/data/joy/BBL/projects/pncPreterm/masks/prior_grey_thr01_2mm_MNI_bin.nii.gz',3) #this is the prior grey matter mask (warped to MNI space and binarized)

#Create matrix: rows are components, columns are voxels
seed.mat<-timeseries2matrix(img, mask)

#Find, for each voxel, which component has highest loading
whichCompStr <-apply(seed.mat,2,which.max) # however, some of those are all zeros, need to remove
foo <-apply(seed.mat,2,sum) 		   # this is sum of loadings across column; if 0, entire column is 0
whichCompStr[which(foo==0)]<-0 		   # assign 0-columns to 0

#Writing that to an image where every voxel is assigned to one component
newImg<-antsImageClone(mask)               # prep for writing out image
newImg[mask==1]<-as.matrix(whichCompStr)   # put assigned values back in the image	
antsImageWrite(newImg,"/data/joy/BBL/projects/pncPreterm/tablesFigures/NMF_preterm_all26Components.nii.gz")

##Create composite gam figure
#Assign t-values for components where gestational age is FDR-significantly associated with volume (see GamAnalyses.R for which components survive fdr correction)
tvalMap<-antsImageClone(mask)
tvalVoxVector<-whichCompStr					   # a vector that contains # of Comp for each voxel
tvalVoxVector[which(tvalVoxVector %in% c(3,5,6,9,11,12,13,14,15,16,17,20,21,24,25))]<-0        # assign all components that did not survive fdr correction to 0
tvalVoxVector[which(tvalVoxVector==1)] <- 1       		   # for the remaining components, assign p- or t-value (we chose t-values)
tvalVoxVector[which(tvalVoxVector==2)] <- 2
tvalVoxVector[which(tvalVoxVector==4)] <- 4
tvalVoxVector[which(tvalVoxVector==7)] <- 7
tvalVoxVector[which(tvalVoxVector==8)] <- 8
tvalVoxVector[which(tvalVoxVector==10)] <- 10
tvalVoxVector[which(tvalVoxVector==18)] <- 18
tvalVoxVector[which(tvalVoxVector==19)] <- 19
tvalVoxVector[which(tvalVoxVector==22)] <- 22
tvalVoxVector[which(tvalVoxVector==23)] <- 23
tvalVoxVector[which(tvalVoxVector==26)] <- 26

tvalMap[mask==1]<-as.matrix(tvalVoxVector)			 # essentially this is a replaced image with t-values or 0s instead of comp numbers
antsImageWrite(tvalMap,"/data/joy/BBL/projects/pncPreterm/tablesFigures/NMF_preterm_gaFdrComponents_hardcodedNumbers.nii.gz")

##Create composite mediation figure
#Assign t-values for components with FDR-corrected significant mediation relationships between ga and cognition (note: these t-values are hard-coded because they were calculated in SPSS)
tvalMap2<-antsImageClone(mask)
tvalVoxVector2<-whichCompStr					   # a vector that contains # of Comp for each voxel
tvalVoxVector2[which(tvalVoxVector2 %in% c(3,5,6,7,9,10,11,12,13,14,15,16,17,18,20,21,23,24,25))]<-0        # assign all components that did not survive fdr correction to 0
tvalVoxVector2[which(tvalVoxVector2==1)] <- 1    		   # for the remaining components, assign p- or t-value (we chose t-values)
tvalVoxVector2[which(tvalVoxVector2==2)] <- 2
tvalVoxVector2[which(tvalVoxVector2==4)] <- 4
tvalVoxVector2[which(tvalVoxVector2==8)] <- 8
tvalVoxVector2[which(tvalVoxVector2==19)] <- 19
tvalVoxVector2[which(tvalVoxVector2==22)] <- 22
tvalVoxVector2[which(tvalVoxVector2==26)] <- 26

tvalMap2[mask==1]<-as.matrix(tvalVoxVector2)			 # essentially this is a replaced image with t-values or 0s instead of comp numbers
antsImageWrite(tvalMap2,"/data/joy/BBL/projects/pncPreterm/tablesFigures/NMF_preterm_mediationFdrComponents_hardcodedNumbers.nii.gz")
