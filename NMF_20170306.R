##Merge NMF Data
NMF_Data <- read.csv("~/Desktop/Research/Data Spreadsheets/NmfResults18Bases_20170306.csv")
neo<-read.csv("~/Desktop/Research/Data Spreadsheets/gaData_final.csv")
neo2<-neo[!is.na(neo$ga),]
demo <-read.csv("~/Desktop/Research/Data Spreadsheets/n1601_demographics_go1_20161212.csv")
TBV <- read.csv("~/Desktop/Research/Data Spreadsheets/n1601_antsCtVol.csv")
cog <- read.csv("~/Desktop/Research/Data Spreadsheets/n1601_cnb_factor_scores_tymoore_20151006.csv")
dataComb1 <- merge(NMF_Data, neo2)
dataComb2 <- merge(dataComb1, demo)
dataComb3 <- merge(dataComb2, TBV)
dataComb4 <- merge(dataComb3, cog)
write.csv(dataComb4,"~/Desktop/Research/Data Spreadsheets/MergedNMF_20170306.csv")
data.NMF <- read.csv("~/Desktop/Research/Data Spreadsheets/MergedNMF_20170306.csv")

#make "white" variable
data.NMF$white<-0
data.NMF$white[which(data.NMF$race==1)]<-1 

#make preterm variable
data.NMF$ptBin<-NA
data.NMF$ptBin[data.NMF$ga<37]<-1
data.NMF$ptBin[data.NMF$ga>=37]<-0

#preterm only
data.NMF.PT <- data.NMF[which(data.NMF$ptBin == 1),]

##NMF analysis,excluding TBV
nmfPT_noTBV<- names(data.NMF.PT)[grep("Nmf18",names(data.NMF.PT))]
datanmfPT_noTBV <- data.NMF.PT[,nmfPT_noTBV]
Model <- function(x) {
  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+ga, data=data.NMF.PT)
  anova(ContModel)$pTerms.table[3,3]
}

pvaluesPT.nmf_noTBV <- apply(datanmfPT_noTBV, 2, FUN = Model)
pvaluesPT.nmf_noTBV <- as.data.frame(pvaluesPT.nmf_noTBV)
pvalues.adjustedPT.nmf_noTBV <- pvaluesPT.nmf_noTBV
pvalues.adjustedPT.nmf_noTBV[,1] <- p.adjust(pvaluesPT.nmf_noTBV[,1], method = "fdr")

row.names(pvalues.adjustedPT.nmf_noTBV)[pvalues.adjustedPT.nmf_noTBV<0.05]

##including TBV
nmfPT_withTBV <- c(names(data.NMF.PT)[grep("Nmf18",names(data.NMF.PT))], "mprage_antsCT_vol_TBV")
datanmfPT_withTBV <- data.NMF.PT[,nmfPT_withTBV]
Model <- function(x) {
  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+mprage_antsCT_vol_TBV+ga, data=data.NMF.PT)
  anova(ContModel)$pTerms.table[4,3]
}

pvaluesPT.nmf_withTBV <- apply(datanmfPT_withTBV, 2, FUN = Model)
pvaluesPT.nmf_withTBV <- as.data.frame(pvaluesPT.nmf_withTBV)
pvalues.adjustedPT.nmf_withTBV <- pvaluesPT.nmf_withTBV
pvalues.adjustedPT.nmf_withTBV[,1] <- p.adjust(pvaluesPT.nmf_withTBV[,1], method = "fdr")

row.names(pvalues.adjustedPT.nmf_withTBV)[pvalues.adjustedPT.nmf_withTBV<0.05]

##NMF analysis, dimensional with full group
##excluding TBV
nmffull_noTBV<- names(data.NMF)[grep("Nmf18",names(data.NMF))]
datanmffull_noTBV <- data.NMF[,nmffull_noTBV]
Model <- function(x) {
  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+s(ga), method="REML", data=data.NMF)
  anova(ContModel)$s.table[2,4]
}

nmffull_noTBV<- names(data.NMF)[grep("Nmf18",names(data.NMF))]
datanmffull_noTBV <- data.NMF[,nmffull_noTBV]
Model <- function(x) {
  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+ga, method="REML", data=data.NMF)
  anova(ContModel)$pTerms.table[3,3]
}

pvaluesfull.nmf_noTBV <- apply(datanmffull_noTBV, 2, FUN = Model)
pvaluesfull.nmf_noTBV <- as.data.frame(pvaluesfull.nmf_noTBV)
pvalues.adjustedfull.nmf_noTBV <- pvaluesfull.nmf_noTBV
pvalues.adjustedfull.nmf_noTBV[,1] <- p.adjust(pvaluesfull.nmf_noTBV[,1], method = "fdr")

row.names(pvalues.adjustedfull.nmf_noTBV)[pvalues.adjustedfull.nmf_noTBV<0.05]

##including TBV
nmffull_withTBV <- c(names(data.NMF)[grep("Nmf18",names(data.NMF))], "mprage_antsCT_vol_TBV")
datanmffull_withTBV <- data.NMF[,nmffull_withTBV]
Model <- function(x) {
  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+mprage_antsCT_vol_TBV+s(ga), method="REML", data=data.NMF)
  anova(ContModel)$s.table[2,4]
}

nmffull_withTBV <- c(names(data.NMF)[grep("Nmf18",names(data.NMF))])
datanmffull_withTBV <- data.NMF[,nmffull_withTBV]
Model <- function(x) {
  ContModel <- gam(x~s(ageAtScan1)+sex+medu1+mprage_antsCT_vol_TBV+ga, method="REML", data=data.NMF)
  anova(ContModel)$pTerms.table[4,3]
}

pvaluesfull.nmf_withTBV <- apply(datanmffull_withTBV, 2, FUN = Model)
pvaluesfull.nmf_withTBV <- as.data.frame(pvaluesfull.nmf_withTBV)
pvalues.adjustedfull.nmf_withTBV <- pvaluesfull.nmf_withTBV
pvalues.adjustedfull.nmf_withTBV[,1] <- p.adjust(pvaluesfull.nmf_withTBV[,1], method = "fdr")

row.names(pvalues.adjustedfull.nmf_withTBV)[pvalues.adjustedfull.nmf_withTBV<0.05]

#make sq age term so that model doesn't choke on two splines
data.NMF$ageSq<-NA
data.NMF$ageSq<-(data.NMF$ageAtScan1-mean(data.NMF$ageAtScan1))^2

#for 12 NMF components, define mixed model, test with relative likelihood ratio
C1Model <- gamm(Nmf18C1 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C1Model) #p=1
C1Model_plot <-gam(Nmf18C1 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C1Model_plot, ylab="Component C1", xlab="Gestational Age (weeks)")

C3Model <- gamm(Nmf18C3 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C3Model) #p=1
C3Model_plot <-gam(Nmf18C3 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C3Model_plot, ylab="Component C3", xlab="Gestational Age (weeks)")

C4Model <- gamm(Nmf18C4 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C4Model) #p=0.0123
C4Model_plot <-gam(Nmf18C4 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C4Model_plot, ylab="Component C4", xlab="Gestational Age (weeks)")

C8Model <- gamm(Nmf18C8 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C8Model) #p=1
C8Model_plot <-gam(Nmf18C8 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C8Model_plot, ylab="Component C8", xlab="Gestational Age (weeks)")

C9Model <- gamm(Nmf18C9 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C9Model) #p=1
C9Model_plot <-gam(Nmf18C9 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C9Model_plot, ylab="Component C9", xlab="Gestational Age (weeks)")

C11Model <- gamm(Nmf18C11 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C11Model) #p=1
C11Model_plot <-gam(Nmf18C11 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C11Model_plot, ylab="Component C11", xlab="Gestational Age (weeks)")

C12Model <- gamm(Nmf18C12 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C12Model) #p=1
C12Model_plot <-gam(Nmf18C12 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C12Model_plot, ylab="Component C12", xlab="Gestational Age (weeks)")

C13Model <- gamm(Nmf18C13 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C13Model) #p=1
C13Model_plot <-gam(Nmf18C13 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C13Model_plot, ylab="Component C13", xlab="Gestational Age (weeks)")

C14Model <- gamm(Nmf18C14 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C14Model) #p=0.0037
C14Model_plot <-gam(Nmf18C14 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C14Model_plot, ylab="Component C14", xlab="Gestational Age (weeks)")

C15Model <- gamm(Nmf18C15 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C15Model) #p=0.0224
C15Model_plot <-gam(Nmf18C15 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C15Model_plot, ylab="Component C15", xlab="Gestational Age (weeks)")

C16Model <- gamm(Nmf18C16 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C16Model) #p=1
C16Model_plot <-gam(Nmf18C16 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C16Model_plot, ylab="Component C16", xlab="Gestational Age (weeks)")

C17Model <- gamm(Nmf18C17 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C17Model) #p=1
C17Model_plot <-gam(Nmf18C17 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C17Model_plot, ylab="Component C17", xlab="Gestational Age (weeks)")

C18Model <- gamm(Nmf18C18 ~ sex + ageAtScan1 + ageSq + medu1 + s(ga), method='REML', data=data.NMF)$lme
exactRLRT(C18Model) #p=1
C18Model_plot <-gam(Nmf18C18 ~ sex +  s(ageAtScan1) + medu1 + s(ga),method='REML',data=data.NMF)
visreg(C18Model_plot, ylab="Component C18", xlab="Gestational Age (weeks)")


##GA*Age interaction
nmfinteract_noTBV<- names(data.NMF)[grep("Nmf22",names(data.NMF))]
datanmfinteract_noTBV <- data.NMF[,nmfinteract_noTBV]
Model <- function(x) {
  ContModel <- lm(x~ga*ageAtScan1+sex+medu1, data=data.NMF)
  anova(ContModel)[5,5]
}

pvaluesinteract_noTBV <- apply(datanmfinteract_noTBV, 2, FUN = Model)
pvaluesinteract_noTBV <- as.data.frame(pvaluesinteract_noTBV)
pvalues.adjustedinteract_noTBV <- pvaluesinteract_noTBV
pvalues.adjustedinteract_noTBV[,1] <- p.adjust(pvaluesinteract_noTBV[,1], method = "fdr")

row.names(pvalues.adjustedinteract_noTBV)[pvalues.adjustedinteract_noTBV<0.05]

##NMF and Cognitive Models
GAM_accModel <- gam(Overall_Accuracy~s(ageAtScan1)+sex+medu1+ga, data=data.NMF) #p=0.025
GAM_effModel <- gam(Overall_Efficiency~s(ageAtScan1)+sex+medu1+ga, data=data.NMF)  #p=0.126
GAM_execModel <- gam(F1_Exec_Comp_Res_Accuracy~s(ageAtScan1)+sex+medu1+ga, data=data.NMF) #p=0.018
GAM_memModel <- gam(F3_Memory_Accuracy~s(ageAtScan1)+sex+medu1+ga, data=data.NMF)  #p=0.569
GAM_socModel <- gam(F2_Social_Cog_Accuracy~s(ageAtScan1)+sex+medu1+ga, data=data.NMF) #p=0.135

C1Model <- gam(Nmf18C1~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C1Model)  #p=9.94e-07

C3Model <- gam(Nmf18C3~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C3Model) #p=0.006

C4Model <- gam(Nmf18C4~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C4Model) #p=0.008

C5Model <- gam(Nmf18C5~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C5Model) #p=0.012

C8Model <- gam(Nmf18C8~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C8Model) #p=5.94e-05

C9Model <- gam(Nmf18C9~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C9Model) #p=0.011

C11Model <- gam(Nmf18C11~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C11Model) #p=0.0030

C12Model <- gam(Nmf18C12~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C12Model) #p=0.009511

C13Model <- gam(Nmf18C13~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C13Model) #p=2.01e-05

C14Model <- gam(Nmf18C14~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C14Model) #p=0.000611

C15Model <- gam(Nmf18C15~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C15Model) #p=1.31e-05

C16Model <- gam(Nmf18C16~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C16Model) #p=0.0247

C17Model <- gam(Nmf18C17~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C17Model) #p=3.39e-06

C18Model <- gam(Nmf18C18~s(ageAtScan1)+sex+medu1+F1_Exec_Comp_Res_Accuracy, data=data.NMF)
anova.gam(C18Model) #p=0.028414


TempModel<-gam( Nmf22C11~s(ageAtScan1)+sex+medu1+s(ga), data=data.NMF)
summary(tempModel)
tempModel<-gam( Nmf22C1~s(ageAtScan1)+sex+medu1+ga, data=data.NMF)
anova.gam(accModelGaCont)  