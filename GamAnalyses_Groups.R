##########################################
#### GAM MODELS FOR PREMATURITY STUDY ####
##########################################

#Load data
data.NMF <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#########################
#### GROUP VARIABLES ####
#########################

##How many subjects in the each preterm and term bin (with postterm, late term, and full term collapsed into "full term")
#full term
data.NMF$pretermBins <- "fullTerm"

#early term
data.NMF$pretermBins[which(data.NMF$ga<39 )]<- "earlyTerm"

#late preterm
data.NMF$pretermBins[which(data.NMF$ga<37 )]<- "latePreterm"

#moderately preterm
data.NMF$pretermBins[which(data.NMF$ga<34 )]<- "moderatelyPreterm"

#very preterm
data.NMF$pretermBins[which(data.NMF$ga<32 )]<- "veryPreterm"

#extremely preterm
data.NMF$pretermBins[which(data.NMF$ga<28 )]<- "extremelyPreterm"

#Make the bins factors with a specific order (prevents table() from reordering the variables alphabetically)
data.NMF$pretermBins <- factor(data.NMF$pretermBins, levels=c("extremelyPreterm", "veryPreterm", "moderatelyPreterm", "latePreterm", "earlyTerm", "fullTerm"))

#Frequencies of preterm bins
pretermBinsTable <- table(data.NMF$pretermBins)

##Create group factor variables
#full and early term vs. late preterm
data.NMF$term_late <- NA
data.NMF$term_late[data.NMF$pretermBins == "earlyTerm"] <- 0
data.NMF$term_late[data.NMF$pretermBins == "fullTerm"] <- 0
data.NMF$term_late[data.NMF$pretermBins == "latePreterm"] <- 1

#full and early term vs. moderately preterm
data.NMF$term_mod <- NA
data.NMF$term_mod[data.NMF$pretermBins == "earlyTerm"] <- 0
data.NMF$term_mod[data.NMF$pretermBins == "fullTerm"] <- 0
data.NMF$term_mod[data.NMF$pretermBins == "moderatelyPreterm"] <- 1

#full and early term vs. late and moderately preterm
data.NMF$term_modLate <- NA
data.NMF$term_modLate[data.NMF$pretermBins == "earlyTerm"] <- 0
data.NMF$term_modLate[data.NMF$pretermBins == "fullTerm"] <- 0
data.NMF$term_modLate[data.NMF$pretermBins == "latePreterm"] <- 1
data.NMF$term_modLate[data.NMF$pretermBins == "moderatelyPreterm"] <- 1

####################
#### GAM MODELS ####
####################

#Load library
library(mgcv)

#Get NMF variable names
nmfComponents <- names(data.NMF)[grep("Nmf26",names(data.NMF))]

#Run gam models
gam_term_late <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + term_late, list(i = as.name(x))), method="REML", data = data.NMF)
})

gam_term_mod <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + term_mod, list(i = as.name(x))), method="REML", data = data.NMF)
})

gam_term_modLate <- lapply(nmfComponents, function(x) {
  gam(substitute(i ~ s(age) + sex + medu1 + term_modLate, list(i = as.name(x))), method="REML", data = data.NMF)
})

#Look at model summaries
sum_term_late <- lapply(gam_term_late, summary)
sum_term_mod <- lapply(gam_term_mod, summary)
sum_term_modLate <- lapply(gam_term_modLate, summary)

#Pull p-values
p_term_late <- sapply(gam_term_late, function(v) summary(v)$p.table[4,4])
p_term_mod <- sapply(gam_term_mod, function(v) summary(v)$p.table[4,4])
p_term_modLate <- sapply(gam_term_modLate, function(v) summary(v)$p.table[4,4])

#Convert to data frame
p_term_late <- as.data.frame(p_term_late)
p_term_mod <- as.data.frame(p_term_mod)
p_term_modLate <- as.data.frame(p_term_modLate)

#Print original p-values to three decimal places
p_term_late <- round(p_term_late,3)
p_term_mod <- round(p_term_mod,3)
p_term_modLate <- round(p_term_modLate,3)

#FDR correct p-values
pfdr_term_late <- p.adjust(p_term_late[,1],method="fdr")
pfdr_term_mod <- p.adjust(p_term_mod[,1],method="fdr")
pfdr_term_modLate <- p.adjust(p_term_modLate[,1],method="fdr")

#Convert to data frame
pfdr_term_late <- as.data.frame(pfdr_term_late)
pfdr_term_mod <- as.data.frame(pfdr_term_mod)
pfdr_term_modLate <- as.data.frame(pfdr_term_modLate)

#To print fdr-corrected p-values to three decimal places
pfdr_term_late <- round(pfdr_term_late,3)
pfdr_term_mod <- round(pfdr_term_mod,3)
pfdr_term_modLate <- round(pfdr_term_modLate,3)

#List the NMF components that survive FDR correction
Nmf_term_late <- row.names(pfdr_term_late)[pfdr_term_late<0.05]
Nmf_term_mod <- row.names(pfdr_term_mod)[pfdr_term_mod<0.05]
Nmf_term_modLate <- row.names(pfdr_term_modLate)[pfdr_term_modLate<0.05]

#################
#### T TESTS ####
#################

#full term and early term vs late preterm
C1_term_late <- t.test(Nmf26C1~term_late, data=data.NMF)
C2_term_late <- t.test(Nmf26C2~term_late, data=data.NMF)
C4_term_late <- t.test(Nmf26C4~term_late, data=data.NMF)
C7_term_late <- t.test(Nmf26C7~term_late, data=data.NMF)
C8_term_late <- t.test(Nmf26C8~term_late, data=data.NMF)
C10_term_late <- t.test(Nmf26C10~term_late, data=data.NMF)
C18_term_late <- t.test(Nmf26C18~term_late, data=data.NMF)
C19_term_late <- t.test(Nmf26C19~term_late, data=data.NMF)
C22_term_late <- t.test(Nmf26C22~term_late, data=data.NMF)
C23_term_late <- t.test(Nmf26C23~term_late, data=data.NMF)
C26_term_late <- t.test(Nmf26C26~term_late, data=data.NMF)

#full term and early term vs moderately preterm
C1_term_mod <- t.test(Nmf26C1~term_mod, data=data.NMF)
C2_term_mod <- t.test(Nmf26C2~term_mod, data=data.NMF)
C4_term_mod <- t.test(Nmf26C4~term_mod, data=data.NMF)
C7_term_mod <- t.test(Nmf26C7~term_mod, data=data.NMF)
C8_term_mod <- t.test(Nmf26C8~term_mod, data=data.NMF)
C10_term_mod <- t.test(Nmf26C10~term_mod, data=data.NMF)
C18_term_mod <- t.test(Nmf26C18~term_mod, data=data.NMF)
C19_term_mod <- t.test(Nmf26C19~term_mod, data=data.NMF)
C22_term_mod <- t.test(Nmf26C22~term_mod, data=data.NMF)
C23_term_mod <- t.test(Nmf26C23~term_mod, data=data.NMF)
C26_term_mod <- t.test(Nmf26C26~term_mod, data=data.NMF)

#full term and early term vs moderately and late preterm
C1_term_modLate <- t.test(Nmf26C1~term_modLate, data=data.NMF)
C2_term_modLate <- t.test(Nmf26C2~term_modLate, data=data.NMF)
C4_term_modLate <- t.test(Nmf26C4~term_modLate, data=data.NMF)
C7_term_modLate <- t.test(Nmf26C7~term_modLate, data=data.NMF)
C8_term_modLate <- t.test(Nmf26C8~term_modLate, data=data.NMF)
C10_term_modLate <- t.test(Nmf26C10~term_modLate, data=data.NMF)
C18_term_modLate <- t.test(Nmf26C18~term_modLate, data=data.NMF)
C19_term_modLate <- t.test(Nmf26C19~term_modLate, data=data.NMF)
C22_term_modLate <- t.test(Nmf26C22~term_modLate, data=data.NMF)
C23_term_modLate <- t.test(Nmf26C23~term_modLate, data=data.NMF)
C26_term_modLate <- t.test(Nmf26C26~term_modLate, data=data.NMF)
