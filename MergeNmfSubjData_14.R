#################
### LOAD DATA ###
#################
subjData <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_subjData.csv", header=TRUE, na.strings="NA")
sensitivityData <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n243_Prematurity_subjData_NoPsychMeds.csv", header=TRUE, na.strings="NA")
nmfData <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/14_components/NmfResults14Bases_bblids.csv", header=TRUE)

##################
### MERGE DATA ###
##################
#NOTE: We use all=FALSE because NMF was run on n=282 (not removing those with missing medu) but the final subject level data has n=278 (after removing those missing medu).
dataComb1 <-merge(subjData, nmfData, by=c("bblid","scanid"), all=FALSE)
dataComb2 <-merge(sensitivityData, nmfData, by=c("bblid","scanid"), all=FALSE)

#################
### SAVE DATA ###
#################
write.csv(dataComb1, file="/data/joy/BBL/projects/pncPreterm/subjectData/n278_Prematurity_allData_NMF14.csv", row.names=FALSE)
write.csv(dataComb2, file="/data/joy/BBL/projects/pncPreterm/subjectData/n243_Prematurity_allData_NoPsychMeds_NMF14.csv", row.names=FALSE)
