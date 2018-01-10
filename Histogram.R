##########################
#### CREATE HISTOGRAM ####
##########################

#Load data
subjData <- read.csv("/data/joy/BBL/projects/pncPreterm/subjectData/n282_Prematurity_allData.csv", header=TRUE, na.strings = "NA")

#Create and save a histogram of the number of subjects by the values of one NMF component for the NMF schematic.
pdf('/data/joy/BBL/projects/pncPreterm/tablesFigures/NMF_Component1_histogram.pdf')
hist(subjData$Nmf26C1, breaks=10, border="white", col="#FF6633")
dev.off()
