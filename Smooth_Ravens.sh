#Smooth the Ravens data for Rula's study before running NMF.
#We settled on 8mm FWHM

cat /data/joy/BBL/projects/pncPreterm/subjectData/n282_nassarPrematurity_bblids_scanids.csv | while IFS="," read -r a b ;

do

RavensPath=`ls -d /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_ravens/${b}_RAVENS_2GM_2mm.nii.gz`;

RavensOutdir=/data/joy/BBL/projects/pncPreterm/images/Ravens_smoothed8mm

RavensPathName=$(echo $RavensPath|cut -d, -f1)
RavensFullFileName=$(basename $RavensPath)
RavensFileName=$(basename $RavensPath | cut -d. -f1)

#Save full filenames to text files for zipping the data later
echo $RavensFullFileName >> /data/joy/BBL/projects/pncPreterm/subjectData/n282_Ravens_FileNames.csv

#NOTE: fslmaths requires smoothing parameters in sigma
#FWHM = 2.355*sigma
#8mm FWHM = 3.40 sigma

sig=3.40

fslmaths $RavensPathName -s $sig $RavensOutdir/${RavensFileName}_smoothed${sig}sigma.nii.gz

done
