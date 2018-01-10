#Create lists of those missing a Ravens nii.gz file on chead.

#!/bin/bash

cat /data/joy/BBL/projects/pncPreterm/subjectData/n282_nassarPrematurity_bblids_scanids.csv | while IFS="," read -r a b ;
do

if [ ! -f /data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/t1struct/voxelwiseMaps_ravens/${b}_RAVENS_2GM_2mm.nii.gz ]; then

    echo $a,$b >> /data/joy/BBL/projects/pncPreterm/subjectData/n282_MissingRavens.csv
   
fi

done
