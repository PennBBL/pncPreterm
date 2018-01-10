########################################
#### DATA PREP FOR PREMATURITY DATA ####
########################################
1. Use the script "DataPrep.R" to preprocess the data. This script will:
   a. Load all data files
   b. Remove those missing gestational age data
   c. Create the preterm group (<37 weeks gestation)
   d. Transform age from months to years
   e. Recode male as 0 and female as 1
   f. Merge the data files
   g. Exclude based on healthExcludev2 and t1Exclude (and calculate how many are missing at each step)
   h. Save the final file as "n282_nassarPrematurity_subjData.csv"
   i. Save the list of the bblids and scanids for the final sample (n=282)
   j. Calculate how many were on psychiatric psychotropic medications at the time of scanning.


###########################################
#### NMF ANALYSES FOR PREMATURITY DATA ####
###########################################

1. Look for missing data using the script, "FindMissingRavens.sh".
2. Smooth the Ravens data and save the filenames into text files for zipping the data using the script, "Smooth_Ravens.sh"
   a. To check that the correct number of smoothed files are in the directory: ls -1 | wc -l
3. Zip the files using the script "ZipFiles.sh".
4. Then copy them to CBICA
      ssh kaczkura@cbica-cluster.uphs.upenn.edu
      sudo -u pncnmf sudosh
      scp -r akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncPreterm/images/n282_RavensData_smoothed8mm.tar.gz /cbica/projects/pncNmf/n282_preterm/images/Ravens_datafiles_smoothed8mm
5. Extract the files (can also delete the tar file)
      tar -xvf n282_RavensData_smoothed8mm.tar.gz
      rm n282_RavensData_smoothed8mm.tar.gz
      ls -1 | wc -l
6. Permissions
      1. If you need to allow others permission to access the directory, use:
      chmod 777 Ravens_datafiles_smoothed8mm/
7. Copy the list of bblids and scanids from chead to cbica.
      scp -r akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncPreterm/subjectData/n282_nassarPrematurity_bblids_scanids.csv /cbica/projects/pncNmf/n282_preterm/subjectData
8. Create a text file with the image paths on CBICA. See script "GetFilePaths.sh" on cbica.
9. Copy the mask from chead to cbica
      #scp -r akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncT1AcrossDisorder/masks/corticalSubcortical_mask.nii.gz /cbica/projects/pncNmf/n282_preterm/masks
      scp -r akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/studies/pnc/n1601_dataFreeze/neuroimaging/pncTemplate/priors/prior_grey_thr01_2mm.nii.gz /cbica/projects/pncNmf/n282_preterm/masks
10. Run the sge commands for submit_script_extractBasesMT.sh using the following script "Run_NMF_nassarPrematurity.sh" to set up and run this script for multiple components.
    a. Be sure to check that the output directory for the error files is correctly specified in this script.
11. Calculate reconstruction error:
   a. After running a range of components (2-30, evens), you need to calculate the reconstruction error to help you to determine the ideal number of components to use.
         ssh -Y kaczkura@cbica-cluster.uphs.upenn.edu
               sudo -u pncnmf sudosh
         cd /cbica/projects/pncNmf/n282_preterm/scripts
         matlab -nodesktop
         addpath('/sbia/sbiasfw/external/matlab/extras/nifti/20130306/')
         calcRecError
   b. The script will run for a few minutes and then the reconstruction error figures will be saved as .png files in the scripts folder.
   c. Move the gradient figures to chead (since sudo doesn't allow -Y to see figures):
      scp reconstructionError.png akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncPreterm/tablesFigures
      scp reconstructionErrorGradient.png akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncPreterm/tablesFigures
   d. Use firefox to view on chead and sftp to copy them to local computer.
   e. We calculate the reconstruction error as the Frobenius between the data and the estimated non-negative approximation. At the end, two figures are produced. The first gives the reconstruction error as a function of the estimated number of components, while the second one gives its gradient. Look at the gradient plot (the one increasing): the aim is to find the "elbow" of the plot, ie, when the change seems to saturate. This is your ideal number of components.
12. Once you've decided on the ideal component number, update the script "formatNmfData.m" to convert the .mat output to .csv where each component equals one column (the output will be in results/NumBases26/OPNMF).
    cd /cbica/projects/pncNmf/n282_preterm/scripts
    matlab -nodesktop
    formatNmfData
    exit
13. Add bblid names to the .csv file
    a. Aris says it is safe to assume that the output file is in the exact same order as the bblid list used to run NMF.
       1. Use cbind to combine the bblids and results using the script "Add_bblids.R".
14. Look at the results
    cd /cbica/projects/pncNmf/n282_preterm/results/
    fslview /cbica/home/kaczkura/pncAslAcrossDisorder/NMF/images/pnc_template_brain_2mm.nii.gz $(for i in {1..26}; do echo `ls NumBases26/OPNMF/niiImg/Basis_${i}.nii ` -b 0.004,0.04 -l Hot ; done) &
15. Copy to chead
      scp NmfResults26Bases_bblids.csv akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncPreterm/subjectData
16. Merge the NMF and subject data with the script on chead, "MergeNmfSubjData.R".
17. Copy the .csv file to the local computer (for sharing with Rula)
      sftp chead
      get /data/joy/BBL/projects/pncPreterm/subjectData/n282_Prematurity_allData.csv
18. Run the gam models and FDR correct the p-values using the script "GamAnalyses.R".


######################################################
#### MAKE PUBLICATION QUALITY NMF IMAGES IN CARET ####
######################################################

1. Zip the NMF component files
    ssh kaczkura@cbica-cluster.uphs.upenn.edu
    sudo -u pncnmf sudosh
    cd /cbica/projects/pncNmf/n282_preterm/results/NumBases26/OPNMF/niiImg
    tar -cvf NMF26components.tar.gz Basis_*

2. Copy the files to chead
    scp -r NMF26components.tar.gz akaczkurkin@chead.uphs.upenn.edu:/data/joy/BBL/projects/pncPreterm/results

3. Unzip the files
    tar -xvf NMF26components.tar.gz
    
4. Look at the results
    fslview /data/joy/BBL/studies/pnc/template/pnc_template_brain_2mm.nii.gz $(for i in {1..26}; do echo `ls Basis_${i}.nii ` -b 0.004,0.04 -l Hot ; done) &

5. Warp your NMF component .nii files into MNI space for caret:
   a. Use the script "WarpEachToMNI.sh"

6. Use sftp to download the MNI space files to the local computer (the files will be in Machintosh HD/Users/antoniak or look for them in "All My Files").
    get /data/joy/BBL/projects/pncPreterm/results/EachComponentWarpedToMNI/NMF*

7. Open Caret (Applications/caret/bin_macosx64/caret5)
   a. If opening a spec file for the first time:
      1. Go to File -> Open Spec file...
      2. Computer/Applications/caret/data_files/standard_mesh_atlases/standard_mesh_atlases_TED/Human.PALS.LEFT
      3. Load
   b. Or if you have already opened a spec file before:
      1. File -> Open recent Spec file -> /standard_mesh_atlases_TED/Human.PALS.LEFT
      2. Load
   c. Go to Attributes -> Map volume(s) to surface(s)
      1. Choose Metric -> Next
      2. Add Volume from disk
         a. Navigate to where you saved the NMF nii files
         b. Choose the NMF_1.nii file
         c. Open
         d. Next
      3. Map to caret with atlas
         a. Space: Choose FLIRT
         b. Atlas: Choose the hemisphere that matches the spec file you loaded (LEFT hemisphere).
         c. Only select "Show Mapping to Average Fiducial Surface" (the first button).
         d. Okay
         e. Next
      4. Data File naming
         a. Don't change anything
         b. Next
      5. Mapping Algorithm
         a. Choose: METRIC_MAXIMUM_VOXEL
         b. Next
         c. Finish
         d. Wait for the summary box to appear. Close.
   d. On the caret diaglog box:
      1. Click on D/C
      2. Primary Overlay, Data type: Choose Metric.
      3. Use the M and L buttons at the top of the screen to switch views quickly.
      4. Use command+shift+4 to take a screen shot. Rename the screen shot NMF1.png.
      5. For the composite image only (): At the top of the D/C page, change Page Selection to Metric Settings
         a. Click on User scale, make Pos Min/Max = 3.09 and 4.00
         b. Choose Positive only under
         c. Click on show bar


#######################################################
#### MAKE A SINGLE FIGURE WITH ALL NMF COMPONENTS ####
#######################################################

1. Threshold and binarize each image using the script "ThresholdBinarizeNMF.sh". This script will:
   a. Threshold each Basis_*.nii image output from the NMF analyses at .004 (this threshold value was recommended by Aris).
   b. Binarize these thresholded images and save separately.
   c. This has to be done separately because we need both thresholded only images and thresholded/binarized images.

2. Remove small clusters
   a. Use Adon’s matlab code to remove small clusters.
      1. Copy the function "returnNumberOfNeighboorhoods" from scripts/ to your results folder /Threshold004Bin
      2. The input needs to be the thresholded (.004) and binarized images.
      3. You have to try several different thresholds to make sure it’s not cutting out the main clusters (200, 300, anf 400 are reasonable to try).
         a. I ultimately went with 400, consistent with Marieta's CT study.
      4. This produces a binary mask with the remaining clusters

      matlab -nodisplay -nojvm -r "returnNumberOfNeighboorhoods('./Basis_1_thr004Bin.nii.gz', 400); exit;"

      5. mkdir NeighborThresh400 and move the images into this folder
3. Mask the thresholded (.004) images by the masks you just created using the script "MaskByNeighborThr400.sh".
4. Warp the masked image into 2mm MNI space for loading in caret using the script "WarpThr400ToMNI.sh".
5. Merge all NMF component images into a single image using script "MergeNMFcomponents.sh".
6. Warp and binarize the mask using the script "WarpBinMask.sh".
7. Use the script "CompositeNmfBrainImage.R" to assigning a p-value or t-value to each component for loading into caret.
   a. NOTE: You will need to run the script "GamAnalyses.R" to see which NMF components you want to plot.
8. Load the file "NMF_Prematurity_gaFDRresultsOnly.nii.gz" in caret as above.


############################
#### MEDIATION ANALYSES ####
############################

1. To run mediation analyses:



Use the script "MediationAnalyses.R" to run mediation with bootstrapped confidence intervals in R.
