#Merge the 26 NMF components into a single image for plotting

outdir=/data/joy/BBL/projects/pncPreterm/results
 
/data/joy/BBL/projects/pncPreterm/results/n282_26NmfComponentsMerged.nii.gz#fslmerge -t $outdir/n282_26NmfComponentsMerged.nii.gz $indir/Basis_*_thr004_maskedByNeighborThr400_2mmMNI.nii.gz

fslmerge -t /data/joy/BBL/projects/pncPreterm/results/n282_26NmfComponentsMerged.nii.gz $(ls /data/joy/BBL/projects/pncPreterm/results/MaskedByNeighborThresh400_WarpedToMNI/Basis_*_thr004_maskedByNeighborThr400_2mmMNI.nii.gz)

fslmaths -Tmean -mul 26 output