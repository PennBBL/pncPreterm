#Mask the thresholded .004 images by the NeighborThr400 mask.

numComponents="1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26"

indir=/data/joy/BBL/projects/pncPreterm/results/Threshold004

maskdir=/data/joy/BBL/projects/pncPreterm/results/NeighborThresh400

outdir=/data/joy/BBL/projects/pncPreterm/results/MaskedByNeighborThresh400

for i in $numComponents
do
        echo ""

        echo "Component number is $i"

fslmaths $indir/Basis_${i}_thr004.nii.gz -mas $maskdir/Basis_${i}_thr004Bin_NeighborThr400.nii.gz $outdir/Basis_${i}_thr004_maskedByNeighborThr400.nii.gz

done
