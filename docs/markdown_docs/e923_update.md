# e923 Update 
This utility copies the roughness length fields from sfx files to FA climate file, written by Jan Mašek.

It is necessary for ALARO at high resolutions (<5 km). See Jan's original instructions below.
NB: 
* intially this tool was meant for ECOCLIMAPII first generation (within DEODE workflow we use ECOCLIMAPII SG).




## Original instructions:

Utility e923\_update serves for updating roughness fields in e923 clim
files from SURFEX, i.e. using GMTED2010 topography and ECOCLIMAP
physiography. As a preparatory step, climake for target domain must be
run, delivering PGD file and preliminary clim files.

Update of subgrid-scale orographic characteristics (variance, anisotropy,
orientation of the main axis) from PGD file is also possible, but it must
be done on the level of climake, using modified e923 step and ALADIN
executable with modified subroutine EINCLI1. It is not needed if you
do not want to use parameterization of gravity wave drag (LGWD=F).
