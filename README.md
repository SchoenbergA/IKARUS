## IKARUS - spatial classification
IKARUS is a warpper for Hannah Meyers 'CAST' for a more easy workflow for classifications. The package contains function for the computation of TrainingDatasets aswell as for the random forest based classification. It supports the Leave-Location-Out Cross Validation (LLOCV) approach.
For an introduction see the vignette: IKARUS_usage.html

## Installation
Install via devtools from github:

``` r
# current version (without LLOCV)
devtools::install_github("SchoenbergA/IKARUS@master")
# develop version for LLOCV
devtools::install_github("SchoenbergA/IKARUS@develop")

```

## Current in work

Actual working on automated computation of TrainPoly from input SpatialPoint layers.
Further working on functions for automated classification and estimating AOA in one line of code.
