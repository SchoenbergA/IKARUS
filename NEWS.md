# IKARUS 0.1.0.9000
development version for LLOCV logic

* new features
extract_tDat_LLOCV - equal to 'extract_tDat' but uses a "location" column and saves the location for each pixel to        returning dataframe
RFclass_LLOCV - equal to 'RFclass' but uses LLOCV instead of simple random CV, requires a tDat with class and location    information. nk is a parameter to select the k folds for CV.
BestPredFFS_LLOCV - equal to 'BestPredFFS' but uses LLOCV instead of simple random CV, requires a tDat with class and location  and comnined class_location  information. nk is a parameter to select the k folds for CV.

* add example data
lau_TrainPoly_LLOCV2 - contains location informations, has smaller polygons than 'lau_TrainPoly'

# IKARUS 0.1.0
release version

* add IKARUS_usage.html 

# IKARUS 0.0.0.93

* add vignette

* some bugfixing
ClassSegVal - now example calls correct function name
ClassSegVal - help now contains description for the resulting tabel
ClassSegVal - some changes in code and returning table
RFclass - now example calls correct variable



# IKARUS 0.0.0.92

* major spelling corrections

* simplified example data loading

# IKARUS 0.0.0.91

*new feature
classSegVal - validation of Tree classification by Tree Crown Segments

# IKARUS 0.0.0.9000
initial version

*new feature
BestPredFFS - Forward feature selection for training data.
extract_Traindat - function for extracting Training Data from a Raster Object.
RFclass - classification function for RandomForest.
IKARUS - Package - Package informations and Example data documentation.
