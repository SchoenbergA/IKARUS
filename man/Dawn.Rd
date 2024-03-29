% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Dawn.R
\name{Dawn}
\alias{Dawn}
\title{Executive function for testing AOA}
\usage{
Dawn(
  FFS = FALSE,
  Tpoints,
  buf_size,
  design,
  Stk,
  Stk_name,
  plot_res = TRUE,
  save_png = FALSE,
  save_res = FALSE,
  path_res,
  path_png,
  fsize = 24
)
}
\arguments{
\item{FFS}{boolean - if True uses a FFS. Default= FALSE}

\item{Tpoints}{SpatialPoints - PointLayer with the TrainingPositions}

\item{buf_size}{numeric - with for the buffer in meter}

\item{design}{character - from for the buffer. Choose from "ROUND","FLAT","SQUARE"}

\item{Stk}{RasterStack - with predictors for classfication}

\item{Stk_name}{character - name of input Stack for the name to write in images and output name.}

\item{plot_res}{boolean - set 'TRUE' to plot the prediction and AOA. Default = TRUE.}

\item{save_png}{boolean - set 'TRUE' to write the images in.png format for prediction and AOA. Default = FALSE.}

\item{save_res}{boolean - set 'TRUE' to write the prediction and AOA in .grd format. Default = FALSE.}

\item{path_res}{character - the path to save the resulting RasterLayer. Required if  "save_res" option is used.}

\item{fsize}{numeric - the desired front size for the main and sub text. default = 24.}

\item{path_png_cl}{character - the path to save the resulting plots for classification as png. Required if  "save_png" option is used.}

\item{path_png_aoa}{character - the path to save the resulting plots for AOA png. Required if  "save_png" option is used.}
}
\value{
Returns the classification, model and the AOA. Optional saves the resuting RasterLayer and saves resuöting prediction and AOA to .png images.
}
\description{
wrapper for IKARUS and AOA with buffered TrainPoints.
}
\details{
This function is a wrapper for the IKARUS workflow for a LLOCV Random Forest classification and further uses the AOA approach by Meyer 2020.
By default the resulting prediction and AOA are plotted. By default further uses all varibales for the model, optional uses a FFS (if FFS=TRUE). Optional saves the RasterLayers. Further can save the results to .png format to a desired path.
}
\examples{
# load libs
require(raster)
require(caret)
require(CAST)
require(doParallel)
require(rgeos)
# required for visualization
require(viridis)
require(png)
require(latticeExtra)
require(gridExtra)
# load data
lau_Stk <- raster::stack(system.file("extdata","lau_RGB.grd",package = "IKARUS"))
lau_tp <-rgdal::readOGR(system.file("extdata","lau_Tpoints.shp",package = "IKARUS"))
# handle CRS string
crs(lau_tp) <- crs(lau_Stk)
### run Dawn
# with training design 0.3 meter radius in cycles
test1 <- Dawn(FFS = F,Tpoints = lau_tp,buf_size = 0.3,design = "ROUND",Stk = lau_Stk,Stk_name = "RGB")
# access results
plot(test1$prediction) # prediciton
plot(test1$AOA) # AOA
test1$model_LLOCV # model
### Test series with DAWN
test1 <- Dawn(FFS = F,Tpoints = lau_tp,buf_size = 0.3,design = "ROUND",Stk = lau_Stk,Stk_name = "RGB")
test2 <- Dawn(FFS = F,Tpoints = lau_tp,buf_size = 0.6,design = "ROUND",Stk = lau_Stk,Stk_name = "RGB")
test3 <- Dawn(FFS = F,Tpoints = lau_tp,buf_size = 1.0,design = "ROUND",Stk = lau_Stk,Stk_name = "RGB")
}
\author{
Andreas Schönberg
}
