% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RFclass_LLOCV.R
\name{RFclass_LLOCV}
\alias{RFclass_LLOCV}
\title{Random Forest Classification with Leave Location Out Cross-Validation}
\usage{
RFclass_LLOCV(
  tDat,
  predCol = "default",
  predStk = NULL,
  classCol = "class",
  classLocCol = "class_location",
  nk = NULL,
  Cores = 1
)
}
\arguments{
\item{tDat}{data.frame - with values of the predictors (see details)}

\item{predCol}{numeric - seq of columns with predictor values. By default uses 1:(length(tDat)-1) for tDat format computed by IKARUS::exrct_Traindat}

\item{predStk}{\itemize{
\item RasterStack - with the predictors.
}}

\item{classCol}{character - name of the column containing the class information}

\item{classLocCol}{character - name of the column containing the class and location information}

\item{nk}{\itemize{
\item numeric - number for k in spacefolds
}}

\item{Cores}{numeric - amount of Cores to exclude from calculation, default = 1}
}
\value{
returns a list with the model and the prediction
}
\description{
RF Classification with LLOCV
}
\details{
\itemize{
\item predCol -  specific predictors can be selected by setting predCol = x:y
\item tDat - the use of IKARUS::exrct_Traindat is recommended.
\item parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
}
}
\examples{
# load data
require(caret)
require(CAST)
require(doParallel)
require(raster)
require(IKARUS)
lau_Stk <- raster::stack(system.file("extdata","lau_RGB.grd",package = "IKARUS"))
lau_tP <-rgdal::readOGR(system.file("extdata","lau_TrainPolygon.shp",package = "IKARUS"))
# handle CRS string
crs(lau_tP) <- crs(lau_Stk)
### extract values using 'exrct_Tdat' to generate training dataset
tDat <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,classCol="class",locname="location")
# check for class column and predictor columns in input training dataset
head(tDat)
# classification
model1 <- RFclass_LLOCV(tDat = tDat,predCol = "default",predStk = lau_Stk,classCol = "class",nk=5)
# check model
model1$model_LLOCV
# plot prediction
plot(model1$prediction)
}
\author{
Andreas Schönberg
}
