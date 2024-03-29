% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/BestPredFFS_LLOCV.R
\name{BestPredFFS_LLOCV}
\alias{BestPredFFS_LLOCV}
\title{Select best performance layers for classification with a LLOCV}
\usage{
BestPredFFS_LLOCV(
  tDat,
  predCol = "default",
  classCol = NULL,
  classLocCol = "class_location",
  nk = NULL,
  Cores = 1
)
}
\arguments{
\item{tDat}{data.frame - with values of the predictors (see details)}

\item{predCol}{numeric - seq of columns with the predictor values. By default uses 1:(length(tDat)-1) for tDat format computed by 'IKARUS::exrct_Traindat'}

\item{classCol}{character - name of the column containing the class information}

\item{classLocCol}{character - name of the column containing the class and location information}

\item{nk}{\itemize{
\item numeric - number for k in spacefolds
}}

\item{Cores}{numeric - amount of Cores to exclude from calculation, default = 1}
}
\value{
returns a list of best performing predictors
}
\description{
uses a forward feature selection (FFS) to select the best predictors for the classification
}
\details{
The function is used to select best performing predictor variables for a classification. The
\itemize{
\item predCol -  specific predictors can be selected by setting predCol = x:y
\item tDat - the use of IKARUS::exrct_Traindat is recommended.
\item parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
}
}
\note{
The function will compute a huge number of models. Depending on the sizes of the training data the
process can take long time even with multicore processing.
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
# FFS with all layers in the RasterStack (this example could take some minutes)
ffs <- BestPredFFS_LLOCV(tDat=tDat,classCol = "class",classLocCol="class_location",nk=5)
# some code to look at the results
ffs$selectedvars # show seleted variables
ffs$perf_all # show performance of all combinations
ffs$finalModel # show confusion matrix
}
\author{
Andreas Schönberg
}
