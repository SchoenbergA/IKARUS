#' IKARUS spectral Classification Tool
#'
#' @name IKARUS - package
#' @docType package
#' @title 'CAST' based Tool for spectral Classification models
#' @description In general the \code{IKARUS} package is a wrapper for the \code{CAST} package (Hanna Meyer,2020).
#' \code{IKARUS} is designed to easily perform spectral classifications with \code{CAST} and provides additional functions to handle Input data.
#'
#' @note
#'
#' @author Andreas Schönberg
#' @import CAST
#' @import doParallel
#' @import raster
#' @import caret
#' @keywords package
NULL
#' @docType data
#' @name lau_Stk - data
#' @title A RasterStack with spectral data
#' @description  An RGB orthophoto merged with the NIR band of a IRC image.
#' Additional artifically layers: NDVI along with Sum and Sobal filter of 3x3 for the NDVI.
#' The scene shows some trees in the Lautaret vally in the frence alps.
#' Bandnames are "blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3". Resolution 0.15 meter.
#' @format \code{"raster::stack"}
NULL
#' @docType data
#' @name lau_TrainPoly - data
#' @title Polygon layer with training areas
#' @description  Polygon layer with training areas for 'lau_Stk'.
#' @format \code{"rgdal::readOGR"}
NULL
