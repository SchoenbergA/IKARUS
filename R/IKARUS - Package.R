#' IKARUS Pixel-based Classification Tool
#'
#' @name IKARUS - package
#' @docType package
#' @title Tool for Pixel-based Classifications
#' @description In general the \code{IKARUS} package is a wrapper for the \code{CAST} package (Hanna Meyer,2020).
#' \code{IKARUS} is designed to easily perform pixel-based classifications with \code{CAST} and provides additional functions to handle input data.
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
#' @description  An RGB orthoimage merged with the NIR band of a IRC image.
#' Additional artificial layers: NDVI along with Sum and Sobel filter of 3x3 for the NDVI.
#' The scene shows a few trees in the Lautaret vally in the French Alps.
#' Bandnames are "blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3". Resolution 0.15 meter.
#' @format \code{"raster::stack"}
NULL
#' @docType data
#' @name lau_TrainPolygon - data
#' @title Polygon layer with training areas.
#' @description  Polygon layer with training areas for 'lau_Stk'.
#' @format \code{"rgdal::readOGR"}
NULL
#' @docType data
#' @name lau_TPoints - data
#' @title Point layer with training areas.
#' @description  Point layer with training areas for 'lau_Stk'.
#' @format \code{"rgdal::readOGR"}
NULL
