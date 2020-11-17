#' Extract training data from RasterStacks for LLOCV
#' @description Extracts values from a RasterStack of predictor variables by training polygons
#' @param trainPoly SpatialPolygonsDataframe -  with training polygons to assign values
#' @param predStk RasterStack - with layers to extract values from
#' @param classCol character - name of the column containing the class information, default=NULL
#' @param locname character - name of the column containing the location information, default=NULL

#' @return Returns a data.frame with values of each Rasterlayer per pixel for the training polygons along with the location.
#' Additionally adds a column of the respective class information.
#' @details This function is used to extract training data from a Raster Stack. This training dataset is used for IKARUS::BestPredFFS and IKARUS::RFclass.

#' * classCol - the column with information about the class of a polygon. Supports either character or numeric values for classes (eg 1,2,3 or "tree","stone","grass")
#' * predictor selection - specific predictors can be selected by using [[]] in parameter 'predStk'. E.g. predStk = x[[1:4]].
#' * locname - the column with information about the location of the polygon for LLOCV. Supports either character or numeric values for classes (eg 1,2,3 or "north","east","west")
#' @note the function will check for INF and or NA values. INF values are first set to NA and further all NA will be deleted to prevent errors in further processing with IKARUS::BestPredFFS and IKARUS::RFclass.
#' @author Andreas Schönberg
#' @examples
#' # load data
#' require(raster)
#' require(IKARUS)
#' lau_Stk <- raster::stack(system.file("extdata","lau_Stk.tif",package = "IKARUS"))
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' lau_tP <-rgdal::readOGR(system.file("extdata","lau_TrainPoly_LLOCV2.shp",package = "IKARUS"))
#' # handle CRS string
#' crs(lau_tP) <- crs(lau_Stk)
#' ### check column names
#' names(lau_tP)
#' # -> lau_tP has both character and numeric class information
#' ### extract values using character class information
#' tDat <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,"class","location")
#' head(tDat)
#' ### extract values using numeric class information
#' tDat2 <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,"class_num","loc_num")
#' head(tDat2)
#' @export exrct_Traindat_LLOCV
#' @aliases exrct_Traindat_LLOCV



exrct_Traindat_LLOCV <- function(trainPoly,predStk,classCol=NULL,locname=NULL,lyrname=names(predStk)){


  # check input
  if(length(lyrname)!=nlayers(predStk)){
    stop("Incorrect number of layer names: Input layernames are more or less than Rasterlayers ")
  }
  names(predStk)<-lyrname

  # start extraction with location

  cat("IKARUS starting Extraction",sep = "\n")

  # get levels for factors and save orgnames in lvls
  classpos <- which(names(trainPoly)==classCol)
  nfactor <-length(unique(trainPoly[[classpos,]]))
  trainPoly[[classpos]] <- as.factor(trainPoly[[classpos]])

  lvlClass <-levels(as.factor(trainPoly[[classpos]]))

  locpos <- which(names(trainPoly)==locname)
  nflocpos <-length(unique(trainPoly[[locpos,]]))
  trainPoly[[locpos]] <- as.factor(trainPoly[[locpos]])

  lvlLoc <-levels(as.factor(trainPoly[[locpos]]))

  trainPoly[[locpos]]
  as.factor(trainPoly[[locpos]])

  #rasterize
  shp2rst <- raster::rasterize(trainPoly,predStk,field=classCol)
  #plot(shp2rst)
  locID <- raster::rasterize(trainPoly,predStk,field=locname)
  #plot(locID)
  maskedStk <- mask(shp2rst,predStk)
  # reduce mask to one layer
  masked <- maskedStk[[1]]
  names(masked) <-classCol

  trainStk <- addLayer(predStk,masked,locID)
  names(trainStk) <- c(names(predStk),classCol,"locationID")
  dat <- getValues(trainStk)

  # check for INF and NA
  if(any(is.infinite(dat))==TRUE){
    nINF <- sum(is.infinite(dat))
    # set inf to NA
    cat(" ",sep = "\n")
    cat(paste("INF values detected: setting ",nINF,"INF to NA"),sep = "\n")
    dat[mapply(is.infinite, dat)] <- NA
  }

  if(any(is.na(dat))==TRUE){
    nNA <- sum(is.na(dat))
    # delete NAs
    cat(" ",sep = "\n")
    cat(paste("NAs detected: deleting",nNA," NAs"),sep = "\n")
    dat_clean <-na.omit(dat)
  }

  # transform to dataframe
  TrainDat <- as.data.frame(dat_clean)

  #rename factors to input names
  for (i in (1:max(TrainDat$locationID))){
    TrainDat$locationID[TrainDat$locationID==i] <- lvlLoc[i]
  }
  # rename classcol to 'class'
  names(TrainDat)[names(TrainDat) == classCol] <-"class"
  #rename factors to input names
  for (i in (1:max(TrainDat$class))){
    TrainDat$class[TrainDat$class==i] <- lvlClass[i]
  }
  # change name to org input name
  names(TrainDat)[names(TrainDat) == "class"] <-classCol
  #add location by class
  TrainDat$class_location <- paste(TrainDat$class,sep="_",TrainDat$locationID)
  cat(" ",sep = "\n")
  cat("IKARUS finished Extraction",sep = "\n")
  return(TrainDat)


} # end of function
