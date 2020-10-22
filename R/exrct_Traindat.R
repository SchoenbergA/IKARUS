#' Extract training data from RasterStacks
#' @description Extracts values from a RasterStack of predictor variables by training polygons
#' @param trainPoly SpatialPolygonsDataframe -  with training polygons to assign values
#' @param predStk RasterStack - with layers to extract values from
#' @param classCol character - name of the column containing the class information, default=NULL

#' @return Returns a data.frame with values of each Rasterlayer per pixel for the training polygons.
#' Additionally adds a column of the respective class information.
#' @details This function is used to extract training data from a Raster Stack. This training dataset is used for IKARUS::BestPredFFS and IKARUS::RFclass.

#' * classCol - the column with information about the class of a polygon. Supports either character or numeric values for classes (eg 1,2,3 or "tree","stone","grass")
#' * predictor selection - specific predictors can be selected by using [[]] in parameter 'predStk'. E.g. predStk = x[[1:4]].
#' @note the function will check for INF and or NA values. INF values are first set to NA and further all NA will be deleted to prevent errors in further processing with IKARUS::BestPredFFS and IKARUS::RFclass.
#' @author Andreas Schönberg
#' @examples
#' # load data
#' lau_Stk_path <-system.file("extdata","lau_Stk.tif",package = "IKARUS")
#' lau_Stk <- raster::stack(lau_Stk_path)
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' tP_path <-system.file("extdata","lau_TrainPoly.shp",package = "IKARUS")
#' lau_tP <-rgdal::readOGR(tP_path)
#' ### check column names
#' names(lau_tP)
#' # -> lau_tP has both character and numeric class information
#' ### extract values using character class information
#' tDat <- exrct_Traindat(lau_tP,lau_Stk,"class")
#' head(tDat)
#' ### extract values using numeric class information
#' tDat2 <- exrct_Traindat(lau_tP,lau_Stk,"class_num")
#' head(tDat2)
#' @export exrct_Traindat
#' @aliases exrct_Traindat



exrct_Traindat <- function(trainPoly,predStk,classCol=NULL,lyrname=names(predStk)){


  # check input
  if(length(lyrname)!=nlayers(predStk)){
    stop("Incorrect number of layer names: Input layernames are more or less than Rasterlayers ")
  }
  names(predStk)<-lyrname

    # start extraction

    cat("IKARUS starting Extraction",sep = "\n")

    # get levels for factors and save orgnames in lvls
    classpos <- which(names(trainPoly)==classCol)
    nfactor <-length(unique(trainPoly[[classpos,]]))
    trainPoly[[classpos]] <- as.factor(trainPoly[[classpos]])

    lvlClass <-levels(as.factor(trainPoly[[classpos]]))

    #rasterize
    shp2rst <- raster::rasterize(trainPoly,predStk,field=classCol)
    #plot(shp2rst)
    #plot(locID)
    maskedStk <- mask(shp2rst,predStk)

    # reduce mask to one layer
    masked <- maskedStk[[1]]
    names(masked) <-classCol

    trainStk <- addLayer(predStk,masked)
    names(trainStk) <- c(names(predStk),classCol)
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

    # rename classcol to 'class'
    names(TrainDat)[names(TrainDat) == classCol] <-"class"
    #rename factors to input names
    for (i in (1:max(TrainDat$class))){
      TrainDat$class[TrainDat$class==i] <- lvlClass[i]
    }
    # change name to org input name
    names(TrainDat)[names(TrainDat) == "class"] <-classCol
    cat(" ",sep = "\n")
    cat("IKARUS finished Extraction",sep = "\n")
    return(TrainDat)


} # end of function

