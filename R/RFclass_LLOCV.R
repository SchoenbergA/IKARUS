#' Random Forest Classification with Leave Location Out Cross-Validation
#' @description RF Classification with LLOCV
#' @param tDat data.frame - with values of the predictors (see details)
#' @param predStk - RasterStack - with the predictors.
#' @param predCol numeric - seq of columns with predictor values. By default uses 1:(length(tDat)-1) for tDat format computed by IKARUS::exrct_Traindat
#' @param classCol character - name of the column containing the class information
#' @param classLocCol character - name of the column containing the class and location information
#' @param nk - numeric - number for k in spacefolds
#' @param Cores numeric - amount of Cores to exclude from calculation, default = 1
#' @return returns a list with the model and the prediction
#' @details
#' * predCol -  specific predictors can be selected by setting predCol = x:y
#' * tDat - the use of IKARUS::exrct_Traindat is recommended.
#' * parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
#' @author Andreas Schönberg
#' @examples
#'# load data
#' require(caret)
#' require(CAST)
#' require(doParallel)
#' require(raster)
#' require(IKARUS)
#' lau_Stk <- raster::stack(system.file("extdata","lau_Stk.tif",package = "IKARUS"))
#' lau_tP <-rgdal::readOGR(system.file("extdata","lau_TrainPoly_LLOCV2.shp",package = "IKARUS"))
#' # handle CRS string
#' crs(lau_tP) <- crs(lau_Stk)
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' ### extract values using 'exrct_Tdat' to generate training dataset
#' tDat <- exrct_Traindat_LLOCV(lau_tP,lau_Stk,classCol="class",locname="location")
#' # check for class column and predictor columns in input training dataset
#' head(tDat)
#' # classification
#' model1 <- RFclass_LLOCV(tDat = tDat,predCol = "default",predStk = lau_Stk,classCol = "class")
#' #check model
#' model1$model_LLOCV
#' # plot prediction
#' plot(model1$prediction)
#' # classification with only RGB + NIR
#' model2 <- RFclass_LLOCV(tDat = tDat,predCol = 1:4,predStk = lau_Stk[[1:4]],classCol = "class")
#' #check model
#' model2$model_LLOCV
#' # plot prediction
#' plot(model2$prediction)

#' @export RFclass_LLOCV
#' @aliases RFclass_LLOCV


RFclass_LLOCV <- function(tDat,predCol="default",predStk=NULL,classCol="class",classLocCol="class_location",nk=NULL,Cores=1){

  # check input
  cat("checking inputs ",sep="\n")
  ## missing arguments
  if(is.null(predStk)){
    stop("missing argument predStk")
  }
  if(is.null(predCol)){
    stop("missing argument predCol")
  }
  if(is.null(classCol)){
    stop("missing argument classCol")
  }
  if(is.null(classLocCol)){
    stop("missing argument classLocCol")
  }
  if(is.null(nk)){
    stop("missing argument nk")
  }
  if(any(names(tDat)==classCol)==FALSE){
    stop("selected column name for 'classCol' could not be found in tDat")
  }
  # prepare class column
  classColumn <- which(names(tDat)==classCol)
  # prepare predictor columns
  if(any(predCol=="default")==TRUE){
    predCol <- seq(1:(length(tDat)-3))
  }

  # selected predictor check
  npred <-length(predCol)
  nlayStk <- nlayers(predStk)
  if(identical(npred,nlayStk)==FALSE){
    stop("number of selected predictors in Stack is not equal to number of selected predictors in Tdat ")
  }
  cat("using predictors:  ")
  cat(paste(names(tDat[,predCol]),collapse = ", "),sep="\n")
  # prepare Cores
  cl =  makeCluster(detectCores()-Cores)
  cat(paste("using",length(cl),"of",length(cl)+Cores,"availible Cores"),sep="\n")

              #set seed
              set.seed(112019)
              #create Spacefolds, k= amount of unique spatial units
              indices = CAST::CreateSpacetimeFolds(tDat, spacevar = classLocCol, k = nk)
              #set seed


              set.seed(112019)
              #create trainControl for LLOCV
              tC <-trainControl(method = "cv", classProbs = TRUE, index = indices$index, indexOut = indices$indexOut)
              cat(" ",sep = "\n")
              cat("IKARUS starting model with LLOCV",sep = "\n")
              # start cores
              set.seed(112019)
              registerDoParallel(cl)
              #start RF LLOCV
              starttime <- Sys.time()
              LLOCVmodel = caret::train(tDat[,predCol],
                                        tDat[,classColumn],
                                        method = "rf", withinSE = FALSE, metric= "Kappa",
                                        importance = TRUE, trControl = tC)
  #stop RF
  stopCluster(cl)
  stoptime <- Sys.time()
  diftim <-round(difftime(stoptime,starttime,units = "hours"),4)
  cat(" ",sep = "\n")
  cat("finished model",sep = "\n")
  cat(paste0("needed ",diftim," hours",sep = "\n"))
  cat("best result:",sep = "\n")
  cat(" ",sep = "\n")
  # results
  print(subset(LLOCVmodel$results, mtry == LLOCVmodel$bestTune[1,1]))
  cat("order of classes:",sep = "\n")
  print(LLOCVmodel$level[1:length(LLOCVmodel$level)])
  cat(" ",sep = "\n")
  # predict
  cat("IKARUS starting prediciton",sep = "\n")

  pred <- raster::predict(predStk,LLOCVmodel)

  # list output
  LS_output <- list("prediction"=pred,"model_LLOCV"=LLOCVmodel)
  cat(" ",sep = "\n")
  cat("IKARUS finished",sep = "\n")
  return(LS_output)
} # end of main function
