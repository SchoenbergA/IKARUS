#' Select best performance layers for classification with a LLOCV
#' @description uses a forward feature selection (FFS) to select the best predictors for the classification
#' @param tDat data.frame - with values of the predictors (see details)
#' @param predCol numeric - seq of columns with the predictor values. By default uses 1:(length(tDat)-1) for tDat format computed by 'IKARUS::exrct_Traindat'
#' @param classCol character - name of the column containing the class information
#' @param classLocCol character - name of the column containing the class and location information
#' @param nk - numeric - number for k in spacefolds
#' @param Cores numeric - amount of Cores to exclude from calculation, default = 1

#' @return returns a list of best performing predictors
#' @details The function is used to select best performing predictor variables for a classification. The
#'  * predCol -  specific predictors can be selected by setting predCol = x:y
#'  * tDat - the use of IKARUS::exrct_Traindat is recommended.
#'  * parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
#' @note The function will compute a huge number of models. Depending on the sizes of the training data the
#'  process can take long time even with multicore processing.
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
#' # FFS with all layers in the RasterStack (this example could take some minutes)
#' #ffs <- BestPredFFS_LLOCV(tDat=tDat,classCol = "class",classLocCol="class_location",nk=5)

#' # FFS with selected layers "blue","green","red","nir"
#' ffs2 <- BestPredFFS_LLOCV(tDat=tDat,predCol = 1:4,classCol = "class",classLocCol="class_location",nk=5)

#' # some code to look at the results
#' ffs1$selectedvars # show seleted variables
#' ffs1$perf_all # show performance of all combinations
#' ffs1$finalModel # show confusion matrix
#' @export BestPredFFS_LLOCV
#' @aliases BestPredFFS_LLOCV

BestPredFFS_LLOCV <- function(tDat,predCol="default",classCol=NULL,classLocCol="class_location",nk=NULL,Cores=1){

  #check input
  cat("checking inputs ",sep="\n")
  ## missing arguments
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
  if(any(names(tDat)==classLocCol)==FALSE){
    stop("selected column name for 'classLocCol' could not be found in tDat")
  }
  # prepare columns
  classColumn <- which(names(tDat)==classCol)
  # prepare predictor columns
  if(any(predCol=="default")==TRUE){
    predCol <- seq(1:(length(tDat)-3))
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

  # start FFS
  starttime <- Sys.time()
  FFSmodel <- CAST::ffs(tDat[,predCol],
                        tDat[,classColumn],
                        method = "rf", withinSE = FALSE, metric= "Kappa",
                        importance = TRUE, trControl = tC)



  #stop FFS
  stopCluster(cl)
  stoptime <- Sys.time()
  diftim <-round(difftime(stoptime,starttime,units = "hours"),4)
  cat(" ",sep = "\n")
  cat(paste0("needed ",diftim," hours"))
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat("selected variables",sep = "\n")
  cat(paste(FFSmodel$selectedvars,collapse = ", "))
  cat(" ",sep = "\n")
  cat(" ",sep = "\n")
  cat("IKARUS finished",sep = "\n")
  return(FFSmodel)
} # end of main function
