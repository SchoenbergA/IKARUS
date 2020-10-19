#' Select best performance layers for classification
#' @description uses a foreward featrue selection (FFS) to select the best predictors for the classification
#' @param tDat dataframe - with values of the predictors (see details)
#' @param predCol numeric - seq of columns with the predictor values- By default uses 1:(length(tDat)-1) for tDat fromat computed by IKARUS::exrct_Traindat
#' @param classCol character - name of the column containing the class information, default=NULL
#' @param Cores numeric - amount of Cores to exclude from calculation, default = 1

#' @return returns a classification model
#' @details The function is used to select best fitting predictor variables for a classification.
#'  * predCol -  Columns which should be used for the model. To select specific columns use predCol= x:y
#'  * tDat - the use of IKARUS::exrct_Traindat is recommended.
#'  * parallel processing - the function uses parallel processing for multicore processors. by default all cores -1 are used.
#' @note The function will compute a huge number of models. Depended on the sizes of the Trainingdata the
#'  process can take long time even with multicore processing.
#' @author Andreas Schönberg
#' @examples
#' # load data
#' lau_Stk_path <-system.file("extdata","lau_Stk.tif",package = "IKARUS")
#' lau_Stk <- raster::stack(lau_Stk_path)
#' tP_path <-system.file("extdata","lau_TrainPoly.shp",package = "IKARUS")
#' lau_tP <-rgdal::readOGR(tP_path)
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")

#' ### extract values using 'IKARUS::exrct_Traindat'
#' tDat <- exrct_Traindat(lau_tP,lau_Stk,"class")

#' # FFS with all layers in the RasterStack (this example could take some minutes)
#' #ffs <- BestPredFFS(tDat=tDat,classCol = "class")

#' # FFS with selected layers "blue","green","red","nir"
#' ffs1 <- BestPredFFS(tDat=tDat,predCol = 1:4,classCol = "class")

#' # some code to look at the results
#' ffs1$selectedvars # show seleted variables
#' ffs1$perf_all # show performance of all combinations
#' ffs1$finalModel # show confusion matrix
#' @export BestPredFFS
#' @aliases BestPredFFS

BestPredFFS <- function(tDat,predCol="default",classCol=NULL,Cores=1){

  #check input
  cat("checking inputs ",sep="\n")
  ## missing arguments
  if(is.null(classCol)){
    stop("missing argument classCol")
  }
        if(any(names(tDat)==classCol)==FALSE){
          stop("selected column name for 'classCol' could not be found in tDat")
        }
  # prepare columns
  classColumn <- which(names(tDat)==classCol)
  # prepare predictor columns
  if(any(predCol=="default")==TRUE){
    predCol <- seq(1:(length(tDat)-1))
  }
  cat("using predictors:  ")
  cat(paste(names(tDat[,predCol]),collapse = ", "),sep="\n")

  #set seed
  set.seed(112019)
  #create Spacefolds, k= amount of unique spatial units
  tC <-trainControl(method = "cv", number = 5)
  # start cores
  set.seed(112019)
  cl <- makeCluster(detectCores()-Cores)
  cat(paste("using",length(cl),"of",length(cl)+Cores,"availible Cores"),sep="\n")
  registerDoParallel(cl)
  #start FFS
  cat(" ",sep = "\n")
  cat("IKARUS starting FFS with CV",sep = "\n")
  cat(" ",sep = "\n")
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
