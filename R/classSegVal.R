#' Validate a Tree Prediction with TreeCrown Segments
#' @description calculates the amount of cells for class "trees" which overlap with TreeCrown Segments.
#' @param pred RasterLayer - Prediction to be validated
#' @param seg PolygonLayer - Segments with TreeCrowns
#' @param classTree numeric - The class representing Trees
#' @param reclass optional - numeric value of class to merge into class "trees", for multiple classes to merge use reclass=c(x,y)
#' @return returns the validation score
#' @author Andreas Schönberg
#' @examples
#' # load data
#' require(caret)
#' require(CAST)
#' require(doParallel)
#' require(raster)
#' require(IKARUS)
#' require(mapview)
#' Stk_path <-system.file("extdata","lau_Stk.tif",package = "IKARUS")
#' lau_Stk <- raster::stack(Stk_path)
#' tP_path <-system.file("extdata","lau_TrainPoly.shp",package = "IKARUS")
#' lau_tP <-rgdal::readOGR(tP_path)
#' # load segments
#' seg_path <-system.file("extdata","lau_TreeSeg.shp",package = "IKARUS")
#' lau_seg <-rgdal::readOGR(seg_path)
#' #set layer names
#' names(lau_Stk)<- c("blue","green","red","nir","NDVI","NDVI_sum3","NDVI_sobel3")
#' ### extract values using 'exrct_Tdat' to generate training dataset
#' tDat <- exrct_Traindat(lau_tP,lau_Stk,"class")
#' # check for class column and predictor columns in input training dataset
#' head(tDat)
#' # classification
#' model1 <- RFclass(tDat = tDat,predCol = "default",predStk = lau_Stk,classCol = "class")
#' #check model and seg
#' plot(model1$prediction)
#' plot(lau_seg)
#'
#' # test several combinations of classes for tress
#' tree <- valSeg(  pred=model1$prediction,  seg=lau_seg,  classTree=4,  reclass=NULL)
#' tnsha <- valSeg(  pred=model1$prediction,  seg=lau_seg,  classTree=4,  reclass=2)
#' tnshangras <- valSeg(  pred=model1$prediction,  seg=lau_seg,  classTree=4,  reclass=c(2,3))


#' @export classSegVal
#' @aliases classSegVal

classSegVal <- function(pred,seg,classTree=NULL,reclass=NULL) {



  cat("IKARUS starting validation",sep = "\n")

  # reclassify
  if(is.null(reclass)==FALSE){


    # reclass all classes from var "reclass" to tree class
    for (i in 1:length(reclass)){
      if(i==1){
        m <- c(reclass[[i]]-1,reclass[[i]],classTree)
        mat <- matrix(m, ncol=1, byrow=TRUE)
        pred_re <- reclassify(pred,mat)
      } else {
        m <- c(reclass[[i]]-1,reclass[[i]],classTree)
        mat <- matrix(m, ncol=1, byrow=TRUE)
        pred_re <- reclassify(pred_re,mat)

      }
    } #end reclass
    pred <-pred_re
  }
  plot(pred)


  # validation per segments
  # rasterize segments
  rst_seg <-raster::rasterize(seg,pred,background=0)
  #reclass segments to 20
  segMax  <- cellStats(rst_seg,max)
  m2 <- c(0,segMax,20)
  mat2 <- matrix(m2, ncol=1, byrow=TRUE)
  seg2 <- reclassify(rst_seg, mat2)


  # set all class except treeclass to 0 treeclass to 1

  # get vector with all classes
  vec <-raster::unique(pred)
  vec <-vec[-which(vec==classTree)]
  print(vec)



  for (i in 1:length(vec)){
    if(i==1){
      m <- c(vec[i]-1,vec[i],0)
      mat <- matrix(m, ncol=1, byrow=TRUE)
      trees <- reclassify(pred,mat)
    } else {

      m <- c(vec[i]-1,vec[i],0)
      mat <- matrix(m, ncol=1, byrow=TRUE)
      trees <- reclassify(trees,mat)

    }
  } #end reclass
  plot(trees)
  # reclass TreeClass to class 1
  if(classTree!=1){
    m <- c(classTree-1,classTree,1)
    mat <- matrix(m, ncol=1, byrow=TRUE)
    trees <- reclassify(trees,mat)
  }

  # simple raster calc
  reslt <- trees+seg2

  # get values
  n0 <-ncell(reslt[reslt==0]) # no tree no seg
  n1 <-ncell(reslt[reslt==1]) # tree out of seg miss
  n20 <-ncell(reslt[reslt==20])# seg with no class miss
  n21 <-ncell(reslt[reslt==21])# class and seg hit

  nseg <- ncell(seg2[seg2==20]) #total cells in seg
  ncla <- ncell(trees[trees==1]) #total cells in trees

  # calculate validationscore
  valiscore <- round(n21/ncla,4) # % of hits to total class
  overclass <- round(n1/ncla,4) #% of miss to toal class
  overseg <- round(n20/nseg,4) #% of empty seg cells
  result <- data.frame(matrix(nrow = 1, ncol =8))
  result[,1]<-ncla
  result[,2]<-nseg
  result[,3]<-n1
  result[,4]<-n20
  result[,5]<-n21
  result[,6]<-valiscore
  result[,7]<-overseg
  result[,8]<-overclass
  names(result)<- c("nclass","nseg","overclass","underclass","hit",
                    "hitrate","rate underclass","rate overclass")

  valscore <- paste(valiscore,"@underclass",overseg,"@overclass",overclass)



  cat("valdiation score: class in seg @ empty seg",sep = "\n")

  cat(" ",sep = "\n")
  cat("IKARUS finished validation",sep = "\n")
  print(result)
  plot(trees)
  return(trees)

} #end main function
