#' Validate a Tree Prediction with TreeCrown Segments
#' @description calculates the amount of cells for a class which overlap with responding Polygons.
#' @param pred RasterLayer - Prediction to be validated
#' @param rsp PolygonLayer - Response Polygons for a class
#' @param rsp_class numeric - The class representing the response area
#' @param reclass optional - numeric value of class to merge into response class, for multiple classes to merge use reclass=c(x,y)
#' @return returns the validation score
#' * return and plot the "tree [1] and no tree [2]" layer, will be reclassified if 'reclass' is used.
#' * result table
#' + nclass - amount of cells for selected class (may differ if 'reclass' is used)
#' + nseg - total amount of cells for response area
#' + nover - amount of cells for classification outside response area
#' + nunder -  amount of cells inside response area without correct class
#' + nhit - amount of cells inside response area with correct class
#' + hitrate - response area with correct class in % (sum of missrate and hitrate is 100%)
#' + missrate - response area without correct class in % (sum of missrate and hitrate is 100%)
#' + overrate - amount of cells for responding class outside responding area in % of total amount of cell of the class

#' @author Andreas Schönberg



#' @export ClassVal
#' @aliases ClassVal

ClassVal <- function(pred,rsp,rsp_class=NULL,reclass=NULL) {



  cat("IKARUS starting validation",sep = "\n")

  # reclassify
  if(is.null(reclass)==FALSE){


    # reclass all classes from var "reclass" to tree class
    for (i in 1:length(reclass)){
      if(i==1){
        m <- c(reclass[[i]]-1,reclass[[i]],rsp_class)
        mat <- matrix(m, ncol=1, byrow=TRUE)
        pred_re <- reclassify(pred,mat)
      } else {
        m <- c(reclass[[i]]-1,reclass[[i]],rsp_class)
        mat <- matrix(m, ncol=1, byrow=TRUE)
        pred_re <- reclassify(pred_re,mat)

      }
    } #end reclass
    pred <-pred_re
  }
  # plot(pred)


  # validation per segments
  # rasterize segments
  rst_rsp <-raster::rasterize(rsp,pred,background=0)
  #reclass segments to 20
  rspMax  <- cellStats(rst_rsp,max)
  m2 <- c(0,rspMax,20)
  mat2 <- matrix(m2, ncol=1, byrow=TRUE)
  rsp2 <- reclassify(rst_rsp, mat2)


  # set all class except treeclass to 0 treeclass to 1

  # get vector with all classes
  vec <-raster::unique(pred)
  vec <-vec[-which(vec==rsp_class)]




  for (i in 1:length(vec)){
    if(i==1){
      m <- c(vec[i]-1,vec[i],0)
      mat <- matrix(m, ncol=1, byrow=TRUE)
      rsp_cl <- reclassify(pred,mat)
    } else {

      m <- c(vec[i]-1,vec[i],0)
      mat <- matrix(m, ncol=1, byrow=TRUE)
      rsp_cl <- reclassify(rsp_cl,mat)

    }
  } #end reclass
  # plot(rsp_cl)
  # reclass TreeClass to class 1
  if(rsp_class!=1){
    m <- c(rsp_class-1,rsp_class,1)
    mat <- matrix(m, ncol=1, byrow=TRUE)
    rsp_cl <- reclassify(rsp_cl,mat)
  }

  # simple raster calc
  reslt <- rsp_cl+rsp2

  # get values
  n0 <-ncell(reslt[reslt==0]) # no class no response
  n1 <-ncell(reslt[reslt==1]) # class out of response
  n20 <-ncell(reslt[reslt==20])# response with no class
  n21 <-ncell(reslt[reslt==21])# class and response hit

  nrsp <- ncell(rsp2[rsp2==20]) #total cell of response area
  ncla <- ncell(rsp_cl[rsp_cl==1]) #total cells of class area

  # hit and miss of response area
  hitrate <- round(n21/nrsp,4) # % of response area with correct classification class
  missrate <- round(n20/nrsp,4) #% of response area without correct class
  ### Both together are 100% and refer to the response area

  # outside response area
  overrate <- round(n1/ncla,4) #% of cells for class outside response
  # % refer to total amount of cell for the class

  result <- data.frame(matrix(nrow = 1, ncol =8))
  result[,1]<-ncla # "nclass"
  result[,2]<-nrsp # "nresponse"
  result[,3]<-n1   # "nover"
  result[,4]<-n20  # "nunder"
  result[,5]<-n21 # "nhit"
  result[,6]<-hitrate
  result[,7]<-missrate
  result[,8]<-overrate
  names(result)<- c("nclass","nrsp","nover","nunder","nhit",
                    "hitrate","missrate","overrate")
  cat(" ",sep = "\n")
  valscore <- paste(hitrate,"@",overrate)



  cat(paste("valdiation score: ",valscore),sep = "\n")
  print(result)
  cat(" ",sep = "\n")
  cat("IKARUS finished validation",sep = "\n")
  #print(result)
  #plot(rsp_cl)
  return(result)

} #end main function
