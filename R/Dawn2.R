#' Executive function for testing AOA
#' @description wrapper for IKARUS and AOA with buffered TrainPoints.
#' @param FFS boolean - if True uses a FFS. Default= FALSE
#' @param Tpoints SpatialPoints - PointLayer with the TrainingPositions
#' @param buf_size numeric - with for the buffer in meter
#' @param design character - from for the buffer. Choose from "ROUND","FLAT","SQUARE"
#' @param Stk RasterStack - with predictors for classification
#' @param Stk_name character - name of input Stack for the name to write in images and output name.
#' @param plot_res boolean - set 'TRUE' to plot the prediction and AOA. Default = TRUE.
#' @param save_png boolean - set 'TRUE' to write the images in.png format for prediction and AOA. Default = FALSE.
#' @param fsize numeric - the desired front size for the main and sub text. default = 24.
#' @param save_res boolean - set 'TRUE' to write the prediction and AOA in .grd format. Default = FALSE.
#' @param path_res character - the path to save the resulting RasterLayer. Required if  "save_res" option is used.
#' @param path_png_cl character - the path to save the resulting plots for classification as png. Required if  "save_png" option is used.
#' @param path_png_aoa character - the path to save the resulting plots for AOA png. Required if  "save_png" option is used.
#' @param rsp SpatialPolygons - Response Polygon for a class used for validation.
#' @param rsp_class character - The name of the response class.
#' @param tunit character - Unit which should be use for the runtime displayed in the results. Default = "sec".
#' @return Returns the classification, model and the AOA. Optional saves the resulting RasterLayer and saves resuöting prediction and AOA to .png images.
#' @details This function is a wrapper for the IKARUS workflow for a LLOCV Random Forest classification and further uses the AOA approach by Meyer 2020.
#' By default the resulting prediction and AOA are plotted. By default further uses all varibales for the model, optional uses a FFS (if FFS=TRUE). Optional saves the RasterLayers. Further can save the results to .png format to a desired path.
#' Further the 'rsp' argument allow the direct estimation of the model performance by comparing one selected class with a response layer.
#' @author Andreas Schönberg

#' @export Dawn2
#' @aliases Dawn2



Dawn2 <- function(FFS=FALSE,Tpoints,buf_size,design,Stk,Stk_name,plot_res=TRUE,save_png=FALSE,save_res=FALSE,path_res,path_png,fsize=24,rsp=NULL,rsp_class=NULL,validate=F,tunit="sec"){

  ### prepare training data

  # compute TP buffer
  buf <-gBuffer(Tpoints,byid = T,width = buf_size,capStyle = design)

  # extract Training Data
  tDat <- exrct_Traindat_LLOCV(buf,Stk,"class","location")

  ###
  # run classification and return prediction
  if(FFS==T){
    tstart <- Sys.time()
    FFSmodel <- BestPredFFS_LLOCV(tDat=tDat,classCol = "class",classLocCol="class_location",nk=5)
    tstop <- Sys.time()
    # compute prediction and arrange pred and model equal to outpout from RFclassLLOCV to fit in following dawn code
    pred <- raster::predict(Stk,FFSmodel)

    # rename variables
    cl <- list("prediction"=pred,"model_LLOCV"=FFSmodel)
    cat(" ",sep = "\n")
    cat("IKARUS finished",sep = "\n")
  } else {
    tstart <- Sys.time()
    cl <- RFclass_LLOCV(tDat = tDat,nk=5,predStk = Stk,classCol = "class")
    tstop <- Sys.time()
  }


  if(validate == T){

    # get position of response class
    ucl <-unique(Tpoints$class)
    # sort for alphabetic order
    sucl <-sort(ucl)
    # get position
    rsp_pos <-which(sucl==rsp_class)

    # get validation score
    val <- ClassVal(  pred=cl$prediction,  rsp=rsp,  rsp_class=rsp_pos,  reclass=NULL)

  }
  # AOA calculation
  cat(" ",sep = "\n")
  cat("calculation AOA",sep = "\n")
  # run AOA
  aoa <- CAST::aoa(newdata = Stk,model = cl$model_LLOCV)

  ### Output modes


  # set name of design
  if (design=="FLAT"){
    form = "f"
  }
  if (design=="SQUARE"){
    form = "r"
  }
  if (design=="ROUND"){
    form = "c"
  }

  # merge name (bufsize * 100 to prevent "." in output filename)
  name_cl  <- paste0(Stk_name,"_",form,buf_size*100,"_CL")

  name_aoa <- paste0(Stk_name,"_",form,buf_size*100,"_AOA")

  # prepare sub text for CL with accuracy and kappa for best tune
  res_values <-(subset(cl$model_LLOCV$results, mtry == cl$model_LLOCV$bestTune[1,1]))
  sub_cl <- paste0("accuracy: ",round(res_values[2],4)," kappa: ",round(res_values[3],4))

  # prepare sub text for AOA with percent of AOA area
  per_aoa <- sum(aoa$AOA[aoa$AOA ==1]) /  ncell(aoa$AOA)
  sub_aoa <- paste0("AOA_area: ", round(per_aoa,4))

  # get raster without attributes to run with splot
  pred <- setValues(raster(cl$prediction), cl$prediction[])

  ### save result plots as png
  if(plot_res==TRUE){
    if(validate == T){


      # set main and sub for validation
      main_val=paste("Response for ",rsp_class)
      sub_val=paste("hit@over: ",val$hitrate,"@",val$overrate)

      grid.arrange(
        spplot(pred,col.regions=viridis(100),colorkey=NULL,sub=sub_cl,main=name_cl,par.settings = list(fontsize = list(text = 8))),
        spplot(aoa$AOA ,col.regions=c("red","grey"),colorkey=NULL ,main=name_aoa,sub=sub_aoa,par.settings = list(fontsize = list(text = 8))),
        spplot(pred,col.regions=viridis(100),colorkey=NULL,main=main_val,sub=sub_val,par.settings = list(fontsize = list(text = 8)))+spplot(rsp[1],col.regions=c("transparent"),col="red",lwd=2,colorkey=NULL),
        ncol=3)

    } else{
      # plot prediction and AOA in grid
      grid.arrange(
        spplot(pred,col.regions=viridis(100),colorkey=NULL,sub=sub_cl,main=name_cl,par.settings = list(fontsize = list(text = 12))),
        spplot(aoa$AOA ,col.regions=c("red","grey"),colorkey=NULL ,main=name_aoa,sub=sub_aoa,par.settings = list(fontsize = list(text = 12))),
        ncol=2)


      cat(" ",sep = "\n")
      cat("plotting results",sep = "\n")
    }

  }

  if(save_png==TRUE){
    if(validate == T){

      # set name for validation img
      name_val <- paste0(Stk_name,"_",form,buf_size*100,"_VAL")

      # save plot code to variable due to png requires print() to run with a function
      pCL <- spplot(pred,col.regions=viridis(100),colorkey=NULL,sub=sub_cl,main=name_cl,par.settings = list(fontsize = list(text = fsize)))
      pAO <-spplot(aoa$AOA ,col.regions=c("red","grey") ,colorkey=NULL,main=name_aoa,sub=sub_aoa,par.settings = list(fontsize = list(text = fsize)))
      pVA <-spplot(pred,col.regions=viridis(100),colorkey=NULL,main=main_val,sub=sub_val,par.settings = list(fontsize = list(text = fsize)))+spplot(rsp[1],col.regions=c("transparent"),col="red",lwd=2,colorkey=NULL)

      # open device for .png
      png(filename=paste0(path_png,name_cl,".png"))
      # print the plot
      print(pCL)
      # close device
      dev.off()

      # open device for .png
      png(filename=paste0(path_png,name_aoa,".png"))
      # print the plot
      print(pAO)
      # close device
      dev.off()

      # open device for .png
      png(filename=paste0(path_png,name_val,".png"))
      # print the plot
      print(pVA)
      # close device
      dev.off()

      # cat path
      cat(" ",sep = "\n")
      cat(paste0("images saved to:",path_png,sep = "\n"))
    }


    # save plot code to variable due to png requires print() to run with a function
    pCL <- spplot(pred,col.regions=viridis(100),colorkey=NULL,sub=sub_cl,main=name_cl,par.settings = list(fontsize = list(text = fsize)))
    pAO <-spplot(aoa$AOA ,col.regions=c("red","grey") ,colorkey=NULL,main=name_aoa,sub=sub_aoa,par.settings = list(fontsize = list(text = fsize)))

    # open device for .png
    png(filename=paste0(path_png,name_cl,".png"))
    # print the plot
    print(pCL)
    # close device
    dev.off()

    # open device for .png
    png(filename=paste0(path_png,name_aoa,".png"))
    # print the plot
    print(pAO)
    # close device
    dev.off()

    # cat path
    cat(" ",sep = "\n")
    cat(paste0("images saved to:",path_png,sep = "\n"))
  }

  # save results
  if(save_res==T){
    writeRaster(cl$prediction,paste0(path_res,name_cl,".grd"), format="raster")
    writeRaster(aoa$AOA,paste0(path_res,name_aoa,".grd"), format="raster")
    cat(" ",sep = "\n")
    cat(paste0("result raster saved to:",path_res,sep = "\n"))
  }

  # validation


  # output
  cat(" ",sep = "\n")
  cat("finished",sep = "\n")

  # caluclate timediffenrence
  timedif <-difftime(tstop,tstart,units = tunit)
  timedif

  # print without "timediffernece" add
  tdf <-paste(round(timedif,4),units(timedif))

  # save values
  if(validate == T){
    # data frame
    df <- data.frame(matrix(nrow = 1, ncol = 7))

    colnames(df) <- c("Stk/design", "accuracy","kappa","AOA","Val_hit","Val_over","runtime")
    df[1] <- paste0(Stk_name,"_",form,buf_size*100)
    df[2] <- round(res_values[2],4)
    df[3] <- round(res_values[3],4)
    df[4] <- round(per_aoa,4)
    df[5] <- val$hitrate
    df[6] <- val$overrate
    df[7] <- tdf


    re <-list("prediction"=cl$prediction,"model_LLOCV"=cl$model_LLOCV,"AOA"=aoa$AOA,"VALUES" =df)
    return(re)
  }

  # save values
  df <- data.frame(matrix(nrow = 1, ncol = 7))

  colnames(df) <- c("Stk/design", "accuracy","kappa","AOA","Val_hit","Val_over","runtime")
  df[1] <- paste0(Stk_name,"_",form,buf_size*100)
  df[2] <- round(res_values[2],4)
  df[3] <- round(res_values[3],4)
  df[4] <- round(per_aoa,4)
  df[7] <- tdf

  re <-list("prediction"=cl$prediction,"model_LLOCV"=cl$model_LLOCV,"AOA"=aoa$AOA,"VALUES" =df)
  return(re)

} # end function

