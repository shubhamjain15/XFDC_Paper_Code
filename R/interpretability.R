## Variable importance for RF model###############################################################
rf_importance <- function(models, features,quantscol){
  out <- data.frame(Feature = features, f0.01 = NA, f0.1 = NA, f0.5 = NA, f1 = NA, f5 = NA, f10 = NA, f20 = NA, f30 = NA, f40 = NA, f50 = NA, f60 = NA,
                    f70 = NA, f80 = NA, f90 = NA, f95 = NA,f99 = NA,f99.99 = NA,Slope = NA)
  for(i in quantscol){
    df <- models[[i]]
    df2 <- importance(df)
    out[,i] <- df2[,1]
  }
  return(out)
}


## SHAP############################################################################################ 
shap_build <- function(models, XY_data, features,quantscol){
  XY_scaled <- XY_data[["XY_scaled"]]
  out <- list()
  for(i in quantscol){
    unified <- randomForest.unify(models[[i]], XY_scaled[,c(features)])
    out[[i]] <- treeshap(unified,  XY_scaled[,c(features)], verbose = 0, interactions = FALSE)
  }
  return(out)
}

## SHAP Interactions ############################################################################################ 
shap_interactions <- function(models, XY_data, features,quantscol){
  XY_scaled <- XY_data[["XY_scaled"]]
  out <- list()
  for(i in quantscol){
    unified <- randomForest.unify(models[[i]], XY_scaled[,c(features)])
    out[[i]] <- treeshap(unified,  XY_scaled[,c(features)], verbose = 0, interactions = TRUE)
  }
  return(out)
}

##SHAP importance###################################################################################
SHAP_global_importance <- function(shap_list_rf, features,quantscol){
  out <- data.frame(Feature = features, f0.01 = NA, f0.1 = NA, f0.5 = NA, f1 = NA, f5 = NA, f10 = NA, f20 = NA, f30 = NA, f40 = NA, f50 = NA, f60 = NA,
                    f70 = NA, f80 = NA, f90 = NA, f95 = NA,f99 = NA,f99.99 = NA,Slope = NA)
  for(i in quantscol){
    df <- shap_list_rf[[i]]$shaps
    out[,i] <- colMeans(abs(df))
  }
  return(out)
}
