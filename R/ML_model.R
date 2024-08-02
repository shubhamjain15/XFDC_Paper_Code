# Function for Empirical Distribution Matching (EDM)############################
edm_correction <- function(Y_ML, Y_TR_ML, Y_TR_OBS) {
  if (length(Y_TR_ML) != length(Y_TR_OBS)) {
    stop("Training vectors Y_TR_ML and Y_TR_OBS must have equal length.")
  }
  ranked_Y_TR_ML <- sort(Y_TR_ML)
  ranked_Y_TR_OBS <- sort(Y_TR_OBS)
  m_EDM <- cov(ranked_Y_TR_ML, ranked_Y_TR_OBS) / var(ranked_Y_TR_ML)
  interp_func <- approxfun(ranked_Y_TR_ML, ranked_Y_TR_OBS, rule = 2)
  Y_EDM <- sapply(Y_ML, function(y) {
    if (y >= min(ranked_Y_TR_ML) && y <= max(ranked_Y_TR_ML)) {
      interp_func(y)
    } else if (y > max(ranked_Y_TR_ML)) {
      max(ranked_Y_TR_OBS) + m_EDM * (y - max(ranked_Y_TR_ML))
    } else {
      min(ranked_Y_TR_OBS) + m_EDM * (y - min(ranked_Y_TR_ML))
    }
  })
  return(Y_EDM)
}

##RF_model#######################################################################################
build_rf <- function(XY_data, quantscol,features){
  XY_train <- XY_data[["XY_train"]]
  XY_test <- XY_data[["XY_test"]]
  preds_test <- data.frame(XY_test[,names(XY_test) %in% c("STAID", quantscol)])
  for(i in quantscol){
    train <- XY_train[, names(XY_train) %in% c(features, i)]
    set.seed(123)
    m1 <- tuneRF(x= as.data.frame(train[, features]),y = as.numeric(unlist(train[,i])),ntreeTry= 1000,mtryStart = 6,stepFactor = 1.5,improve= 0.001,trace = FALSE)
    m2 <- randomForest(x= as.data.frame(train[, features]),y = as.numeric(unlist(train[,i])),ntree = 1000, mtry = m1[as.numeric(which.min(m1[,2])),1])
    preds_test[,i] <- predict(m2, XY_test)
  }
  return(preds_test)
}

build_rf_edm <- function(XY_data, quantscol,features){
  XY_train <- XY_data[["XY_train"]]
  XY_test <- XY_data[["XY_test"]]
  preds_test <- data.frame(XY_test[,names(XY_test) %in% c("STAID", quantscol)])
  preds_train <- data.frame(XY_train[,names(XY_train) %in% c("STAID", quantscol)])
  for(i in quantscol){
    train <- XY_train[, names(XY_train) %in% c(features, i)]
    set.seed(123)
    m1 <- tuneRF(x= as.data.frame(train[, features]),y = as.numeric(unlist(train[,i])),ntreeTry= 1000,mtryStart  = 2,stepFactor = 1.5,improve= 0.01,trace = FALSE)
    m2 <- randomForest(x= as.data.frame(train[, features]),y = as.numeric(unlist(train[,i])),ntree = 1000, mtry = which.min(m1))
    preds_test[,i] <- predict(m2, XY_test)
    preds_train[,i] <- predict(m2,XY_train)
    preds_test[,i] <- edm_correction(preds_test[,i], preds_train[,i], as.numeric(unlist(train[,i])))
  }
  return(preds_test)
}
##Complete RF model #############################################################################
build_rf_models <-  function(XY_data, quantscol,features){
  XY_all <- XY_data[["XY_scaled"]]
  out <- list()
  for(i in quantscol){
    train <- XY_all[, names(XY_all) %in% c(features, i)]
    set.seed(123)
    m1 <- tuneRF(x= as.data.frame(train[, features]),y = as.numeric(unlist(train[,i])),ntreeTry= 1000,mtryStart  = 3,stepFactor = 1.5,improve= 0.001,trace = FALSE)
    m2 <- randomForest(x= as.data.frame(train[, features]),y = as.numeric(unlist(train[,i])),ntree = 1000, mtry = m1[as.numeric(which.min(m1[,2])),1],importance = TRUE)
    out[[i]] <- m2
  }
  return(out)
}

##Error#########################################################################################
relerr <- function(obs, pred){
  error <- sum(abs(obs - pred))/17
  return(error)
}
r2 <- function(sim, obs) {
  mean_obs <- mean(obs)
  mean_sim <- mean(sim)
  numerator <- sum((obs - mean_obs) * (sim - mean_sim))^2
  denominator <- sum((obs - mean_obs)^2) * sum((sim - mean_sim)^2)
  r_squared <- numerator / denominator
  return(r_squared)
}
rmsd <- function(obs,sim){
  error <- sqrt(sum((log10(obs)- log10(sim))^2)/length(obs))
  return(error)
}

auc <- function(obs,sim,probs){
  auc_obs <- trapz((probs/100),(as.numeric(obs)))
  auc_preds <- trapz((probs/100),(as.numeric(sim)))
  return(abs(auc_preds - auc_obs))
}

#Log space NSE
errors_all <- function(preds,XY_data,quantscol,quants){
  errors <-  list()
  #quant error
  quant_error <- data.frame(quants = quants,  NSE = as.numeric(NA),VE = as.numeric(NA))
  obsv <- XY_data[["XY_test"]][,quantscol]
    preds <- preds[,quantscol]
    nse <- c();lnse <- c();ve <- c();pbias <- c();kge <- c(); R2 <- c(); mae <- c(); rmse <- c()
    for(j in quantscol){
      nse <- c(nse,NSE(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
      ve <- c(ve,VE(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
      pbias <- c(pbias,pbias(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
      kge <- c(kge,KGE(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
      R2 <- c(R2,r2(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
      mae <- c(mae,mae(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
      rmse <- c(rmse,rmse(sim = as.numeric(unlist(preds[,j])),obs= as.numeric(unlist(obsv[,j]))))
    }
    quant_error$NSE <- round(nse,2);quant_error$VE <- round(ve,2); 
    quant_error$pbias <- round(pbias,2); quant_error$kge <- round(kge,2); quant_error$R2 <- round(R2,2); 
    quant_error$mae <- round(mae,2)
    quant_error$rmse <- round(rmse,2)
    errors[["quant_error"]] <- quant_error
    
  #monotonic violation
    quantscol <- quantscol[1:17]
    monotonic_violation <- data.frame(STAID = XY_data[["XY_test"]]$STAID,  Violations = as.numeric(NA))
    preds_df <- rf_preds[,c("STAID",quantscol)]
    for (x in 1:nrow(monotonic_violation)){
      site <- as.character(monotonic_violation$STAID[x])
      min_val <- cummin(as.numeric(preds_df[preds_df$STAID == site,2:18]))
      k  = 0 
      for(j in 1:17){
        if(min_val[j] != as.numeric(preds_df[preds_df$STAID == site,2:18])[j]){
          k = k +1
        }
      }
      monotonic_violation[monotonic_violation$STAID == site,"Violations"] <- k
    }
    errors[["monotonic_viol"]] <- monotonic_violation
  
  #Gage error
    gage_error <- data.frame(STAID = XY_data[["XY_test"]]$STAID,RMSD = as.numeric(NA))
    obsv <- XY_data[["XY_test"]][,quantscol[1:17]]
    preds_df <- rf_preds[,quantscol[1:17]]
      re <- c()
      for(j in 1:nrow(obsv)){
        preds <- as.numeric(unlist(preds_df[j,]))
        obs <- as.numeric(unlist(obsv[j,]))
        re <- c(re,rmsd(sim= preds, obs = obs))
      }
      gage_error[,"RMSD"] <- re 
    errors[["gage_error"]] <- gage_error
  return(errors)
}
