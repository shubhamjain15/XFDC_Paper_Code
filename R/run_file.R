###Load Libraries###############################################################
source("load_libraries.R")

#1.DATA PREPARATION#############################################################
#1.1. Download GAGES II and Flow data
source("importData.R")
gages <- importGages()
flows <- downloadFlow(gages)
saveRDS(flows,"./outputs/RData/flows_complete.RDS")

#1.2. Prepare gages and FDC dataframe
source("prepareData.R")
gages <- filter_gages(flows,gages)
write.csv(gages,"./Outputs/RData/gages.csv")

flows <- flows%>%dplyr::filter(site_no %in% gages$STAID)
flows[flows == 0] <- 0.001

FDCs <- fdcquants(flows)
write.csv(FDCs,"./Outputs/RData/FDCs.csv")

#1.3. Create train and test dataframe
XY_data <- create_train_test(gages,FDCs)
saveRDS(XY_data,"./Outputs/RData/XY_data.RDS")

#Long Station name changed to "40211410"

#2.Model########################################################################
source("ML_model.R")
rf_preds <- build_rf(XY_data, quantscol,features)   #train test 
#rf_preds_edm <- build_rf_edm(XY_data,quantscol,features)   #bias corrected but not applied to paper
errors_list <- errors_all(rf_preds,XY_data,quantscol,quants)
#errors_list_edm <- errors_all(rf_preds_edm,XY_data,quantscol,quants)

rmsd_region <- errors_list_edm$monotonic_viol

df <- errors_list$quant_error%>%select("quants","NSE","VE","pbias","kge","R2")
write.csv(df,"./outputs/tables/quant_error.csv")
write.csv(errors_list_edm$gage_error,"./Outputs/tables/gage_error.csv")
rm(df)

df <- errors_list$gage_error
df <- df%>%left_join(gages%>%select("STAID","HUC02"), by = "STAID")
df2 <- df%>%group_by(HUC02)%>%summarise(median = median(RMSD))
sum(df$RMSD < 1)/198
df3 <- gages%>%filter(STAID %in% df$STAID[df$RMSD >1])

models_rf <- build_rf_models(XY_data, quantscol,features)  #complete model

#3.Interpretability#############################################################
set.seed(123)
source("interpretability.R")
PFI_rf <- rf_importance(models_rf,features,quantscol)

set.seed(123)
SHAP_rf <- shap_build(models_rf,XY_data,features,quantscol)
SHAP_rf_importance <- SHAP_global_importance(SHAP_rf,features,quantscol)

shap_interactions <- shap_interactions(models_rf,XY_data,features,c("f50","Slope"))

saveRDS(shap_interactions,"./outputs/RData/SHAP_interactions.RDS")

save.image(file = "my_environment.RData")

#4.Plots########################################################################
#variable importance plots
source("plot_global_importance.R")
source("plot_pdp.R")
source("plot_shap_regional.R")

source("plot_contribution_function.R")
source("plot_shap_force.R")

source("plot_generalization_error.R")