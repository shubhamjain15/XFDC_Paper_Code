####################################################################################################################
#Names and values                                                                                                  #
####################################################################################################################
features <- names(XY_data[["XY"]])[2:21]
feature_names <- c("Drainage area","Stream density", "Elevation","Watershed Slope","Baseflow index", "% Developed", "% Forest","% Grassland","%Wetland" , "Permeability"
                   ,"% Clay", "% Silt", "% Sand","% Poorly drained", "Precipitation", "Relative humidity", "Wet days", "% Snow", "Seasonality Index", "Aridity Index")
features_lookup <- feature_names
names(features_lookup) <- features
quants <- c("0.01","0.1","0.5","1","5","10","20","30","40","50","60","70","80","90","95","99","99.99","Slope")
probs <- c(0.01,0.1,0.5,1,5,10,20,30,40,50,60,70,80,90,95,99,99.99)
quantscol <- names(XY_data[["XY"]])[22:39]
mean_fdc <- colMeans(XY_data[["XY"]][,22:39])
std_devs <- apply(XY_data[["XY"]][, 22:39], 2, sd)
std_devs <- setNames(std_devs, quantscol)
####################################################################################################################
#A) Filter Gages                                                                                                   #
## at least 10 years of complete record                                                                            #      
## cease to flow percentile > 0.99                                                                                 #
####################################################################################################################
filter_gages <- function(flows,gages){
  complete_count <- flows%>%
    group_by(site_no, year = format(as.Date(Date),"%Y"))%>%
    summarise(n_complete = n_distinct(Date))%>%
    group_by(site_no) %>%
    summarize(n_years_complete = sum(n_complete >= 365))
  flow_summary <- flows%>%
    group_by(site_no)%>%
    summarise(n = n(), n_zero = sum(Flow ==0))%>%
    mutate(ctf = 1 - (n_zero/n))%>%
    left_join(complete_count,by = "site_no")%>%
    filter(ctf >= 0.99 & n_years_complete >=10)
  
  gages <- gages%>%filter(STAID %in% flow_summary$site_no)
  return(gages)
}

####################################################################################################################
#B) Create FDC dataframe                                                                                           #
##Get flow values at 17 exceededence probabilities                                                                 #
####################################################################################################################
fdcquants <- function(flows){
  flow_quants <- data.frame(site_no=as.character(unique(flows$site_no)),
                            f0.01 = NA, f0.1 = NA, f0.5 = NA, f1 = NA, f5 = NA, f10 = NA, f20 = NA, f30 = NA,f33.3 = NA, f40 = NA, f50 = NA, f60 = NA,
                            f66.6 = NA, f70 = NA, f80 = NA, f90 = NA, f95 = NA,f99 = NA,f99.99 = NA)
  
  probs <- (100 - c(0.01, 0.1, 0.5, 1, 5, 10, 20, 30,33.33, 40, 50, 60,
                    66.66,70, 80, 90, 95,99,99.99)) / 100
  
  for (i in unique(flows$site_no)){
    flow_quants[flow_quants$site_no == i,2:ncol(flow_quants)] <- quantile(flows$Flow[flows$site_no == i],probs,type = 6)
    
  }
  return(flow_quants)
}

####################################################################################################################
#C) Create XY & train, test                                                                                        #
# training data based on 20% in each HUC02                                                                         #
# scaling on train data and apply the same scaling on test data                                                    #
# return a list with XY - no scaling, XY_scaled - scaling on all values, XY_train - scaling on training data       #
## XY-test - scaling from train data applied on test data                                                          #
####################################################################################################################
create_train_test <- function(gages,FDCs){
  X <- gages%>%select(c(STAID,HUC02,DRAIN_SQKM,STREAMS_KM_SQ_KM,ELEV_MEAN_M_BASIN,SLOPE_PCT,BFI_AVE,DEVNLCD06,FORESTNLCD06,GRASSNLCD06,WETNLCD06,
                        PERMAVE,CLAYAVE,SILTAVE,SANDAVE,poordrain,PPTAVG_BASIN,RH_BASIN,WD_BASIN,SNOW_PCT_PRECIP,PRECIP_SEAS_IND,aridity))
  features <- names(X)[-c(1:2)]
  XY <- X%>%left_join(FDCs, by = c("STAID" = "site_no"))
  XY[,23:41] <- ((XY[,23:41]*2446575580800)/(XY$DRAIN_SQKM*10^12))  ##mm/day
  XY$Slope <- (log10(XY$f33.3) - log10(XY$f66.6))/(.6666 - 0.3333)
  XY <- XY%>%select(-c(f33.3,f66.6))
  set.seed(123)
  XY_test <- XY%>%group_by(HUC02)%>%sample_frac(0.2)%>%ungroup()
  XY_train <- XY%>%filter(!STAID %in% XY_test$STAID)
  
  scaled_X_train <- scale(XY_train[,features])
  scaled_X_test  <- scale(XY_test[,features], center=attr(scaled_X_train, "scaled:center"), 
                          scale=attr(scaled_X_train, "scaled:scale"))
  XY_train[,features] <- scaled_X_train 
  XY_test[,features] <- scaled_X_test
  
  XY_scaled <- XY
  XY_scaled[,features] <- scale(XY_scaled[,features])
  
  XY = XY%>%select(-HUC02)
  XY_scaled = XY_scaled%>%select(-HUC02)
  XY_train = XY_train%>%select(-HUC02)
  XY_test = XY_test%>%select(-HUC02)
  outlist <- list(XY = XY,XY_scaled = XY_scaled,XY_train = XY_train,XY_test = XY_test)
}
