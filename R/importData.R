####################################################################################################################
#A) Download gages                                                                                                 #
## Import and Clean GAGESII Dataset                                                                                #
## Use the gagesII dataset to identify streamgages                                                                 #
## Remove gages ending before 1991 and with less than 10 years of data. Further cleaning later after getting flows #
####################################################################################################################

importGages <- function() {
  conterm_basinid <- read_csv("Data/GAGESII/conterm_basinid.txt", 
                              col_types = cols(BOUND_SOURCE = col_skip(),
                                               HCDN_2009 = col_skip(),
                                               HBN36 = col_skip(),
                                               OLD_HCDN = col_skip(),
                                               NSIP_SENTINEL = col_skip(),
                                               FIPS_SITE = col_skip(),
                                               COUNTYNAME_SITE = col_skip(),
                                               NAWQA_SUID = col_skip()))
  conterm_bas_morph <- read_csv("Data/GAGESII/conterm_bas_morph.txt",
                                col_types = cols(LAT_CENT = col_skip(),BAS_COMPACTNESS = col_skip(),
                                                 LONG_CENT = col_skip()))
  
  
  conterm_hydromod_dams <- read_csv("Data/GagesII/conterm_hydromod_dams.txt", 
                                    col_types = cols(DDENS_2009 = col_skip(), STOR_NID_2009 = col_skip(),
                                                     MAJ_DDENS_2009 = col_skip(), RAW_AVG_DIS_ALLDAMS = col_skip(),
                                                     STOR_NOR_2009 = col_skip(),
                                                     RAW_AVG_DIS_ALL_MAJ_DAMS = col_skip(), 
                                                     RAW_DIS_NEAREST_DAM = col_skip(), 
                                                     RAW_DIS_NEAREST_MAJ_DAM = col_skip(), 
                                                     pre1940_DDENS = col_skip(), pre1940_NDAMS = col_skip(), 
                                                     pre1940_STOR = col_skip(), pre1950_DDENS = col_skip(), 
                                                     pre1950_NDAMS = col_skip(), pre1950_STOR = col_skip(), 
                                                     pre1960_DDENS = col_skip(), pre1960_NDAMS = col_skip(), 
                                                     pre1960_STOR = col_skip(), pre1970_DDENS = col_skip(), 
                                                     pre1970_NDAMS = col_skip(), pre1970_STOR = col_skip(), 
                                                     pre1980_DDENS = col_skip(), pre1980_NDAMS = col_skip(), 
                                                     pre1980_STOR = col_skip(), pre1990_DDENS = col_skip(), 
                                                     pre1990_NDAMS = col_skip(), pre1990_STOR = col_skip()))
  
  conterm_hydromod_other <- read_csv("Data/GAGESII/conterm_hydromod_other.txt", 
                                     col_types = cols( CANALS_PCT = col_skip(),
                                                       CANALS_MAINSTEM_PCT = col_skip(),
                                                       FRESHW_WITHDRAWAL = col_skip(),
                                                       MINING92_PCT = col_skip(), 
                                                       NPDES_MAJ_DENS = col_skip(),
                                                       RAW_DIS_NEAREST_MAJ_NPDES = col_skip(),
                                                       POWER_SUM_MW = col_skip(), RAW_AVG_DIS_ALLCANALS = col_skip(), 
                                                       RAW_AVG_DIS_ALL_MAJ_NPDES = col_skip(), PCT_IRRIG_AG = col_skip(),
                                                       RAW_DIS_NEAREST_CANAL = col_skip(),POWER_NUM_PTS = col_skip()))
  
  
  
  conterm_lc06_basin <- read_csv("Data/GAGESII/conterm_lc06_basin.txt", 
                                 col_types = cols(CROPSNLCD06 = col_skip(), DECIDNLCD06 = col_skip(), 
                                                  DEVHINLCD06 = col_skip(), DEVLOWNLCD06 = col_skip(), 
                                                  DEVMEDNLCD06 = col_skip(), DEVOPENNLCD06 = col_skip(), 
                                                  EVERGRNLCD06 = col_skip(), WATERNLCD06 = col_skip(), BARRENNLCD06 = col_skip(),
                                                  MIXEDFORNLCD06 = col_skip(), PASTURENLCD06 = col_skip(),
                                                  SNOWICENLCD06 = col_skip(), SHRUBNLCD06 = col_skip(), PLANTNLCD06 = col_skip())) 
  
  
  conterm_topo <- read_csv("Data/GAGESII/conterm_topo.txt",
                           col_types = cols(ELEV_MAX_M_BASIN = col_skip(),
                                            ELEV_MIN_M_BASIN = col_skip(),
                                            ELEV_MEDIAN_M_BASIN = col_skip(),
                                            ELEV_STD_M_BASIN = col_skip(),
                                            ELEV_SITE_M = col_skip(),
                                            RRMEAN = col_skip(),
                                            RRMEDIAN = col_skip(),
                                            ASPECT_DEGREES = col_skip(),
                                            ASPECT_NORTHNESS = col_skip(),
                                            ASPECT_EASTNESS = col_skip()))
  
  conterm_soils <- read_csv("Data/GAGESII/conterm_soils.txt", 
                            col_types = cols(#AWCAVE = col_skip(), 
                              HGAD = col_skip(),
                              HGAC = col_skip(),HGBD = col_skip(),
                              HGCD = col_skip(),HGBC = col_skip(),
                              BDAVE = col_skip(), 
                              HGA = col_skip(), HGB = col_skip(),
                              #CLAYAVE = col_skip(), 
                              HGVAR = col_skip(), KFACT_UP = col_skip(), 
                              NO10AVE = col_skip(), NO200AVE = col_skip(), 
                              NO4AVE = col_skip(), OMAVE = col_skip(), 
                              #PERMAVE = col_skip(), 
                              RFACT = col_skip(), 
                              AWCAVE = col_skip(),
                              ROCKDEPAVE = col_skip(), 
                              #SANDAVE = col_skip(), 
                              #SILTAVE = col_skip(), 
                              WTDEPAVE = col_skip()))
  
  conterm_class <- read_csv("Data/GAGESII/conterm_bas_classif.txt",
                            col_types = cols(AGGECOREGION = col_skip(),
                                             WR_REPORT_REMARKS = col_skip(),
                                             ADR_CITATION = col_skip()))
  
  conterm_climate <- read_csv("Data/GAGESII/conterm_climate.txt",
                              col_types = cols(PPTAVG_SITE = col_skip(),
                                               #PET = col_skip(),
                                               T_AVG_SITE = col_skip(),T_AVG_BASIN = col_skip(),
                                               T_MAX_BASIN = col_skip(),
                                               T_MAXSTD_BASIN = col_skip(),
                                               T_MAX_SITE = col_skip(),
                                               T_MIN_BASIN = col_skip(),
                                               T_MINSTD_BASIN = col_skip(),
                                               T_MIN_SITE = col_skip(),
                                               #RH_BASIN = col_skip(),
                                               RH_SITE = col_skip(),
                                               FST32F_BASIN = col_skip(),
                                               LST32F_BASIN = col_skip(),
                                               FST32SITE = col_skip(),
                                               LST32SITE = col_skip(),
                                               WD_SITE = col_skip(),
                                               WDMAX_BASIN = col_skip(),
                                               WDMIN_BASIN = col_skip(),
                                               WDMAX_SITE = col_skip(),
                                               WDMIN_SITE = col_skip(),
                                               #SNOW_PCT_PRECIP = col_skip(),
                                               JAN_PPT7100_CM = col_skip(),
                                               FEB_PPT7100_CM = col_skip(),
                                               MAR_PPT7100_CM = col_skip(),
                                               APR_PPT7100_CM = col_skip(),
                                               MAY_PPT7100_CM = col_skip(),
                                               JUN_PPT7100_CM = col_skip(),
                                               JUL_PPT7100_CM = col_skip(),
                                               AUG_PPT7100_CM = col_skip(),
                                               SEP_PPT7100_CM = col_skip(),
                                               OCT_PPT7100_CM = col_skip(),
                                               NOV_PPT7100_CM = col_skip(),
                                               DEC_PPT7100_CM = col_skip(),
                                               JAN_TMP7100_DEGC = col_skip(),
                                               FEB_TMP7100_DEGC = col_skip(),
                                               MAR_TMP7100_DEGC = col_skip(),
                                               APR_TMP7100_DEGC = col_skip(),
                                               MAY_TMP7100_DEGC = col_skip(),
                                               JUN_TMP7100_DEGC = col_skip(),
                                               JUL_TMP7100_DEGC = col_skip(),
                                               AUG_TMP7100_DEGC = col_skip(),
                                               SEP_TMP7100_DEGC = col_skip(),
                                               OCT_TMP7100_DEGC = col_skip(),
                                               NOV_TMP7100_DEGC = col_skip(),
                                               DEC_TMP7100_DEGC = col_skip()))
  conterm_hydro <- read_csv("Data/GAGESII/conterm_hydro.txt",
                            col_types = cols(STRAHLER_MAX= col_skip(),
                                             MAINSTEM_SINUOUSITY= col_skip(),
                                             REACHCODE= col_skip(),
                                             ARTIFPATH_PCT= col_skip(),
                                             ARTIFPATH_MAINSTEM_PCT= col_skip(),
                                             HIRES_LENTIC_PCT= col_skip(),
                                             PERDUN= col_skip(),
                                             PERHOR= col_skip(),
                                             TOPWET= col_skip(),
                                             CONTACT= col_skip(),
                                             RUNAVE7100= col_skip(),
                                             WB5100_JAN_MM= col_skip(),
                                             WB5100_FEB_MM= col_skip(),
                                             WB5100_MAR_MM= col_skip(),
                                             WB5100_APR_MM= col_skip(),
                                             WB5100_MAY_MM= col_skip(),
                                             WB5100_JUN_MM= col_skip(),
                                             WB5100_JUL_MM= col_skip(),
                                             WB5100_AUG_MM= col_skip(),
                                             WB5100_SEP_MM= col_skip(),
                                             WB5100_OCT_MM= col_skip(),
                                             WB5100_NOV_MM= col_skip(),
                                             WB5100_DEC_MM= col_skip(),
                                             WB5100_ANN_MM= col_skip(),
                                             PCT_1ST_ORDER= col_skip(),
                                             PCT_2ND_ORDER= col_skip(),
                                             PCT_3RD_ORDER= col_skip(),
                                             PCT_4TH_ORDER= col_skip(),
                                             PCT_5TH_ORDER= col_skip(),
                                             PCT_6TH_ORDER_OR_MORE= col_skip(),
                                             PCT_NO_ORDER= col_skip()))
  
  ####################################################
  ## filter sites based on selected characteristics ##
  ## still need to determine our selection criteria ##
  ####################################################
  sites <- conterm_basinid %>%
    left_join(conterm_class)%>%
    left_join(conterm_hydromod_other) %>%
    left_join(conterm_hydromod_other) %>%
    left_join(conterm_hydromod_dams) %>%
    left_join((conterm_hydro))%>%
    left_join(conterm_lc06_basin) %>%
    left_join(conterm_topo) %>%
    left_join(conterm_soils)%>%
    left_join(conterm_climate)%>%
    left_join(conterm_bas_morph)%>%
    filter(CLASS == "Ref" & DRAIN_SQKM <= 5000 & DRAIN_SQKM >= 10)%>%
    rowwise()%>%
    mutate(WETNLCD06 = sum(WOODYWETNLCD06,EMERGWETNLCD06))%>%
    mutate(poordrain = sum(HGC,HGD))%>%
    mutate_at(vars(PPTAVG_BASIN), ~. * 10)%>%
    mutate(aridity = PET/(PPTAVG_BASIN))%>%
    select(- c(WOODYWETNLCD06,EMERGWETNLCD06,PET,HGC,HGD))
  
  whatgages <- dataRetrieval::whatNWISdata(siteNumber =sites$STAID,parameterCd = "00060",service="dv",statCd = "00003")
  whatgages <- whatgages%>%filter(end_date > as.Date("1991-01-01") & count_nu >= 3652)
  
  sites <- sites%>%filter(STAID %in% whatgages$site_no)
  return(sites)
}

##################################################################################################################
#B) Download Flows                                                                                               #
## Use dataRetrival to download flows at identified gages                                                        #
## gages = data.frame returned by importGages()                                                                  #
##################################################################################################################
downloadFlow <- function(gages) {

  flow <- readNWISdv(siteNumbers = gages$STAID,
                     startDate = "1991-01-01",
                     endDate = "2020-12-31",
                     parameterCd = "00060")
  flow <- renameNWISColumns(flow)
  flow <- flow%>%
            filter(Flow >= 0)%>%
            select(site_no, Date, Flow) %>%
            group_by(site_no) %>%
            arrange(Flow) %>%
            mutate(excPrb = (rank(-Flow, na.last = "keep"))*100/((sum(!is.na(Flow)))))

  return(flow)
}

downloadFlow <- function(gages) {
  flow <- ww_dvUSGS(gages$STAID,start_date = "1991-01-01",
                    end_date = "2020-12-31",
                    parameterCd = "00060",
                    parallel = TRUE,verbose = FALSE)
  flow <- flow%>%select(c("site_no","Date","Flow"))
  flow <- flow%>%
    filter(Flow >= 0)%>%
    select(site_no, Date, Flow) %>%
    group_by(site_no) %>%
    arrange(Flow) %>%
    mutate(excPrb = (rank(-Flow, na.last = "keep"))*100/((sum(!is.na(Flow)))))
  return(flow)
}
