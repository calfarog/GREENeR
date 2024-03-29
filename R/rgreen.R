#
#' @title Aggregate loop
#'
#' @description This function aggregate the variables of data frame.
#'
#' @param listaShrt, cdat_greenLt A data frame
#' @param years A vector of integer
#' @param type A character with values "L" when running launch_green and "N"
#' when running region_nut_balance
#' @return A matrix
#'
#' @importFrom data.table := setkey
#'
#' @keywords internal
#'
aggregate_loop <- function(listaShrt, cdat_greenLt, years, type){

  cdat_total <- NULL

  for (yr in years) {
    print(paste("Calculating year:", yr))
    shr_hydro_IDt <- unique(listaShrt[[1]][Year == yr]$HydroID)
    cdat_greenLt_Year <- cdat_greenLt[Year == yr]
    cdat_greenLt_Year[cdat_greenLt_Year$HydroID %in% shr_hydro_IDt,
                      CatchLoad := (1 - LakeFrRet) *
                        (CatchToRiver + CatchLoad) * BB]
    if (type == "N"){
      cdat_greenLt_Year[cdat_greenLt_Year$HydroID %in% shr_hydro_IDt,
                        CatchRivRet := (CatchToRiver + CatchLoad) * (1 - BB)]
      cdat_greenLt_Year[cdat_greenLt_Year$HydroID %in% shr_hydro_IDt,
                        CatchLakeRet := LakeFrRet *
                          (CatchToRiver + CatchLoad) * BB]
    }

    data.table::setkey(cdat_greenLt_Year, "To_catch")

    if(length(listaShrt)>1){
      for (iShr in 2:(length(listaShrt))) {
        shr_hydro_IDt <- listaShrt[[iShr]][Year == yr]$HydroID
        df_agt <- cdat_greenLt_Year[.(shr_hydro_IDt)]
        df_agt <- df_agt[, .(To_catch, CatchLoad)]
        df_agt <- df_agt[, .(CatchLoad = sum(CatchLoad)), .(HydroID = To_catch)]

        cdat_greenLt_Year[match(df_agt$HydroID,cdat_greenLt_Year$HydroID),
                          CatchLoad := df_agt$CatchLoad]
        cdat_greenLt_Year[cdat_greenLt_Year$HydroID %in% shr_hydro_IDt,
                          CatchLoad := (1 - LakeFrRet) *
                            (CatchToRiver + CatchLoad) * BB]
        if (type == "N"){
          cdat_greenLt_Year[cdat_greenLt_Year$HydroID %in% shr_hydro_IDt,
                            CatchRivRet := (CatchToRiver + CatchLoad) * (1 - BB)]
          cdat_greenLt_Year[cdat_greenLt_Year$HydroID %in% shr_hydro_IDt,
                            CatchLakeRet := LakeFrRet *
                              (CatchToRiver + CatchLoad) * BB]
        }

      }
    }

    cdat_total <- rbind(cdat_total, cdat_greenLt_Year)
  }

  cdat_total
}

#'
#' @title Append empty columns
#'
#' @description This function add empty values to the data frame.
#'
#' @param df data frame. A data frame to add two new columns, Fix and Soil
#'
#' @return One object, a data frame.
#'
#' @keywords internal
#'
append_empty_cols <- function(df){

  if (length(df) == 12) {
    df <- df %>%
      dplyr::rename(Atm = Bg) %>%
      dplyr::mutate("Soil" = 0, "Fix" = 0)
  }

  df
}

#
#' @title Calibration of the GREEN model
#'
#' @description Runs GREEN model calibration
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param n_iter numeric. Number of iterations for the calibration process.
#' @param low numeric. Lower bounds of the calibration parameters.
#' @param upp numeric. Upper bounds of the calibration parameters.
#' @param years integer. Years to be used in the calibration. For sequences use
#' c(yearini:yearend).
#' @return One object, a data frame with the model calibration
#'
#' @importFrom parallel detectCores parSapply
#' @importFrom parallelly availableCores
#' @importFrom dplyr rename bind_cols
#' @importFrom magrittr %>%
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' # the parameter for the calibration of the model
#' n_iter <- 2 # number of iterations
#' # the lower limits for all params (alpha_P, alpha_L, sd_coef)
#' low <- c(10, 0.000, 0.1)
#' # the upper limits for all params (alpha_P, alpha_L, sd_coef)
#' upp <- c(70, 0.3,  0.9)
#' # years in which the model should be executed
#' years <- 1990:2018
#' # execution of the calibration
#' dF_calib <- calib_green(catch_data_TN, annual_data_TN, n_iter, low, upp,
#' years)
#' }
#'
#' @export
calib_green <- function(catch_data, annual_data, n_iter, low, upp, years){

  catch_data <- check_colnames_catch(catch_data)
  annual_data <- check_colnames_annual(annual_data)

  annual_data <- append_empty_cols(annual_data)

  par_range <- data.frame(min = low, max = upp)
  latin_range <- as.data.frame(FME::Latinhyper(par_range, n_iter))

  n_cores <- parallelly::availableCores()
  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterExport(cluster, list("launch_green", "check_colnames_annual",
                                        "check_colnames_catch",
                                        "calib_green_help", "check_years",
                                        "append_empty_cols", "aggregate_loop",
                                        "data_preparation"),
                          envir = environment())
  parallel::clusterEvalQ(cluster, c(library("data.table"), library("dplyr")))
  time <- system.time(
    r <- parallel::parSapply(cluster, 1:n_iter, calib_green_help,
                             catch_data = catch_data,
                             annual_data = annual_data,
                             years = years,
                             latin_range = latin_range,
                             simplify = FALSE)
  )
  parallel::stopCluster(cluster)

  r <- do.call("cbind", r)

  print(paste("Elapsed time:", time[3]))
  data.frame(t(r))
}

#
#' @title Parallel calibration help function
#'
#' @description This is a help function for calib_green function
#'
#' @param task An integer with the number of simulation
#' @param catch_data data frame. It defines the topological sequence
#' of catchments.
#' @param annual_data data frame. It defines the sources of nutrient for each
#' year and catchments.
#' @param latin_range data frame. It defines the values for the params taking +
#' into account the limits.
#' @param years integer. It defines the years of interest.
#' @return A matrix
#'
#' @keywords internal
#'
calib_green_help <- function(task, catch_data, annual_data, years, latin_range){

  alpha_p <- as.vector(latin_range[task, 1])
  alpha_l <- as.vector(latin_range[task, 2])
  sd_c <- as.vector(latin_range[task,3])
  df_model_res <- launch_green(catch_data, annual_data, alpha_p, alpha_l,
                               sd_c, years)
  df_res_calib <- df_model_res[!is.na(df_model_res$ObsLoad), ]
  df_res_calib <- df_res_calib[order(df_res_calib$Year,
                                     df_res_calib$HydroID),]

  gof_val <- gof(sim = df_res_calib$CatchLoad,
                 obs = df_res_calib$ObsLoad,
                 digits = 4)

  S_OutLoad <- sum(df_model_res[df_model_res$To_catch== -1,
                                c(CatchToRiver)] )

  result <- rbind(as.data.frame(gof_val),
                  alpha_P = alpha_p,
                  alpha_L = alpha_l,
                  sd_coeff = sd_c,
                  SumOutLoad = S_OutLoad)

  result
}

#'
#' @title Check colnames
#'
#' @description This function checks that all column names for the catch_data
#' data frame are correct.
#'
#' @param df data frame.
#' @return A data frame
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @keywords internal
#'
check_colnames_annual <- function(df){

  header_TP <- c('BasinID', 'YearValue', 'HydroID', 'NextDownID', 'Bg', 'Min',
                 'Man', 'Sd', 'Ps', 'YearlyMass', 'ForestFraction', 'InvNrmRain')

  header_TN <- c('BasinID', 'YearValue', 'HydroID', 'NextDownID', 'Atm', 'Min',
                 'Man', 'Fix', 'Soil', 'Sd', 'Ps', 'YearlyMass',
                 'ForestFraction', 'InvNrmRain')

  if (all(header_TP %in% names(df))) {
    df <- df %>%
      dplyr::select(BasinID, YearValue, HydroID, NextDownID, Bg, Min, Man,
                    Sd, Ps, YearlyMass, ForestFraction, InvNrmRain)
    return(df)
  }

  if (all(header_TN %in% names(df))) {
    df <- df %>%
      dplyr::select(BasinID, YearValue, HydroID, NextDownID, Atm, Min, Man,
                    Fix, Soil, Sd, Ps, YearlyMass, ForestFraction, InvNrmRain)
    return(df)
  }

  stop('The names of the variables of the data frame does not match')

}

#'
#' @title Check colnames catch_data
#'
#' @description This function checks that all column names for the annual_data
#' data frame are correct.
#'
#' @param df data frame.
#' @return A data frame
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
#' @keywords internal
#'
check_colnames_catch <- function(df){

  header <- c("HydroID", "To_catch", "Shreve", "LakeFrRet", "NrmLengthKm")

  if (all(header %in% names(df))) {
    df <- df %>%
      dplyr::select(HydroID, To_catch, Shreve, LakeFrRet, NrmLengthKm)
    return(df)
  }

  stop('The names of the variables of the data frame does not match')

}

#'
#' @title Check years
#'
#' @description This function check if years are in data frame.
#'
#' @param loc_years numeric. A vector of integer with the years of interest
#' @param years numeric. A vector of integer with the years with information in
#' data frame
#' @return A vector of integer
#'
#' @keywords internal
#'
check_years <- function(loc_years, years){

  if (length(loc_years[loc_years %in% years]) != length(loc_years)) {
    warning("Some years are not in the scenario")
  }

  if (is.null(loc_years)) {
    stop("Years can not be null")
  } else {
    years <- loc_years[loc_years %in% years]
  }

  years
}

#
#' @title data_preparation
#'
#' @description Prepare data to launch green
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @return One list with two data table
#'
#' @importFrom data.table := as.data.table
#'
#' @keywords internal
#'
data_preparation <- function(catch_data, annual_data){

  catch_data <- check_colnames_catch(catch_data)
  annual_data <- check_colnames_annual(annual_data)

  annual_data <- append_empty_cols(annual_data)

  annual_datat <- data.table::as.data.table(annual_data)
  catch_datat <- data.table::as.data.table(catch_data)

  list("annual_datat" = annual_datat, "catch_datat" = catch_datat)
}

#'
#' @title Geospatial Regression Equation parallel execution returning the source
#' apportionment
#'
#' @description Run GREEN model with selected parameter set and returns the
#' nutrient load by each source for all catchments in the Basin.
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param alpha_p numeric. First model parameter, the basin retention
#' coefficient.
#' @param alpha_l numeric. Second model parameter, the river retention
#' coefficient.
#' @param sd_coef numeric. Third model parameter, fraction of domestic diffuse
#' sources that reaches the stream network.
#' @param loc_years integer. Years in which the model should be executed.
#' @return One object, a data frame with the nutrient load by each source for
#' all catchments in the Basin
#'
#' @importFrom parallel makeCluster detectCores clusterExport clusterEvalQ
#' parLapply stopCluster
#' @importFrom reshape2 dcast
#' @importFrom dplyr rename bind_rows bind_cols
#' @importFrom magrittr %>%
#' @importFrom parallelly availableCores
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' # the parameter to assess the basin model
#' alpha_p <- 35.09
#' alpha_l <- 0.02
#' sd_coef <- 0.2
#' # year in which the model should be executed
#' loc_years <- 1990:2018
#' # Computing the source apportionment
#' basin_loads_s <- green_shares(catch_data_TN, annual_data_TN, alpha_p, alpha_l,
#' sd_coef, loc_years)
#' }
#'
#' @export
#'
green_shares <- function(catch_data, annual_data, alpha_p, alpha_l, sd_coef,
                         loc_years){

  catch_data <- check_colnames_catch(catch_data)
  annual_data <- check_colnames_annual(annual_data)

  if (length(annual_data) == 12) {
    inputs <- c("Bg", "Min", "Man", "Sd", "Ps")
  } else {
    inputs <- c("Atm", "Min", "Man", "Fix", "Soil", "Sd", "Ps")
  }

  annual_data <- append_empty_cols(annual_data)

  inputs_df <- lapply(seq_len(length(inputs)), function(x){
    annual_data[inputs[-x]] <- 0
    return(annual_data)
  })
  names(inputs_df) <- inputs

  n_cores <- parallelly::availableCores()
  cluster <- parallel::makeCluster(n_cores)
  parallel::clusterExport(cluster, list("launch_green", "check_colnames_annual",
                                        "check_colnames_catch",
                                        "aggregate_loop", "check_years",
                                        "append_empty_cols",
                                        "data_preparation"),
                          envir = environment())
  parallel::clusterEvalQ(cluster, c(library("data.table"), library("dplyr")))
  results <- parallel::parLapply(cluster, inputs_df, launch_green,
                                 catch_data = catch_data,
                                 alpha_p = alpha_p,
                                 alpha_l = alpha_l,
                                 sd_coef = sd_coef,
                                 loc_years = loc_years)
  parallel::stopCluster(cluster)
  names(results) <- inputs

  results2 <- dplyr::bind_rows(results, .id = "group")
  results2_cast <- reshape2::dcast(results2[, c("group","HydroID","To_catch","Year","CatchLoad")],
                                   HydroID + To_catch + Year ~ group,
                                   value.var = "CatchLoad")

  if (length(results2_cast) == 8) {
    results2_cast$CatchLoad <- rowSums(results2_cast[, c(4:8)])
  } else if(length(results2_cast) == 10) {
    results2_cast$CatchLoad <- rowSums(results2_cast[, c(4:10)])
  }

  return(results2_cast)
}

#
#' @title GREEN execution
#'
#' @description Applies the model GREEN (Grizzetti et al.
#' [@grizzetti2012changes, @grizzetti2021eu]) to a basin and returns annual
#' nutrient (nitrogen or phosphorus) load for all catchments in the basin
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param alpha_p numeric. First model parameter, the basin retention
#' coefficient.
#' @param alpha_l numeric. Second model parameter, the river retention
#' coefficient.
#' @param sd_coef numeric. Third model parameter, fraction of domestic diffuse
#' sources that reaches the stream network.
#' @param loc_years integer. Years in which the model should be executed.
#' @param atm_coeff numeric. It defines the atmospheric attenuation coefficient.
#' @return One object, a data frame
#'
#' @importFrom data.table := setkey as.data.table
#'
#' @keywords internal
#'
launch_green <- function(catch_data, annual_data, alpha_p, alpha_l, sd_coef,
                         loc_years, atm_coeff = 0.38) {

  years <- check_years(loc_years, unique(annual_data$YearValue))

  preparation <- data_preparation(catch_data, annual_data)
  annual_datat <- preparation$annual_datat
  catch_datat <- preparation$catch_datat

  annual_datat[, AA := exp(-alpha_p * InvNrmRain)]

  annual_datat[, SumDS := (Min + Man + Fix + Soil + Atm * (1 - ForestFraction))]

  annual_datat[, SumPSCmp1 := (Ps + atm_coeff * Atm * ForestFraction +
                               sd_coef * Sd )]

  annual_datat[, CatchToRiver := (SumDS * AA + SumPSCmp1)]

  green_loadst <- annual_datat[, .(HydroID, Year = YearValue,
                                   ObsLoad = YearlyMass, CatchToRiver)]

  catch_datat[, BB := exp(-1 * alpha_l * NrmLengthKm)]

  data.table::setkey(catch_datat, "HydroID")
  data.table::setkey(green_loadst, "HydroID")
  cdat_greenLt <- catch_datat[green_loadst]

  data.table::setkey(cdat_greenLt, "Shreve")

  listaShrt <- split(cdat_greenLt[, .(HydroID, Shreve, Year)], by="Shreve")

  cdat_greenLt$CatchLoad <- 0

  type <- "L"
  cdat_total <- aggregate_loop(listaShrt, cdat_greenLt, years, type)

  cdat_total

}


#'
#' @title Nutrient balance based in the application of the Geospatial Regression
#' Equation returning the diffuse, land retention, point sources
#'
#' @description Computes the basin nutrient balance.
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param alpha_p numeric. First model parameter, the basin retention
#' coefficient.
#' @param alpha_l numeric. Second model parameter, the river retention
#' coefficient.
#' @param sd_coef numeric. Third model parameter, fraction of domestic diffuse
#' sources that reaches the stream network.
#' @param loc_years integer. Years in which the model should be executed.
#' @param atm_coeff numeric. A value for atmospheric attenuation coefficient.
#' @return One object, a data frame with the basin nutrient balance
#'
#' @importFrom data.table as.data.table setkey
#' @importFrom dplyr rename bind_cols
#' @importFrom magrittr %>%
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' # the parameter to assess the basin model
#' alpha_p <- 35.09
#' alpha_l <- 0.02
#' sd_coef <- 0.2
#' # year in which the model should be executed
#' loc_years <- 1990:2018
#' # Computing the nutrient balance
#' basin_loads_b <- region_nut_balance(catch_data_TN, annual_data_TN, alpha_p, alpha_l,
#' sd_coef, loc_years)
#' }
#'
#' @export
#'
region_nut_balance <- function(catch_data, annual_data, alpha_p, alpha_l, sd_coef,
                      loc_years, atm_coeff = 0.38) {

  years <- check_years(loc_years, unique(annual_data$YearValue))

  preparation <- data_preparation(catch_data, annual_data)
  annual_datat <- preparation$annual_datat
  catch_datat <- preparation$catch_datat

  annual_datat[, AA := exp(-alpha_p * InvNrmRain)]

  annual_datat[, Atm2DiffAgri := Atm * (1 - ForestFraction) ]
  annual_datat[, SumDS := (Min + Man + Fix + Soil + Atm2DiffAgri)]

  annual_datat[, Atm2Diff :=  atm_coeff * Atm * ForestFraction]
  annual_datat[, SD2Diff :=  sd_coef * Sd]
  annual_datat[, Diffuse := (SumDS * AA + Atm2Diff + SD2Diff )]

  annual_datat[, Atm2LandRet := (1 - atm_coeff) * Atm * ForestFraction]
  annual_datat[, SD2LandRet := ((1 - sd_coef) * Sd) ]
  annual_datat[, DiffAgri2LandRet := (SumDS * (1 - AA))]
  annual_datat[, LandRet := (DiffAgri2LandRet + Atm2LandRet + SD2LandRet)]
  annual_datat[, SumPSCmp1 := (Ps +  Atm2Diff + SD2Diff)]
  annual_datat[, CatchToRiver := (SumDS * AA + SumPSCmp1)]

  green_loadst <- annual_datat[, .(HydroID, Year = YearValue,
                                   Atm, Sd, Min, Man, Fix, Soil,
                                   PS = Ps,
                                   DiffuseAgri = SumDS,
                                   Atm2DiffAgri,
                                   Atm2Diff,
                                   SD2Diff,
                                   Diffuse,
                                   DiffAgri2LandRet,
                                   Atm2LandRet,
                                   SD2LandRet,
                                   LandRet,
                                   CatchToRiver)]

  catch_datat[, BB := exp(-1 * alpha_l * NrmLengthKm)]

  data.table::setkey(catch_datat, "HydroID")
  data.table::setkey(green_loadst, "HydroID")
  cdat_greenLt <- catch_datat[green_loadst]

  data.table::setkey(cdat_greenLt, "Shreve")

  listaShrt <- split(cdat_greenLt[, .(HydroID, Shreve, Year)],
                     by = "Shreve")

  cdat_greenLt$CatchLoad <- 0
  cdat_greenLt$CatchRivRet <- 0
  cdat_greenLt$CatchLakeRet <- 0

  type <- "N"
  cdat_total <- aggregate_loop(listaShrt, cdat_greenLt, years, type)

  cdat_total
}

#'
#' @title Read NS data
#'
#' @description Function to read the data and return the data frame for GREEN
#' execution.
#'
#' @param path string. A string with the path of the CSV files.
#' @param tsn file. A CSV file with nine variables YearValue (integer),
#' HydroID (integer), Atm (float), Min (float), Man (float), Fix (float),
#' Soil (float), Sd (float) and Ps (float).
#' @param obs file. A CSV file with three variables YearValue (integer),
#' HydroID (integer) and YearlyMass (float).
#' @param ff file. A CSV file with three variables YearValue (integer),
#' HydroID (integer) and ForestFraction (float).
#' @param rain file. A CSV file with three variables YearValue (integer),
#' HydroID (integer) and Rain (float).
#' @param topo file. A CSV file with two variables HydroID (integer) and
#' Next_HydroID (integer).
#' @param lr file. A CSV file with three variables HydroID (integer),
#' AvgDepth (float) and ResTime (float).
#' @param length file. A CSV file with two variables HydroID (integer) and
#' LengthKm (float).
#'
#' @return One object, a list with two data frame. First position of the list
#' contains the catch data and the second one the annual data.
#'
#' @importFrom utils read.csv
#'
#' @examples
#' \donttest{
#' path <- "https://raw.githubusercontent.com/calfarog/GREENeR_data/main/data/csv/"
#' ns_data <- read_NSdata(path, "TS_nutrients.csv", "Obs_monitoring.csv",
#' "ForestFr.csv", "Precipitation.csv", "Topology.csv", "LakeProperties.csv",
#' "Length.csv")
#' }
#'
#' @export
#'
read_NSdata <- function(path, tsn, obs, ff, rain, topo, lr, length) {

  tsn <- read.csv(paste0(path, tsn), T)

  if (length(tsn) == 9){
    type_SC <- "TN"
  }else{
    type_SC <- "TP"
  }

  topodat <- read.csv(paste0(path, topo), T)
  lakprop <- read.csv(paste0(path, lr), T)
  length <- read.csv(paste0(path, length), T)

  if (type_SC == "TN"){
    lakeR_par <- 7.3
  }else{
    lakeR_par <- 26
  }

  lakprop$LakeFrRet <- 1 - 1 / (1 + lakeR_par / lakprop$AvgDepth_m *
                                  lakprop$ResTime)
  lakprop$LakeFrRet[is.na(lakprop$LakeFrRet)] <- 0
  lakprop$LakeFrRet[lakprop$LakeFrRet > 0.1] <- 0.1

  lakprop$AvgDepth_m <- NULL
  lakprop$ResTime <- NULL

  length$NrmLengthKm <- length$LengthKm / max(length$LengthKm)
  length$LengthKm <- NULL

  names(topodat)[2] <- c("To_catch")

  topodat <- shreve(topodat)

  scen1 <- merge(topodat, lakprop, by = "HydroID")
  scen1 <- merge(scen1, length, by = "HydroID")

  lr <- read.csv(paste0(path, obs), T)
  ff <- read.csv(paste0(path, ff), T)
  pre <- read.csv(paste0(path, rain), T)

  minRain <- 50
  pre$InvNrmRain <- (1 / apply(cbind(minRain, pre$Rain), 1, max)) /
    (1 / minRain)

  scen2 <- merge(tsn, lr, by = c("YearValue", "HydroID"), all.x= TRUE)
  scen2 <- merge(scen2, ff, by = c("YearValue", "HydroID"),all.x= TRUE)
  scen2 <- merge(scen2, pre[, c(1,2,4)], by = c("YearValue", "HydroID"),
                 all.x= TRUE)

  scen2 <- merge(scen2, scen1[, c(1:2)], by = "HydroID")

  scen2$BasinID <- c(1234)

  if(length(scen2) == 14){
    scen2 <- scen2[, c(14,2,1,13,3,4,5,6,7,8,9,10,11,12)]
  }else{
    scen2 <- scen2[, c(12,2,1,11,3,4,5,6,7,8,9,10)]
  }

  names(scen2)[4] <- c("NextDownID")

  l_scen <- list(scen1, scen2)

  return(l_scen)

}

#'
#' @title Read geometry
#'
#' @description Function to read the geometry file.
#'
#' @param file string. A string with the name and extension of the geometry file.
#'
#' @return One object, a sf file.
#'
#' @importFrom sf st_read
#'
#' @export
#'
read_geometry<- function(file){

  the_sf_shape <- st_read(file)

  return(the_sf_shape)

}


#'
#' @title Shreve
#'
#' @description Function to read the data and return the data frame for GREEN
#' execution.
#'
#' @param the_SC table. A table with topology data.
#'
#' @return One object, a data frame with the shreve.
#'
#' @export
#'
shreve <- function(the_SC){

  the_SC$Shreve<-NA

  AllOrig <- unique(the_SC$HydroID)
  AllDesting <- unique(the_SC$To_catch)
  WholeHydroID <- unique(c(AllOrig, AllDesting))
  WholeHydroID <- setdiff(WholeHydroID, -1)
  remai_hydroId <- WholeHydroID

  shr1 <- setdiff(AllOrig, AllDesting)

  the_SC$Shreve[the_SC$HydroID %in% shr1] <- 1
  TheSrhTable <- rbind(data.frame(HydroID = shr1, shreve = 1))

  remai_hydroId <- setdiff(remai_hydroId, shr1)

  while(length(remai_hydroId)>0){

    the_Scen2 <- the_SC[the_SC$To_catch %in% remai_hydroId[], ]
    DestToRemov <- the_Scen2$To_catch[is.na(the_Scen2$Shreve)]
    the_Scen3 <- the_Scen2[!(the_Scen2$To_catch %in% DestToRemov),]

    NexShr <- aggregate(the_Scen3$Shreve, by = list(the_Scen3$To_catch), max)
    names(NexShr) <-c("HydroID ", "Shreve")

    NexShr$Shreve <- NexShr$Shreve + 1

    the_SC$Shreve[the_SC$HydroID %in% NexShr$HydroID ] <- NexShr$Shreve

    remai_hydroId <- setdiff(remai_hydroId,NexShr$HydroID)

  }

  return(the_SC)

}
