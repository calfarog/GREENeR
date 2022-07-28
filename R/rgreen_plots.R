#'
#' @title Calculate shreve
#'
#' @param catch_data data frame.
#'
#' @importFrom stats aggregate
#'
#' @return A data frame
#'
#' @keywords internal
#'
calculate_shreve <- function(catch_data) {

  catch_data$NewShreve <- NA

  all_orig <- unique(catch_data$HydroID)
  all_desting <- unique(catch_data$To_catch)
  whole_hydroID <- unique(c(AllOrig, AllDesting))
  Whole_hydroID <- setdiff(WholeHydroID, -1)
  remai_hydroId <- Whole_hydroID

  shr1 <- setdiff(AllOrig, AllDesting)
  catch_data[catch_data$Shreve == 1, "HydroID"]

  catch_data$NewShreve[catch_data$HydroID %in% shr1 ] <- 1
  the_srh_table <- rbind(data.frame(HydroID = shr1, shreve = 1))

  remai_hydroId <- setdiff(remai_hydroId, shr1)

  while (length(remai_hydroId) > 0) {

    the_scen2 <- catch_data[catch_data$To_catch %in% RemaiHydroId, ]
    dest_to_remov <- the_scen2$To_catch[is.na(the_scen2$NewShreve)]
    the_scen3 <- the_scen2[!(the_scen2$To_catch %in% dest_to_remov), ]

    nex_shr <- stats::aggregate(the_scen3$NewShreve,
                                by = list(the_scen3$To_catch), max)
    names(nex_shr) <-c("HydroID ","NewShreve")

    nex_shr$NewShreve <- nex_shr$NewShreve + 1

    catch_data$NewShreve[catch_data$HydroID %in% nex_shr$HydroID] <-
      nex_shr$NewShreve

    remai_hydroId <- setdiff(remai_hydroId, nex_shr$HydroID)
    print(paste("Length of remain HydroID", length(remai_hydroId)))

  }

  catch_data
}

#'
#' @title Boxplot of best parameters
#'
#' @description Returns boxplots of best model parameters ranked according to
#' different goodness-of-fit measures, and also boxplot with the distribution of
#'  the parameters values.
#'
#' @param df_cb data frame. Table with the result of the calibration process.
#' @param rate_bs numeric. Rate (\%) of parameters selected from the whole set
#' produced in the calibration.
#'
#' @importFrom graphics par boxplot points
#'
#' @return Multiple boxplots
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TP)
#' data(annual_data_TP)
#' # the parameter for the calibration of the model
#' n_iter <- 2 # number of iterations
#' # the lower limits for all params (alpha_P, alpha_L, sd_coef)
#' low <- c(10, 0.000, 0.1)
#' # the upper limits for all params (alpha_P, alpha_L, sd_coef)
#' upp <- c(70, 0.3,  0.9)
#' # years in which the model should be executed
#' years <- 1990:2018
#' # execution of the calibration
#' df_calib <- calib_green(catch_data_TP, annual_data_TP, n_iter, low, upp,
#' years)
#' # Generating the box plots
#' rateBS <- 5 # rate of best set of parameter to include in the plots
#' calib_boxplot(df_calib, rateBS)
#' }
#'
#' @export
#'
calib_boxplot <- function(df_cb, rate_bs) {


  num_best <- nrow(df_cb) * rate_bs / 100

  bes_par <- NULL
  top_best <- NULL
  ind_mtr <- c(6, 15, 9, 10, 11, 17)

  for (ind in ind_mtr) {
    if (ind == 6) {
      irow <- which(abs(df_cb[, ind]) ==  min(abs(df_cb[, ind]),
                                              na.rm = TRUE))[1]
    } else if (ind != 6) {
      irow <- which(df_cb[,ind] == max(df_cb[, ind], na.rm = TRUE))[1]
    }
    bes_par <- rbind(bes_par, data.frame(metric = names(df_cb)[ind],
                                         df_cb[irow, ]))

    if (ind == 6) {
      top_best <-
        rbind(top_best,
              data.frame(metric = names(df_cb)[ind],
                         df_cb[order(abs(df_cb[, ind]))[1:num_best], ]))

    } else if (ind != 6) {
      top_best <-
        rbind(top_best,
              data.frame(metric = names(df_cb)[ind],
                         df_cb[order(-df_cb[ind])[1:num_best] ,]))
    }

  }

  unique(bes_par$metric)

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  graphics::par(mfrow = c(3, 3), mar = c(4.1, 4.1, 2.1, 2.1))
  graphics::boxplot(top_best$cp ~ top_best$metric,
                    las = 2,
                    main = "cp",
                    xlab = "",
                    ylab = "")
  graphics::boxplot(top_best$mNSE ~ top_best$metric,
                    las = 2,
                    main = "mNSE",
                    xlab = "",
                    ylab = "")
  graphics::boxplot(top_best$NSE ~ top_best$metric,
                    las = 2,
                    main = "NSE",
                    xlab = "",
                    ylab = "")
  graphics::boxplot(top_best$rNSE ~ top_best$metric,
                    las = 2,
                    main = "rNSE",
                    xlab = "",
                    ylab = "")
  graphics::boxplot(top_best$R2 ~ top_best$metric,
                    las = 2,
                    main = "R2",
                    xlab = "",
                    ylab = "")
  graphics::boxplot(top_best$PBIAS.. ~ top_best$metric,
                    las = 2,
                    main ="PBIAS",
                    xlab = "",
                    ylab = "")

  bes_par <- bes_par[order(bes_par$metric), ]
  graphics::boxplot(top_best$alpha_P ~ top_best$metric,
                    las = 2,
                    main = "alpha_P",
                    xlab = "",
                    ylab = "")
  graphics::points(bes_par["alpha_P"], col = "red", pch = 18)
  graphics::boxplot(top_best$alpha_L ~ top_best$metric,
                    las = 2,
                    main = "alpha_L",
                    xlab = "",
                    ylab = "")
  graphics::points(bes_par["alpha_L"], col = "red",pch = 18)
  graphics::boxplot(top_best$sd_coeff ~ top_best$metric,
                    las = 2,
                    main = "sd_coeff",
                    xlab = "",
                    ylab = "")
  graphics::points(bes_par["sd_coeff"], col = "red", pch = 18)

}

#'
#' @title Dot plot of goodness-of-fit metric vs parameter value
#'
#' @description Dot plot of goodness-of-fit metric vs parameters value
#'
#' @param df_cb data frame. A table with the result of the calibration process.
#' @param par character. Goodness of fit measures. See alternatives link "NSE"
#' "rNSE", "NSE", "mNSE", "MAE", "PBIAS", "cp", "R2".
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom classInt classIntervals
#' @importFrom graphics par layout plot legend
#'
#' @return Multiple dot plots
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
#' df_calib <- calib_green(catch_data_TN, annual_data_TN, n_iter, low, upp,
#' years)
#' # Generating the dot plots
#' gof_mes <- "NSE"
#' calib_dot(df_calib, gof_mes)
#' }
#'
#' @export
#'
calib_dot <- function(df_cb, par) {

  rbPal <- grDevices::colorRampPalette(
    c('red4', 'red', 'orange', 'green', "green4", "cyan1"))

  intervals <- classInt::classIntervals(df_cb[, par],
                                        n = min(nrow(df_cb), 6),
                                        style = "quantile",
                                        warnSmallN = FALSE)
  cuts <- cut(df_cb[, par], intervals$brks, labels = FALSE,
              include.lowest = TRUE)
  df_cb$colors <- rbPal(min(nrow(df_cb), 6))[as.numeric(cuts)]

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  graphics::par(mar = c(4.1, 4.1, 2.1, 2.1))

  graphics::layout(mat = matrix(c(1, 2, 3, 4),
                                nrow = 2,
                                ncol = 2))


  with(df_cb, {
    graphics::plot(alpha_P, alpha_L, col = df_cb$colors,
                   xlab = "alpha_P", ylab = "alpha_L", pch = 20)
  })

  with(df_cb, {
    graphics::plot(alpha_P, sd_coeff, col = df_cb$colors,
                   xlab = "alpha_P", ylab = "sd_coeff", pch = 20)
  })

  with(df_cb, {
    graphics::plot(sd_coeff, alpha_L, col = df_cb$colors,
                   xlab = "sd_coeff", ylab = "alpha_L", pch = 20)
  })

  DF_leg <- data.frame(
    cbind(lower = round(intervals$brks[1:(length(intervals$brks) - 1)],
                        digits = 2),
          upper = round(intervals$brks[2:length(intervals$brks)], digits = 2)))

  labels <- paste0("(" , DF_leg$lower," , " , DF_leg$upper, "]")
  graphics::plot(NULL, xaxt = 'n', yaxt = 'n', bty = 'n',
                 ylab = '', xlab = '', xlim = 0:1, ylim = 0:1)
  graphics::legend("center", title = par, legend = labels,
                   col = rbPal(min(nrow(df_cb), 6)),
                   pch = 16, bty = "n", cex = 1.5)

}

#'
#' @title Plot comparing observed vs modeled loads for two set of parameters
#'
#' @description Returns a scatter plot comparing observed versus modeled loads
#' obtained with two model parameter sets
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param alpha_p1 numeric. The basin retention coefficient of the first set of
#' parameters.
#' @param alpha_l1 numeric. The river retention coefficient of the first set of
#' parameters.
#' @param sd_coef1 numeric. Fraction of domestic diffuse sources that reaches
#' the stream network of the first set of parameters.
#' @param alpha_p2 numeric. The basin retention coefficient of the second set of
#' parameters.
#' @param alpha_l2 numeric. The river retention coefficient of the second set of
#' parameters.
#' @param sd_coef2 numeric. Fraction of domestic diffuse sources that reaches
#' the stream network of the second set of parameters.
#' @param name_basin character. Name of the basin (title of the plot).
#' @param years numeric. Years to be shown in the plot.
#' @param setPlabels character. Labels identifying each set of parameter.
#'
#' @importFrom hydroGOF gof
#' @importFrom graphics par plot abline legend
#'
#' @return A scatter plot and a list with two data frames with model GREEN
#' applied to two model parameter sets
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#'
#' # the first set of parameters to assess the basin model
#' alpha_p <- 35.09
#' alpha_l <- 0.02
#' sd_coef <- 0.2
#'
#' # the second set of parameters to assess the basin model
#' alpha_p2 <- 41.23
#' alpha_l2 <- 0.0015
#' sd_coef2 <- 0.6
#'
#' # years in which the plot will we shown
#' years <- 1990:2018
#'
#' nameBasin <- "Lay"
#'
#' # generating the scatter plot comparing two set of parameters observed
#' # versus modeled loads by year
#' setPlabels <- c("bestNSE","bestR2")
#' compare_calib(catch_data_TN, annual_data_TN, alpha_p , alpha_l, sd_coef,
#' alpha_p2, alpha_l2, sd_coef2, years, nameBasin, setPlabels)
#' }
#'
#' @export
#'
compare_calib <- function(catch_data, annual_data, alpha_p1, alpha_l1,
                          sd_coef1, alpha_p2, alpha_l2, sd_coef2, years,
                          name_basin, setPlabels) {

  df_scen_global <- launch_green(catch_data, annual_data, alpha_p1, alpha_l1,
                                 sd_coef1, years)
  df_scen_local <- launch_green(catch_data, annual_data, alpha_p2, alpha_l2,
                                sd_coef2, years)

  df_gl <- df_scen_global[df_scen_global$To_catch == -1,]
  df_lo <- df_scen_local[df_scen_local$To_catch == -1,]

  df_observ_g <- df_scen_global[!is.na(df_scen_global$ObsLoad), ]
  df_observ_l <- df_scen_local[!is.na(df_scen_local$ObsLoad), ]

  df_observ_g <- df_observ_g %>%
    rename(PredictLoad = CatchLoad)

  gof_glo <- hydroGOF::gof(sim = df_observ_g$PredictLoad,
                           obs = df_observ_g$ObsLoad,
                           digits = 4)
  gof_loc <- hydroGOF::gof(sim = df_observ_l$CatchLoad,
                           obs = df_observ_l$ObsLoad,
                           digits = 4)
  result <- cbind(as.data.frame(gof_glo), as.data.frame(gof_loc))
  result$index <- rownames(result)
  result <- result[, c(3, 1, 2)]
  result[, 2:3] <- round(result[, 2:3], 3)
  names(result) <-c("index", setPlabels[1], setPlabels[2])
  result$gl <- apply( result , 1 , paste , collapse = "  " )

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  graphics::par(mfrow = c(1, 1))
  graphics::plot(df_observ_g$ObsLoad,
                 df_observ_g$PredictLoad,
                 main = name_basin,
                 xlab = "Observed",
                 ylab = "Predicted",
                 xlim = c(0, max(df_observ_g$ObsLoad, df_observ_l$ObsLoad)),
                 ylim = c(0, max(df_observ_g$PredictLoad,
                                 df_observ_l$CatchLoad)))
  graphics::points(df_observ_l$ObsLoad,
                   df_observ_l$CatchLoad,
                   col = "red")
  graphics::abline(coef = c(0, 1))
  graphics::legend('bottomright', legend = c(setPlabels[1], setPlabels[2]) ,
                   col=c("black","red"), bty = 'n', pch = 15, cex = 1.3)
  graphics::legend('topleft', legend = result$gl[c(6, 9, 10, 11, 15, 17)],
                   col = "black", bty = 'n', cex = 1.0,
                   title = paste0("       ", setPlabels[1], "      ", setPlabels[2]))


  list(df_gl ,df_lo)
}

#
#' @title Create levels
#'
#' @description This function create the levels for the data frame.
#'
#' @param df,no_sf data frame.
#'
#' @return A list
#'
#' @importFrom stats quantile
#'
#' @keywords internal
#'
create_levels <- function(df, no_sf){

  df <- merge(df, no_sf, by = "HydroID")

  theQuantile <- stats::quantile(df$Shreve)
  ShrLevel1 <- df[df$Shreve <= theQuantile[3], ]$HydroID
  ShrLevel2 <- df[df$Shreve > theQuantile[3] &
                    df$Shreve <= theQuantile[4], ]$HydroID
  ShrLevel3 <- df[df$Shreve > theQuantile[4] &
                    df$Shreve <= theQuantile[5], ]$HydroID

  df$shrLevel <- NA
  if(!is.logical(df$shrLevel)) {
    df$shrLevel <- as.character(df$shrLevel)
  }
  df[df$HydroID %in% ShrLevel1, ]$shrLevel <- "level1"
  df[df$HydroID %in% ShrLevel2, ]$shrLevel <- "level2"
  df[df$HydroID %in% ShrLevel3, ]$shrLevel <- "level3"

  list("df" = df, "ShrLevel1" = ShrLevel1, "ShrLevel2" = ShrLevel2,
       "ShrLevel3" = ShrLevel3)
}

#
#' @title Preprocessing data for scenario summary
#'
#' @description This function blah, blah, blah....
#'
#' @param hydroSf data frame.
#' @param long_basin numeric. The longitude of the basin.
#' @param unit character. The unit of the data.
#' @param param_list list. A list with the maps to plot.
#' @param style character. The style of the plot.
#' @param legend_position numeric. It indicates de position of the legend of the
#' plot. (Default: 1)
#' @return A list with maps
#'
#' @keywords internal
#'
create_lits_of_maps <- function(hydroSf, long_basin, unit, legend_position,
                                param_list, style){

  scale_barRef <- c(0, floor(long_basin / 8), floor(long_basin / 4))
  scale_barTextS <- 0.7

  map_list <- list()
  for (i in param_list){
    map <- create_map(hydroSf, i[1], style, scale_barRef, scale_barTextS,
                      paste(i[2], unit), palette = i[3],
                      legend_position = legend_position)
    map_list <- c(map_list, list(map))
  }

  map_list

}

#
#' @title Create map
#'
#' @description This function create a tm map.
#'
#' @param hydroSf data frame.
#' @param var_name character. The name of a data variable that it is contains
#' in data frame
#' @param style_map,scale_barRefs,scale_barTexts numeric.
#' @param title character. The title of the plot
#' @param palette character. The colors of the map
#' @param legend_position numeric. It indicates de position of the legend of the
#' plot. (Default: 1)
#'
#' @return A plot with map of the basin
#'
#' @importFrom tmap tm_shape tm_fill tm_layout tm_scale_bar
#'
#' @keywords internal
#'
create_map <- function(hydroSf, var_name, style_map, scale_barRefs,
                       scale_barTexts, title, palette, legend_position = 1){

  tm <- tmap::tm_shape(hydroSf) +
    tmap::tm_fill(col = var_name,
                  style = style_map,
                  title = title,
                  palette = palette) +
    tmap::tm_scale_bar(breaks = scale_barRefs,
                       text.size = scale_barTexts)

  if (legend_position == 1){
    position <- c("left", "bottom")
  } else if (legend_position == 2) {
    position <- c("left", "top")
  } else if(legend_position == 3) {
    position <- c("right", "bottom")
  } else if(legend_position == 4) {
    position <- c("right", "top")
  } else {
    stop("Invalid value for legend position.")
  }

  tm <- tm + tmap::tm_layout(legend.position = position,
                             legend.format = list(digits = 1))

  tm
}

#
#' @title Evolution plot
#'
#' @description This function plot the evolution of the data
#'
#' @param data data frame.
#' @param title character. The plot title
#' @param xaxis.title character. The title of x axis
#' @param yaxis.title character. The title of y axis
#' @param x character. The name of variable in data to represent in x axis
#' @param y character. The name of variable in data to represent in y axis
#' @param colour character. The name of variable in data to represent colour
#' @param wrap character. The name of variable in data to represent wrap
#' @return No return value, called for the side effect of drawing a plot
#'
#' @importFrom ggplot2 ggplot aes_string geom_line theme_bw labs
#' scale_color_brewer facet_wrap
#' @importFrom stats as.formula
#'
#' @keywords internal
#'
evolution_plot <- function(data, title = NULL, xaxis.title = NULL, yaxis.title,
                           x, y, colour = NULL, wrap = NULL){
  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes_string(x = x, y = y, colour = colour)) +
    ggplot2::geom_line(alpha = 1.5, size = 1.2) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::labs(title = title, y = yaxis.title, x = xaxis.title)  +
    ggplot2::scale_color_brewer(palette = "Set1", name = "") +
    ggplot2::theme(legend.position = "bottom", legend.direction = "horizontal")

  if (is.null(xaxis.title))
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x  = ggplot2::element_blank())

  if (!is.null(wrap))
    p <- p + ggplot2::facet_wrap(stats::as.formula(paste("~", wrap)))

  p

}


#
#' @title Evolution plot area
#'
#' @description This function blah, blah, blah....
#'
#' @param annual_data,data data frame.
#' @param title character. The plot title
#' @param xaxis.title character. The title of x axis
#' @param yaxis.title character. The title of y axis
#' @param x character. The name of variable in data to represent in x axis
#' @param y character. The name of variable in data to represent in y axis
#' @param colour character. The name of variable in data to represent colour
#' @param wrap character. The name of variable in data to represent wrap
#' @return A plot
#'
#' @importFrom ggplot2 ggplot aes_string geom_area theme_bw labs
#' scale_fill_brewer theme element_blank
#'
#' @keywords internal
#'
evolution_plot_area <- function(annual_data, data, title = NULL,
                                xaxis.title = NULL, yaxis.title, x, y,
                                colour = NULL, wrap = NULL){

  ymin <- min(annual_data$YearValue)
  ymax <- max(annual_data$YearValue)
  p <- ggplot2::ggplot(data = data,
                       ggplot2::aes_string(x = x, y = y, fill = colour)) +
    ggplot2::geom_area(alpha = 1.5, size = 1.2) +
    ggplot2::theme_bw(base_size = 15) +
    ggplot2::labs(title = title, y = yaxis.title, x = xaxis.title) +
    ggplot2::theme(legend.position = "bottom",
                   legend.text = element_text(size = 12),
                   legend.margin = margin(t = -0.5, unit = 'cm') ) +
    ggplot2::guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = "")) +
    ggplot2::scale_x_continuous(breaks = seq(ymin, ymax, by = 2))

  if (length(annual_data) == 17) {
    p <- p + ggplot2::scale_fill_brewer(palette = "Set1", name = "")
  } else if(length(annual_data) == 15) {
    p <- p + ggplot2::scale_fill_brewer(palette = "Dark2", name = "")
  }

  if (is.null(xaxis.title))
    p <- p + ggplot2::theme(axis.title.x = ggplot2::element_blank(),
                            axis.text.x  = ggplot2::element_blank())

  p

}

#
#' @title Density plot
#'
#' @description This function blah, blah, blah....
#'
#' @param df_plot data frame.
#' @param plot_index numeric. The indexes to plot
#' @param basin_name character. The name of the basin
#' @param cSD numeric. The standard deviation
#' @return No return value, called for the side effect of drawing a plot
#'
#' @importFrom graphics legend plot lines
#' @importFrom stats density sd
#'
#' @keywords internal
#'
gr_density_plot <- function(df_plot, plot_index, basin_name, cSD) {

  index1 <- length(df_plot)

  df <- df_plot[, c(plot_index)] / df_plot[, index1]
  table <- sapply(df, function(x) c("Stand dev" = sd(x),
                                    "Mean"= mean(x,na.rm = TRUE) ,
                                    "xlim"= mean(x,na.rm = TRUE) + cSD * sd(x),
                                    "ylim"= max(density(x)$y)))

  xlim <- c(0, max(table[c("xlim"), ]))
  ylim <- c(0, max(table[c("ylim"), ]))

  plot(1,
       type = "n",
       xlab = expression("t/y/" ~ km^2 ~ "" ),
       ylab = "Density",
       cex.lab = 1.5,
       las = 1,
       xlim = xlim,
       ylim = ylim,
       main = basin_name)
  colors <- c("blue","green","red")


  for (i in seq_len(length(plot_index))) {
    lines(density(df[, i]), col = colors[i])
  }

  legend("topright",
         legend = names(df),
         lty = 1,
         col = colors,
         bty = "n",
         cex = 1.5)

}

#
#' @title Map average load input by source
#'
#' @description Map showing the mean load input by source
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param sh_file sf object. The spatial information.
#' @param basin_name character. The title of the map
#' @param plot.type character. Alternatives of the map: input load (kt) by type
#' divided by year and catchment. “gr1”: by km2; “gr2”: by year/km2.
#' @param style_map character. Alternatives to create the intervals in the maps.
#' Chosen style: one of "fixed", "sd", "equal", "pretty", "quantile", "kmeans",
#' "hclust", "bclust", "fisher", "jenks".
#' @param scale_barTextS numeric. To modify the size of the text in the legend.
#' @param legend_position numeric. Legend position: 1 (default): "right",
#' "bottom"; 2: "left", "up"; 3: "right", "bottom"; 4: "right", "up".
#'
#' @return No return value, called for the side effect of drawing a plot
#'
#' @importFrom sf st_area
#' @importFrom tidyselect everything
#' @importFrom dplyr group_by summarise across
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom ggplot2 scale_x_continuous facet_wrap
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' data(sh_file)
#' # The title of the plot
#' mapTitle <- "Time series for the Lay Basin"
#' # the Input Load Map by source type 1 (lines)
#' input_maps(catch_data_TN, annual_data_TN, sh_file, mapTitle, "gr1",
#' legend_position = 2)
#' # the Input Load Map by source type 2 (lines & area)
#' input_maps(catch_data_TN, annual_data_TN, sh_file, mapTitle, "gr2",
#' legend_position = 2)
#' }
#'
#' @export
#'
input_maps <- function(catch_data, annual_data, sh_file, basin_name, plot.type,
                       style_map = "fisher", scale_barTextS = 0.7,
                       legend_position = 1){

  sh_file$AreaSkm <- sf::st_area(sh_file) / 1000000
  sh_file$AreaSkm <- as.numeric(sh_file$AreaSkm)
  basin_area <- as.numeric(sum(sf::st_area(sh_file)) / 1000000)
  long_basin <- sqrt(basin_area)

  df_no_sf <- as.data.frame(sh_file)
  df_no_sf <- df_no_sf[, c("HydroID", "DrainAreaS", "AreaSkm" )]
  annual_data_aux <- merge(annual_data, df_no_sf, by = "HydroID")

  index1 <- length(annual_data_aux) - 5
  index2 <- length(annual_data_aux) - 7
  index3 <- length(annual_data_aux) - 6
  index4 <- length(annual_data_aux)

  switch (
    plot.type,
    "gr1" = {
      df_load_hydro <- annual_data_aux[, c(1, 3, 5:index1)] %>%
        dplyr::group_by(HydroID) %>%
        dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

      df_load_hydro <- data.frame(df_load_hydro)

      df_load_hydro$TOTAL <- rowSums(df_load_hydro[,c(3:index2)])

      if(length(annual_data_aux) == 14){
        names(df_load_hydro)<- c("HydroID", "Year", "Bg", "Min", "Man",
                                 "Sd", "PS", "TOTAL")
      } else if(length(annual_data_aux) == 16){
        names(df_load_hydro)<- c("HydroID", "Year", "Atm", "Min", "Man",
                                 "Fix", "Soil", "Sd", "Ps", "TOTAL")
      }

      hydroSf_merge <- merge(sh_file, df_load_hydro[,c(1,3:index3)],
                             by = "HydroID")

      print(multiple_map(hydroSf_merge,
                         length(annual_data_aux),
                         long_basin,
                         unit = "kt/y",
                         legend_position = legend_position))
    },
    "gr2" = {
      df_temp <- annual_data_aux[, c(1, 3, 5:index1, index4)]
      df_temp[, 3:index2] <- df_temp[, 3:index2] / df_temp[, c("AreaSkm")]

      df_load_hydroSkm <- df_temp[, 1:index2] %>%
        dplyr::group_by(HydroID) %>%
        dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

      df_load_hydroSkm  <- data.frame(df_load_hydroSkm)
      df_load_hydroSkm$TOTAL <- rowSums(df_load_hydroSkm[, 3:index2])

      if (length(annual_data_aux) == 14){
        names(df_load_hydroSkm)<- c("HydroID", "Year", "Bg", "Min", "Man",
                                    "Sd", "PS", "TOTAL")
      } else if(length(annual_data_aux) == 16){
        names(df_load_hydroSkm)<- c("HydroID", "Year", "Atm", "Min", "Man",
                                    "Fix", "Soil", "Sd", "Ps", "TOTAL")
      }

      sf_mergeSkm <- merge(sh_file, df_load_hydroSkm[,c(1,3:index3)],
                           by = "HydroID")
      print(multiple_map(sf_mergeSkm, length(annual_data_aux), long_basin,
                         unit = "kt/y/km2", legend_position = legend_position))
    },
    {
      stop("Variable type.plot should be 'gr1' or 'gr2'")
    }
  )

}

#
#' @title Plot input load by source
#'
#' @description A grouped barplot representing the average input load by source
#' for the whole basin or a three density plots showing the distribution of
#' nutrient sources (7 for nitrogen, 5 for phosphorous).
#'
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param sh_file sf object. The spatial information.
#' @param basin_name character. The title of the plot.
#' @param plot.type character. Possible values: Bar plot ("B") or
#' Density plot (“D”).
#' @param coef_SD numeric. The standard deviation coefficient.
#' @return No return value, called for the side effect of drawing a plot
#'
#' @importFrom sf st_area
#' @importFrom graphics par barplot
#' @import dplyr
#' @import magrittr
#'
#' @examples
#' # the data of the TN scenario
#' data(annual_data_TN)
#' data(sh_file)
#' # The name of the basin
#' basin_name <- "Lay"
#' # the barplot
#' input_plot(annual_data_TN, sh_file, basin_name, "B")
#' # the density plots
#' input_plot(annual_data_TN, sh_file, basin_name, "D")
#'
#' @export
#'
input_plot <- function(annual_data, sh_file, basin_name, plot.type,
                       coef_SD = 1.0){

  sh_file$AreaSkm <- sf::st_area(sh_file) / 1000000
  sh_file$AreaSkm <- as.numeric(sh_file$AreaSkm)

  df_no_sf <- as.data.frame(sh_file)
  df_no_sf <- df_no_sf[, c("HydroID","DrainAreaS","AreaSkm" )]
  annual_data <- merge(annual_data, df_no_sf, by = "HydroID")

  if (plot.type == "D") {
    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    graphics::par(mfrow=c(1, 3))
    gr_density_plot(annual_data, c(7, 6), basin_name, coef_SD)
    indexD1 <- c(length(annual_data) - 6, length(annual_data) - 5)
    gr_density_plot(annual_data, indexD1, basin_name, coef_SD)

    if (length(annual_data) == 16) {
      gr_density_plot(annual_data, c(5, 8, 9), basin_name, coef_SD)
    }
  } else if (plot.type == "B") {

    index1 <- length(annual_data) - 5
    index2 <- length(annual_data) - 8

    df_load_tot_type_year <- annual_data[, c(3, 5:index1)] %>%
      dplyr::group_by(YearValue) %>%
      dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

    df_load_tot_type_year <- data.frame(df_load_tot_type_year)

    LoadAvg <- colMeans(df_load_tot_type_year[, c(2:index2)])

    if (length(annual_data) == 14) {
      names(LoadAvg)<- c("Bg","Min","Man","Sd","PS")
    } else if (length(annual_data) == 16) {
      names(LoadAvg)<- c("Atm","Min","Man","Fix","Soil","Sd","PS")
    }

    oldpar <- par(no.readonly = TRUE)
    on.exit(par(oldpar))
    graphics::par(mfrow=c(1, 1))
    graphics::barplot(LoadAvg / 1000,
                      col = grDevices::rainbow(7),
                      border = "white",
                      font.axis = 2,
                      beside = TRUE,
                      legend = rownames(LoadAvg),
                      xlab = "Inputs",
                      ylab = " Mean Anual Input by Source (kt/y)",
                      font.lab = 2,
                      las = 1)
  } else {
    stop("Variable plot.type must be 'D' for density plot or 'B' for barplot.")
  }

}

#
#' @title Time series of annual load inputs by source
#'
#' @description Creates a time series plot showing basin inputs by
#' source
#'
#' @param catch_data data frame. Definition of the topological sequence of
#' catchments.
#' @param annual_data data frame. Sources of nutrient for each year and
#' catchments.
#' @param sh_file sf object. The spatial information.
#' @param basin_name character. The title of the plot
#' @param plot.type character. Alternative of the plot: “gr1”: stacked area;
#' “gr2”: lines & area; “gr3”: by km2; “gr4” by km2 and Shreve.
#' @return A time-series plot
#'
#' @importFrom sf st_area
#' @importFrom stats quantile
#' @importFrom tidyselect everything
#' @importFrom dplyr group_by summarise across select
#' @importFrom magrittr %>%
#' @importFrom reshape2 melt
#' @importFrom ggplot2 scale_x_continuous facet_wrap
#' @importFrom gridExtra grid.arrange
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' data(sh_file)
#' # The title of the plot
#' plotTitle <- "Time series for the Lay Basin"
#' # the time serie plot 1 (lines)
#' input_Tserie(catch_data_TN, annual_data_TN, sh_file, plotTitle, "gr1")
#' # the time serie plot 2 (lines & area)
#' input_Tserie(catch_data_TN, annual_data_TN, sh_file, plotTitle, "gr2")
#' # the time serie plot 3 (by km2)
#' input_Tserie(catch_data_TN, annual_data_TN, sh_file, plotTitle, "gr3")
#' # the time serie plot 4 (by km2 and Shreve)
#' input_Tserie(catch_data_TN, annual_data_TN, sh_file, plotTitle, "gr4")
#' }
#'
#' @export
input_Tserie <- function(catch_data, annual_data, sh_file, basin_name,
                         plot.type){

  sh_file$AreaSkm <- sf::st_area(sh_file) / 1000000
  sh_file$AreaSkm <- as.numeric(sh_file$AreaSkm)
  basin_area <- as.numeric(sum(sf::st_area(sh_file)) / 1000000)

  df_no_sf <- as.data.frame(sh_file)
  df_no_sf <- df_no_sf[, c("HydroID", "DrainAreaS", "AreaSkm")]

  annual_data <- merge(annual_data, df_no_sf, by = "HydroID")

  theQuantile <- stats::quantile(catch_data$Shreve)
  ShrLevel1 <- catch_data[catch_data$Shreve <= theQuantile[3], ]$HydroID
  ShrLevel2 <- catch_data[catch_data$Shreve > theQuantile[3] &
                            catch_data$Shreve <= theQuantile[4], ]$HydroID
  ShrLevel3 <- catch_data[catch_data$Shreve > theQuantile[4] &
                            catch_data$Shreve <= theQuantile[5], ]$HydroID

  annual_data$shrLevel <- NA
  annual_data[annual_data$HydroID %in% ShrLevel1, ]$shrLevel <- "level1"
  annual_data[annual_data$HydroID %in% ShrLevel2, ]$shrLevel <- "level2"
  annual_data[annual_data$HydroID %in% ShrLevel3, ]$shrLevel <- "level3"

  sh_sr1 <- sh_file[sh_file$HydroID %in% ShrLevel1, ]
  sh_sr2 <- sh_file[sh_file$HydroID %in% ShrLevel2, ]
  sh_sr3 <- sh_file[sh_file$HydroID %in% ShrLevel3, ]

  sh_sr1Area <- as.numeric(sum(sf::st_area(sh_sr1)) / 1000000)
  sh_sr2Area <- as.numeric(sum(sf::st_area(sh_sr2)) / 1000000)
  sh_sr3Area <- as.numeric(sum(sf::st_area(sh_sr3)) / 1000000)

  index1 <- length(annual_data) - 6
  index2 <- length(annual_data) - 9
  index3 <- length(annual_data)
  index4 <- index2 + 1

  df_load_tot_type_year <- annual_data[, c(3, 5:index1)] %>%
    dplyr::group_by(YearValue) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

  df_load_tot_type_year <- data.frame(df_load_tot_type_year)

  if (length(annual_data) == 15) {
    names(df_load_tot_type_year)<- c("Year", "Bg", "Min", "Man", "Sd", "PS")
  } else if (length(annual_data) == 17) {
    names(df_load_tot_type_year)<- c("Year", "Atm", "Min", "Man", "Fix", "Soil",
                                     "Sd", "Ps")
  }

  ymin <- min(df_load_tot_type_year$Year)
  ymax <- max(df_load_tot_type_year$Year)

  switch (
    plot.type,
    "gr1" = {
      df_load_tot_type_year_melt <- reshape2::melt(df_load_tot_type_year,
                                                   id.vars = "Year")

      df_load_tot_type_year_melt$value <- df_load_tot_type_year_melt$value / 1000
      p1 <- evolution_plot_area(annual_data = annual_data,
                                data = df_load_tot_type_year_melt,
                                title = paste0(basin_name," Input Time Series"),
                                yaxis.title = "Input by Source (kt/y)",
                                xaxis.title = "Year",
                                x = "Year",
                                y = "value",
                                colour = "variable")
      print(p1)
    },
    "gr2" = {
      df_load_tot_type_year_melt <- reshape2::melt(df_load_tot_type_year,
                                                   id.vars = "Year")
      p1 <-  evolution_plot(data = df_load_tot_type_year_melt,
                            yaxis.title = "Input by Source (kt/y)",
                            xaxis.title = "Year",
                            x = "Year",
                            y = "value",
                            colour = "variable")
      p1 <- p1 +
        ggplot2::theme(legend.position = "bottom",
                       legend.text = element_text(size = 12),
                       legend.margin = margin(t = -0.5, unit = 'cm') ) +
        ggplot2::guides(colour = guide_legend(nrow = 1,
                                              byrow = TRUE,
                                              title = "")) +
        ggplot2::scale_x_continuous(breaks = seq(ymin, ymax, by = 2))

      df_load_tot_type_year$Total <- rowSums(df_load_tot_type_year[, 2:index2] )
      df_load_tot_type_year_aux <- df_load_tot_type_year %>%
        dplyr::select(Year, Total)

      p2 <-  evolution_plot(data = df_load_tot_type_year_aux,
                            title = paste0(basin_name," Input Time Series"),
                            yaxis.title = "Total Input (kt/y)",
                            xaxis.title = "Year", x = "Year", y = "Total")
      p2 <- p2 +
        ggplot2::scale_x_continuous(breaks = seq(ymin, ymax, by = 2))

      print(gridExtra::grid.arrange(p2, p1, nrow = 2))
    },
    "gr3" = {
      df_load_km2_type_year <- df_load_tot_type_year
      df_load_km2_type_year[, 2:index2] <-
        df_load_km2_type_year[, 2:index2] / basin_area

      df_load_avg_type_year_melt <- reshape2::melt(df_load_km2_type_year,
                                                   id.vars = "Year")

      p1 <- evolution_plot_area(annual_data = annual_data,
                                data = df_load_avg_type_year_melt,
                                title = paste0(basin_name,
                                               " Annual Input Time Series"),
                                yaxis.title = "Input by source (t/y/km2)",
                                xaxis.title = "Year",
                                x = "Year",
                                y = "value",
                                colour = "variable") +
        ggplot2::scale_x_continuous(breaks = seq(ymin, ymax, by = 2))

      print(p1)
    },
    "gr4" = {
      df_load_km2_type_shr_year <- annual_data[, c(3, index3, 5:index1)] %>%
        dplyr::group_by(YearValue, shrLevel) %>%
        dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)),
                         .groups = "rowwise")

      df_load_km2_type_shr_year <- data.frame(df_load_km2_type_shr_year)

      if (length(annual_data) == 15){
        names(df_load_km2_type_shr_year) <- c("Year", "shrLevel", "Bg", "Min",
                                              "Man", "Sd", "PS")
      } else if (length(annual_data) == 17) {
        names(df_load_km2_type_shr_year) <- c("Year", "shrLevel", "Atm", "Min",
                                              "Man", "Fix", "Soil", "Sd", "PS")
      }

      df_load_km2_type_shr_year[
        df_load_km2_type_shr_year$shrLevel == "level1", c(3:index4)] <-
        df_load_km2_type_shr_year[
          df_load_km2_type_shr_year$shrLevel == "level1",
          c(3:index4)] / sh_sr1Area

      df_load_km2_type_shr_year[
        df_load_km2_type_shr_year$shrLevel == "level2", c(3:index4)] <-
        df_load_km2_type_shr_year[
          df_load_km2_type_shr_year$shrLevel == "level2", c
          (3:index4)] / sh_sr2Area

      df_load_km2_type_shr_year[
        df_load_km2_type_shr_year$shrLevel == "level3", c(3:index4)] <-
        df_load_km2_type_shr_year[
          df_load_km2_type_shr_year$shrLevel == "level3",
          c(3:index4)] / sh_sr3Area

      df_load_avg_type_shr_year_melt <-
        reshape2::melt(df_load_km2_type_shr_year,
                       id.vars = c("Year","shrLevel"))

      df_load_avg_type_shr_year_melt$etiquetas <-
        factor(df_load_avg_type_shr_year_melt$shrLevel,
               labels = c(paste0("Upper Part= " ,  floor(sh_sr1Area)," km2"),
                          paste0("Middle Part= ", floor(sh_sr2Area)," km2"),
                          paste0("Lower Part= " ,  floor(sh_sr3Area)," km2") ))

      p1 <- evolution_plot(data = df_load_avg_type_shr_year_melt,
                           title = paste0(basin_name," Time Series of Loads"),
                           yaxis.title = "Input by source (t/y/km2)",
                           xaxis.title = "Year",
                           x = "Year",
                           y = "value",
                           colour = "variable") +
        ggplot2::theme(legend.position = "bottom",
                       legend.text = element_text(size = 12),
                       legend.margin = margin(t = -0.5, unit = 'cm') ) +
        ggplot2::guides(colour = guide_legend(nrow = 1,
                                              byrow = TRUE,
                                              title = "")) +
        ggplot2::facet_wrap(~ etiquetas)
      print(p1)
    },
    {
      stop("Variable type.plot should be 'gr1', 'gr2', 'gr3' or 'gr4'")
    }

  )

}

#
#' @title Load DA map
#'
#' @description This function creates a tmap
#'
#' @param hydroSf data frame.
#' @param long_basin numeric.
#' @param unity character. The units of hydroSf
#' @param style_map character. The style of the map
#' @param legend_position numeric. It indicates de position of the legend of the
#' plot. (Default: 1)
#' @return A plot with map of the basin
#'
#' @importFrom tmap tm_shape tm_fill tm_layout tm_scale_bar
#'
#' @keywords internal
#'
load_map <- function(hydroSf, long_basin, title, style_map,
                     legend_position = 1) {

  scale_barRef <- c(0, floor(long_basin / 8), floor(long_basin / 4))
  scale_barTextS <- 0.7
  palet <-"YlOrRd"

  print(
    create_map(hydroSf, "CatchLoad", style_map, scale_barRef,
               scale_barTextS, title, palet, legend_position = legend_position))
}

#
#' @title Load SA map
#'
#' @description This function creates a tmap
#'
#' @param hydroSf_merge data frame.
#' @param refN_P numeric. The number of variables of the data frame
#' @param long_basin numeric.
#' @param unity character. The units of hydroSf
#' @param legend_position numeric. It indicates de position of the legend of the
#' plot. (Default: 1)
#' @return A plot with map of the basin
#'
#' @importFrom tmap tm_shape tm_fill tm_layout tm_scale_bar
#'
#' @keywords internal
#'
load_SA_map <- function(hydroSf_merge, refN_P, long_basin, unity,
                        legend_position = 1) {

  style_map <- "log10"

  c_man <- c("Man_1", "Manure", "YlOrRd")
  c_min <- c("Min_1", "Mineral", "YlOrRd")
  c_sd <- c("Sd_1", "Sc.Dwellings", "YlOrRd")
  c_ps <- c("Ps_1", "Sc.Dwellings", "YlOrRd")
  c_tot <- c("CatchLoad_1", "CatchLoad", "-viridis")

  if(refN_P == 24) {
    c_atm <- c("Atm_1", "Atmospheric", "YlOrRd")

    c_soil <- c("Soil_1", "Atmospheric", "YlOrRd")

    c_fix <- c("Fix_1", "Fix", "YlOrRd")

    param_list <- list(c_man, c_min, c_ps, c_sd, c_fix, c_atm, c_soil, c_tot)
    map_list <- create_lits_of_maps(hydroSf_merge, long_basin, unity,
                                    legend_position, param_list, style_map)
  } else if(refN_P == 22) {
    c_bg <- c("Bg_1", "Background", "YlOrRd")

    param_list <- list(c_man, c_min, c_ps, c_sd, c_bg, c_tot)
    map_list <- create_lits_of_maps(hydroSf_merge, long_basin, unity,
                                    legend_position, param_list, style_map)

  } else {
    stop("The number of columns of the data frame does not match.")
  }

  print(
    tmap::tmap_arrange(map_list)
  )

}

#' @title Multiple Map
#'
#' @description This function creates an arrange of tmap
#'
#' @param hydroSf data frame.
#' @param refN_P numeric. The number of variables of the data frame
#' @param long_basin numeric.
#' @param unit character. The units of hydroSf
#' @param legend_position numeric. It indicates de position of the legend of the
#' plot. (Default: 1)
#'
#' @importFrom tmap tmap_arrange
#'
#' @return A plot with map of the basin
#'
#' @keywords internal
#'
multiple_map <- function(hydroSf, refN_P, long_basin, unit,
                         legend_position = 1){

  style_map <- "fisher"

  c_man <- c("Man", "Manure", "YlOrRd")
  c_min <- c("Min", "Mineral", "YlOrRd")
  c_sd <- c("Sd", "Sc.Dwellings", "YlOrRd")
  c_tot <- c("TOTAL", "TOT.Diff.", "-viridis")

  if (refN_P == 16) {
    c_ps <- c("Ps", "PointS", "-magma")
    c_fix <- c("Fix", "Fix", "YlOrRd")
    c_atm <- c("Atm", "Atmospheric", "YlOrRd")
    c_soil <- c("Soil", "Soil", "YlOrRd")

    param_list <- list(c_man, c_min, c_fix, c_sd, c_atm, c_soil, c_tot, c_ps)
    map_list <- create_lits_of_maps(hydroSf, long_basin, unit, legend_position,
                                    param_list, style_map)
    mmap <- tmap::tmap_arrange(map_list)
  } else if (refN_P == 14) {
    c_ps <- c("PS", "PointS", "-magma")
    c_bg <- c("Bg", "BackGr.", "YlOrRd")
    param_list <- list(c_man, c_min, c_sd, c_bg, c_tot, c_ps)
    map_list <- create_lits_of_maps(hydroSf, long_basin, unit, legend_position,
                                    param_list, style_map)
    mmap <- tmap::tmap_arrange(map_list)
  }

  mmap

}

#
#' @title Map average load output by source
#'
#' @description Creates maps showing basin output total or by source loads
#'
#' @param green_file data frame of GREEN model results from green_shares()
#' function. Nutrient Load by source apportionment of nutrient for each year
#' and catchments.
#' @param sh_file sf object. The spatial information of the basin.
#' @param basin_name character. The title of the map.
#' @param plot.type character. Alternatives of the map: “gr1”: output load
#' (kt/y) by source; “gr2”: Total Load, log10 (kt/y); “gr3”: Total Load
#' by km2 (kt/year/km2).
#' @param legend_position numeric. Legend position: 1 (default): "right",
#' "bottom"; 2: "left", "up"; 3: "right", "bottom"; 4: "right", "up".
#' @return No return value, called for the side effect of drawing a plot
#'
#' @importFrom sf st_area
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line theme_bw labs scale_color_brewer
#' facet_wrap theme guides guide_legend element_text margin element_blank
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr group_by summarise across
#' @importFrom magrittr %>%
#' @importFrom tidyselect everything
#' @importFrom tmap tmap_arrange
#' @importFrom graphics par barplot
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' data(sh_file)
#' # the parameter to assess the basin model
#' alpha_p <- 35.09
#' alpha_l <- 0.02
#' sd_coef <- 0.2
#' # years in which the model should be executed
#' loc_years <- 1990:2018
#' # Computing the source apportionment
#' basin_sa <- green_shares(catch_data_TN, annual_data_TN, alpha_p, alpha_l,
#' sd_coef, loc_years)
#' # The title of the Map
#' mapTitle <- "Output Loads  for the Lay Basin"
#' # Basin Output Load  Maps by source
#' Lpos <- 1
#' nutrient_maps(basin_sa, sh_file, mapTitle, "gr1", Lpos)
#' # Basin Output Specific Load  Maps
#' Lpos <- 1
#' nutrient_maps(basin_sa, sh_file, mapTitle, "gr2", Lpos)
#' # Basin Output Specific Load by km2 Maps
#' Lpos <- 1
#' nutrient_maps(basin_sa, sh_file, mapTitle, "gr3", Lpos)
#' }
#'
#' @export
#'
nutrient_maps <- function(green_file, sh_file, basin_name, plot.type,
                          legend_position = 1) {

  sh_file$AreaSkm <- sf::st_area(sh_file) / 1000000
  sh_file$AreaSkm <- as.numeric(sh_file$AreaSkm)

  basin_area <- as.numeric(sum(sf::st_area(sh_file) ) / 1000000)
  long_basin <- sqrt(basin_area)

  df_no_sf <- as.data.frame(sh_file)
  df_no_sf <- df_no_sf[, c("HydroID", "NextDownID", "Shreve", "DrainAreaS",
                           "AreaSkm")]

  result <- create_levels(green_file, df_no_sf)
  green_file <- result$df

  index1 <- length(green_file) - 5
  index2 <- length(green_file) - 2

  switch(plot.type,
         "gr1" = {
           df_load_SA_hydro <- green_file[, c(1, 2, 3:index1)] %>%
             dplyr::group_by(HydroID ) %>%
             dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))
           df_load_SA_hydro[, 3:index1] <- df_load_SA_hydro[, 3:index1] + 0.1

           hydroSf_SA_merge <- merge(sh_file, df_load_SA_hydro[, c(1, 3:index1)],
                                     by = "HydroID")

           numvar <- length(hydroSf_SA_merge)

           load_SA_map(hydroSf_merge = hydroSf_SA_merge, refN_P = numvar,
                       long_basin = long_basin, unity = "(kt/y)",
                       legend_position = legend_position)
         },
         "gr2" = {
           df_load_hydro <- green_file[, c(1, 2, index1)] %>%
             dplyr::group_by(HydroID) %>%
             dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

           df_load_hydro  <- data.frame(df_load_hydro)
           names(df_load_hydro) <- c("HydroID", "Year","CatchLoad")

           hydroSf_merge <- merge(sh_file, df_load_hydro[,c("HydroID", "CatchLoad")],
                                  by = "HydroID")

           title <- "Total load (kt/y) log10 scale"
           load_map(hydroSf_merge, long_basin, title, "log10",
                    legend_position = legend_position)
         },
         "gr3" = {
           df_load_hydro_km <- green_file[, c(1, 2, index1, index2)] %>%
             dplyr::group_by(HydroID, DrainAreaS) %>%
             dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)),
                              .groups = "rowwise")

           df_load_hydro_DA  <- data.frame(df_load_hydro_km)
           names(df_load_hydro_DA) <- c("HydroID", "DrainAreaS", "Year", "CatchLoad")
           df_load_hydro_DA$CatchLoad <- df_load_hydro_DA$CatchLoad /
             df_load_hydro_DA$DrainAreaS

           hydroSf_mergeDA <- merge(sh_file,
                                    df_load_hydro_DA[, c("HydroID", "CatchLoad")],
                                    by = "HydroID")

           title <- "Specific load (kt/km2/y)"
           load_map(hydroSf_mergeDA, long_basin, title, "fisher",
                    legend_position = legend_position)
         },
         {
           stop("Variable type.plot should be 'gr1','gr2' or 'gr3'")
         }
  )

}

#
#' @title Output load time series plot
#'
#' @description Creates a time series plot showing basin model results
#'
#' @param green_file data frame. Nutrient Load by source apportionment of
#' nutrient for each year and catchments.
#' @param sh_file sf object. The spatial information.
#' @param basin_name character. The title of the plot.
#' @param plot.type character. Alternative of the plot: output load (t) by
#' source; gr1: Basin average by Shreve (t/y/km2); gr2: Outlet total (kt/y);
#' gr3: Outlet by source apportionment (kt/y).
#' @return No return value, called for the side effect of drawing a plot
#'
#' @importFrom sf st_area
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot aes geom_line theme_bw labs scale_color_brewer
#' facet_wrap theme guides guide_legend element_text margin element_blank
#' @importFrom gridExtra grid.arrange
#' @importFrom dplyr group_by summarise across
#' @importFrom magrittr %>%
#' @importFrom tidyselect everything
#' @importFrom tmap tmap_arrange
#' @importFrom graphics par barplot
#'
#' @examples
#' \donttest{
#' # the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' data(sh_file)
#' # the parameter to assess the basin model
#' alpha_p <- 35.09
#' alpha_l <- 0.02
#' sd_coef <- 0.2
#' # years in which the model should be executed
#' loc_years <- 1990:2018
#' # Computing the source apportionment
#' basin_sa <- green_shares(catch_data_TN, annual_data_TN, alpha_p, alpha_l,
#' sd_coef, loc_years)
#' # The title of the plot
#' plotTitle <- "Time series Load Output for the Lay Basin"
#' # Output Load Basin average time series (lines)
#' nutrient_tserie(basin_sa, sh_file, plotTitle, "gr1")
#' # Total Load in the Basin Outlet time series (lines)
#' nutrient_tserie(basin_sa, sh_file, plotTitle, "gr2")
#' # Total Load in the Basin Outlet by source apportionment time series (lines)
#' nutrient_tserie(basin_sa, sh_file, plotTitle, "gr3")
#' }
#'
#' @export
#'
nutrient_tserie <- function(green_file, sh_file, basin_name, plot.type) {

  sh_file$AreaSkm <- sf::st_area(sh_file) / 1000000
  sh_file$AreaSkm <- as.numeric(sh_file$AreaSkm)

  df_no_sf <- as.data.frame(sh_file)
  df_no_sf <- df_no_sf[, c("HydroID", "NextDownID", "Shreve", "DrainAreaS",
                           "AreaSkm")]

  result <- create_levels(green_file, df_no_sf)
  green_file <- result$df
  ShrLevel1 <- result$ShrLevel1
  ShrLevel2 <- result$ShrLevel2
  ShrLevel3 <- result$ShrLevel3

  sh_sr1DrAr <- sum(green_file[green_file$HydroID %in% ShrLevel1,
                               c("DrainAreaS")])
  sh_sr2DrAr <- sum(green_file[green_file$HydroID %in% ShrLevel2,
                               c("DrainAreaS")])
  sh_sr3DrAr <- sum(green_file[green_file$HydroID %in% ShrLevel3,
                               c("DrainAreaS")])

  index1 <- length(green_file) - 5
  index2 <- length(green_file) - 2
  index3 <- length(green_file)
  index4 <- length(green_file) - 6
  ymin<- min(green_file$Year)
  ymax <- max(green_file$Year)

  if (length(green_file) == 15) {
    colores <- "Set1"
  } else if (length(green_file) == 13) {
    colores <- "Dark2"
  }

  df_load_avg_type_shr_year <- green_file[, c(2,index1,index2,index3)] %>%
    dplyr::group_by(Year,shrLevel) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)),
                     .groups = "rowwise")

  df_load_avg_type_shr_year <- data.frame(df_load_avg_type_shr_year)
  df_load_avg_type_shr_year$LoadByDA <- df_load_avg_type_shr_year$CatchLoad_1 /
    df_load_avg_type_shr_year$DrainAreaS_1

  df_load_avg_type_shr_year_melt <- reshape2::melt(
    df_load_avg_type_shr_year[, c(1, 2, 5)], id.vars = c("Year","shrLevel"))
  df_load_avg_type_shr_year_melt$etiquetas <-
    factor(df_load_avg_type_shr_year_melt$shrLevel,
           labels = c(paste("DrainArea=", floor(sh_sr1DrAr)),
                      paste("DrainArea=", floor(sh_sr2DrAr)),
                      paste("DrainArea=", floor(sh_sr3DrAr))))

  switch (plot.type,
          "gr1" = {
            print(
              ggplot2::ggplot(data = df_load_avg_type_shr_year_melt,
                              ggplot2::aes(x = Year, y = value)) +
                ggplot2::geom_line(alpha = 1.5,size = 1.2) +
                ggplot2::theme_bw(base_size = 15) +
                ggplot2::labs(title =
                                paste0(basin_name,
                                       " Load time series by shreve level "),
                              y = expression("Mean annual specific load (t/y/" ~
                                               km^2 ~ ")" ) ,
                              x = "Year")  +
                ggplot2::scale_color_brewer(palette = colores, name = "") +
                ggplot2::facet_wrap(~ etiquetas)
            )
          },
          "gr2" = {
            greenFile_OUT <- green_file[green_file$NextDownID == -1, ]

            print(
              ggplot2::ggplot(data = greenFile_OUT,
                              ggplot2::aes(x = Year, y = CatchLoad/1000)) +
                ggplot2::geom_line(alpha = 1.5, size = 1.2) +
                ggplot2::theme_bw(base_size = 15) +
                ggplot2::labs(title = paste0(basin_name," Outlet Load time serie "),
                              y = "Outlet annual load (kt/y)",
                              x = "Year")  +
                ggplot2::scale_color_brewer(palette = colores, name = "")
            )
          },
          "gr3" = {
            greenFile_OUT <- green_file[green_file$NextDownID == -1, ]
            greenFileOut_melt <- reshape2::melt(greenFile_OUT[, c(2:index4)],
                                                id.vars = c( "Year"))

            print(
              ggplot2::ggplot(data = greenFileOut_melt,
                              ggplot2::aes(x = Year, y = value / 1000,
                                           fill = variable)) +
                ggplot2::geom_area(alpha = 1.5,size = 1.2) +
                ggplot2::theme_bw(base_size = 15  ) +
                ggplot2::labs(title = basin_name,
                              y = "Outlet annual load with source
                        contribution (kt/y)",
                              x = "Year")  +
                ggplot2::scale_fill_brewer(palette = colores) +
                ggplot2::theme(legend.position = "bottom",
                               legend.text = element_text(size = 12),
                               legend.margin = margin(t = -0.5, unit = 'cm') ) +
                ggplot2::guides(fill = guide_legend(nrow = 1, byrow = TRUE,
                                                    title = "")) +
                ggplot2::scale_x_continuous(breaks = seq(ymin, ymax, by = 2))
            )
          },
          {
            stop("Variable type.plot should be 'gr1','gr2' or 'gr3'")
          }
  )

}

#'
#' @title Nutrient balance flow plot
#'
#' @description Nutrient balance flow in Sankey plot
#'
#' @param Nbalance_out data frame. Nutrient balance result from the Nutbalance()
#' function
#'
#' @return A Sankey diagram and a data frame with the some variable values
#'
#' @importFrom networkD3 sankeyNetwork JS
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
#' # years in which the model should be executed
#' loc_years <- 1990:2018
#' # Computing the nutrient balance
#' nut_bal <- region_nut_balance(catch_data_TN, annual_data_TN, alpha_p, alpha_l,
#' sd_coef, loc_years)
#' # Plot the sankey plot with the result of the balance
#' sank <- N4_sankey(nut_bal)
#' }
#'
#' @export
#'
N4_sankey <- function(Nbalance_out) {

  data <- as.data.frame(Nbalance_out)
  n_years <- length(levels(as.factor(data$Year)))

  PS <- sum(data$PS) / n_years
  min <- sum(data$Min) / n_years
  man <- sum(data$Man) / n_years
  fix <- sum(data$Fix) / n_years
  soil <- sum(data$Soil) / n_years
  atm <- sum(data$Atm) / n_years
  sd <- sum(data$Sd) / n_years
  diffuse_agri <- sum(data$DiffuseAgri) / n_years
  diffuse <- PS + sum(data$Diffuse) / n_years
  atm_2_agri <- sum(data$Atm2DiffAgri) / n_years
  atm_2_diff <- sum(data$Atm2Diff) / n_years
  atm_2_land_ret <- sum(data$Atm2LandRet) / n_years
  sd_2_diff <- sum(data$SD2Diff) / n_years
  sd_2_land_ret <- sum(data$SD2LandRet) / n_years
  agri_2_diffuse <- diffuse_agri - sum(data$DiffAgri2LandRet) / n_years
  agri_2_land_ret <- sum(data$DiffAgri2LandRet) / n_years
  load_2_riv_ret <- sum(data$CatchRivRet) / n_years
  load_2_lake_ret <- sum(data$CatchLakeRet) / n_years
  load_2_outlet <- sum(data$CatchLoad[which(data$To_catch < 1)]) / n_years

  name <- c("PS", "Min", "Man", "Fix", "Soil", "Atm", "Sd",
            "Agriculture land diffuse sources", "Load to stream", "Land Ret",
            "River Ret", "Lake Ret", "Load to outlet")
  nodesID <- c(1:length(name)) - 1
  nodes <- data.frame(nodesID, name)

  source <- c(0,
              1, 2, 3, 4,
              5, 5, 5,
              6, 6,
              7, 7,
              8, 8, 8)

  target <- c(8,
              7, 7, 7, 7,
              7, 8, 9,
              8, 9,
              8, 9,
              10, 11, 12)

  value <- c(PS,
             min, man, fix, soil,
             atm_2_agri, atm_2_diff, atm_2_land_ret,
             sd_2_diff, sd_2_land_ret,
             agri_2_diffuse, agri_2_land_ret,
             load_2_riv_ret, load_2_lake_ret, load_2_outlet)

  links <- data.frame(source, target, value)

  print(
    networkD3::sankeyNetwork(
      Links = links,
      Nodes = nodes,
      Source = "source",
      Target = "target",
      Value = "value",
      NodeID = "name",
      units = "TP T/y",
      colourScale = networkD3::JS("d3.scaleOrdinal(d3.schemeCategory20);"),
      fontSize = 30, nodeWidth = 30, sinksRight = TRUE)
  )

  name <- c("PS", "Min", "Man", "Fix", "Soil", "Atm", "Sd",
            "Agriculture land diffuse sources", "Load to stream", "Land Ret",
            "River Ret", "Lake Ret", "Load to Outlet")

  output <- c(PS, min, man, fix, soil, atm, sd,
              diffuse_agri,
              diffuse,
              (agri_2_land_ret + atm_2_land_ret + sd_2_land_ret),
              load_2_riv_ret, load_2_lake_ret,
              load_2_outlet)

  outloads <- data.frame(name, "Mean_TN_t/y" = output)

  outloads
}

#
#' @title Preprocessing data for scenario summary
#'
#' @description This function blah, blah, blah....
#'
#' @param annual_data data frame.
#' @return A data frame with the total load by year and type
#'
#' @importFrom hydroGOF gof
#' @importFrom ggplot2 ggplot aes ggtitle facet_wrap geom_point geom_abline
#' @importFrom graphics abline legend plot points
#' @importFrom stats quantile
#' @importFrom dplyr across
#' @importFrom tidyselect everything
#'
#' @keywords internal
#'
preproc_scenSummary <- function(annual_data){

  df_load_avg_type_year <- annual_data[, c(2, 5:11)] %>%
    dplyr::group_by(YearValue) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

  df_load_avg_type_year <- data.frame(DF_LoadAvg_Type_Year)

  df_load_avg_type_shr_year <- annual_data[, c(2, 15, 5:11)] %>%
    dplyr::group_by(YearValue,shrLevel) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), list(mean)))

  df_load_avg_type_shr_year <- data.frame(df_load_avg_type_shr_year)

  df_load_tot_type_year <- annual_data[, c(2, 5:11)] %>%
    dplyr::group_by(YearValue) %>%
    dplyr::summarise(dplyr::across(tidyselect::everything(), list(sum)))

  df_load_tot_type_year <- data.frame(df_load_tot_type_year)

  df_load_tot_type_year
}

#'
#' @title Scatter plot of goodness-of-fit metric vs parameters
#'
#' @description Scatter plot of goodness-of-fit metric vs parameters
#'
#' @param df_cb data frame. A table with the result of the calibration process.
#' @param param character. Goodness of fit metric:"NSE", "rNSE", "NSE",
#' "mNSE", "MAE", "PBIAS", "cp", "R2",...
#'
#' @importFrom graphics plot
#'
#' @return Multiple scatter plot
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
#' df_calib <- calib_green(catch_data_TN, annual_data_TN, n_iter, low, upp,
#' years)
#' gof_mes <- "NSE"
#' scatter_plot(df_calib, gof_mes)
#' }
#'
#' @export
#'
scatter_plot <- function(df_cb, param) {

  oldpar <- par(no.readonly = TRUE)
  on.exit(par(oldpar))
  graphics::par(mfrow = c(1, 3))
  if (param == "PBIAS..") {
    df_cb[, param] <- abs(df_cb[, param])
  }

  graphics::plot(df_cb$alpha_P, df_cb[, param],
                 ylab = param,
                 xlab = "alpha_P")
  graphics::plot(df_cb$alpha_L, df_cb[, param],
                 ylab = param,
                 xlab = "alpha_L")
  graphics::plot(df_cb$sd_coeff, df_cb[, param],
                 ylab = param,
                 xlab= "sd_coeff")

}

#'
#' @title Selection of best calibration parameters
#'
#' @description Return the best calibration parameter set according to one
#' goodness-of-fit metric
#'
#' @param df_cb data frame. The result of the calibration process.
#' @param par numeric. Goodness-of-fit measures. "NSE", "rNSE", "NSE",
#' "mNSE", "MAE", "PBIAS", "cp", "R2",...
#'
#' @return A vector with the 3 parameters
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
#' df_calib <- calib_green(catch_data_TN, annual_data_TN, n_iter, low, upp,
#' years)
#' # Extract the best set of parameter according to a Goodnes of fit metric
#' gof_mes <- "NSE"
#' NSE_bestParams <- select_params(df_calib, gof_mes)
#' }
#'
#' @export
#'
select_params <- function(df_cb, par){

  best_par <- NULL
  ind_mtr <- c(6, 15, 9, 10, 11, 17)

  for (ind in ind_mtr) {
    if (ind == 6 | ind == 17 ) {
      irow <- which(abs(df_cb[, ind]) ==  min(abs(df_cb[, ind]),
                                              na.rm = TRUE))[1]
    } else if (ind %in% c(15, 9, 10, 11)) {
      irow <- which(df_cb[, ind] == max(df_cb[, ind], na.rm = TRUE))[1]
    }
    best_par <- rbind(best_par, data.frame(metric = names(df_cb)[ind],
                                           df_cb[irow, ]))

  }

  best_par[best_par$metric == par, c(par, "alpha_P", "alpha_L", "sd_coeff")]
}

#'
#' @title Scatter plot comparing observed vs modeled loads by year
#'
#' @description Plot
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
#' @param name_basin character. The title of the plot.
#' @param years numeric. Years to be shown in the plot.
#'
#' @importFrom ggplot2 ggplot ggtitle aes geom_point theme_bw geom_abline
#' facet_wrap
#'
#' @return Multiple scatter plot and a data frame with annual nutrient
#' (nitrogen or phosphorus) load for all catchments in the basin
#'
#' @examples
#' \donttest{
#'# the data of the TN scenario
#' data(catch_data_TN)
#' data(annual_data_TN)
#' # the parameter to assess the basin model
#' alpha_p <- 35.09
#' alpha_l <- 0.02
#' sd_coef <- 0.2
#' # years in which the plot will we shown
#' years <- 1990:2018
#' # generating the scatter plot comparing observed vs modeled loads by year
#' name_basin <- "Lay NSE"
#' simobs_annual_plot(catch_data_TN, annual_data_TN, alpha_p, alpha_l,
#' sd_coef, years, name_basin)
#' }
#'
#' @export
#'
simobs_annual_plot <- function(catch_data, annual_data, alpha_p, alpha_l,
                            sd_coef, years, name_basin) {

  df_scen_global <- launch_green(catch_data, annual_data, alpha_p, alpha_l,
                                sd_coef, years)

  df_observ_g <- df_scen_global[!is.na(df_scen_global$ObsLoad), ]
  df_observ_g <- df_observ_g %>%
    rename(PredictLoad = CatchLoad)

  names(df_observ_g)
  p <- ggplot2::ggplot(data = df_observ_g,
                       ggplot2::aes(x = ObsLoad,
                                    y = PredictLoad)) +
    ggplot2::ggtitle(name_basin)  +
    ggplot2::geom_point() +
    ggplot2::theme_bw(base_size = 10  ) +
    ggplot2::geom_abline(intercept = 0, slope = 1) +
    ggplot2::facet_wrap(~Year)

  print(p)

  df_scen_global

}
