# Suppress 'no visible binding for global variable' warnings
utils::globalVariables(
  c(
    'ATT',
    "Time",
    "lower",
    "upper",
    "diff_lower",
    "diff_upper",
    "date_unix",
    "ID",
    "time",
    "Y",
    "Level",
    "sim",
    "mean_p",
    "Units",
    "Type",
    "BestControl",
    "pow",
    "duration",
    "investment",
    "Level"
  )
)

#' Data reading function for GeoLift.
#'
#' @description
#'
#' \code{GeoDataRead} reads a data-frame and processes it for GeoLift.
#' The function will clean the data, generate a time variable that
#' increases by 1 for each time period (day/week/month), and aggregate
#' the data by time and location. It is important to have data for each
#' location and time-period and avoid special characters in the names of
#' the geographical units.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param date_id Name of the date variable (String).
#' @param location_id Name of the location variable (String).
#' @param Y_id Name of the outcome variable (String).
#' @param format Format of the dates in the data frame.
#' @param X List of names of the covariates.
#' @param summary Display a summary of the data-reading process. FALSE by default.
#'
#' @return
#' A data frame for GeoLift inference and power calculations.
#'
#' @import augsynth
#' @import gsynth
#' @import panelView
#' @import plyr
#' @import dplyr
#' @import doParallel
#' @import foreach
#' @import ggrepel
#' @import MarketMatching
#' @import stringr
#' @import directlabels
#' @import ggplot2
#' @import tibble
#' @import tidyr
#' @import parallel
#' @import graphics
#' @import stats
#' @import utils
#' @import rlang
#'
#' @export
GeoDataRead <- function(data,
                        date_id = "date",
                        location_id = "location",
                        Y_id = "units",
                        format = "mm/dd/yyyy",
                        X = c(),
                        summary = FALSE) {

  format <- tolower(format)

  # Acceptable date formats
  valid_formats_day <- c("mm/dd/yyyy", "mm-dd-yyyy", "mm.dd.yyyy", "mmddyyyy",
                         "dd/mm/yyyy", "dd-mm-yyyy", "dd.mm.yyyy", "ddmmyyyy",
                         "yyyy/mm/dd", "yyyy-mm-dd", "yyyy.mm.dd", "yyyymmdd")
  valid_formats_week <- c("ww/yyyy", "ww-yyyy", "ww.yyyy", "wwyyyy",
                          "yyyy/ww", "yyyy-ww", "yyyy.ww", "yyyyww")
  valid_formats_month <- c("mm/yyyy", "mm-yyyy", "mm.yyyy", "mmyyyy",
                           "yyyy/mm", "yyyy-mm", "yyyy.mm", "yyyymm")
  valid_formats <- c(valid_formats_day, valid_formats_week, valid_formats_month)

  if (!(format %in% valid_formats) ){
    message("Error: Please enter a valid date format. Valid formats are:")
    print(valid_formats)
    return(NULL)
  }

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(date = date_id,
                          Y = Y_id,
                          location = location_id)

  # Remove white spaces in date variable
  data$date <- as.character(data$date)
  data$date <- trimws(data$date)

  # Location in lower-case for compatibility with GeoLift
  data$location <- tolower(data$location)
  initial_locations <- length(unique(data$location))

  # Determine the separator
  if (str_count(format, pattern = fixed("/")) > 0){
    sep <- "/"
  } else if(str_count(format, pattern = fixed("-")) > 0) {
    sep <- "-"
  } else if(str_count(format, pattern = fixed(".")) > 0) {
    sep = "."
  } else {
    sep <- ""
  }

  # Make sure the data is complete for formats without sep
  if ( sep == "" & min(nchar(data$date)) != nchar(format)) {
    message("Error: The length of the date is incorrect.")
    message("Make sure the entries have trailig zeroes (1/1/2012 -> 01/01/2012)")
    return(NULL)
  }

  # Remove separators

  if (format %in% valid_formats_day){
    date_format <- gsub(sep,"",format) #Remove sep
    date_format <- gsub("yyyy","Y",date_format)
    date_format <- gsub("mm","m",date_format)
    date_format <- gsub("dd","d",date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%",date_format[1], sep, "%", date_format[2], sep, "%", date_format[3])

    data$date_unix <- data$date
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format="%Y-%m-%d") %>%
      as.numeric()

  } else if (format %in% valid_formats_week) {
    date_format <- gsub(sep,"",format) #Remove sep
    date_format <- gsub("yyyy","Y",date_format)
    date_format <- gsub("ww","W",date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%",date_format[1], sep, "%", date_format[2], sep, "%w")

    data$date_unix <- data$date
    data$date_unix <- paste0(data$date_unix, sep, "0")
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format="%Y-%m-%d") %>%
      as.numeric()

  } else if (format %in% valid_formats_month) {
    date_format <- gsub(sep,"",format) #Remove sep
    date_format <- gsub("yyyy","Y",date_format)
    date_format <- gsub("mm","m",date_format)
    date_format <- unlist(strsplit(date_format, split = ""))

    reformat <- paste0("%",date_format[1], sep, "%", date_format[2], sep, "%d")

    data$date_unix <- data$date
    data$date_unix <- paste0(data$date_unix, sep, "1")
    # Create date in unix time
    data$date_unix <- data$date_unix %>%
      as.Date(reformat) %>%
      as.POSIXct(format="%Y-%m-%d") %>%
      as.numeric()
  }

  #Remove NAs from date conversion
  if (sum(is.na(data$date_unix)) > 0){
    message(paste0(sum(is.na(data$date_unix)), " rows dropped due to inconsistent time format."))
    data <- data[is.na(data$date_unix) == FALSE,]
  }

  # Find the time increments
  time_increments <- unique(sort(data$date_unix, FALSE))[2] -
    unique(sort(data$date_unix, FALSE))[1]

  data$time <- (data$date_unix-min(data$date_unix))/
    as.numeric(time_increments) + 1

  #Recode to avoid missing weeks (time increases always by 1)
  TimePeriods <- data.frame(time = sort(unique(data$time)))
  TimePeriods$ID <- seq.int(nrow(TimePeriods))

  data <- data %>% dplyr::left_join(TimePeriods)
  data$time <- data$ID

  data <- subset(data, select = -c(date_unix, ID))

  #Remove revenue with zeroes
  #data <- filter(data, Y > 0)

  # Remove cities with missing time periods
  total_periods <- max(data$time)
  complete_cases <- table(data$location, data$time)
  complete_cases[complete_cases > 0] <- 1
  complete <- rowSums(complete_cases) == total_periods
  incomplete_locations <- length(complete) - length(complete[complete==TRUE])
  complete <- complete[complete==TRUE]
  data <- data %>% dplyr::filter(location %in% names(complete))


  # Aggregate Outcomes by time and location
  data_raw <- data
  data <- data_raw %>% dplyr::group_by(location, time) %>% dplyr::summarize(Y = sum(Y))
  for (var in X){
    data_aux <- data_raw %>% dplyr::group_by(location, time) %>% dplyr::summarize(!!var := sum(!!sym(var)))
    data <- data %>% dplyr::left_join(data_aux, by = c("location", "time"))
  }

  # Print summary of Data Reading
  if (summary == TRUE) {
    message(paste0(
      "##################################",
      "\n#####       Summary       #####\n",
      "##################################\n",
      "\n* Raw Number of Locations: ", initial_locations,
      "\n* Time Periods: ", total_periods,
      "\n* Final Number of Locations (Complete): ", length(unique(data$location))  ) )
  }

  return(as.data.frame(data))

}


#' Plotting function for Exploratory Analisis.
#'
#' @description
#'
#' \code{GeoPlot} takes a data frame used for GeoLift to generate
#' a plot of each location's time series.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param treatment_start Treatment start (Default = 0).
#'
#' @return
#' A plot of each location's time series.
#'
#' @export
GeoPlot <- function(data,
                    Y_id = "Y",
                    time_id = "time",
                    location_id = "location",
                    treatment_start = 0){

  if (treatment_start == 0){
    size_vline <- 0
  } else{
    size_vline <- 0.8
  }

  p <- ggplot(data,aes(y = !!sym(Y_id),x = !!sym(time_id), colour = !!sym(location_id), label = !!sym(location_id) )) +
    geom_line(show.legend = FALSE) +
    geom_vline(xintercept = treatment_start, linetype="dashed", size = size_vline, color = "grey35") +
    geom_dl(aes(label = !!sym(location_id)), method = list(dl.combine("last.points"), cex = 0.8)) +
    xlim(1,1.15*(max(data[[time_id]])))+
    ylab("") +
    theme_minimal()

  print(p)
}


#' Helper to trim controls for shorter run-times with minimal loss
#' of precision.
#'
#' @description
#'
#' As the number of controls for a Geo test increases, the model
#' complexity grows as does the algorithm's run-time. However,
#' there are diminishing marginal returns in adding too many control
#' locations, especially if their time-series are very similar.
#' \code{TrimControls} provides a method to trim the number of controls
#' in order to reduce run-times with minimal loss of precision. In general,
#' it is recommended to have 4 to 5 times the number of controls locations
#' than the ones we have for test locations.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param max_controls Max number of controls, recommended 4x-5x
#' the number of test locations.
#' @param test_locations List of test locations.
#' @param forced_control_locations List of locations to be forced
#' as controls.
#'
#' @return
#' A data frame with reduced control locations.
#'
#' @export
TrimControls <- function(data,
                         Y_id = "Y",
                         time_id = "time",
                         location_id = "location",
                         max_controls = 20,
                         test_locations = c(),
                         forced_control_locations = c()){


  data <- data %>% dplyr::rename(time = time_id,
                          Y = Y_id,
                          location = location_id)

  if (max_controls > length(unique(data$location))){
    print("Error: There can't be more controls than total locations.")
    return(NULL)
  }

  # Calculate the Average Time-Series
  avg_Y <- data %>% dplyr::group_by(time) %>% dplyr::summarize(Y_mean = mean(Y))

  # Append it to the data
  data_aux <- data %>% dplyr::left_join(avg_Y, by = "time")

  # Compute the difference for each time/location
  data_aux$diff <- data_aux$Y - data_aux$Y_mean

  # Calculate the average difference
  data_aux <- data_aux %>% dplyr::group_by(location) %>% dplyr::summarize(mean_diff = mean(diff))

  # Calculate the percentiles for stratified sampling
  perc <- quantile(data_aux$mean_diff, probs = seq(0, 1, 0.2))
  perc <- unname(perc)

  data_aux$percentile <- 0

  for (location in 1:nrow(data_aux)){
    for(percentile in 1:(length(perc)-1)){
      if (data_aux$mean_diff[location] > perc[percentile] &
          data_aux$mean_diff[location] <= perc[percentile+1]){

        data_aux$percentile[location] <- percentile

      }
    }
  }

  data_locs <- data_aux %>%
    dplyr::filter(!(location %in% test_locations)) %>%
    dplyr::group_by(percentile) %>%
    dplyr::sample_n(round(max_controls/length(perc)), replace = TRUE) %>%
    dplyr::distinct(location)

  final_locations <- unique(c(data_locs$location, test_locations, forced_control_locations))

  data <- data %>% dplyr::filter(location %in% final_locations)

  return(data)

}


#' Auxiliary function to generate the treatment variable D.
#'
#' @description
#'
#' \code{fn_treatment} generates the treatment variable D, where
#' D = 1 for test locations in the test period and D = 0 otherwise.
#'
#' @param df A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations List of names of the test locations (String).
#' @param treatment_start_time Time index of the start of the treatment.
#' @param treatment_end_time Time index of the end of the treatment.
#'
#' @return
#' Data frame with the additional treatment variable (D).
#'
#' @export
fn_treatment <- function(df,
                         locations,
                         treatment_start_time,
                         treatment_end_time) {

  df$D <- 0
  df$D[df$location %in% locations &
         treatment_start_time <= df$time &
         df$time <= treatment_end_time] <- 1

  df <- df %>% dplyr::filter(time <= treatment_end_time) #Remove periods after treatment

  return(df)

}


#' P-value calculation for GeoLift.
#'
#' @description
#'
#' \code{pvalue} calculates the p-value for a GeoLift object.
#'
#' @param augsyn GeoLift object.
#'
#' @return
#' P-value.
#'
#' @export
pvalue <- function(augsyn){

  if(paste(augsyn$call)[1] == "single_augsynth"){
    zscore <- summary(augsyn)[['average_att']][['Estimate']]/
      summary(augsyn)[['average_att']][['Std.Error']]
  }
  else if(paste(augsyn$call)[1] == "multisynth"){
    estimates <- summary(augsyn)$att %>% dplyr::filter(Level == "Average" & is.na(Time) == TRUE)
    zscore <- estimates[['Estimate']]/estimates[['Std.Error']]
  }
  else {
    return(999)
  }

  return(2*(1-pnorm(abs(zscore))))
}


#' Calculate p-value for GeoLift.
#'
#' @description
#'
#' \code{pvalueCalc} calculates the p-value for a GeoLift object.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param sim Time simulation index.
#' @param max_time Treatment end index.
#' @param tp Time period index.
#' @param es Effect Size.
#' @param locations List of test locations.
#' @param cpic Cost Per Incremental Conversion.
#' @param X List of names of covariates.
#'
#' @return
#' List that contains:
#'          \itemize{
#'          \item{"location":}{ Test locations.}
#'          \item{"pvalue":}{ P Value.}
#'          \item{"tp":}{ Time period index.}
#'          \item{"es":}{ Effect Size used for the simulation.}
#'          \item{"treatment_start_time":}{ Treatment start time for the simulation}
#'          \item{"investment":}{ Estimated Investment}
#'         }
#'
#' @export
pvalueCalc <- function(data,
                       sim,
                       max_time,
                       tp,
                       es,
                       locations,
                       cpic,
                       X) {

  treatment_start_time <- max_time - tp - sim + 2
  treatment_end_time <- treatment_start_time + tp - 1
  pre_test_duration <- treatment_start_time - 1
  pre_treatment_start_time <- 1

  data_aux <- fn_treatment(data, locations=locations,
                           treatment_start_time,
                           treatment_end_time)

  data_aux$Y_inc <- data_aux$Y
  data_aux$Y_inc[data_aux$D==1] <- data_aux$Y_inc[data_aux$D==1]*(1+es)


  if (length(X) == 0){
    pVal <- pvalue(augsynth::augsynth(Y_inc ~ D, unit = location, time = time,
                            data = data_aux,
                            t_int = treatment_start_time,
                            progfunc = "GSYN", scm = T))
  }
  else if (length(X) > 0){
    fmla <- as.formula(paste("Y_inc ~ D |",
                             sapply(list(X),
                                    paste, collapse = "+")))

    pVal <- pvalue(augsynth::augsynth(fmla, unit = location, time = time,
                            data = data_aux,
                            t_int = treatment_start_time,
                            progfunc = "GSYN", scm = T))
  }


  investment <- cpic*data_aux$Y[data_aux$D==1]*(es)

  return (
    c(paste(locations, collapse="; "),
      pVal,
      tp,
      es,
      treatment_start_time,
      investment)
  )
}


#' Power Calculation for GeoLift for known test locations.
#'
#' @description
#' This function runs power calculations for input historical geo data
#' for a pre-determined set of test locations.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations A semi-colon separated list of test geo locations.
#' @param effect_size A vector of effect sizes to test by default a
#' sequence between 0 - 15 percent: seq(0,10000,50)/1000.
#' @param treatment_periods Expected length of the test. A vector of
#' possible lengths can be entered for multiple options.
#' @param horizon Lookback window for the power analysis. For daily
#' data 30 days are recommended and 8-12 weeks for monthly data.
#' The default window is 30.
#' @param cpic Cost Per Incremental Conversion for estimated test
#' minimum budget. The default value is 0, in which case no investment
#' estimation will be provided.
#' @param X List of names of covariates. No covariates are used
#' by default.
#' @param Y_id Name of the outcome variable. "Y" by default.
#' @param location_id Name of the location variable. "Y" by default.
#' @param time_id Name of the time variable. "Y" by default.
#'
#' @return
#' GeoLiftPower object that contains:
#'      \itemize{
#'          \item{"location":}{ Test units of the simulation}
#'          \item{"pvalue":}{ P Value for each simulation}
#'          \item{"duration":}{ Duration of the simulation}
#'          \item{"effect_size":}{ Effect Size used for the simulation}
#'          \item{"treatment_start":}{ Treatment start time for the simulation}
#'          \item{"investment":}{ Estimated Investment}
#'      }
#'
#' @export
GeoLiftPower <- function(data,
                         locations,
                         effect_size = seq(0,10000,50)/1000,
                         treatment_periods,
                         horizon = 30,
                         cpic = 0,
                         X = c(),
                         Y_id = "Y",
                         location_id = "location",
                         time_id = "time"){

  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  registerDoParallel(cl)

  parallel::clusterCall(cl, function()
    attachNamespace('augsynth'))
  parallel::clusterCall(cl, function()
    attachNamespace('dplyr'))
  parallel::clusterCall(cl, function()
    attachNamespace('tidyr'))

  parallel::clusterExport(
    cl,
    c('fn_treatment','pvalue','pvalueCalc'),
    envir=environment()
  )

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  if (max(treatment_periods)/max_time > 0.8){
    message(paste0("Warning: Small pre-treatment period.
                   \nTthe treatment is larger that 80% of all available data."))
  }

  results <- data.frame(matrix(ncol=7,nrow=0))
  colnames(results) <- c("location","pvalue","duration","lift",
                         "treatment_start", "investment", "cpic")

  for (es in effect_size){ #iterate through lift %
    for (tp in treatment_periods){ #lifts
      t_n <- max(data$time) - tp + 1 #Number of simulations without extrapolation

      a <- foreach(sim = 1:(t_n-horizon),
                   .combine=cbind,
                   .errorhandling = 'remove') %dopar% {
                     pvalueCalc(
                       data,
                       sim,
                       max_time,
                       tp,
                       es,
                       locations,
                       cpic,
                       X)

                   }

      for (i in 1:ncol(a)) {
        results <- rbind(results, data.frame(location = a[[1,i]],
                                             pvalue = as.numeric(a[[2,i]]),
                                             duration = as.numeric(a[[3,i]]),
                                             lift = as.numeric(a[[4,i]]),
                                             treatment_start = as.numeric(a[[5,i]]),
                                             investment = as.numeric(a[[6,i]]),
                                              cpic = cpic ) )
      }

    }
  }

  stopCluster(cl)
  class(results) <- c("GeoLiftPower", class(results))

  return(results)
}


#' Plotting function for GeoLiftPower.
#'
#' @description
#'
#' Plotting functoin for \code{GeoLiftPower}.
#'
#' @param x GeoLiftPower object.
#' @param power Power level. By default 0.8.
#' @param conf.level Confidence level. By default 0.9.
#' @param table Plot the table of power estimates. TRUE by default.
#' @param ... additional arguments
#'
#' @return
#' GeoLiftPower plot.
#'
#' @export
plot.GeoLiftPower <- function(x,
                              power=0.8,
                              conf.level = 0.9,
                              table = TRUE, ...) {

  if (!inherits(x, 'GeoLiftPower')) {
    stop('object must be class GeoLiftPower')
  }

  treatment_periods <- unique(x$duration)
  lift <- unique(x$lift)


  resultsM <- matrix(0,nrow = length(treatment_periods), ncol=length(lift),
                     dimnames=list("Treatment Periods" = treatment_periods, "Lift" = lift))

  for (duration in rownames(resultsM)){
    for (lifts in colnames(resultsM)){
      resultsM[[duration, lifts]] <- round(nrow(x[x$pvalue < 1 - conf.level &
                                                    x$duration == as.numeric(duration) &
                                                    x$lift == as.numeric(lifts),]) /
                                             nrow(x[x$duration == as.numeric(duration) &
                                                      x$lift == as.numeric(lifts),]),5)
    }
  }

  spending <- x %>% dplyr::group_by(duration, lift) %>% dplyr::summarize(inv = mean(investment))

  if (table == TRUE){
    print(t(resultsM))
  }

  #print(str(resultsM))
  #resultsM<- cbind(resultsM,
  #                 matrix(0,nrow = length(treatment_periods), ncol=1,
  #                        dimnames=list("Treatment Periods" = treatment_periods, "Lift" = -1)))

  #print(resultsM)
  #print(str(resultsM))
  #lift <- c(-1,lift)
  if (sum(spending$inv > 0)) {
    for(tp in 1:nrow(resultsM)){
      par(mar = c(5, 5, 10, 5))
      #print(lift)
      #print(resultsM[tp,])
      scatter.smooth(lift, resultsM[tp,],span=0.5, type="n", ylim = c(0,1),
                     xlab="Effect Size", ylab="Power",  lpars = list(col = "red", lwd = 3),
                     main=c("Treatment Periods: ",as.character(rownames(resultsM)[tp]), "\n investment"))
      abline(h = power)
      par(new=TRUE)
      plot((spending %>% dplyr::filter(duration == rownames(resultsM)[tp]))$inv, resultsM[tp,], type="n", xaxt = "n", yaxt = "n", ylab = "", xlab = "")
      axis(side=3)
    }

  } else {
    for(tp in 1:nrow(resultsM)){
      scatter.smooth(lift, resultsM[tp,],span=0.5, type="n", ylim = c(0,1),
                     xlab="Effect Size", ylab="Power",  lpars = list(col = "red", lwd = 3),
                     main=c("Treatment Periods: ",as.character(rownames(resultsM)[tp])))
      abline(h = power)
    }
  }

}


#' Power Calculation to determine the number of test periods
#' with unknown test locations.
#'
#' @description
#'
#' \code{NumberLocations} calculates power to determine the
#' number of test periods with unknown test locations.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param number_locations List of number of locations to test. If not specified,
#' the number of locations will be computed by percentiles up to half of the
#' total number of locations.
#' @param treatment_periods Number of treatment periods.
#' @param n_sim Number of simulations.
#' @param X List of covariate names.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param plot Plots results when TRUE.
#' @param power Power level. By default 0.8.
#' @param conf.level Confidence level. By default 0.9.
#'
#' @return
#' Table of average power by number of locations.
#'
#' @export
NumberLocations <- function(data,
                            number_locations = c(),
                            treatment_periods,
                            n_sim = 50,
                            X = c(),
                            Y_id = "Y",
                            location_id = "location",
                            time_id = "time",
                            plot = TRUE,
                            power = 0.8,
                            conf.level = 0.9){

  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  registerDoParallel(cl)

  parallel::clusterCall(cl, function()
    attachNamespace('augsynth'))
  parallel::clusterCall(cl, function()
    attachNamespace('dplyr'))
  parallel::clusterCall(cl, function()
    attachNamespace('tidyr'))

  parallel::clusterExport(
    cl,
    c('fn_treatment','pvalue','pvalueCalc'),
    envir=environment()
  )

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)
  locs <- unique(as.character(data$location))

  if (treatment_periods/max_time > 0.8){
    message(paste0("Warning: Small pre-treatment period.
                   \nTthe treatment is larger that 80% of all available data."))
  }

  #times <- trunc(quantile(data$time, probs = c(0.5, 0.75, 1), names = FALSE ))

  results <- data.frame(matrix(ncol=4, nrow=0))
  colnames(results) <- c("location","pvalue", "n", "treatment_start")

  if (length(number_locations) == 0) {
    number_locations <- round(quantile(c(1:length(unique(data$location))),
                                       probs = seq(0,0.5,0.05), names = FALSE))
  }

  for(n in number_locations){
    #for (t in times){

      a <- foreach(sim = 1:n_sim,
                   .combine=cbind,
                   .errorhandling = 'remove') %dopar% {
                     pvalueCalc(
                       data = data,
                       sim = 1,
                       max_time = max_time,#max_time,
                       tp = treatment_periods,
                       es = 0,
                       locations = as.list(sample(locs,n, replace = FALSE )),
                       cpic = 0,
                       X = c())

                   }

      for (i in 1:ncol(a)) {
        results <- rbind(results, data.frame(location = a[[1,i]],
                                             pvalue = as.numeric(a[[2,i]]),
                                             n = n,
                                             treatment_start = as.numeric(a[[5,i]]) ) )
        }
    #}
  }

  stopCluster(cl)

  if(plot == TRUE){
    results$pow <- 0
    results$pow[results$pvalue > 1 - conf.level] <- 1

    resultsM <- results %>% dplyr::group_by(n) %>%  dplyr::summarize(mean_pow = mean(pow))
    resultsM <- tibble::add_row(resultsM, n = 0, mean_pow = 0, .before = 1)

    print(" Average Power By Number of Locations")
    print(resultsM)

    plot(resultsM, type = "l", col = "red", lwd = 3,
         main="Power Calculation by Number of Locations",
         ylab="Power", xlab="Number of Test locations")
    abline(h = power)
  }

  return(results)

}


#' Market selection tool.
#'
#' @description
#'
#' \code{MarketSelection} helps calculate the best markets based
#' on Dynamic Time Warping between the locations' time-series.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param Y_id Name of the outcome variable (String).
#'
#' @return
#' Matrix of the best markets. The second to last columns show
#' the best to worst market matches for the location in the first
#' column.
#'
#' @export
MarketSelection <- function(data,
                            location_id = "location",
                            time_id = "time",
                            Y_id = "Y"){

  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  data$location <- tolower(data$location)

  # Find the best matches based on DTW
  mm <- MarketMatching::best_matches( data = data,
                      id_variable = "location",
                      date_variable = "time",
                      matching_variable = "Y",
                      parallel = TRUE,
                      warping_limit = 1,
                      start_match_period = 1,
                      end_match_period = max(data$time),
                      matches = length(unique(data$location)) - 1,
                      dtw_emphasis = 1 )

  # Create a matrix with each row being the raked best controls for each location
  best_controls <- mm$BestMatches %>% tidyr::pivot_wider(id_cols = location,
                                                  names_from = rank,
                                                  values_from = BestControl)

  best_controls <- as.matrix(best_controls)
  colnames(best_controls) <- NULL

  return(best_controls)

}


#' Power calculations for unknown test market locations, number of
#' test markets, and test duration.
#'
#' @description
#'
#' \code{GeoLiftPower.search} provides power calculations for unknown
#' test markets, number of test locations, and test duration.
#'
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param treatment_periods List of treatment periods to calculate power for.
#' @param N List of number of test markets to calculate power for.
#' @param horizon Lookback window for the power analysis. For daily
#' data 30 days are recommended and 8-12 weeks for monthly data.
#' The default window is 30.
#' @param X List of names of covariates.
#' @param Y_id Name of the outcome variable (String).
#' @param location_id Name of the location variable (String).
#' @param time_id Name of the time variable (String).
#' @param top_results Number of results to display
#'
#' @return
#' Data frame with the ordered list of best locations and their
#' average power.
#'
#' @export
GeoLiftPower.search <- function(data,
                                treatment_periods,
                                N = 1,
                                horizon = 30,
                                X = c(),
                                Y_id = "Units",
                                location_id = "location",
                                time_id = "time",
                                top_results = 5){

  cl <- parallel::makeCluster(parallel::detectCores() - 1)
  registerDoParallel(cl)

  parallel::clusterCall(cl, function()
    attachNamespace('augsynth'))
  parallel::clusterCall(cl, function()
    attachNamespace('dplyr'))
  parallel::clusterCall(cl, function()
    attachNamespace('tidyr'))

  parallel::clusterExport(
    cl,
    c('fn_treatment','pvalue','pvalueCalc', 'MarketSelection'),
    envir=environment()
  )

  # Part 1: Treatment and pre-treatment periods
  data <- data %>% dplyr::rename(Y = paste(Y_id), location = paste(location_id), time = paste(time_id))
  max_time <- max(data$time)
  data$location <- tolower(data$location)

  if (max(treatment_periods)/max_time > 0.8){
    message(paste0("Warning: Small pre-treatment period.
                   \nTthe treatment is larger that 80% of all available data."))
  }

  results <- data.frame(matrix(ncol=4,nrow=0))
  colnames(results) <- c("location","pvalue","duration",
                         "treatment_start")

  BestMarkets <- MarketSelection(data, location_id = "location", time_id = "time", Y_id = "Y")

  for (n in N){
    BestMarkets_aux <- BestMarkets[,1:n]
    for (test in 1:nrow(BestMarkets_aux)){ #iterate through lift %
      for (tp in treatment_periods){ #lifts
        t_n <- max(data$time) - tp + 1 #Number of simulations without extrapolation

        a <- foreach(sim = 1:(t_n-horizon),
                     .combine=cbind,
                     .errorhandling = 'remove') %dopar% {
                       pvalueCalc(
                         data,
                         sim,
                         max_time,
                         tp,
                         es = 0,
                         locations = as.list(BestMarkets_aux[test,]),
                         cpic = 0,
                         X)

                     }

        for (i in 1:ncol(a)) {
          results <- rbind(results, data.frame(location=a[[1,i]],
                                               pvalue = as.numeric(a[[2,i]]),
                                               duration = as.numeric(a[[3,i]]),
                                               treatment_start = as.numeric(a[[5,i]]) ) )
        }


      }
    }
  }

  resultsM <- results %>%
    dplyr::group_by(location) %>%
    dplyr::summarize(mean_p = mean(pvalue)) %>%
    dplyr::arrange(desc(mean_p))

  stopCluster(cl)
  class(results) <- c("GeoLift.search", class(resultsM))

  resultsM$rank <- 1:nrow(resultsM)

  if (top_results > nrow(resultsM)){
    top_results = nrow(resultsM)
  }

  print(paste0("Best ", top_results, "controls:"))
  print(head(resultsM[,1], top_results))

  return(resultsM)

}


#' GeoLift inference calculation.
#'
#' @description
#'
#' \code{GeoLift} performs inference for a geo-test.
#'
#' @param Y_id Name of the outcome variable (String).
#' @param time_id Name of the time variable (String).
#' @param location_id Name of the location variable (String).
#' @param X List of names of covariates.
#' @param data A data.frame containing the historical conversions by
#' geographic unit, It requires a "locations" column with the geo name,
#' a "Y" column with the outcome data (units), a time column with the indicator
#' of the time period (starting at 1), and covariates.
#' @param locations List of test locations.
#' @param treatment_start_time Time index of the start of the treatment.
#' @param treatment_end_time Time index of the end of the treatment.
#'
#' @return
#' GeoLift object that contains:
#'          \itemize{
#'          \item{"results":}{ Generalized Augmented Sunthetic Controls results.}
#'          \item{"inference":}{ Data frame with inference statistics (ATT, Lift, p-value, and Confidence Interval.)}
#'          \item{"data":}{ Input data.}
#'          \item{"y_obs":}{ Observed outcome metric.}
#'          \item{"y_hat":}{ Counterfactual outcome metric.}
#'          \item{"ATT":}{ ATT estimate.}
#'          \item{"ATT_se":}{ Standrd Error of the ATT estimate.}
#'          \item{"TreatmentStart":}{ Time id of treatment start.}
#'          \item{"TreatmentEnd":}{ Time id of treatment end.}
#'          \item{"test_id":}{ List of names of the test locations.}
#'          \item{"incremental":}{ Incremental outcome units (Obersved - Counterfactual).}
#'          \item{"Y_id":}{ Name of the outcome variable.}
#'          }
#'
#' @export
GeoLift <- function(Y_id = "Y",
                    time_id = "time",
                    location_id = "location",
                    X = c(),
                    data,
                    locations,
                    treatment_start_time,
                    treatment_end_time){

  # Rename variables to standard names used by GeoLift
  data <- data %>% dplyr::rename(time = time_id,
                          Y = Y_id,
                          location = location_id)

  data$location <- tolower(data$location)

  data_aux <- fn_treatment(data, locations = locations,
                           treatment_start_time,
                           treatment_end_time)


  if (length(X) == 0){
    fmla <- as.formula("Y ~ D")
  }
  else if (length(X) > 0){
    fmla <- as.formula(paste("Y ~ D |",
                             sapply(list(X),
                                    paste, collapse = "+")))

  }

  augsyn <- augsynth::augsynth(fmla, unit = location, time = time,
                     data = data_aux,
                     t_int = treatment_start_time,
                     progfunc = "GSYN", scm = T)

  inference_df <- data.frame(matrix(ncol=5, nrow=0))
  colnames(inference_df) <- c("ATT",
                              "Perc.Lift",
                              "pvalue",
                              "Lower.Conf.Int",
                              "Upper.Conf.Int")

  if(paste(augsyn$call)[1] == "single_augsynth"){
    mean <- summary(augsyn)[['average_att']][['Estimate']]
    se <- summary(augsyn)[['average_att']][['Std.Error']]

    loc_id <- c(which(augsyn$data$trt == 1))
    locs_id <- as.data.frame(loc_id, nrow = length(loc_id))
    locs_id$name <- unlist(locations)

    y_obs <- c(augsyn$data$X[loc_id,], augsyn$data$y[loc_id,])
    y_hat <- predict(augsyn)
    ATT <- predict(augsyn, ATT = T)
    ATT_se <- summary(augsyn)$att$Std.Error

    lift <- (sum(augsyn$data$y[loc_id,])-
               sum(predict(augsyn)[treatment_start_time:treatment_end_time])) /
      sum(predict(augsyn)[treatment_start_time:treatment_end_time])

    incremental <- sum(augsyn$data$y[loc_id,])-
      sum(predict(augsyn)[treatment_start_time:treatment_end_time])

    inference_df <- inference_df %>% tibble::add_row(ATT = mean,
                                             Perc.Lift = 100 * round(lift, 3),
                                             pvalue = pvalue(augsyn),
                                             Lower.Conf.Int = mean-qnorm(0.95,0,1)*se,
                                             Upper.Conf.Int = mean+qnorm(0.95,0,1)*se)
  }
  else if(paste(augsyn$call)[1] == "multisynth"){
    estimates <- summary(augsyn)$att %>% dplyr::filter(Level == "Average" & is.na(Time) == TRUE)
    mean <- estimates[['Estimate']]
    se <- estimates[['Std.Error']]

    loc_id <- c(which(augsyn$data$trt == min(augsyn$data$trt)))
    locs_id <- as.data.frame(loc_id, nrow = length(loc_id))
    locs_id$name <- augsyn$data$units[loc_id]

    y_obs <- t(cbind(augsyn$data$X[loc_id,],augsyn$data$y[loc_id,]))
    y_hat <- predict(augsyn)[1:max(data$time),-1]
    ATT <- matrix(summary(augsyn)$att$Estimate, ncol = (length(locations)+1))[1:max(data$time),]
    ATT_se <- matrix(summary(augsyn)$att$Std.Error, ncol = (length(locations)+1))[1:max(data$time),]

    lift <- (sum(augsyn$data$y[loc_id,]) -
               sum(predict(augsyn)[treatment_start_time:treatment_end_time,-1])) /
      sum(predict(augsyn)[treatment_start_time:treatment_end_time,-1])

    incremental <- sum(augsyn$data$y[loc_id,]) -
      sum(predict(augsyn)[treatment_start_time:treatment_end_time,-1])

    inference_df <- inference_df %>% tibble::add_row(ATT = mean,
                                             Perc.Lift = 100 * round(lift, 3),
                                             pvalue = pvalue(augsyn),
                                             Lower.Conf.Int = mean-qnorm(0.95,0,1)*se,
                                             Upper.Conf.Int = mean+qnorm(0.95,0,1)*se)
  }


  if(inference_df$pvalue < 0.05){
    significant <- "The results are significant at a 95% level."
  } else if(inference_df$pvalue < 0.10){
    significant <- "The results are significant at a 90% level."
  } else if(inference_df$pvalue < 0.20){
    significant <- "The results are significant at a 80% level."
  } else {
    significant <- "The results are not statistically significant."
  }

  res <- list("results" = augsyn,
              "inference" = inference_df,
              "data" = data_aux,
              "y_obs" = y_obs,
              "y_hat" = y_hat,
              "ATT" = ATT,
              "ATT_se" = ATT_se,
              "TreatmentStart" = treatment_start_time,
              "TreatmentEnd" = treatment_end_time,
              "test_id" = locs_id,
              "incremental" = incremental,
              "Y_id" = Y_id)

  message(paste0(
    paste0("\nGeoLift Output\n\n"),
    paste0("Test results for ", (treatment_end_time - treatment_start_time + 1),
           " treatment periods, from time-stamp ",
           treatment_start_time, " to ", treatment_end_time,
           " for test markets:")))
  #paste(toupper(locations), collapse = ", ")
  for (i in 1:length(locations)){
    message(paste(i, toupper(locations[i])))
  }
  message(paste0(
    "##################################",
    "\n#####     Test Statistics    #####\n",
    "##################################\n",
    "\nPercent Lift: ",
    100 * round(lift, 3), "%\n\n",
    "Incremental ", paste(Y_id), ": ", round(incremental,0), "\n\n",
    "Average Estimated Treatment Effect (ATT): ", round(mean, 3),
    "\n\n",significant,
    "\n\nThere is a ", round(100 * inference_df$pvalue, 2),
    "% chance of observing an effect this large or larger assuming treatment effect is zero.",
    sep="")
  )

  class(res) <- c("GeoLift", class(res))
  return(res)

}


#' Plot for GeoLift.
#'
#' @description
#'
#' Plot for GeoLift objects.
#'
#' @param GeoLift GeoLift object.
#' @param type Type of plot. By default "Lift" which plots the
#' incrementality on the outcome variable. If type is set to "ATT",
#' the average ATT is plotted.
#' @param outcome Name of the outcome variable. By default "Units".
#' @param conf.level Confidence level. By defaul 0.9.
#' @param test_locs Plot the average results by default. If set to
#' TRUE, the ATT by test location is plotted.
#' @param ... additional arguments
#'
#' @return
#' GeoLift plot.
#'
#' @export
plot.GeoLift <- function(GeoLift,
                         type="Lift",
                         outcome = "Units",
                         conf.level = 0.9,
                         test_locs = FALSE, ...) {

  if (!inherits(GeoLift, 'GeoLift')) {
    stop('object must be class GeoLift')
  }

  if (test_locs == TRUE & paste(GeoLift$results$call)[1] == "multisynth"){
    if (type == "ATT"){
      ATT.loc.plot(GeoLift, type="Lift", outcome = "Units", conf.level = 0.9, test_locs, ...)
    }
    else if (type == "Lift"){
      Lift.loc.plot(GeoLift, type="Lift", outcome = "Units", conf.level = 0.9, test_locs, ...)
    } else {
      message("Error: Please select a correct plot type: TreatmentSchedule/Lift/ATT")
    }
  }

  else if (type == "TreatmentSchedule"){
    panelView(Y ~ D, data = GeoLift$data, index = c("location", "time"), pre.post = TRUE)

  } else if (type == "ATT"){
    ATT.plot(GeoLift, conf.level = 0.9, ...)

  } else if (type == "Lift"){
    Lift.plot(GeoLift, outcome)

  } else {
    message("Error: Please select a correct plot type: TreatmentSchedule/Lift/ATT")
  }

}


#' Average ATT plot function for GeoLift.
#'
#' @description
#'
#' Average ATT plot function GeoLift.
#'
#' @param GeoLift GeoLift object.
#' @param outcome Name of the outcome variable. By default "Units".
#' @param conf.level Confidence level. By defaul 0.9.
#' @param ... additional arguments
#'
#' @return
#' ATT plot for GeoLift.
#'
#' @export
ATT.plot <- function(GeoLift,
                     outcome = "Units",
                     conf.level = 0.9, ...){

  if(paste(GeoLift$results$call)[1] == "single_augsynth"){
    df <- as.data.frame(GeoLift$ATT)
    colnames(df) <- c("ATT")
    df$Time <- 1:nrow(df)
    df$se <- GeoLift$ATT_se
    df$lower <- df$ATT-qnorm(1-((1-conf.level))/2,0,1)*df$se
    df$upper <- df$ATT+qnorm(1-(1-conf.level)/2,0,1)*df$se
    df$lower[1:(GeoLift$TreatmentStart-1)] <- 0
    df$upper[1:(GeoLift$TreatmentStart-1)] <- 0
    df$diff_lower <- df$ATT
    df$diff_upper <- df$ATT
    df$diff_lower[df$ATT > 0] <- 0
    df$diff_upper[df$ATT < 0] <- 0

    ymin <- min(min(df$lower), min(df$diff_lower))
    if (ymin < 0) {ymin <- 1.05*ymin
    } else {ymin <- 0.95 *ymin}
    ymax <- max(max(df$upper), max(df$diff_upper))
    if (ymax < 0) {ymax <- 0.95*ymax
    } else {ymax <- 1.05*ymax}

    ggplot(df,aes(y = ATT,x = Time)) +
      geom_line(color="steelblue4", size = 0.95) +
      geom_vline(xintercept = GeoLift$TreatmentStart,
                 linetype="dashed", size=0.8, color = "grey35") +
      geom_hline(yintercept = 0, linetype="dashed", size=0.8, color = "indianred3") +
      annotate("rect", xmin = GeoLift$TreatmentStart,
               ymin = ymin, xmax= GeoLift$TreatmentEnd,
               ymax = ymax, alpha=0.15) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_ribbon(aes(ymin = diff_lower, ymax = diff_upper),
                  alpha = 0.2, fill = "wheat3") +
      theme_minimal() +
      ylab("Average ATT")

  } else if(paste(GeoLift$results$call)[1] == "multisynth"){
    df <- as.data.frame(GeoLift$ATT[,1])
    colnames(df) <- c("ATT")
    df$Time <- 1:nrow(df)
    df$se <- GeoLift$ATT_se[,1]
    df$lower <- df$ATT-qnorm(1-((1-conf.level))/2,0,1)*df$se
    df$upper <- df$ATT+qnorm(1-(1-conf.level)/2,0,1)*df$se
    df$lower[1:(GeoLift$TreatmentStart-1)] <- 0
    df$upper[1:(GeoLift$TreatmentStart-1)] <- 0
    df$diff_lower <- df$ATT
    df$diff_upper <- df$ATT
    df$diff_lower[df$ATT > 0] <- 0
    df$diff_upper[df$ATT < 0] <- 0

    ymin <- min(min(df$lower), min(df$diff_lower))
    if (ymin < 0) {ymin <- 1.05*ymin
    } else {ymin <- 0.95 *ymin}
    ymax <- max(max(df$upper), max(df$diff_upper))
    if (ymax < 0) {ymax <- 0.95*ymax
    } else {ymax <- 1.05*ymax}

    ggplot(df,aes(y = ATT,x = Time)) +
      geom_line(color="steelblue4", size = 0.95) +
      geom_vline(xintercept = GeoLift$TreatmentStart,
                 linetype="dashed", size=0.8, color = "grey35") +
      geom_hline(yintercept = 0, linetype="dashed", size=0.8, color = "indianred3") +
      annotate("rect", xmin = GeoLift$TreatmentStart,
               ymin = ymin, xmax= GeoLift$TreatmentEnd,
               ymax = ymax, alpha=0.15) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_ribbon(aes(ymin = diff_lower, ymax = diff_upper),
                  alpha = 0.2, fill = "wheat3") +
      theme_minimal() +
      ylab("Average ATT")
  }

}


#' ATT plot function for GeoLift for each test location.
#'
#' @description
#'
#' ATT plot function for GeoLift for each test location.
#'
#' @param GeoLift GeoLift object.
#' @param conf.level Confidence level. By defaul 0.9.
#' @param ... additional arguments
#'
#' @return
#' ATT plot for GeoLift for each test location.
#'
#' @export
ATT.loc.plot <- function(GeoLift,
                         conf.level = 0.9, ...){
  index <- 2

  for (test in GeoLift$test_id$name){

    df <- as.data.frame(GeoLift$ATT[,index])
    colnames(df) <- c("ATT")
    df$Time <- 1:nrow(df)
    df$se <- GeoLift$ATT_se[,index]
    df$lower <- df$ATT-qnorm(1-((1-conf.level))/2,0,1)*df$se
    df$upper <- df$ATT+qnorm(1-(1-conf.level)/2,0,1)*df$se
    df$lower[1:(GeoLift$TreatmentStart-1)] <- 0
    df$upper[1:(GeoLift$TreatmentStart-1)] <- 0
    df$diff_lower <- df$ATT
    df$diff_upper <- df$ATT
    df$diff_lower[df$ATT > 0] <- 0
    df$diff_upper[df$ATT < 0] <- 0

    ymin <- min(min(df$lower), min(df$diff_lower))
    if (ymin < 0) {ymin <- 1.05*ymin
    } else {ymin <- 0.95 *ymin}
    ymax <- max(max(df$upper), max(df$diff_upper))
    if (ymax < 0) {ymax <- 0.95*ymax
    } else {ymax <- 1.05*ymax}

    plot <- ggplot(df,aes(y = ATT,x = Time)) +
      geom_line(color="steelblue4", size = 0.95) +
      geom_vline(xintercept = GeoLift$TreatmentStart,
                 linetype="dashed", size=0.8, color = "grey35") +
      geom_hline(yintercept = 0, linetype="dashed", size=0.8, color = "indianred3") +
      annotate("rect", xmin = GeoLift$TreatmentStart,
               ymin = ymin, xmax= GeoLift$TreatmentEnd,
               ymax = ymax, alpha=0.15) +
      geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
      geom_ribbon(aes(ymin = diff_lower, ymax = diff_upper),
                  alpha = 0.2, fill = "wheat3") +
      theme_minimal() +
      ylab(paste("Average ATT | ", toupper(GeoLift$test_id$name[(index - 1)])))


    print(plot)
    index <- index + 1

  }

}


#' Aggregate Lift plot function for GeoLift.
#'
#' @description
#'
#' Aggregate Lift plot function for GeoLift.
#'
#' @param GeoLift GeoLift object.
#' @param outcome Name of the outcome variable. By default "Units".
#' @param ... additional arguments
#'
#' @return
#' Aggregate Lift plot.
#'
#' @export
Lift.plot <- function(GeoLift,
                      outcome, ...) {

  if(paste(GeoLift$results$call)[1] == "single_augsynth"){
    y_obs <- as.data.frame(GeoLift$y_obs)
    colnames(y_obs) <- c("Units")
    y_obs$Time <- 1:nrow(y_obs)
    y_hat <- as.data.frame(GeoLift$y_hat)
    colnames(y_hat) <- c("Units")
    y_hat$Time <- 1:nrow(y_hat)

    df <- y_obs %>%  dplyr::mutate(Type = 'Observed') %>%
      bind_rows(y_hat %>% dplyr::mutate(Type = 'Estimated'))

    ymin <- min(df$Units)
    if (ymin < 0) {ymin <- 1.05*ymin
    } else {ymin <- 0.95 *ymin}
    ymax <- max(df$Units)
    if (ymax < 0) {ymax <- 0.95*ymax
    } else {ymax <- 1.05*ymax}

    ggplot(df,aes(y = Units,x = Time, group = Type, size=Type)) +
      geom_line(aes(linetype = Type, color = Type)) +
      geom_vline(xintercept = GeoLift$TreatmentStart,
                 linetype="dashed", size=0.8, color = "grey35") +
      annotate("rect", xmin = GeoLift$TreatmentStart,
               ymin = ymin, xmax= GeoLift$TreatmentEnd,
               ymax = ymax, alpha=0.15) +
      scale_linetype_manual(values=c("dashed", "solid")) +
      scale_color_manual(values=c('indianred1', 'grey20')) +
      scale_size_manual(values=c(1.1, 0.8)) +
      theme_minimal() +
      ylab(paste(outcome))

  } else if (paste(GeoLift$results$call)[1] == "multisynth"){
    y_obs <- as.data.frame(rowSums(GeoLift$y_obs))
    colnames(y_obs) <- c("Units")
    y_obs$Time <- 1:nrow(y_obs)
    y_hat <- as.data.frame(rowSums(GeoLift$y_hat))
    colnames(y_hat) <- c("Units")
    y_hat$Time <- 1:nrow(y_hat)

    df <- y_obs %>%  dplyr::mutate(Type = 'Observed') %>%
      bind_rows(y_hat %>% dplyr::mutate(Type = 'Estimated'))

    ymin <- min(df$Units)
    if (ymin < 0) {ymin <- 1.05*ymin
    } else {ymin <- 0.95 *ymin}
    ymax <- max(df$Units)
    if (ymax < 0) {ymax <- 0.95*ymax
    } else {ymax <- 1.05*ymax}

    ggplot(df,aes(y = Units,x = Time, group = Type, size=Type)) +
      geom_line(aes(linetype = Type, color = Type)) +
      geom_vline(xintercept = GeoLift$TreatmentStart,
                 linetype="dashed", size=0.8, color = "grey35") +
      annotate("rect", xmin = GeoLift$TreatmentStart,
               ymin = ymin, xmax= GeoLift$TreatmentEnd,
               ymax = ymax, alpha=0.15) +
      scale_linetype_manual(values=c("dashed", "solid")) +
      scale_color_manual(values=c('indianred1', 'grey20')) +
      scale_size_manual(values=c(1.1, 0.8)) +
      theme_minimal() +
      ylab(paste(outcome))
  }

}


#' Lift plot function for GeoLift for each test location.
#'
#' @description
#'
#' Lift plot function for GeoLift for each test location.
#'
#' @param GeoLift GeoLift object.
#' @param outcome Name of the outcome variable. By default "Units".
#'
#' @return
#' Lift plot function for GeoLift for each test location.
#' @param ... additional arguments
#'
#' @export
Lift.loc.plot <- function(GeoLift,
                          outcome, ...) {

  index <- 2

  for (test in GeoLift$test_id$name){
    y_obs <- as.data.frame(GeoLift$y_obs[,(index - 1)])
    colnames(y_obs) <- c("Units")
    y_obs$Time <- 1:nrow(y_obs)
    y_hat <- as.data.frame(GeoLift$y_hat[,(index - 1)])
    colnames(y_hat) <- c("Units")
    y_hat$Time <- 1:nrow(y_hat)

    df <- y_obs %>%  dplyr::mutate(Type = 'Observed') %>%
      bind_rows(y_hat %>% dplyr::mutate(Type = 'Estimated'))

    ymin <- min(df$Units)
    if (ymin < 0) {ymin <- 1.05*ymin
    } else {ymin <- 0.95 *ymin}
    ymax <- max(df$Units)
    if (ymax < 0) {ymax <- 0.95*ymax
    } else {ymax <- 1.05*ymax}

    plot <- ggplot(df,aes(y = Units,x = Time, group = Type, size=Type)) +
      geom_line(aes(linetype = Type, color = Type)) +
      geom_vline(xintercept = GeoLift$TreatmentStart,
                 linetype="dashed", size=0.8, color = "grey35") +
      annotate("rect", xmin = GeoLift$TreatmentStart,
               ymin = ymin, xmax= GeoLift$TreatmentEnd,
               ymax = ymax, alpha=0.15) +
      scale_linetype_manual(values=c("dashed", "solid")) +
      scale_color_manual(values=c('indianred1', 'grey20')) +
      scale_size_manual(values=c(1.1, 0.8)) +
      theme_minimal() +
      ylab(paste(outcome, " | ", toupper(GeoLift$test_id$name[(index - 1)]) ))

    print(plot)
    index <- index + 1
  }

}


#' Summary method for GeoLift.
#'
#' @description
#'
#' GeoLift summary output with additional information about the
#' test.
#'
#' @param GeoLift GeoLift object.
#'
#' @return
#' GeoLift summary object that contains:
#'      \itemize{
#'          \item{"ATT":}{ ATT estimate.}
#'          \item{"PercLift":}{ Lift estimate}
#'          \item{"pvalue":}{ Experiment p-value.}
#'          \item{"LowerCI":}{ Lower Confidence Interval.}
#'          \item{"UpperCI":}{ Upper Confidence Interval.}
#'          \item{"GlobalL2Imbalance":}{ Global L2 Imbalance.}
#'          \item{"GlobalL2ImbalanceScaled":}{ Scaled Global L2 Imbalance.}
#'          \item{"IndL2Imbalance":}{ Individual L2 Imbalance.}
#'          \item{"IndL2ImbalanceScaled":}{ Scaled Individual L2 Imbalance.}
#'          \item{"ATT":}{ ATT.}
#'          \item{"start":}{ Treatment start.}
#'          \item{"end":}{ Treatment end.}
#'          \item{"type":}{ Single or Multiple test locations.}
#'          \item{"Y_id":}{ Name of the outcome variable.}
#'          \item{"incremental":}{ Incremental outcome units.}
#'       }
#'
#' @export
summary.GeoLift <- function(GeoLift){

  if (!inherits(GeoLift, 'GeoLift')) {
    stop('object must be class GeoLift')
  }

  summ <- list()

  if(paste(GeoLift$results$call)[1] == "single_augsynth"){
    summ$ATT_est <- GeoLift$inference$ATT
    summ$PercLift <- GeoLift$inference$Perc.Lift
    summ$pvalue <- GeoLift$inference$pvalue
    summ$LowerCI <- GeoLift$inference$Lower.Conf.Int
    summ$UpperCI <- GeoLift$inference$Upper.Conf.Int
    summ$IndL2Imbalance <- summary(GeoLift$results)$l2_imbalance
    summ$IndL2ImbalanceScaled <- summary(GeoLift$results)$scaled_l2_imbalance
    summ$ATT <- summary(GeoLift$results)$att
    summ$start <- GeoLift$TreatmentStart
    summ$end <- GeoLift$TreatmentEnd
    summ$type <- "single"
    summ$Y_id <- GeoLift$Y_id
    summ$incremental <- GeoLift$incremental
  }

  else if (paste(GeoLift$results$call)[1] == "multisynth"){
    summ$ATT_est <- GeoLift$inference$ATT
    summ$PercLift <- GeoLift$inference$Perc.Lift
    summ$pvalue <- GeoLift$inference$pvalue
    summ$LowerCI <- GeoLift$inference$Lower.Conf.Int
    summ$UpperCI <- GeoLift$inference$Upper.Conf.Int
    summ$GlobalL2Imbalance <- summary(GeoLift$results)$global_l2
    summ$GlobalL2ImbalanceScaled <- summary(GeoLift$results)$scaled_global_l2
    summ$IndL2Imbalance <- summary(GeoLift$results)$ind_l2
    summ$IndL2ImbalanceScaled <- summary(GeoLift$results)$scaled_ind_l2
    summ$ATT <- summary(GeoLift$results)$att
    summ$start <- GeoLift$TreatmentStart
    summ$end <- GeoLift$TreatmentEnd
    summ$type <- "multi"
    summ$Y_id <- GeoLift$Y_id
    summ$incremental <- GeoLift$incremental
  }

  class(summ) <- "summary.GeoLift"

  return(summ)

}


#' Summary plotting method for GeoLift.
#'
#' @description
#'
#' Summary plotting method for GeoLift.
#'
#' @param summ GeoLift summary object.
#'
#' @return
#'
#' Plot GeoLift summary.
#'
#' @export
print.summary.GeoLift <- function(summ){

  if (!inherits(summ, 'summary.GeoLift')) {
    stop('object must be class summary.GeoLift')
  }


  message("\nGeoLift Results Summary\n")

  if(summ$type == "single"){
    message(paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\n* Average ATT: ", round(summ$ATT_est,3),
      "\n* Percent Lift: ", round(summ$PercLift,2),"%",
      "\n* Incremental ", paste(summ$Y_id), ": ",round(summ$incremental,0),
      "\n* P-value: ", round(summ$pvalue,2),
      "\n* 90% Confidence Interval: (", round(summ$LowerCI,3),
      ", ", round(summ$UpperCI,3), ")",

      "\n\n##################################",
      "\n#####   Balance Statistics   #####\n",
      "##################################",
      "\n* L2 Imbalance: ", round(summ$IndL2Imbalance,3),
      "\n* Scaled L2 Imbalance: ", round(summ$IndL2ImbalanceScaled,3)
    )
    )

  }

  else if (summ$type == "multi"){
    message(paste0(
      "##################################",
      "\n#####     Test Statistics    #####\n",
      "##################################\n",
      "\n* Average ATT: ", round(summ$ATT_est,3),
      "\n* Percent Lift: ", round(summ$PercLift,2),"%",
      "\n* Incremental ", paste(summ$Y_id), ": ",round(summ$incremental,0),
      "\n* P-value: ", round(summ$pvalue,2),
      "\n* 90% Confidence Interval: (", round(summ$LowerCI,2),
      ", ", round(summ$UpperCI,2), ")",

      "\n\n##################################",
      "\n#####   Balance Statistics   #####\n",
      "##################################\n",
      "\n* Global L2 Imbalance: ", round(summ$GlobalL2Imbalance,3),
      "\n* Scaled Global L2 Imbalance: ", round(summ$GlobalL2ImbalanceScaled,3),
      "\n* Individual L2 Imbalance: ", round(summ$IndL2Imbalance,3),
      "\n* Scaled Individual L2 Imbalance: ", round(summ$IndL2ImbalanceScaled,3),

      "\n\n##################################",
      "\n#####  Results By Location   #####\n",
      "##################################"
    )
    )
    for (level in unique(summ$ATT$Level)){
      sig <- "+++"
      z <- summ$ATT[summ$ATT$Level == level & is.na(summ$ATT$Time),]$Estimate /
        summ$ATT[summ$ATT$Level == level & is.na(summ$ATT$Time),]$Std.Error
      p <- 2*(1-pnorm(abs(z)))
      if (p <= 0.01) {
        sig <- "***"
      } else if (p <= 0.05){
        sig <- "**"
      } else if (p <= 0.1) {
        sig <- "*"
      } else if (p <= 0.2) {
        sig <- "'"
      } else { sig <- ""}

      if (level == "Average"){
        message(paste0("\nGlobal Avg. ATT: ",
                       round(summ$ATT[summ$ATT$Level == level & is.na(summ$ATT$Time),]$Estimate,3),
                       " ", sig
        )
        )
      } else{
        message(paste0("\n", toupper(level), " Avg. ATT: ",
                       round(summ$ATT[summ$ATT$Level == level & is.na(summ$ATT$Time),]$Estimate,3),
                       " ", sig
        )
        )
      }
    }
    message(paste0("\n***: Significant at a 99% Confidence Level",
                   "\n**: Significant at a 95% Confidence Level",
                   "\n*: Significant at a 90% Confidence Level",
                   "\n': Significant at an 80% Confidence Level"))
  }

}
