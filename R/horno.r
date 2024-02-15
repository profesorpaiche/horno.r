# ============================================================================ #
# Set of functions to calculate the heat wave magnitude index                  #
#                                                                              #
# Structure of the functions:                                                  #
#                                                                              #
# findWaves                                                                    #
# |                                                                            #
# +-- dailyMagnitude                                                           #
# |   |                                                                        #
# |   +-- magnitudeThresholds                                                  #
# |                                                                            #
# +-- groupWaves                                                               #
# |                                                                            #
# +-- waveDays                                                                 #
# |                                                                            #
# +-- extremeDays                                                              #
#     |                                                                        #
#     +-- dailyThresholds                                                      #
#                                                                              #
# Author: Dante Castro Garro                                                   #
# Date: 2023-08-07                                                             #
# ============================================================================ #

# Functions                                                                    #
# ============================================================================ #

#' Finding heat waves
#'
#' General function to find heat waves for a temperature time series. The
#' function can be used to find cold waves as well.
#'
#' @param date (vector.Date) Dates for each record.
#' @param temperature (vector.numeric) Temperature records.
#' @param reference (vector.integer) Range of years to use as reference for
#' the thresholds calcualtions.
#' @param type (character) Calculation for heat or cold waves.
#'
#' @return (data.frame) variables related to the heat or cold waves. date (Date)
#' same as input. temperature (numeric) same as input. wave_day (bool) is it a
#' wave day?. wave_event (integer) unique id for each wave event. magnitude_day
#' (numeric) daily magnitude. magnitude_event (numeric) total magntiude for
#' each wave event.
#'
#' @import dplyr
#' @importFrom lubridate year
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' waves <- findWaves(t2m_hamburg$date, t2m_hamburg$temperature)
#'
#' @export
#'
findWaves <- function(date, temperature, reference = c(1981, 2010), type = "heat wave", qthresh = NULL) {
    if (is.null(qthresh)) qthresh <- ifelse(type == "heat wave", 0.9, 0.1)
    ts <- tibble(date = date, temperature = temperature) %>%
        mutate(
            year = year(date),
            month = substring(as.character(date), 6, 7),
            day = substring(as.character(date), 9, 10),
            month_day = paste0(month, "-", day),
            extreme_day = extremeDays(date, temperature, reference, type, qthresh),
            wave_day = waveDays(extreme_day, date),
            wave_event = groupWaves(wave_day, date),
            magnitude_day = dailyMagnitude(date, temperature, reference, type)
        ) %>%
        group_by(wave_event) %>%
        mutate(magnitude_event = sum(magnitude_day)) %>%
        ungroup() %>%
        mutate(magnitude_event = ifelse(is.na(wave_event), NA, magnitude_event)) %>%
        select(temperature, date, wave_day, wave_event, magnitude_day, magnitude_event)
    return(ts)
}

#' Calculate daily magnitude
#'
#' Function that calculates the dialy mangitude of heat/cold wave days
#'
#' @param date (vector.Date) Dates for each record.
#' @param temperature (vector.numeric) Temperature records.
#' @param reference (vector.integer) Range of years to use as reference for
#' the thresholds calcualtions.
#' @param type (character) Calculation for heat or cold waves.
#'
#' @return (vector.numeric) Daily magntiude
#'
#' @import dplyr
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' magnitude <- dailyMagnitude(t2m_hamburg$date, t2m_hamburg$temperature)
#'
#' @export
#'
dailyMagnitude <- function(date, temperature, reference = c(1981, 2010), type = "heat wave") {
    thresholds <- magnitudeThresholds(date, temperature, reference = c(1981, 2010))
    p25 <- pull(thresholds, p25)
    p75 <- pull(thresholds, p75)
    ts <- tibble(temperature = temperature, date = date)

    # Threshold depending on the type of event
    if (type == "heat wave") {
        magnitude <- ts %>% mutate(magnitude = temperature - p25)
    } else if (type == "cold wave") {
        magnitude <- ts %>% mutate(magnitude = p75 - temperature)
    } else {
        stop("event_type must be either 'heat wave' or 'cold wave'")
    }
    magnitude <- magnitude %>%
        mutate(magnitude = magnitude / (p75 - p25)) %>%
        mutate(magnitude = ifelse(magnitude < 0, 0, magnitude)) %>%
        pull(magnitude)
    return(magnitude)
}

#' Mangitude threshold
#'
#' Calculate the required thresholds for the daily mangitude. The thresholds
#'  are based on the 25th and 75th percentile of annual maximum temperature.
#'
#' @param date (vector.Date) Dates for each record.
#' @param temperature (vector.numeric) Temperature records.
#' @param reference (vector.integer) Range of years to use as reference for
#' the thresholds calcualtions.
#'
#' @return (dataframe) 25th and 75th percentiles as data.frame
#'
#' @import dplyr
#' @importFrom lubridate year
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' mag_thresh <- dailyMagnitude(t2m_hamburg$date, t2m_hamburg$temperature)
#'
#' @export
#'
magnitudeThresholds <- function(date, temperature, reference = c(1981, 2010)) {
    ts <- tibble(date = date, temperature = temperature) %>%
        mutate(year = year(date))
    thresholds <- ts %>%
        filter(year %in% reference[1]:reference[2]) %>%
        group_by(year) %>%
        summarise(annual_max = max(temperature)) %>%
        ungroup() %>%
        summarize(
            p25 = quantile(annual_max, 0.25),
            p75 = quantile(annual_max, 0.75)
        )
    return(thresholds)
}

#' Numbering the events
#'
#' Giving uniques ID's to each wave event.
#'
#' @param wave_days (vector.bool) is it an extreme wave day?
#' @param date (vector.Date) Dates for each record.
#'
#' @return (vector.integer) Unique ID for each heat/cold wave. The rest of the
#'  days are filled with NA's
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' ext <- extremeDays(t2m_hamburg$date, t2m_hamburg$temperature)
#' wave_day <- waveDays(ext, t2m_hamburg$date)
#' wave_id <- groupWaves(wave_day, t2m_hamburg$date)
#'
#' @export
#'
groupWaves <- function(wave_days, date) {
    n <- length(wave_days)
    res <- rep(NA, n)
    events_true <- which(wave_days)
    ne <- 1
    res[events_true[1]] <- ne
    for (i in 2:length(events_true)) {
        if (date[events_true[i]] - date[events_true[i - 1]] > 1) ne <- ne + 1
        res[events_true[i]] <- ne
    }
    return(res)
}

#' Wave days
#'
#' Identification of days that belong to a heat/cold wave, which is defined by
#'  3 or more consecutive extreme days.
#'
#' @param extreme_day (vector.bool) is it an extreme (hot/cold) day?
#' @param date (vector.Date) Dates for each record.
#'
#' @return (vector.integer) is it a heat/cold wave day?
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' ext <- extremeDays(t2m_hamburg$date, t2m_hamburg$temperature)
#' wave_day <- waveDays(ext, t2m_hamburg$date)
#'
#' @export
#'
waveDays <- function(extreme_day, date) {
    n <- length(extreme_day)
    wave_day <- rep(NA, n)
    for (i in 3:n) {
        period = (i - 2):i
        if (any(diff(date[period]) > 1)) next
        if (any(is.na(extreme_day[period]))) next
        if (all(extreme_day[period])) {
            wave_day[period] <- TRUE
        } else {
            wave_day[i] <- FALSE
        }
    }
    return(wave_day)
}

#' Extreme hot/cold days
#'
#' Identification of extreme hot/cold days. An extreme day happens when the
#'  temperature exceeds a daily threshold.
#'
#' @param date (vector.Date) Dates for each record.
#' @param temperature (vector.numeric) Temperature records.
#' @param reference (vector.integer) Range of years to use as reference for
#'  the thresholds calcualtions.
#' @param type (character) Calculation for heat or cold waves.
#' @param qthresh (numeric) Quantile to be used as threshold. If not defined,
#'  a default value (0.9 or 0.1) will be used based on the `type` parameter.
#'
#' @return (vector.bool) is it a extreme hot or cold day?
#'
#' @import dplyr
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' ext <- extremeDays(t2m_hamburg$date, t2m_hamburg$temperature)
#'
#' @export
#'
extremeDays <- function(date, temperature, reference = c(1981, 2010), type = "heat wave", qthresh = 0.9) {
    # Calculating the daily threshold
    ts <- tibble(date = date, temperature = temperature) %>%
        mutate(daily_threshold = dailyThreshold(date, temperature, reference, qthresh))

    # Identification of the extreme days
    if (type == "heat wave") {
        ts <- ts %>% mutate(extreme_day = temperature >= daily_threshold)
    } else if (type == "cold wave") {
        ts <- ts %>% mutate(extreme_day = temperature <= daily_threshold)
    } else {
        stop("event_type must be either 'heat wave' or 'cold wave'")
    }
    extreme_days <- ts %>% pull(extreme_day)
    return(extreme_days)
}

#' Daily thresholds
#'
#' Calculate the calendar thresholds based on a quantile for a 31-day-centered
#'  window in the reference period.
#'
#' @param date (vector.Date) Dates for each record.
#' @param temperature (vector.numeric) Temperature records.
#' @param reference (vector.integer) Range of years to use as reference for
#'  the thresholds calcualtions.
#' @param qthresh (numeric) Quantile to be used as threshold. If not defined,
#'  a default value (0.9 or 0.1) will be used based on the `type` parameter.
#'
#' @return (vector.numeric) threshold for each calendar day
#'
#' @import dplyr
#' @importFrom lubridate year
#'
#' @author Dante Castro Garro
#'
#' @examples
#' data(t2m_hamburg)
#' day_thresh <- dailyThreshold(t2m_hamburg$date, t2m_hamburg$temperature)
#'
#' @export
#'
dailyThreshold <- function(date, temperature, reference = c(1981, 2010), qthresh = 0.9) {
    # Time series as a table with the date splitted
    ts <- tibble(date = date, temperature = temperature) %>%
        mutate(
            year = year(date),
            month_day = substring(as.character(date), 6, 10)
        )

    # Creating the 31-day window and calculating the threshold
    hsw <- 15
    month_day <- unique(ts$month_day)
    nmd <- length(month_day)
    day_thresh <- data.frame(month_day = month_day, threshold = NA)
    for (md in 1:nmd){
        dummy_date <- as.Date(paste0("2001-", month_day[md]), format = "%Y-%m-%d")
        window <- seq.Date(dummy_date - hsw, dummy_date + hsw, by = 1) %>%
            substring(6, 10)
        ts_windowed <- ts %>%
            filter(year %in% reference[1]:reference[2]) %>%
            filter(month_day %in% window)
        day_thresh$threshold[md] <- quantile(ts_windowed$temperature, qthresh)
    }

    # Combining threshold with original data
    ts_day_thresh <- left_join(ts, day_thresh, by = "month_day")
    daily_thresholds <- ts_day_thresh %>%
        pull(threshold) %>%
        unname()
    return(daily_thresholds)
}
