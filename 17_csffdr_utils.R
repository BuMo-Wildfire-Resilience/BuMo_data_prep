# this contain the files for the tutorial with CSFFDR 


#source(fs::path("C:/r_repo/External_repos/cffdrs-ng/NG_FWI.r"))
#source("C:\\r_repo\\External_repos\\cffdrs-ng\\util.r")
#source("C:\\r_repo\\External_repos\\cffdrs-ng\\daily_summaries.R")
#' Various utility functions used by the other files
library(data.table)
library(lubridate)
#source("NG_FWI.r")












#' Determine if data is sequential at intervals of 1 unit
#'
#' @param data          data to check
#' @return              whether each entry in data is 1 unit from the next entry
isSequential <- function(data) {
  v <- na.omit(unique(data - data.table::shift(data, 1)))
  return(1 == v[[1]] && length(v) == 1)
}

#' Determine if data is sequential days
#'
#' @param df            data to check
#' @return              whether each entry is 1 day from the next entry
isSequentialDays <- function(df) {
  return(isSequential(as.Date(df$DATE)))
}

#' Determine if data is sequential hours
#'
#' @param df            data to check
#' @return              whether each entry is 1 hour from the next entry
isSequentialHours <- function(df) {
  return(isSequential(as.POSIXct(df$TIMESTAMP)))
}

#' Find specific humidity
#'
#' @param temp        Temperature (Celcius)
#' @param rh          Relative humidity (percent, 0-100)
#' @return            Specific humidity (g/kg)
findQ <- function(temp, rh) {
  # find absolute humidity
  svp <- 6.108 * exp(17.27 * temp / (temp + 237.3))
  vp <- svp * rh / 100.0
  return(217 * vp / (273.17 + temp))
}

#' Find relative humidity
#'
#'  @param q           Specific humidity (g/kg)
#'  @param temp        Temperature (Celcius)
#'  @return            Relative humidity (percent, 0-100)
findrh <- function(q, temp) {
  cur_vp <- (273.17 + temp) * q / 217
  return(100 * cur_vp / (6.108 * exp(17.27 * temp / (temp + 237.3))))
}


#' Find day of year. Does not properly deal with leap years.
#'
#' @param mon         Month
#' @param day         Day of month
#' @return            Day of year
julian <- function(mon, day) {
  month <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365)
  return(month[mon] + day)
}

solar_reduction <- function(DTR){
  #simple hardgraves model based on estimate of DTR
  #this uses the numbers DVK found
  #this is a reduction factor due to atmosphere  sol_surf/Sol_top_of_atm
  reduction <- 0.108*pow(DTR,0.59)
  return(reduction)
}

getSunlight <- function(df, with_solrad = FALSE, DST = FALSE) {
  dst_adjust <- 0
  if (DST){
    dst_adjust <- 1
  }
  
  df_copy <- copy(df)
  COLS_ID <- c("LAT", "LONG", "DATE", "TIMEZONE")
  cols_req <- c(COLS_ID, "TIMESTAMP")
  if (with_solrad) {
    cols_req <- c(cols_req, "TEMP")
  }
  for (n in cols_req) {
    stopifnot(n %in% colnames(df))
  }
  # just make date column so we know what type it is
  df_copy[, DATE := as_date(TIMESTAMP)]
  df_stn_dates <- unique(df_copy[, ..COLS_ID])
  dechour <- 12.0
  # calculate common values once
  df_dates <- unique(df_stn_dates[, list(DATE)])
  df_dates[, JD := julian(month(DATE), day(DATE))]
  df_dates[, FRACYEAR := 2.0 * pi / 365.0 * (JD - 1.0 + (dechour - 12.0) / 24.0)]
  df_dates[, EQTIME := 229.18 * (0.000075 + 0.001868 * cos(FRACYEAR) - 0.032077 * sin(FRACYEAR) - 0.014615 * cos(2.0 * FRACYEAR) - 0.040849 * sin(2.0 * FRACYEAR))]
  df_dates[, DECL := 0.006918 - 0.399912 * cos(FRACYEAR) + 0.070257 * sin(FRACYEAR) - 0.006758 * cos(FRACYEAR * 2.0) + 0.000907 * sin(2.0 * FRACYEAR) - 0.002697 * cos(3.0 * FRACYEAR) + 0.00148 * sin(3.0 * FRACYEAR)]
  df_dates[, ZENITH := 90.833 * pi / 180.0]
  # at this point we actually need the LAT/LONG/TIMEZONE
  df_dates <- merge(df_stn_dates, df_dates, by = c("DATE"))
  df_dates[, TIMEOFFSET := EQTIME + 4 * LONG - 60 * TIMEZONE]
  # FIX: is this some kind of approximation that can be wrong?
  #       breaks with (67.1520291504819, -132.37538245496188)
  df_dates[, X_TMP := cos(ZENITH) / (cos(LAT * pi / 180.0) * cos(DECL)) - tan(LAT * pi / 180.0) * tan(DECL)]
  # HACK: keep in range
  df_dates[, X_TMP := pmax(-1, pmin(1, X_TMP))]
  df_dates[, HALFDAY := 180.0 / pi * acos(X_TMP)]
  df_dates[, SUNRISE := (720.0 - 4.0 * (LONG + HALFDAY) - EQTIME) / 60 + TIMEZONE + dst_adjust]
  df_dates[, SUNSET := (720.0 - 4.0 * (LONG - HALFDAY) - EQTIME) / 60 + TIMEZONE + dst_adjust]
  df_all <- merge(df_copy, df_dates, by = COLS_ID)
  if (with_solrad) {
    df_all[, HR := hour(TIMESTAMP)]
    df_all[, TST := as.numeric(HR - dst_adjust) * 60.0 + TIMEOFFSET]
    df_all[, HOURANGLE := TST / 4 - 180]
    df_all[, ZENITH := acos(sin(LAT * pi / 180) * sin(DECL) + cos(LAT * pi / 180) * cos(DECL) * cos(HOURANGLE * pi / 180))]
    ###########################################################################################
    ##################################### DMC-UPDATE ##########################################
    ## calculateing solar radiation using Hargraeves model suggested at:
    ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
    df_all[, ZENITH := pmin(pi / 2, ZENITH)]
    # need later so keep column
    df_all[, COS_ZENITH := cos(ZENITH)]
    # Extraterrestrial solar radiation in kW/m^2
    df_all[, SOLRAD_EXT := 1.367 * COS_ZENITH]
    # Daily total of Extra. Solar Rad in kJ/m^2/day
    df_solrad <- df_all[, list(
      SOLRAD_EXT_SUM = sum(SOLRAD_EXT) * 3600,
      SUM_COS_ZENITH = sum(COS_ZENITH),
      TEMP_RANGE = max(TEMP) - min(TEMP)
    ), by = COLS_ID]
    
    # Daily surface Solar Rad in kJ/m^2/day
    df_solrad[, SOLRAD_DAY_SUM := 0.11 * SOLRAD_EXT_SUM * (TEMP_RANGE^0.59)]
    df_all <- merge(df_all, df_solrad, by = COLS_ID)
    # Hargreaves hourly surface solar rad in kW/m^2
    df_all[, SOLRAD := COS_ZENITH / SUM_COS_ZENITH * SOLRAD_DAY_SUM / 3600]
    # this was a reduction so it wasn't the full amount for the grass calculation?
    # df_all[, SOLRAD := 0.95 * cos(ZENITH)]
    df_all[, SOLRAD := pmax(0, SOLRAD)]
    df_all <- merge(df_all, df_solrad, by = COLS_ID)
  }
  # colnames(df_all) <- toupper(colnames(df_all))
  cols_sun <- intersect(c("SOLRAD", "SUNRISE", "SUNSET"), colnames(df_all))
  # don't include temporary calculations
  cols <- c(names(df), cols_sun)
  df_result <- df_all[, ..cols]
  df_result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  return(df_result)
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ##################################################################################### old code
  #df_copy <- copy(df)
  #COLS_ID <- c("LAT", "LONG", "DATE", "TIMEZONE")
  #cols_req <- c(COLS_ID, "TIMESTAMP")
  #if (with_solrad) {
  #  cols_req <- c(cols_req, "TEMP")
  #}
  #for (n in cols_req) {
  #  stopifnot(n %in% colnames(df))
  #}
  ## just make date column so we know what type it is
  #df_copy[, DATE := as_date(TIMESTAMP)]
  #df_stn_dates <- unique(df_copy[, ..COLS_ID])
  #dechour <- 12.0
  ## calculate common values once
  #df_dates <- unique(df_stn_dates[, list(DATE)])
  #df_dates[, JD := julian(month(DATE), day(DATE))]
  #df_dates[, FRACYEAR := 2.0 * pi / 365.0 * (JD - 1.0 + (dechour - 12.0) / 24.0)]
  #df_dates[, EQTIME := 229.18 * (0.000075 + 0.001868 * cos(FRACYEAR) - 0.032077 * sin(FRACYEAR) - 0.014615 * cos(2.0 * FRACYEAR) - 0.040849 * sin(2.0 * FRACYEAR))]
  #df_dates[, DECL := 0.006918 - 0.399912 * cos(FRACYEAR) + 0.070257 * sin(FRACYEAR) - 0.006758 * cos(FRACYEAR * 2.0) + 0.000907 * sin(2.0 * FRACYEAR) - 0.002697 * cos(3.0 * FRACYEAR) + 0.00148 * sin(3.0 * FRACYEAR)]
  #df_dates[, ZENITH := 90.833 * pi / 180.0]
  ## at this point we actually need the LAT/LONG/TIMEZONE
  #df_dates <- merge(df_stn_dates, df_dates, by = c("DATE"))
  #df_dates[, TIMEOFFSET := EQTIME + 4 * LONG - 60 * TIMEZONE]
  ## FIX: is this some kind of approximation that can be wrong?
  ##       breaks with (67.1520291504819, -132.37538245496188)
  #df_dates[, X_TMP := cos(ZENITH) / (cos(LAT * pi / 180.0) * cos(DECL)) - tan(LAT * pi / 180.0) * tan(DECL)]
  ## HACK: keep in range
  #df_dates[, X_TMP := pmax(-1, pmin(1, X_TMP))]
  #df_dates[, HALFDAY := 180.0 / pi * acos(X_TMP)]
  #df_dates[, SUNRISE := (720.0 - 4.0 * (LONG + HALFDAY) - EQTIME) / 60 + TIMEZONE]
  #df_dates[, SUNSET := (720.0 - 4.0 * (LONG - HALFDAY) - EQTIME) / 60 + TIMEZONE]
  #df_all <- merge(df_copy, df_dates, by = COLS_ID)
  #if (with_solrad) {
  #  df_all[, HR := hour(TIMESTAMP)]
  #  df_all[, TST := as.numeric(HR) * 60.0 + TIMEOFFSET]
  #  df_all[, HOURANGLE := TST / 4 - 180]
  #  df_all[, ZENITH := acos(sin(LAT * pi / 180) * sin(DECL) + cos(LAT * pi / 180) * cos(DECL) * cos(HOURANGLE * pi / 180))]
  #  ###########################################################################################
  #  ##################################### DMC-UPDATE ##########################################
  ### calculateing solar radiation using Hargraeves model suggested at:
  # ## (https://github.com/derekvanderkampcfs/open_solar_model/tree/V1#conclusions)
  # df_all[, ZENITH := pmin(pi / 2, ZENITH)]
  # # need later so keep column
  # df_all[, COS_ZENITH := cos(ZENITH)]
  # # Extraterrestrial solar radiation in kW/m^2
  # df_all[, SOLRAD_EXT := 1.367 * COS_ZENITH]
  # # Daily total of Extra. Solar Rad in kJ/m^2/day
  # df_solrad <- df_all[, list(
  #   SOLRAD_EXT_SUM = sum(SOLRAD_EXT) * 3600,
  #   SUM_COS_ZENITH = sum(COS_ZENITH),
  #   TEMP_RANGE = max(TEMP) - min(TEMP)
  # ), by = COLS_ID]
  # # Daily surface Solar Rad in kJ/m^2/day
  # df_solrad[, SOLRAD_DAY_SUM := 0.11 * SOLRAD_EXT_SUM * (TEMP_RANGE^0.59)]
  # df_all <- merge(df_all, df_solrad, by = COLS_ID)
  # # Hargreaves hourly surface solar rad in kW/m^2
  # df_all[, SOLRAD := COS_ZENITH / SUM_COS_ZENITH * SOLRAD_DAY_SUM / 3600]
  # # this was a reduction so it wasn't the full amount for the grass calculation?
  # # df_all[, SOLRAD := 0.95 * cos(ZENITH)]
  # df_all[, SOLRAD := pmax(0, SOLRAD)]
  # df_all <- merge(df_all, df_solrad, by = COLS_ID)
  #}
  #  # colnames(df_all) <- toupper(colnames(df_all))
  #cols_sun <- intersect(c("SOLRAD", "SUNRISE", "SUNSET"), colnames(df_all))
  ## don't include temporary calculations
  #cols <- c(names(df), cols_sun)
  #df_result <- df_all[, ..cols]
  #df_result[, SUNLIGHT_HOURS := SUNSET - SUNRISE]
  #return(df_result)
}

toDecimal <- function(t) {
  return(hour(t) + (minute(t) + (second(t) / 60.0)) / 60.0)
}

toDaily <- function(w, all = FALSE) {
  # split into morning and afternoon so we can assign rain to the proper fwi 'day'
  # NOTE: actually need to figure out what makes the most sense
  # - if we split at 12 then that means we're in LST not LDT
  # - the rain at 12 is from 1100-1200, so that should be part of today's calculation, not tomorrow's
  wx <- copy(w)
  # set DATE field in case there's only a TIMESTAMP
  wx[, DATE := as.character(as.Date(TIMESTAMP))]
  # use toDecimal() so we only need TIMESTAMP field and we can deal with minutes or even seconds
  wx[, FOR_DATE := ifelse(toDecimal(TIMESTAMP) <= 12, as.character(DATE), as.character(as.Date(DATE) + 1))]
  # wx[, FOR_DATE := DATE]
  precip <- wx[, list(PREC = sum(PREC, na.rm = TRUE)), by = c("FOR_DATE")]
  setnames(precip, "FOR_DATE", "DATE")
  merged <- merge(wx[toDecimal(TIMESTAMP) == 12, -c("FOR_DATE", "PREC")], precip, by = c("DATE"), all = all)
  merged$PREC <- nafill(merged$PREC, fill = 0.0)
  if (all) {
    # fix up columns that would be missing values if no noon value for a day
    merged[, TIMESTAMP := as_datetime(sprintf("%s 12:00:00", as.character(DATE)))]
    merged[, YR := year(TIMESTAMP)]
    merged[, MON := month(TIMESTAMP)]
    merged[, DAY := day(TIMESTAMP)]
    merged[, HR := hour(TIMESTAMP)]
    merged[, MINUTE := minute(TIMESTAMP)]
    merged[, ID := na.omit(unique(merged$ID)[[1]])]
    merged[, LAT := na.omit(unique(merged$LAT)[[1]])]
    merged[, LONG := na.omit(unique(merged$LONG)[[1]])]
    # use default drying day indices from weather guide
    merged$TEMP <- nafill(merged$TEMP, fill = 21.1)
    merged$RH <- nafill(merged$RH, fill = 45)
    merged$WS <- nafill(merged$WS, fill = 13)
  }
  return(merged)
}

seasonal_curing <- function(julian_date) {
  PERCENT_CURED <- c(96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0, 95.0, 93.0, 92.0, 90.5, 88.4, 84.4, 78.1, 68.7, 50.3, 32.9, 23.0, 22.0, 21.0, 20.0, 25.7, 35.0, 43.0, 49.8, 60.0, 68.0, 72.0, 75.0, 78.9, 86.0, 96.0, 96.0, 96.0, 96.0, 96.0, 96.0)
  jd_class <- (julian_date %/% 10) + 1
  first <- PERCENT_CURED[jd_class]
  last <- PERCENT_CURED[jd_class + 1]
  period_frac <- (julian_date %% 10) / 10.0
  return(first + (last - first) * period_frac)
}

save_csv <- function(df, file) {
  df <- data.table(df)
  COLS_ID <- c("id","wstind")
  COLS_LOC <- c("lat", "long")
  COLS_DATE <- c("yr", "mon", "day", "hr", "peak_time", "duration")
  COLS_RH <- c("rh")
  COLS_WX <- c("temp", "ws", "wind_speed_smoothed")
  COLS_PREC <- c("prec")
  COLS_SOLRAD <- c("solrad")
  COLS_INDICES <- c(
    "ffmc",
    "dmc",
    "dc",
    "isi",
    "bui",
    "fwi",
    "dsr",
    "gfmc",
    "gsi",
    "gfwi",
    "peak_isi_smoothed",
    "peak_gsi_smoothed"
  )
  COLS_SUN_TIMES <- c("sunrise", "sunset")
  COLS_EXTRA <- c("mcffmc", "mcgfmc")
  COLS_GFL <- c("grass_fuel_load")
  COLS_PC <- c("percent_cured")
  cols_used <- c()
  result <- copy(df)
  colnames(result) <- tolower(colnames(result))
  apply_format <- function(cols, fmt, as_num = FALSE) {
    fix_col <- Vectorize(function(x) {
      if (as_num) {
        x <- as.numeric(x)
      }
      # HACK: deal with negative 0
      return(gsub("^-0\\.0*$", "0.0", sprintf(fmt, x)))
    })
    
    for (col in names(result)) {
      # HACK: deal with min/max columns
      col_root <- gsub("_max", "", gsub("_min", "", col))
      if (col_root %in% cols) {
        cols_used <<- append(cols_used, col)
        result[[col]] <<- fix_col(result[[col]])
      }
    }
  }
  
  apply_format(COLS_ID, "%s")
  apply_format(COLS_LOC, "%.4f", TRUE)
  apply_format(COLS_DATE, "%02d", TRUE)
  apply_format(COLS_RH, "%.0f", TRUE)
  apply_format(COLS_WX, "%.1f", TRUE)
  apply_format(COLS_PREC, "%.2f", TRUE)
  apply_format(COLS_SOLRAD, "%.4f", TRUE)
  apply_format(COLS_INDICES, "%.1f", TRUE)
  apply_format(COLS_SUN_TIMES, "%s")
  apply_format(COLS_EXTRA, "%.4f", TRUE)
  apply_format(COLS_GFL, "%.2f", TRUE)
  apply_format(COLS_PC, "%.1f", TRUE)
  # order used columns based on original ordering
  cols <- intersect(names(result), cols_used)
  result <- result[, ..cols]
  write.csv(result, file, row.names = FALSE, quote = FALSE)
}

dmc_to_moisture_percent <- function(dmc) {
  MC <- 20 + exp(dmc - 244.72) / 43.43
  return(MC)
}



#' Computes hourly FWI indices for an input hourly weather stream
library(lubridate)
library(data.table)
#source("util.r")
#source("old_cffdrs.r")

DAILY_K_DMC_DRYING <- 1.894
DAILY_K_DC_DRYING <- 3.937

HOURLY_K_DMC <- 2.22
# HOURLY_K_DC <- 0.017066
# HOURLY_K_DMC <- 0.27
HOURLY_K_DC <- 0.085
DMC_OFFSET_TEMP <- 0.0
DC_OFFSET_TEMP <- 0.0

DC_DAILY_CONST <- 0.36
DC_HOURLY_CONST <- DC_DAILY_CONST / DAILY_K_DC_DRYING


OFFSET_SUNRISE <- 0 ##2.5
OFFSET_SUNSET <- 0 ##0.5

# Fuel Load (kg/m^2)
DEFAULT_GRASS_FUEL_LOAD <- 0.35

# default startup values
FFMC_DEFAULT <- 85
DMC_DEFAULT <- 6
DC_DEFAULT <- 15
GFMC_DEFAULT <- FFMC_DEFAULT 

# FIX: figure out what this should be
DEFAULT_LATITUDE <- 55.0
DEFAULT_LONGITUDE <- -120.0

# # just apply "daily" indices to noon directly
# HOUR_TO_START_FROM <- 12
# # result seemed to match better at noon so try starting from there instead
# # # start with daily indices at peak burn
# # HOUR_TO_START_FROM <- 16

MPCT_TO_MC <- 250.0 * 59.5 / 101.0
FFMC_INTERCEPT <- 0.5
DMC_INTERCEPT <- 1.5
DC_INTERCEPT <- 2.8

DATE_GRASS <- 181

# Fine Fuel Moisture Code (FFMC) from moisture %
fine_fuel_moisture_code <- function(moisture_percent) {
  return((59.5 * (250 - moisture_percent) / (MPCT_TO_MC + moisture_percent)))
}

# Fine Fuel Moisture (%) from FFMC
fine_fuel_moisture_from_code <- function(moisture_code) {
  return(MPCT_TO_MC * (101 - moisture_code) / (59.5 + moisture_code))
}

#' Calculate hourly Fine Fuel Moisture Code (FFMC)
#'
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param ws              Wind Speed (km/h)
#' @param rain            Rainfall (mm)
#' @param lastmc          Previous Fine Fuel Moisture (%)
#' @return                Hourly Fine Fuel Moisture (%)
hourly_fine_fuel_moisture <- function(temp, rh, ws, rain, lastmc) {
  # cur <- r[i + 1]
  # temp <- cur$temp
  # rh <- cur$rh
  # ws <- cur$ws
  # rain <- cur$prec
  # lastmc <- mcffmc
  # # 3.3,94.0,3.0,0.0,16.4
  # # 16.43770866,16.43770866,3.20393529,29.83789869,27.60476102,27.60476102
  # # 0.06000000,0.54065437,0.03531092,17.30973068
  rf <- 42.5
  drf <- 0.0579
  # Time since last observation (hours)
  time <- 1.0
  # use moisture directly instead of converting to/from ffmc
  # expects any rain intercept to already be applied
  mo <- lastmc
  if (rain != 0.0) {
    # duplicated in both formulas, so calculate once
    # lastmc == mo, but use lastmc since mo changes after first equation
    mo <- mo + rf * rain * exp(-100.0 / (251 - lastmc)) * (1.0 - exp(-6.93 / rain))
    if (lastmc > 150) {
      mo <- mo + 0.0015 * ((lastmc - 150)^2) * sqrt(rain)
    }
    if (mo > 250) {
      mo <- 250
    }
  }
  # duplicated in both formulas, so calculate once
  e1 <- 0.18 * (21.1 - temp) * (1.0 - (1.0 / exp(0.115 * rh)))
  ed <- 0.942 * (rh^0.679) + (11.0 * exp((rh - 100) / 10.0)) + e1
  ew <- 0.618 * (rh^0.753) + (10.0 * exp((rh - 100) / 10.0)) + e1
  # m = ed if mo >= ed else ew
  m <- ifelse(mo < ed,
              ew,
              ed
  )
  if (mo != ed) {
    # these are the same formulas with a different value for a1
    a1 <- ifelse(mo > ed,
                 rh / 100.0,
                 (100.0 - rh) / 100.0
    )
    k0_or_k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws) * (1 - (a1^8)))
    kd_or_kw <- (1.0/0.50)*drf * k0_or_k1 * exp(0.0365 * temp)
    m <- m + (mo - m) * (10^(-kd_or_kw * time))
  }
  return(m)
}

#' Calculate Initial Spread Index (ISI)
#'
#' @param ws              Wind Speed (km/h)
#' @param ffmc            Fine Fuel Moisure Code
#' @return                Initial Spread Index
initial_spread_index <- function(ws, ffmc) {
  fm <- fine_fuel_moisture_from_code(ffmc)
  fw <- ifelse(40 <= ws,
               12 * (1 - exp(-0.0818 * (ws - 28))),
               exp(0.05039 * ws)
  )
  ff <- 91.9 * exp(-0.1386 * fm) * (1.0 + (fm^5.31) / 4.93e07)
  isi <- 0.208 * fw * ff
  return(isi)
}

#' Calculate Build-up Index (BUI)
#'
#' @param dmc             Duff Moisture Code
#' @param dc              Drought Code
#' @return                Build-up Index
buildup_index <- function(dmc, dc) {
  bui <- ifelse(dmc == 0 & dc == 0,
                0,
                0.8 * dc * dmc / (dmc + 0.4 * dc)
  )
  # use ifelse so table works still
  bui <- ifelse(bui < dmc,
                {
                  p <- (dmc - bui) / dmc
                  cc <- 0.92 + ((0.0114 * dmc)^1.7)
                  dmc - cc * p
                },
                bui
  )
  bui <- ifelse(bui <= 0, 0, bui)
  return(bui)
}

#' Calculate Fire Weather Index (FWI)
#'
#' @param isi             Initial Spread Index
#' @param bui             Build-up Index
#' @return                Fire Weather Index
fire_weather_index <- function(isi, bui) {
  bb <- (0.1 * isi
         * ifelse(bui > 80,
                  1000 / (25 + 108.64 / exp(0.023 * bui)),
                  0.626 * (bui^0.809) + 2
         )
  )
  fwi <- ifelse(bb <= 1,
                bb,
                exp(2.72 * ((0.434 * log(bb))^0.647))
  )
  return(fwi)
}

daily_severity_rating <- function(fwi) {
  return(0.0272 * (fwi^1.77))
}

#' Calculate Hourly Grass Fuel Moisture. Needs to be converted to get GFMC.
#'
#' @param temp            Temperature (Celcius)
#' @param rh              Relative Humidity (percent, 0-100)
#' @param ws              Wind Speed (km/h)
#' @param rain            Rainfall (mm)
#' @param lastmc          Previous grass fuel moisture (percent)
#' @param solrad          Solar radiation (kW/m^2)
#' @return                Grass Fuel Moisture (percent)
hourly_grass_fuel_moisture <- function(temp, rh, ws, rain, solrad, lastmc) {
  # MARK II of the model (2016) wth new solar rad model specific to grass
  #
  # Temp is temperature in C
  # RH is realtive humidty in %
  # ws is average wind speed in km/h
  # rain is rainfall in mm
  # solrad is kW/m2  (radiaiton reaching fuel)
  # mo is the old grass fuel moisture   (not as a code value...so elimates the conversion to code)
  # time - time between obs in HOURS
  #
  #
  # DRF of 1/16.1 comes from reducting the standard response time curve
  # at 26.7C, 20%RH, 2 km/h to 0.85hr.
  #
  #
  #
  # bmw
  
  rf <- 0.27
  drf <- 0.389633
  # Time since last observation (hours)
  time <- 1.0
  mo <- lastmc
  mo <- ifelse(rain != 0,
               {
                 #     mo+=rain*rf*exp(-100.0/(251.0-mo))*(1.0-exp(-6.93/rain))*/ # old routine*/
                 # this new routine assumes layer is 0.3 kg/m2 so 0.3mm of rain adds +100%MC*/
                 # *100 to convert to %...  *1/.3 because of 0.3mm=100%
                 mo <- mo + (rain / 0.3 * 100.0)
                 mo <- ifelse(mo > 250.0, 250.0, mo)
                 mo
               },
               mo
  )
  
  # fuel temp from CEVW*/
  tf <- temp + 17.9 * solrad * exp(-0.034 * ws)
  
  # fuel humidity
  rhf <- ifelse(tf > temp,
                (rh * 6.107 * (10.0^(7.5 * temp / (temp + 237.0)))
                 / (6.107 * (10.0^(7.5 * tf / (tf + 237.0))))),
                rh
  )
  
  # 18.85749879,18.85749879,7.77659602,21.24361786,19.22479551,19.22479551
  # duplicated in both formulas, so calculate once
  e1 <- rf * (26.7 - tf) * (1.0 - (1.0 / exp(0.115 * rhf)))
  # GRASS EMC
  ed <- 1.62 * (rhf^0.532) + (13.7 * exp((rhf - 100) / 13.0)) + e1
  ew <- 1.42 * (rhf^0.512) + (12.0 * exp((rhf - 100) / 18.0)) + e1
  
  moed <- mo - ed
  moew <- mo - ew
  
  
  e <- NULL
  a1 <- NULL
  m <- NULL
  moe <- NULL
  if (moed == 0 || (moew >= 0 && moed <0)){
    m <- mo
    if(moed == 0){
      e <- ed
    }
    if (moew >= 0){
      e <- ew
    }
  }
  else{
    if(moed > 0){
      a1 <- rhf/100.0
      e <- ed
      moe <- moed
    }
    else{
      a1 <- (100.0-rhf)/100.0
      e <- ew
      moe <- moew
    }
    if (a1 < 0){
      #avoids complex number in a1^1.7 xkd calculation
      a1 <- 0
    }
    xkd <- (0.424*(1-a1^1.7)+(0.0694*sqrt(ws)*(1-a1^8)))
    xkd <- xkd*drf*exp(0.0365*tf)
    m <- e+moe*exp(-1.0*log(10.0)*xkd*time)
  }
  
  
  #  m <- ifelse(mo < ed && mo < ew,
  #    ew,
  #    ed
  #  )
  #  print(temp)
  #  print(solrad)
  #  print(ws)
  #  print(rh)
  #  # use ifelse so table works
  #  print(m)
  
  #  m <- ifelse(mo > ed || (mo < ed && mo < ew),
  #    {
  #      # these are the same formulas with a different value for a1
  #      a1 <- ifelse(mo > ed,
  #        rhf / 100.0,
  #        (100.0 - rhf) / 100.0
  #      )
  #      k0_or_k1 <- 0.424 * (1 - (a1^1.7)) + (0.0694 * sqrt(ws) * (1 - (a1^8)))
  #      kd_or_kw <- drf * k0_or_k1 * exp(0.0365 * tf)
  #      m + (mo - m) * (10^(-kd_or_kw * time))
  #      m
  #    },
  #    m
  #  )
  
  return(m)
}


Pign <- function(mc, wind2m, Cint, Cmc, Cws) {
  #  Thisd is the general standard form for the probability of sustained flaming models for each FF cover type
  #     here :
  #       mc is cured moisture (%) in the litter fuels being ignited
  #       wind2m (km/h)  is the estimated 2 metre standard height for wind at hte site of the fire ignition
  #       Cint, Cmc and Cws   are coefficients for the standard Pign model form for a given FF cover type
  
  #       return >> is the Probability of Sustained flaming from a single small flaming ember/brand
  Prob <- 1.0 / (1.0 + exp(-1.0 * (Cint + Cmc * mc + Cws * wind2m)))
  
  return(Prob)
}

curing_factor <- function(cur) {
  # cur is the percentage cure of the grass fuel complex.  100= fully cured
  #   ....The OPPOSITE (100-x) of greenness...
  
  #    This is the Cruz et al (2015) model with the original precision of the coefficent estimates
  #    and as in CSIRO code:https://research.csiro.au/spark/resources/model-library/csiro-grassland-models/
  cf <- ifelse(cur >= 20.0,
               1.036 / (1 + 103.989 * exp(-0.0996 * (cur - 20))),
               0.0
  )
  return(cf)
}




grass_moisture_code <- function(mc, cur, wind) {
  #   THIS is the way to get the CODE value from cured grassland moisture
  #     IT takes fully cured grass moisture  (from the grass moisture model (100% cured)  (from the FMS...updated version of Wotton 2009)
  #        and a estimate of the fuel complex curing (as percent cured)
  #        and estimated wind speed (necessary for a calc
  #     and calculated the probability of sustainable flaming ignition  (a funciton of MC  and wind)
  #     THEN it accounts for curing effect on probability of fire spread sustainability, using the curing factor from Cruz et al (2015) for grass
  #     THEN from this calcuates an 'effective moisture content' which is the moisture content that would be required to achieve
  #        the curing adjusted probabiltiy of sustained flaming if one were calcuating it directly through the standard Pign equation.
  #     and THEN converts this effective moisture content to a CODE value via the FF-scale the FFMC uses for consistency
  
  #     relies on models of:
  #        Prob of sustained flaming for grass model (PsusF(grass)
  #        and  the curing_factor  function
  #        AND and estiamte of open 10 m to 2 m wind reduction (0.75)...hardcoded in here now.....
  
  # MC is moisture content (%)
  # cur=percent curing of the grassland  (%)
  # wind=  10 m open wind (km/h)
  
  #     currently (NOv 2023) the coefficients for the PsusF(grass) models are hardcoded into the GFMC function
  wind2m_open_factor <- 0.75
  
  Intercept <- 1.49
  Cmoisture <- -0.11
  Cwind <- 0.075
  # GRASS: these coefficients (above) could change down the road .....explicitly coded in above*/
  # /* convert from 10 m wind in open to 2 m wind in open COULD be updated */
  wind2m <- wind2m_open_factor * wind
  
  probign <- Pign(mc, wind2m, Intercept, Cmoisture, Cwind)
  
  # /* adjust ignition diretctly with the curing function on ROS */
  newPign <- curing_factor(cur) * probign
  
  # /* now to back calc effective moisture - algebraically reverse the Pign equation*/
  # /* 250 is a saturation value just a check*/
  egmc <- ifelse(newPign > 0.0,
                 (log(newPign / (1.0 - newPign)) - Intercept - Cwind * wind2m) / Cmoisture,
                 250
  )
  # /*   convert to code with FF-scale */
  # return (59.5*(250.0-egmc)/(MPCT_TO_MC + egmc))
  if (egmc > 250.0){
    egmc <- 250.0
  }
  return(fine_fuel_moisture_code(egmc))
}


matted_grass_spread_ROS <- function(ws, mc, cur) {
  #  /*  CUT grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code
  #   We use this for MATTED grass in our post-winter context
  #   --ws=10 m open wind km/h
  #   --mc = moisture content in  cured grass  (%)
  #   --cur = percentage of grassland cured  (%)
  #   output should be ROS in m/min   */
  fw <- 16.67 * ifelse(ws < 5,
                       0.054 + 0.209 * ws,
                       1.1 + 0.715 * (ws - 5.0)**0.844
  )
  fm <- ifelse(mc < 12,
               exp(-0.108 * mc),
               ifelse(mc < 20.0 && ws < 10.0,
                      0.6838 - 0.0342 * mc,
                      ifelse(mc < 23.9 && ws >= 10.0,
                             0.547 - 0.0228 * mc,
                             0.0
                      )
               )
  )
  if (fm < 0){
    fm <- 0.0
  }
  cf <- curing_factor(cur)
  return(fw * fm * cf)
}


standing_grass_spread_ROS <- function(ws, mc, cur) {
  #  /*  standing grass  Rate  of spread from cheney 1998  (and new CSIRO grassland code)
  #   We use this for standing grass in our post-winter context
  #   ITS only the WIND function that chnges here between cut and standing
  #   --ws=10 m open wind km/h
  #   --mc = moisture content in grass  (%)
  #   --cur = percentage of grassland cured  (%)
  #   output should be ROS in m/min   */
  fw <- 16.67 * ifelse(ws < 5,
                       0.054 + 0.269 * ws,
                       1.4 + 0.838 * (ws - 5.0)**0.844
  )
  print_out <- c(mc,ws)
  print(print_out)
  fm <- ifelse(mc < 12,
               exp(-0.108 * mc),
               ifelse(mc < 20.0 && ws < 10.0,
                      0.6838 - 0.0342 * mc,
                      ifelse(mc < 23.9 && ws >= 10.0,
                             0.547 - 0.0228 * mc,
                             0.0
                      )
               )
  )
  if (fm < 0){
    fm <- 0.0
  }
  cf <- curing_factor(cur)
  return(fw * fm * cf)
}


#' Calculate Grass Spread Index (GSI)
#'
#' @param ws              Wind Speed (km/h)
#' @param mc              Grass moisture content (percent)
#' @param cur             Degree of curing (percent, 0-100)
#' @param standing        Grass standing (True/False)
#' @return                Grass Spread Index
grass_spread_index <- function(ws, mc, cur, standing) {
  #  So we don't have to transition midseason between standing and matted grass spread rate models
  #  We will simply scale   GSI   by the average of the   matted and standing spread rates
  #ros <- (matted_grass_spread_ROS(ws, mc, cur) + standing_grass_spread_ROS(ws, mc, cur)) / 2.0
  
  
  #now allowing switch between standing and matted grass
  ros <- NULL
  if (standing){
    #standing
    ros <- standing_grass_spread_ROS(ws, mc, cur)
  }
  else {
    #matted
    ros <- matted_grass_spread_ROS(ws, mc, cur)
  }
  
  return(1.11 * ros)
}


#' Calculate Grass Fire Weather Index
#'
#' @param gsi               Grass Spread Index
#' @param load              Fuel Load (kg/m^2)
#' @return                  Grass Fire Weather Index
grass_fire_weather_index <- Vectorize(function(gsi, load) {
  #  this just converts back to ROS in m/min
  ros <- gsi / 1.11
  Fint <- 300.0 * load * ros
  return(ifelse(Fint > 100,
                log(Fint / 60.0) / 0.14,
                Fint / 25.0
  ))
})

dmc_wetting <- function(rain_total, lastdmc) {
  if (rain_total <= DMC_INTERCEPT) {
    # no wetting if below intercept threshold
    return(0.0)
  }
  b <- ifelse(lastdmc <= 33,
              100.0 / (0.5 + 0.3 * lastdmc),
              ifelse(lastdmc <= 65,
                     14.0 - 1.3 * log(lastdmc),
                     6.2 * log(lastdmc) - 17.2
              )
  )
  rw <- 0.92 * rain_total - 1.27
  wmi <- 20 + 280 / exp(0.023 * lastdmc)
  wmr <- wmi + 1000 * rw / (48.77 + b * rw)
  dmc <- 43.43 * (5.6348 - log(wmr - 20))
  if (dmc <= 0.0) {
    dmc <- 0.0
  }
  # total amount of wetting since lastdmc
  w <- lastdmc - dmc
  return(w)
}

dc_wetting <- function(rain_total, lastdc) {
  if (rain_total <= DC_INTERCEPT) {
    # no wetting if below intercept threshold
    return(0.0)
  }
  rw <- 0.83 * rain_total - 1.27
  smi <- 800 * exp(-lastdc / 400)
  return(400.0 * log(1.0 + 3.937 * rw / smi))
  # # total amount of wetting since lastdc
  # w <- 400.0 * log(1.0 + 3.937 * rw / smi)
  # # don't wet more than lastdc regardless of drying since then
  # if (w > lastdc) {
  #   w <- lastdc
  # }
  # return(w)
}

dmc_wetting_between <- function(rain_total_previous, rain_total, lastdmc) {
  if (rain_total_previous >= rain_total) {
    return(0.0)
  }
  # wetting is calculated based on initial dmc when rain started and rain since
  current <- dmc_wetting(rain_total, lastdmc)
  # recalculate instead of storing so we don't need to reset this too
  # NOTE: rain_total_previous != (rain_total - cur$prec) due to floating point math
  previous <- dmc_wetting(rain_total_previous, lastdmc)
  return(current - previous)
}

dc_wetting_between <- function(rain_total_previous, rain_total, lastdc) {
  if (rain_total_previous >= rain_total) {
    return(0.0)
  }
  # wetting is calculated based on initial dc when rain started and rain since
  current <- dc_wetting(rain_total, lastdc)
  # recalculate instead of storing so we don't need to reset this too
  # NOTE: rain_total_previous != (rain_total - cur$prec) due to floating point math
  previous <- dc_wetting(rain_total_previous, lastdc)
  return(current - previous)
}

dmc_drying_ratio <- function(temp, rh) {
  return(pmax(0.0, HOURLY_K_DMC * (temp + DMC_OFFSET_TEMP) * (100.0 - rh) * 0.0001))
}

duff_moisture_code <- function(
    last_dmc,
    temp,
    rh,
    ws,
    rain,
    mon,
    hour,
    solrad,
    sunrise,
    sunset,
    dmc_before_rain,
    rain_total_prev,
    rain_total) {
  if (0 == rain_total) {
    dmc_before_rain <- last_dmc
  }
  # apply wetting since last period
  dmc_wetting_hourly <- dmc_wetting_between(
    rain_total_prev,
    rain_total,
    dmc_before_rain
  )
  # at most apply same wetting as current value (don't go below 0)
  dmc <- pmax(0.0, last_dmc - dmc_wetting_hourly)
  sunrise_start <- round(sunrise + OFFSET_SUNRISE)
  sunset_start <- round(sunset + OFFSET_SUNSET)
  dmc_hourly <- ifelse(hour >= sunrise_start & hour < sunset_start,
                       dmc_drying_ratio(temp, rh),
                       0.0
  )
  dmc <- dmc + dmc_hourly
  # HACK: return two values since C uses a pointer to assign a value
  return(list(dmc = dmc, dmc_before_rain = dmc_before_rain))
}

dc_drying_hourly <- function(temp) {
  return(pmax(0.0, HOURLY_K_DC * (temp + DC_OFFSET_TEMP)))
}



drought_code <- function(
    last_dc,
    temp,
    rh,
    ws,
    rain,
    mon,
    hour,
    solrad,
    sunrise,
    sunset,
    dc_before_rain,
    rain_total_prev,
    rain_total) {
  
  ##########################################################################################
  #### for now we are using Mike's method for calculating DC
  
  
  if (0 == rain_total){
    dc_before_rain <- last_dc
  }
  
  offset <- 3.0
  mult <- 0.015
  pe <- 0
  rw <- 0
  mr <- 0
  mcdc <- 0
  
  last_mc_dc <- 400*exp(-last_dc/400)
  TIME_INCREMENT <- 1.0
  if(temp > 0){
    pe <- mult*temp + offset/16.0
  }
  
  invtau <- pe/400.0
  
  if((rain_total_prev + rain) <= 2.8){
    mr <- last_mc_dc
  }
  else {
    if(rain_total_prev <= 2.8){
      rw <- (rain_total_prev + rain)*0.83 - 1.27
    }
    else{
      rw <- rain*0.83
    }
    mr <- last_mc_dc + 3.937*rw/2.0
  }
  
  if(mr > 400.0){
    mr <- 400.0
  }
  
  is_daytime <- FALSE
  if(hour >= sunrise && hour <= sunset){
    is_daytime = TRUE
  }
  
  if(is_daytime){
    mcdc <- 0.0 + (mr+0.0)*exp(-1.0*TIME_INCREMENT*invtau)
  }
  else{
    mcdc <- mr
  }
  
  if(mcdc > 400.0){
    mcdc <- 400.0
  }
  
  dc <- 400.0*log(400.0/mcdc)
  
  
  return(list(dc = dc, dc_before_rain = dc_before_rain))
  ##########################################################################################
  
  
  if (0 == rain_total) {
    dc_before_rain <- last_dc
  }
  # apply wetting since last period
  dc_wetting_hourly <- dc_wetting_between(rain_total_prev, rain_total, dc_before_rain)
  # at most apply same wetting as current value (don't go below 0)
  dc <- pmax(0.0, last_dc - dc_wetting_hourly)
  dc_hourly <- dc_drying_hourly(temp)
  # print(sprintf("last_dc=%0.2f, dc_wetting_hourly=%0.2f, dc=%0.2f, dc_hourly=%0.2f\n",
  #        last_dc,
  #        dc_wetting_hourly,
  #        dc,
  #        dc_hourly))
  dc <- dc + dc_hourly
  # HACK: return two values since C uses a pointer to assign a value
  return(list(dc = dc, dc_before_rain = dc_before_rain))
}


# Calculate number of drying "units" this hour contributes
drying_units <- function(temp, rh, ws, rain, solrad) {
  # for now, just add 1 drying "unit" per hour
  return(1.0)
}

rain_since_intercept_reset <- function(temp,
                                       rh,
                                       ws,
                                       rain,
                                       mon,
                                       hour,
                                       solrad,
                                       sunrise,
                                       sunset,
                                       canopy) {
  # for now, want 5 "units" of drying (which is 1 per hour to start)
  TARGET_DRYING_SINCE_INTERCEPT <- 5.0
  if (0 < rain) {
    # no drying if still raining
    canopy$drying_since_intercept <- 0.0
  } else {
    canopy$drying_since_intercept <- canopy$drying_since_intercept + drying_units(temp, rh, ws, rain, solrad)
    if (canopy$drying_since_intercept >= TARGET_DRYING_SINCE_INTERCEPT) {
      # reset rain if intercept reset criteria met
      canopy$rain_total <- 0.0
      canopy$drying_since_intercept <- 0.0
    }
  }
  canopy$rain_total_prev <- canopy$rain_total
  canopy$rain_total <- canopy$rain_total + rain
  return(canopy)
}



#' Calculate hourly FWI indices from hourly weather stream for a single station.
#'
#' @param     w               hourly values weather stream
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @return                    hourly values FWI and weather stream
.stnHFWI <- function(w, ffmc_old, dmc_old, dc_old) {
  
  if (!isSequentialHours(w)) {
    stop("Expected input to be sequential hourly weather")
  }
  if (length(na.omit(unique(w$ID))) != 1) {
    stop("Expected a single ID value for input weather")
  }
  if (length(na.omit(unique(w$LAT))) != 1) {
    stop("Expected a single LAT value for input weather")
  }
  if (length(na.omit(unique(w$LONG))) != 1) {
    stop("Expected a single LONG value for input weather")
  }
  r <- as.data.table(copy(w))
  names(r) <- tolower(names(r))
  mcffmc <- fine_fuel_moisture_from_code(ffmc_old)
  mcgfmc <- mcffmc
  mcgfmc_standing <- mcffmc
  mcgfmc_matted <- mcffmc
  # just use previous index values from current hour regardless of time
  # # HACK: always start from daily value at noon
  # while (12 != r[1]$hr) {
  #   r <- r[2:nrow(r)]
  # }
  # cur <- r[1]
  # dmc_old <- daily_duff_moisture_code(dmc_old, cur$temp, cur$rh, cur$prec, cur$lat, cur$mon)
  # dc_old <- daily_drought_code(dc_old, cur$temp, cur$rh, cur$prec, cur$lat, cur$mon)
  # # HACK: start from when daily value should be "accurate"
  # prec_accum <- 0.0
  # while (HOUR_TO_START_FROM != r[1]$hr) {
  #   # tally up precip between noon and whenever we're applying the indices
  #   prec_accum <- prec_accum + r[1]$prec
  #   r <- r[2:nrow(r)]
  # }
  # cur <- r[1]
  # # HACK: add precip tally to current hour so it doesn't get omitted
  # cur$prec <- cur$prec + prec_accum
  dmc_ <- list(dmc = dmc_old, dmc_before_rain = dmc_old)
  dc_ <- list(dc = dc_old, dc_before_rain = dc_old)
  # FIX: just use loop for now so it matches C code
  canopy <- list(
    rain_total = 0.0,
    rain_total_prev = 0.0,
    drying_since_intercept = 0.0
  )
  results <- NULL
  N <- nrow(r)
  for (i in 1:N)
  {
    
    cur <- copy(r[i])
    canopy <- rain_since_intercept_reset(
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$mon,
      cur$hr,
      cur$solrad,
      cur$sunrise,
      cur$sunset,
      canopy
    )
    # use lesser of remaining intercept and current hour's rain
    rain_ffmc <- ifelse(canopy$rain_total <= FFMC_INTERCEPT,
                        0.0,
                        ifelse((canopy$rain_total - FFMC_INTERCEPT) > cur$prec,
                               cur$prec,
                               canopy$rain_total - FFMC_INTERCEPT
                        )
    )
    mcffmc <- hourly_fine_fuel_moisture(cur$temp, cur$rh, cur$ws, rain_ffmc, mcffmc)
    cur$mcffmc <- mcffmc
    #  convert to code for output, but keep using moisture % for precision
    cur$ffmc <- fine_fuel_moisture_code(mcffmc)
    # not ideal, but at least encapsulates the code for each index
    dmc_ <- duff_moisture_code(
      dmc_$dmc,
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$mon,
      cur$hr,
      cur$solrad,
      cur$sunrise,
      cur$sunset,
      dmc_$dmc_before_rain,
      canopy$rain_total_prev,
      canopy$rain_total
    )
    cur$dmc <- dmc_$dmc
    dc_ <- drought_code(
      dc_$dc,
      cur$temp,
      cur$rh,
      cur$ws,
      cur$prec,
      cur$mon,
      cur$hr,
      cur$solrad,
      cur$sunrise,
      cur$sunset,
      dc_$dc_before_rain,
      canopy$rain_total_prev,
      canopy$rain_total
    )
    cur$dc <- dc_$dc
    cur$isi <- initial_spread_index(cur$ws, cur$ffmc)
    cur$bui <- buildup_index(cur$dmc, cur$dc)
    cur$fwi <- fire_weather_index(cur$isi, cur$bui)
    cur$dsr <- daily_severity_rating(cur$fwi)
    
    mcgfmc_matted <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec, cur$solrad, mcgfmc_matted)
    #for standing grass we make a come very simplifying assumptions based on obs from the field (echo bay study):
    #standing not really affected by rain -- to introduce some effect we introduce just a simplification of the FFMC Rain absorption function
    #which averages 6% or so for rains  (<5mm...between 7% and 5%,    lower for larger rains)(NO intercept)
    #AND the solar radiation exposure is less, and the cooling from the wind is stronger.  SO we assume there is effectively no extra
    #heating of the grass from solar
    #working at the margin like this should make a nice bracket for moisture between the matted and standing that users can use
    #...reality will be in between the matt and stand
    mcgfmc_standing <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec*0.06, 0.0, mcgfmc_standing)
    #mcgfmc <- hourly_grass_fuel_moisture(cur$temp, cur$rh, cur$ws, cur$prec, cur$solrad, mcgfmc)
    #print(mcgfmc)
    mcgfmc <- mcgfmc_standing
    standing <- TRUE
    if (julian(cur$mon, cur$day) < DATE_GRASS){
      standing <- FALSE
      mcgfmc <- mcgfmc_matted
    }
    
    
    cur$mcgfmc <- mcgfmc
    cur$gfmc <- grass_moisture_code(mcgfmc, cur$percent_cured, cur$ws)
    
    cur$gsi <- grass_spread_index(cur$ws, mcgfmc, cur$percent_cured, standing)
    cur$gfwi <- grass_fire_weather_index(cur$gsi, DEFAULT_GRASS_FUEL_LOAD)
    cur$grass_fuel_load <- DEFAULT_GRASS_FUEL_LOAD
    results <- rbind(results, cur)
  }
  return(results)
}

#' Calculate hourly FWI indices from hourly weather stream.
#'
#' @param     df_wx           hourly values weather stream
#' @param     timezone        integer offset from GMT to use for sun calculations
#' @param     ffmc_old        previous value for Fine Fuel Moisture Code
#' @param     dmc_old         previous value for Duff Moisture Code
#' @param     dc_old          previous value for Drought Code
#' @return                    hourly values FWI and weather stream
#' @export hFWI
hFWI <- function(df_wx, timezone, ffmc_old = 85, dmc_old = 6, dc_old = 15) {
  wx <- as.data.table(copy(df_wx))
  old_names <- colnames(wx)
  # add a bunch of dummy columns if they don't exist
  colnames(wx) <- toupper(colnames(wx))
  new_names <- colnames(wx)
  hadStn <- "ID" %in% colnames(wx)
  hadMinute <- "MINUTE" %in% colnames(wx)
  hadDate <- "DATE" %in% colnames(wx)
  hadLatitude <- "LAT" %in% colnames(wx)
  hadLongitude <- "LONG" %in% colnames(wx)
  hadTimestamp <- "TIMESTAMP" %in% colnames(wx)
  wasWind <- "WIND" %in% colnames(wx)
  wasRain <- "RAIN" %in% colnames(wx)
  wasYear <- "YEAR" %in% colnames(wx)
  wasHour <- "HOUR" %in% colnames(wx)
  if (!hadStn) {
    wx[, ID := "STN"]
  }
  if (!hadMinute) {
    wx[, MINUTE := 0]
  }
  if (!hadLatitude) {
    warning(paste0("Using default latitude value of ", DEFAULT_LATITUDE))
    wx[, LAT := DEFAULT_LATITUDE]
  }
  if (!hadLongitude) {
    warning(paste0("Using default longitude value of ", DEFAULT_LONGITUDE))
    wx[, LONG := DEFAULT_LONGITUDE]
  }
  if (wasWind) {
    setnames(wx, c("WIND"), c("WS"))
  }
  if (wasRain) {
    setnames(wx, c("RAIN"), c("PREC"))
  }
  if (wasYear) {
    setnames(wx, c("YEAR"), c("YR"))
  }
  if (wasHour) {
    setnames(wx, c("HOUR"), c("HR"))
  }
  if (!("PERCENT_CURED" %in% names(wx))) {
    wx$JULIAN <- julian(wx$MON, wx$DAY)
    wx$PERCENT_CURED <- seasonal_curing(wx$JULIAN)
  }
  cols_extra_solar <- intersect(names(wx), c("SUNRISE", "SUNSET", "SUNLIGHT_HOURS"))
  if (0 < length(cols_extra_solar)) {
    warning(sprintf("Ignoring and recalculating columns: [%s]", paste0(cols_extra_solar, collapse = ", ")))
    wx <- wx[, -..cols_extra_solar]
  }
  stopifnot(all(wx$RH >= 0 & wx$RH <= 100))
  stopifnot(all(wx$WS >= 0))
  stopifnot(all(wx$PREC >= 0))
  stopifnot(all(wx$MON >= 1 & wx$MON <= 12))
  stopifnot(all(wx$DAY >= 1 & wx$DAY <= 31))
  stopifnot(wx$SOLRAD >= 0)
  stopifnot(wx$GRASS_FUEL_LOAD >= 0)
  stopifnot(wx$PERCENT_CURED >= 0 & wx$PERCENT_CURED <= 100)
  stopifnot(ffmc_old >= 0 & ffmc_old <= 101)
  stopifnot(dmc_old >= 0)
  stopifnot(dc_old >= 0)
  # HACK: just rename for now
  # setnames(wx, c("PREC"), c("RAIN"))
  if (!hadDate) {
    wx[, DATE := as.character(as.Date(sprintf("%04d-%02d-%02d", YR, MON, DAY)))]
  }
  if (!hadTimestamp) {
    wx[, TIMESTAMP := as_datetime(sprintf("%04d-%02d-%02d %02d:%02d:00", YR, MON, DAY, HR, MINUTE))]
  }
  # loop in hFWI function
  write.csv(wx, "structured_input.csv")
  results <- NULL
  for (stn in unique(wx$ID)) {
    by_stn <- wx[ID == stn]
    for (yr in unique(by_stn$YR)) {
      by_year <- by_stn[YR == yr, ]
      print(paste0("Running ", stn, " for ", yr))
      # FIX: convert this to not need to do individual stations
      by_year[, TIMEZONE := timezone]
      needs_solrad <- FALSE
      if(!("SOLRAD" %in% names(by_year))){
        needs_solrad <- TRUE
      }
      w <- getSunlight(by_year, with_solrad = needs_solrad)
      write.csv(w, "structured_input.csv")
      r <- .stnHFWI(w, ffmc_old, dmc_old, dc_old)
      results <- rbind(results, r)
    }
  }
  
  #results <- results[,c('id', 'lat', 'long', 'yr', 'mon', 'day', 'hr', 'temp', 'rh', 'ws', 'prec', 'percent_cured', 'date', 'timestamp', 'timezone', 'solrad', 'sunrise', 'sunset', 'sunlight_hours', 'ffmc', 'dmc', 'dc', 'isi', 'bui', 'fwi', 'dsr', 'gfmc', 'gsi', 'gfwi')]
  
  # # this is all just to remove dummy variables that we added
  # if (!is.null(results)) {
  #   names(results) <- toupper(names(results))
  #   if (!hadStn) {
  #     results <- results[, -c("ID")]
  #   }
  #   if (!hadMinute) {
  #     results <- results[, -c("MINUTE")]
  #   }
  #   if (!hadDate) {
  #     results <- results[, -c("DATE")]
  #   }
  #   if (!hadLatitude) {
  #     results <- results[, -c("LAT")]
  #   }
  #   if (!hadLongitude) {
  #     results <- results[, -c("LONG")]
  #   }
  #   if (!hadTimestamp) {
  #     results <- results[, -c("TIMESTAMP")]
  #   }
  #   # if (wasWind) {
  #   #   setnames(results, c("WS"), c("WIND"))
  #   # }
  #   # if (wasRain) {
  #   #   setnames(results, c("PREC"), c("RAIN"))
  #   # }
  #   # if (wasYear) {
  #   #   setnames(results, c("YR"), c("YEAR"))
  #   # }
  #   # if (wasHour) {
  #   #   setnames(results, c("HR"), c("HOUR"))
  #   # }
  #   setnames(results, new_names, old_names)
  #   # setnames(results, c("PREC"), c("RAIN"))
  # }
  # names(results) <- tolower(names(results))
  # should have gotten rid of all the fields we added to make the processing work
  return(results)
}

# so this can be run via Rscript
if ("--args" %in% commandArgs()) {
  # parser <- ArgumentParser()
  # parser$add_argument()
  args <- commandArgs(trailingOnly = TRUE)
  if (6 == length(args)) {
    # args <- c("-6", "85", "6", "15", "./out/wx_diurnal_r.csv", "./out/wx_diurnal_fwi_r.csv")
    timezone <- as.double(args[1])
    ffmc_old <- as.double(args[2])
    dmc_old <- as.double(args[3])
    dc_old <- as.double(args[4])
    file_in <- args[5]
    file_out <- args[6]
    df_wx <- as.data.table(read.csv(file_in))
    df_fwi <- hFWI(
      df_wx,
      timezone = timezone,
      ffmc_old = ffmc_old,
      dmc_old = dmc_old,
      dc_old = dc_old
    )
    # reorganize columns
    colnames_out <- c(
      "lat",
      "long",
      "yr",
      "mon",
      "day",
      "hr",
      "temp",
      "rh",
      "ws",
      "prec",
      "date",
      "timestamp",
      "timezone",
      "solrad",
      "sunrise",
      "sunset",
      "ffmc",
      "dmc",
      "dc",
      "isi",
      "bui",
      "fwi",
      "dsr",
      "gfmc",
      "gsi",
      "gfwi",
      "mcffmc",
      "mcgfmc",
      "percent_cured",
      "grass_fuel_load"
    )
    print(colnames_out)
    if ("id" %in% names(df_fwi)) {
      colnames_out <- c("id", colnames_out)
    }
    df_fwi <- df_fwi[, ..colnames_out]
    save_csv(df_fwi, file_out)
  } else {
    if(args[1] != "SILENCE"){
      message("Wrong number of arguments") 
    }
  }
}

library(data.table)
library(lubridate)

#stops NG_FWI console from reporting wrong number of parameters
args <- c()
if("--args" %in% commandArgs()){
  args <- commandArgs(trailingOnly = TRUE)
  commandArgs <- function(...) c("SILENCE")  
}

#source("NG_FWI.r")
#source("util.r")


smooth_5pt <- function(source) {
  #binomial smoother  ... specifically for the 24 hour day
  #1pt = 1
  #3pt = (1 2 1) = 4
  #5pt = (1 4 6 4 1) = 16
  #7pt = (1 6 15 20 15 6 1) = 64
  
  cap <- length(source) #normally 24 edge cases on data set input though
  
  dest <- numeric(cap)
  
  dest[1] <- source[1]
  dest[cap] <- source[cap]
  
  miss <- 0
  for (i in 1:3){
    if (source[i] < -90.0){
      miss <- miss + 1
    }
  }
  if (miss == 0){
    dest[2] <- (0.25 * source[1]) + (0.5 * source[2]) + (0.25 * source[3])
  }
  else {
    dest[2] <- source[2]
  }
  
  
  for (i in 3:(cap-2)){
    miss <- 0
    for (j in (i-2):(i+2)){
      
      if (source[j] < -90.0){
        miss <- miss + 1
      }
    }
    if (miss == 0){
      dest[i] <- (1.0/16.0 * source[i - 2]) + (4.0/16.0 * source[i - 1]) + (6.0/16.0 * source[i]) + (4.0/16.0 * source[i + 1]) + (1.0/16.0 * source[i + 2])
    }
    else {
      dest[i] <- source[i]
    }
  }
  
  
  miss <- 0 
  for (i in (cap-2):cap){
    if (source[i] < -90.0){
      miss <- miss + 1
    }
  }
  if (miss == 0){
    dest[cap - 1] <- (0.25 * source[cap - 2]) + (0.5 * source[cap - 1]) + (0.25 * source[cap])
  }
  else {
    dest[cap - 1] <- source[cap - 1]
  }
  
  return(dest)
}


#smooth_7pt <- function(source) {
#binomial smoother  ... specifically for the 24 hour day
#1pt = 1
#3pt = (1 2 1) = 4
#5pt = (1 4 6 4 1) = 16
#7pt = (1 6 15 20 15 6 1) = 64


#  dest <- numeric(24)

#  dest[1] <- source[1]
#  dest[24] <- source[24]

#  miss <- 0
#  for (i in 1:3){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[2] <- (0.25 * source[1]) + (0.5 * source[2]) + (0.25 * source[3])
#  }
#  else {
#    dest[2] <- source[2]
#  }


#  miss <- 0
#  for (i in 1:5){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[3] <- (1.0/16.0 * source[1]) + (4.0/16.0 * source[2]) + (6.0/16.0 * source[3]) + (4.0/16.0 * source[4]) + (1.0/16.0 * source[5])
#  }
#  else {
#    dest[3] <- source[3]
#  }


#  for (i in 4:21){
#    miss <- 0
#    for (j in (i-3):(i+3)){
#      if (source[j] < -90.0){
#        miss <- miss + 1
#      }
#    }
#    if (miss == 0){
#      dest[i] <- (1.0/64.0 * source[i - 3]) + (6.0/64.0 * source[i - 2]) + (15.0/64.0 * source[i - 1]) + (20.0/64.0 * source[i]) + (15.0/64.0 * source[i + 1]) + (6.0/64.0 * source[i + 2]) + (1.0/64.0 * source[i + 3])
#    }
#    else {
#      dest[i] <- source[i]
#    }
#  }


#  miss <- 0
#  for (i in 20:24){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[22] <- (1.0/16.0 * source[20]) + (4.0/16.0 * source[21]) + (6.0/16.0 * source[22]) + (4.0/16.0 * source[23]) + (1.0/16.0 * source[24])
#  }
#  else {
#    dest[3] <- source[3]
#  }


#  miss <- 0 
#  for (i in 22:24){
#    if (source[i] < -90.0){
#      miss <- miss + 1
#    }
#  }
#  if (miss == 0){
#    dest[23] <- (0.25 * source[22]) + (0.5 * source[23]) + (0.25 * source[24])
#  }
#  else {
#    dest[23] <- source[23]
#  }


#  return(dest)
#}

#this function calculates a fake date based on 5am-5am instead of midnight to midnight
#used for generating daily summaries to make data look at 5am-5am instead
#output form is "year-julian_day", with the julian day rounded back if the time is before 5am
#for Jan 1st where the julian day rollback would make it zero it gets bumped to the last day of the previous year
pseudo_date <- function(year, month, day, hour) {
  
  
  
  adjusted_jd <- ifelse(hour >= 5, 
                        julian(month,day),
                        julian(month,day) - 1)
  adjusted_year <- ifelse(adjusted_jd == 0, 
                          year - 1,
                          year)
  adjusted_jd <- ifelse(adjusted_jd == 0,
                        julian(12,31),
                        adjusted_jd)
  
  out <- sprintf("%d-%d",adjusted_year,adjusted_jd)
  return(out)
  
}


generate_daily_summaries <- function(hourly_data){
  #note: need to account for spill over inlast day after pseudo_date calc where there is not 24 hours in the data
  Spread_Threshold_ISI <- 5.0
  
  results <- NULL
  for (stn in unique(hourly_data$id)) {
    print(stn)
    by_stn <- hourly_data[id == stn]
    by_stn[,pseudo_DATE := pseudo_date(yr,mon,day,hr)]
    
    for (p_date in unique(by_stn$pseudo_DATE)) {
      
      by_date <- by_stn[pseudo_DATE == p_date, ]
      
      peak_time_traditional_spot <- which(by_date$hr == 17)
      if(length(peak_time_traditional_spot) == 0){
        next
      }
      
      peak_time <- -1
      duration <- 0
      wind_smooth <- smooth_5pt(by_date$ws)
      peak_isi_smooth <- -1
      peak_gsi_smooth <- -1
      max_ffmc <- 0
      ffmc <- 0
      gfmc <- 0
      dmc <- 0
      dc <- 0
      isi <- 0
      gsi <- 0
      bui <- 0
      fwi <- 0
      gfwi<- 0
      dsr <- 0
      
      for (i in 1:nrow(by_date)){
        smooth_isi <- 0
        if (wind_smooth[i] > -90.0 && by_date[i,ffmc] > -90.0) {
          smooth_isi <- initial_spread_index(wind_smooth[i],by_date[i,ffmc])
        }
        else{
          smooth_isi <- -98.9
        }
        
        if (smooth_isi > peak_isi_smooth){
          peak_time <- i
          peak_isi_smooth <- smooth_isi
        }
        if (by_date[i,ffmc] > max_ffmc){
          max_ffmc <- by_date[i,ffmc]
        }
        if (smooth_isi > Spread_Threshold_ISI){
          duration = duration + 1
        }
      }
      
      
      if (smooth_isi < 5 && duration == 24){
        duration <- 0
      } 
      
      if (max_ffmc < 85.0) {
        peak_time <- peak_time_traditional_spot
      }
      
      ffmc <- by_date[peak_time, ffmc]
      dmc <- by_date[peak_time, dmc]
      dc <- by_date[peak_time, dc]
      isi <- by_date[peak_time, isi]
      bui <- by_date[peak_time, bui]
      fwi <- by_date[peak_time, fwi]
      dsr <- by_date[peak_time, dsr]
      smooth_ws_peak <- wind_smooth[peak_time]
      
      gfmc <- by_date[peak_time, gfmc]
      gsi <- by_date[peak_time, gsi]
      gfwi <- by_date[peak_time, gfwi]
      
      
      
      pick_year <- unique(by_date$yr)
      if(length(pick_year) > 1){
        pick_year <- pick_year[1]
      }
      pick_month <- unique(by_date$mon)
      if(length(pick_month) > 1){
        pick_month <- pick_month[1]
      }
      pick_day <- unique(by_date$day)
      if(length(pick_day) > 1){
        pick_day <- pick_day[1]
      }
      
      
      standing <- TRUE
      if (julian(pick_month, pick_day) < DATE_GRASS){
        standing <- FALSE
      }
      print_out <- c(pick_year, pick_month, pick_day, julian(pick_month,pick_day))
      print(print_out)
      peak_gsi_smooth <- grass_spread_index(smooth_ws_peak,gfmc,by_date[i,percent_cured], standing)
      
      
      sunrise_val <- by_date[peak_time, sunrise]
      sunset_val <- by_date[peak_time, sunset]
      
      peak_time <- by_date[peak_time,hr]
      
      sunrise <- sprintf("%d:%d", as.integer(sunrise_val), as.integer(60*(sunrise_val - as.integer(sunrise_val))))
      sunset <- sprintf("%d:%d", as.integer(sunset_val), as.integer(60*(sunset_val - as.integer(sunset_val))))
      
      daily_report <- c(unique(by_date$id), pick_year, pick_month, pick_day, peak_time, duration, smooth_ws_peak, peak_isi_smooth, peak_gsi_smooth, ffmc, dmc, dc, isi, bui, fwi, dsr, gfmc, gsi, gfwi, max_ffmc, sunrise, sunset)
      
      
      
      results <- rbind(results, daily_report)
      
      
      
    }
  }
  
  colnames(results) <- c("wstind","yr","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed", "peak_gsi_smoothed", "ffmc","dmc","dc","isi","bui","fwi","dsr", "gfmc", "gsi", "gfwi", "max_ffmc", "sunrise", "sunset")
  
  results <- results[,c("wstind","yr","mon","day","peak_time","duration","wind_speed_smoothed","peak_isi_smoothed","peak_gsi_smoothed","ffmc","dmc","dc","isi","bui","fwi","dsr", "gfmc", "gsi", "gfwi","sunrise", "sunset")]
  
  results <- data.frame(results)
  
  return(results)
}




# so this can be run via Rscript
print("echo test 1")
#pulled command line args out a top of script
if (length(args)>0){
  if (2 == length(args)) {
    # args: --input_file --output_file
    input <- args[1]
    output <- args[2]
    df_input <- as.data.table(read.csv(input))
    df_summaries <- generate_daily_summaries(df_input)
    save_csv(df_summaries, output)
  } 
  else {
    message("Wrong number of arguments: arguments are <input_file> <output_file>")
  }
}








