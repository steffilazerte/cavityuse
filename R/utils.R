n_lag <- function(var, n, dir = "forward", return = "matrix") {
  if(dir == "forward") fun <- dplyr::lead
  if(dir == "backward") fun <- dplyr::lag
  p <- matrix(sapply(0:n, FUN = function(x) fun(var, x)), ncol = n+1)

  if(return == "list") lapply(seq_len(nrow(p)), function(i) p[i,]) else p
}

check_cols <- function(x, cols) {
  if(!all(c(cols %in% names(x)))) {
    stop("Required columns missing from '", deparse(substitute(x)), "' (missing: '",
         paste0(cols, collapse = "', '"), "')", call. = FALSE)
  }
}

check_class <- function(x, c) {
 if(class(x) != c)  {
   if(class(x) %in% c("integer", "numeric") & c %in% c("integer", "numeric")) {
     return()
   }
   stop(deparse(substitute(x)), " is formated as ", class(x),
        ", but should be ", c, call. = FALSE)
 }
}


check_time <- function(t) {
  if(!lubridate::is.POSIXct(t)) {
    stop("time column is not in time class. ",
         "Consider the lubridate package to convert", call. = FALSE)
  } else if(is_dst(lubridate::tz(t))) {
    warning("Timezone has daylight savings. Consider using the `tz_offset()` ",
             "function to get the equivalent timezone without DST. Use the ",
             "lubridate::with_tz() function to transform your times")
  } else if (lubridate::tz(t) %in% c("UTC", "GMT")) {
    warning("Timezone is UTC/GMT, but cavityuse requires real local timezones ",
            "are used. Unnless this is the true timezone of the data, convert ",
            "to the local non-DST timezone.")
  }
}


check_date <- function(x) {
 if(!"date" %in% names(x) || !lubridate::is.Date(x$date)) {
  x$date <- lubridate::as_date(x$time)
 }
  x
}

check_data <- function(data) {
  if(!is.data.frame(data)) stop(deparse(substitute(data)),
                                " must be a data frame", call. = FALSE)
}

check_loc <- function(x, loc) {
  if(is.null(loc)) {
    if(!all(c("lon", "lat") %in% names(x))) {
      stop("lon/lat need to be in data frame or supplied as vector with 'loc'",
           call. = FALSE)
    } else {
      loc <- unlist(dplyr::distinct(x[, c("lon", "lat")]))
      if(length(loc) > 2) stop("Can only supply one location per data frame ",
                               "(i.e. one unique pair of lon/lat)",
                               call. = FALSE)
    }
  }
  loc
}

# From Andy Teucher
arg_match <- function(arg) {
  argname <- as.character(substitute(arg))
  choices <- eval(formals(sys.function(sys.parent()))[[argname]])
  stop_message <- paste0("'", argname, "' must be one of '",
                         paste(choices, collapse = "', '"), "'")

  if (is.null(arg))
    stop(stop_message, call. = FALSE)
  if (identical(arg, choices))
    return(arg[1L])
  if (length(arg) > 1L || !arg %in% choices) {
    stop(stop_message, call. = FALSE)
  }
  arg
}

check_tz <- function(tz) {
  if(!tz %in% OlsonNames()) {
    if(tolower(tz) %in% tolower(OlsonNames())) {
      t <- OlsonNames()[tolower(OlsonNames()) %in% tolower(tz)]
      message("Timezone ", tz, " not in OlsonNames(), assuming ", t)
      tz <- t
    } else {
      stop("Timezone: ", tz, " not in OlsonNames()", call. = FALSE)
    }
  }
  tz
}


#' Calculate non-daylight savings timezone offset from UTC
#'
#' @param tz Character. Timezone to calculate offset from
#'
#' @export
tz_offset <- function(tz) {
  tz <- check_tz(tz)

  t <- as.numeric(difftime(as.POSIXct("2016-01-01 00:00:00", tz = "UTC"),
                           as.POSIXct("2016-01-01 00:00:00", tz = tz), units = "hours"))

  if(t > 0) t <- paste0("Etc/GMT-", t)
  if(t <= 0) t <- paste0("Etc/GMT+", abs(t))
  t
}

is_dst <- function(tz) {
  tz <- check_tz(tz)
  t1 <- lubridate::with_tz(as.POSIXct("2018-01-01", tz = tz), "UTC")
  t2 <- lubridate::with_tz(as.POSIXct("2018-06-01", tz = tz), "UTC")
 if(lubridate::hour(t1) != lubridate::hour(t2)) TRUE else FALSE
}
