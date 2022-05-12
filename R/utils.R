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
  } else if (!lubridate::tz(t) %in% c("UTC", "GMT")) {
    stop("Timezone is not UTC/GMT. Most geolocators return time data as UTC. ",
         "Please convert your data back to UTC ",
         "(e.g., `data$time <- lubridate::with_tz(data$time, tz = \"UTC\")",
         call. = FALSE)
  }
}


check_date <- function(x) {
  x$date <- lubridate::as_date(x$time)
  x
}

check_data <- function(data, min_rows = TRUE) {
  if(!is.data.frame(data)) stop(deparse(substitute(data)),
                                " must be a data frame", call. = FALSE)
  if(min_rows && nrow(data) == 0) stop(deparse(substitute(data)),
                                       " must have more than 0 rows of data",
                                       call. = FALSE)

  n <- dplyr::if_else(nrow(data) > 60000, 60000L, nrow(data))
  if(any(dplyr::count(data[1:n,], .data$time)$n > 1)) {
    stop("There are duplicate times in ", deparse(substitute(data)), ". ",
         "Ensure make sure you are analyzing only one individual at a time",
         call. = FALSE)
  }
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

#' @export
tz_find_offset <- function(lon, lat) {
  lutz::tz_lookup_coords(lat[1], lon[1], method = "accurate") %>%
    lutz::tz_offset(lubridate::ymd("2020-01-01"), .) %>%
    dplyr::pull(.data$utc_offset_h)
}


is_dst <- function(tz) {
  tz <- check_tz(tz)
  t1 <- lubridate::with_tz(as.POSIXct("2018-01-01", tz = tz), "UTC")
  t2 <- lubridate::with_tz(as.POSIXct("2018-06-01", tz = tz), "UTC")
 if(lubridate::hour(t1) != lubridate::hour(t2)) TRUE else FALSE
}


#' @export
tz_apply_offset <- function(data, tz_offset, cols = "time") {
  if("offset_applied" %in% names(data) && all(data$offset_applied != 0)) {
    message("Removing previously assigned tz offset...")
    data <- tz_remove_offset(data)
  }
  if(any(!cols %in% names(data))) {
    stop("Cannot add tz offset: 'cols' not in data", call. = FALSE)
  }

  for(col in cols) {
    data <- dplyr::mutate(data,
                          !!col := .data[[col]] + lubridate::hours(!!tz_offset),
                          offset_applied = !!tz_offset)
  }
  data
}

#' @export
tz_remove_offset <- function(data, cols = "time") {
  if(!"offset_applied" %in% names(data)) {
    stop("Cannot remove tz offset: No offset applied", call. = FALSE)
  } else if(length(unique(data$offset_applied)) > 1) {
    stop("Cannot remove tz offset: More than one offset detected in the data...",
         call. = FALSE)
  } else if(any(!cols %in% names(data))) {
    stop("Cannot remove tz offset: 'cols' not in data", call. = FALSE)
  }

  for(col in cols) {
    data <- dplyr::mutate(data,
                          !!col := .data[[col]] - lubridate::hours(.data$offset_applied))
  }
  dplyr::mutate(data, offset_applied = 0)
}
