% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{tz_remove_offset}
\alias{tz_remove_offset}
\title{Remove timezone offset}
\usage{
tz_remove_offset(data, cols = "time")
}
\arguments{
\item{data}{Data frame with `cols` time columns#'}

\item{cols}{Character vector. Time columns from which to remove the offset}
}
\description{
If a timezone offset was applied with `tz_apply_offset()`, this function
will remove it.
}
