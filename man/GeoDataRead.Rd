% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeoLift.R
\name{GeoDataRead}
\alias{GeoDataRead}
\title{Data reading function for GeoLift.}
\usage{
GeoDataRead(
  data,
  date_id = "date",
  location_id = "location",
  Y_id = "units",
  format = "mm/dd/yyyy",
  X = c(),
  summary = FALSE
)
}
\arguments{
\item{data}{A data.frame containing the historical conversions by
geographic unit, It requires a "locations" column with the geo name,
a "Y" column with the outcome data (units), a time column with the indicator
of the time period (starting at 1), and covariates.}

\item{date_id}{Name of the date variable (String).}

\item{location_id}{Name of the location variable (String).}

\item{Y_id}{Name of the outcome variable (String).}

\item{format}{Format of the dates in the data frame.}

\item{X}{List of names of the covariates.}

\item{summary}{Display a summary of the data-reading process. FALSE by default.}
}
\value{
A data frame for GeoLift inference and power calculations.
}
\description{
\code{GeoDataRead} reads a data-frame and processes it for GeoLift.
The function will clean the data, generate a time variable that
increases by 1 for each time period (day/week/month), and aggregate
the data by time and location. It is important to have data for each
location and time-period and avoid special characters in the names of
the geographical units.
}
