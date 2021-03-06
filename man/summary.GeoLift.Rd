% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GeoLift.R
\name{summary.GeoLift}
\alias{summary.GeoLift}
\title{Summary method for GeoLift.}
\usage{
\method{summary}{GeoLift}(object, ...)
}
\arguments{
\item{object}{GeoLift object.}

\item{...}{Optional arguments}
}
\value{
GeoLift summary object that contains:
     \itemize{
         \item{"ATT":}{ ATT estimate.}
         \item{"PercLift":}{ Lift estimate}
         \item{"pvalue":}{ Experiment p-value.}
         \item{"LowerCI":}{ Lower Confidence Interval.}
         \item{"UpperCI":}{ Upper Confidence Interval.}
         \item{"GlobalL2Imbalance":}{ Global L2 Imbalance.}
         \item{"GlobalL2ImbalanceScaled":}{ Scaled Global L2 Imbalance.}
         \item{"IndL2Imbalance":}{ Individual L2 Imbalance.}
         \item{"IndL2ImbalanceScaled":}{ Scaled Individual L2 Imbalance.}
         \item{"ATT":}{ ATT.}
         \item{"start":}{ Treatment start.}
         \item{"end":}{ Treatment end.}
         \item{"type":}{ Single or Multiple test locations.}
         \item{"Y_id":}{ Name of the outcome variable.}
         \item{"incremental":}{ Incremental outcome units.}
         \item{"bias":}{ Estimated corrected bias.}
         \item{"weights":}{ Synthetic Control Weights.}
         \item{"CI":}{ Flag indicating whether to include Confidence Intervals.}
         \item{"alpha":}{ Significance level.}
         \item{"lower":}{ Lower Bound Confidence Interval.}
         \item{"upper":}{ Upper Bound Confidence Interval.}
      }
}
\description{
GeoLift summary output with additional information about the
test.
}
