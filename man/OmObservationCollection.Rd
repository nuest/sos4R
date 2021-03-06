\name{OmObservationCollection}
\Rdversion{1.1}

\docType{class}

\alias{OmObservationCollection}
\alias{OmObservationCollection-class}
\alias{length,OmObservationCollection-method}
\alias{show,OmObservationCollection-method}
\alias{sosResult,OmObservationCollection-method}
\alias{print,OmObservationCollection-method}
\alias{toString,OmObservationCollection-method}
\alias{[,OmObservationCollection-method}
\alias{[[,OmObservationCollection,ANY,missing-method}
\alias{as.list.OmObservationCollection}
\alias{length.OmObservationCollection}
\alias{names.OmObservationCollection}
\alias{sosBoundedBy,OmObservationCollection-method}
\alias{sosBoundedBy,SosObservationOffering_2.0.0-method}

\alias{sosCoordinates,OmObservationCollection-method}
\alias{sosProcedures,OmObservationCollection-method}
\alias{sosFeatureIds,OmObservationCollection-method}
\alias{sosObservedProperties,OmObservationCollection}
\alias{sosObservedProperties,OmObservationCollection-method}
\alias{sosFeaturesOfInterest,OmObservationCollection}
\alias{sosFeaturesOfInterest,OmObservationCollection-method}

\alias{sosGetCRS,OmObservationCollection-method}

\alias{sosUOM,OmObservationCollection-method}
\alias{print.summary.OmObservationCollection}
\alias{summary.OmObservationCollection}

\alias{sosObservableProperties, OmObservation-method}
\alias{sosObservableProperties, OmObservationCollection-method}
\alias{sosObservableProperties, SOS-method}
\alias{sosObservableProperties, SosObservationOffering_2.0.0-method}
\alias{sosObservableProperties, SweCompositePhenomenon-method}
\alias{sosObservableProperties, SwePhenomenonProperty-method}
\alias{sosObservableProperties, list-method}

\title{Class "OmObservationCollection"}

\description{
Collection of arbitrary observations.
}

\section{Objects from the Class}{
Objects can be created by calls to the construction function of the form \code{OmObservationCollection(...)}.
}

\section{Slots}{
  \describe{
    \item{\code{boundedBy}:}{Object of class \code{"list"} containing a representation of the bounding box of the contained observations.}
    \item{\code{members}:}{Object of class \code{"list"} containing objects of class \code{OmObservation} or \code{OmMeasurement}.}
  }
}

\section{Methods}{
  \describe{
    \item{length}{\code{signature(x = "OmObservationCollection")}: Returns the number of observations in the slot \code{members}.}
    \item{sosResult}{\code{signature(obj = "OmObservationCollection")}: Extract the result slots from the containted observations or measurements.}
	 }
}

\references{Cox, S. (Ed.), Observations and Measurements  - Part 1 - Observation schema, Open Geospatial Consortium Inc., OGC 07-022r1, Version: 1.0}

\author{Daniel Nuest \email{daniel.nuest@uni-muenster.de}}

\seealso{
\code{\linkS4class{OmObservation}} or \code{\linkS4class{OmMeasurement}}.
}

\examples{
showClass("OmObservationCollection")

}

\keyword{classes}
