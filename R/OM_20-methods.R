################################################################################
# Copyright (C) 2019 by 52 North                                               #
# Initiative for Geospatial Open Source Software GmbH                          #
#                                                                              #
# Contact: Andreas Wytzisk                                                     #
# 52 North Initiative for Geospatial Open Source Software GmbH                 #
# Martin-Luther-King-Weg 24                                                    #
# 48155 Muenster, Germany                                                      #
# info@52north.org                                                             #
#                                                                              #
# This program is free software; you can redistribute and/or modify it under   #
# the terms of the GNU General Public License version 2 as published by the    #
# Free Software Foundation.                                                    #
#                                                                              #
# This program is distributed WITHOUT ANY WARRANTY; even without the implied   #
# WARRANTY OF MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU #
# General Public License for more details.                                     #
#                                                                              #
# You should have received a copy of the GNU General Public License along with #
# this program (see gpl-2.0.txt). If not, write to the Free Software           #
# Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA or #
# visit the Free Software Foundation web page, http://www.fsf.org.             #
#                                                                              #
# Author: Daniel Nuest (daniel.nuest@uni-muenster.de)                          #
# Created: 2010-09-08                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# construction methods ----
#




#' Class "OmObservationCollection"
#' 
#' Collection of arbitrary observations.
#' 
#' 
#' @name OmObservationCollection
#' @aliases OmObservationCollection OmObservationCollection-class
#' length,OmObservationCollection-method show,OmObservationCollection-method
#' sosResult,OmObservationCollection-method
#' print,OmObservationCollection-method toString,OmObservationCollection-method
#' [,OmObservationCollection-method
#' [[,OmObservationCollection,ANY,missing-method
#' as.list.OmObservationCollection length.OmObservationCollection
#' names.OmObservationCollection sosBoundedBy,OmObservationCollection-method
#' sosCoordinates,OmObservationCollection-method
#' sosProcedures,OmObservationCollection-method
#' sosFeatureIds,OmObservationCollection-method
#' sosObservedProperties,OmObservationCollection
#' sosObservedProperties,OmObservationCollection-method
#' sosFeaturesOfInterest,OmObservationCollection
#' sosFeaturesOfInterest,OmObservationCollection-method
#' as.SpatialPointsDataFrame.OmObservationCollection
#' sosGetCRS,OmObservationCollection-method
#' sosUOM,OmObservationCollection-method print.summary.OmObservationCollection
#' summary.OmObservationCollection sosObservableProperties,
#' OmObservation-method sosObservableProperties, OmObservationCollection-method
#' sosObservableProperties, SOS-method sosObservableProperties,
#' SosObservationOffering_2.0.0-method sosObservableProperties,
#' SweCompositePhenomenon-method sosObservableProperties,
#' SwePhenomenonProperty-method sosObservableProperties, list-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to the
#' construction function of the form \code{OmObservationCollection(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\linkS4class{OmObservation}} or
#' \code{\linkS4class{OmMeasurement}}.
#' @references Cox, S. (Ed.), Observations and Measurements - Part 1 -
#' Observation schema, Open Geospatial Consortium Inc., OGC 07-022r1, Version:
#' 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("OmObservationCollection")
#' 
#' 
#' @export OmObservationCollection
OmObservationCollection <- function(members, boundedBy) {
  new("OmObservationCollection", members = members, boundedBy = boundedBy)
}

OmOM_Observation <- function(phenomenonTime, procedure, observedProperty,
                             featureOfInterest, result, metadata = NA, resultTime = NULL,
                             resultQuality = NA,	parameter = NA) {
  new("OmOM_Observation", phenomenonTime = phenomenonTime, procedure = procedure,
      observedProperty = observedProperty,
      featureOfInterest = featureOfInterest, result = result,
      metadata = metadata, resultTime = resultTime,
      resultQuality = resultQuality,
      parameter = parameter)
}

OmObservationProperty <- function(href = as.character(NA), obs = NULL) {
  new("OmObservationProperty", href = href, obs = obs)
}





#' Class and Construction Function for om:Measurement Elements
#' 
#' Classes and construction functions for objects from the OGC Observations and
#' Measurements specification.
#' 
#' A Measurement contains a \code{\linkS4class{GmlMeasure}}.
#' 
#' @name OmMeasurement
#' @aliases OmMeasurement OmMeasurement-class show,OmMeasurement-method
#' print,OmMeasurement-method toString,OmMeasurement-method
#' sosResult,OmMeasurement-method sosProcedures,OmMeasurement-method
#' sosFeatureIds,OmMeasurement-method
#' sosFeaturesOfInterest,OmMeasurement-method sosGetCRS,OmMeasurement-method
#' names.OmMeasurement as.data.frame.OmMeasurement
#' as.SpatialPointsDataFrame.OmMeasurement sosUOM,OmMeasurement-method
#' @docType class
#' @usage OmMeasurement(samplingTime, procedure, observedProperty,
#' featureOfInterest, result, metadata = NA, resultTime = NULL, resultQuality =
#' NA, parameter = NA)
#' @param samplingTime See slot description.
#' @param procedure See slot description.
#' @param observedProperty See slot description.
#' @param featureOfInterest See slot description.
#' @param result See slot description.
#' @param metadata See slot description.
#' @param resultTime See slot description.
#' @param resultQuality See slot description.
#' @param parameter See slot description.
#' @return The construction functions return an object of the respective class.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction function of the form \code{OmMeasurement(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See also \code{\link{OmObservation-class}},
#' \code{\linkS4class{GmlMeasure}}.
#' @references Cox, S. (Ed.), Observations and Measurements - Part 1 -
#' Observation schema, Open Geospatial Consortium Inc., OGC 07-022r1, Version:
#' 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("OmMeasurement")
#' 
#' @export OmMeasurement
OmMeasurement <- function(samplingTime, procedure, observedProperty,
                          featureOfInterest, result, metadata = NA, resultTime = NULL,
                          resultQuality = NA,	parameter = NA) {
  new("OmMeasurement", samplingTime = samplingTime, procedure = procedure,
      observedProperty = observedProperty,
      featureOfInterest = featureOfInterest, result = result,
      metadata = metadata, resultTime = resultTime,
      resultQuality = resultQuality,
      parameter = parameter)
}


#
#
# Some problem with this function: Could not find function "getGeneric" ...
#setMethod(f = "length", signature = signature(x = "OmObservationCollection"),
#		def = function(x) {
#			.l <- length(x@members)
#			return(.l)
#		}
#)
length.OmObservationCollection <- function(x) {
  length(x@members)
}

setMethod(f = "[[", signature = signature(x = "OmObservationCollection",
                                          i = "ANY", j = "missing"), 
          def = function(x, i, j, ...) {
            if(is.numeric(i)) {
              return(x@members[[i]])
            }
            else {
              warning("Indexing only supported with numeric values!")
            }
          }
)

.getObservationsWithObservedProperty <- function(coll, obsProp) {
  .obsProperties <- sosObservedProperties(coll)
  
  if(any(is.na(.obsProperties))) {
    # warning("NA values in observed property list.")
    # remove NAs
    .obsProperties <- .obsProperties[which(!is.na(.obsProperties))]
  }
  
  if(length(.obsProperties) < 1)
    return(list())
  
  .idx <- c()
  
  for (i in seq(1:length(.obsProperties))) {
    if(is.list(.obsProperties[[i]])) {
      .current <- .obsProperties[[i]]
      #			cat(i, ": current:", .current, "\n")
      if(any(.current == obsProp)) {
        .idx <- c(.idx, i)
        #				cat("found index: ", i, ": ", .idx, "\n")
      }
    }
    else {
      if(.obsProperties[[i]] == obsProp) {
        .idx <- c(.idx, i)
        #				cat("found index: ", i, ": ", .idx, "\n")
      }
    }
  }
  #	cat("Found observed property ", obsProp, " at indices", .idx, "\n")
  if(length(.idx) == 0)
    return(list())
  else
    return(coll[.idx])
}
.getObservationsWithProcedure <- function(coll, procedure) {
  .procedures <- sosProcedures(coll)
  .idx <- which(.procedures %in% procedure)
  #	cat("Found procedure ", procedure, " at indices", .idx, "\n")
  if(length(.idx) == 0)
    return(list())
  else
    return(coll[.idx])
}
.getObservationsWithFoiId <- function(coll, foiId) {
  .featureIds <- sosFeatureIds(coll)
  .idx <- which(.featureIds %in% foiId)
  #	cat("Found foi ", foiId, " at indices", .idx, "\n")
  if(length(.idx) == 0)
    return(list())
  else
    return(coll[.idx])
}

setMethod(f = "[", signature = signature(x= "OmObservationCollection", 
                                         i = "ANY", j = "ANY"),
          def = function(x, i, j, ...) {
            if (missing(j)) {
              if(is.numeric(i)) {
                return(x@members[i])
              }
              else {
                #					cat("Try subsetting with", i, "\n")
                # subset the collection by procedure or observed property
                .byProc <- .getObservationsWithProcedure(x, i)
                #					cat("by procedures: ", toString(.byProc), "\n")
                if(length(.byProc) > 0)
                  return(.byProc)
                
                .byObsProp <- .getObservationsWithObservedProperty(x, i)
                #					cat("by obs prop: ", toString(.byObsProp), "\n")
                if(length(.byObsProp) > 0)
                  return(.byObsProp)
                
                .byFoiId <- .getObservationsWithFoiId(x, i)
                #					cat("by foi id: ", toString(.byObsProp), "\n")
                if(length(.byFoiId) > 0)
                  return(.byFoiId)
                
                return(list())
              }
            }
            else return(x@members[i:j])
          }
)

#
#
#
names.OmObservation <- function(x) {
  .name <- paste(sosProcedures(x), sosObservedProperties(x), sosFeatureIds(x),
                 sep = "_")
  names(.name) <- "proc_obsProp_foiID"
  return(.name)
}

names.OmMeasurement <- function(x) {
  .name <- paste(sosProcedures(x), sosObservedProperties(x), sosFeatureIds(x),
                 sep = "_")
  names(.name) <- "proc_obsProp_foiID"
  return(.name)
}

names.OmObservationCollection <- function(x) {
  .names <- sapply(x@members, names)
  return(.names)
}
