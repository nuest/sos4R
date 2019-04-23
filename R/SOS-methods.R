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
# Created: 2010-06-18                                                          #
# Project: sos4R - https://github.com/52North/sos4R                            #
#                                                                              #
################################################################################

#
# construction functions ----
#




#' Class, and Construction and Accessor Functions for "SOS"
#' 
#' Base class of a connection to a Sensor Observation Service.
#' 
#' From the introduction of the specification document: \dQuote{The goal of SOS
#' is to provide access to observations from sensors and sensor systems in a
#' standard way that is consistent for all sensor systems including remote,
#' in-situ, fixed and mobile sensors.}
#' 
#' @name SOS
#' @aliases SOS SOS-class show,SOS-method print,SOS-method toString,SOS-method
#' SOS_1.0.0 SOS_1.0.0-class show,SOS_1.0.0-method print,SOS_1.0.0-method
#' toString,SOS_1.0.0-method SOS_2.0.0 SOS_2.0.0-class show,SOS_2.0.0-method
#' print,SOS_2.0.0-method toString,SOS_2.0.0-method SOS_versioned
#' SOS_versioned-class sosCapabilitiesDocumentOriginal
#' sosCapabilitiesDocumentOriginal,SOS-method sosCaps sosCaps-methods
#' sosCaps,SOS-method sosContents sosContents-methods sosContents,SOS-method
#' sosDataFieldConverters sosDataFieldConverters-methods
#' sosDataFieldConverters,SOS-method sosDataFieldConverters,SOS_Test-method
#' sosTime sosTime-methods sosTime,SOS-method sosTime,list-method
#' sosFilter_Capabilities sosFilter_Capabilities-methods
#' sosFilter_Capabilities,SOS-method sosFeaturesOfInterest
#' sosFeaturesOfInterest-methods sosFeaturesOfInterest,SOS-method
#' sosFeaturesOfInterest,SOS,character-method
#' sosFeaturesOfInterest,SosObservationOffering-method sosBinding
#' sosBinding-methods sosBinding,SOS-method sosBinding,SOS_1.0.0-method
#' sosBinding,SOS_2.0.0-method sosObservedProperties
#' sosObservedProperties-methods sosObservedProperties,SOS-method
#' sosObservedProperties,SosObservationOffering-method sosOfferingIds
#' sosOfferingIds-methods sosOfferingIds,SOS-method sosOfferings
#' sosOfferings-methods sosOfferings,SOS-method
#' sosOfferings,SOS,character-method sosOperation sosOperation-methods
#' sosOperation,SOS,character-method sosOperationsMetadata
#' sosOperationsMetadata-methods sosOperationsMetadata,SOS-method sosParsers
#' sosParsers-methods sosParsers,SOS-method sosParsers,SOS_Test-method
#' sosProcedures sosProcedures-methods sosProcedures,SOS-method
#' sosProcedures,list-method sosProcedures,SosObservationOffering-method
#' sosResponseFormats sosResponseFormats-methods sosResponseFormats,SOS-method
#' sosResponseFormats,OwsOperation-method
#' sosResponseFormats,SosObservationOffering-method sosOutputFormats
#' sosOutputFormats-methods sosOutputFormats,SOS-method
#' sosOutputFormats,OwsOperation-method sosResponseMode sosResponseMode-methods
#' sosResponseMode,SOS-method sosResponseMode,OwsOperation-method
#' sosResponseMode,SosObservationOffering-method sosResultModels
#' sosResultModels-methods sosResultModels,SOS-method
#' sosResultModels,OwsOperation-method sosResult,character-method
#' sosServiceIdentification sosServiceIdentification-methods
#' sosServiceIdentification,SOS-method sosServiceProvider
#' sosServiceProvider-methods sosServiceProvider,SOS-method sosSrsName
#' sosSrsName-methods sosSrsName,SOS-method sosTimeFormat sosTimeFormat-methods
#' sosTimeFormat,SOS-method sosUrl sosUrl-methods sosUrl,SOS-method
#' sosUrl,SOS_1.0.0-method sosUrl,SOS_2.0.0-method sosVersion
#' sosVersion-methods sosVersion,SOS-method sosExceptionCodeMeaning
#' sosExceptionCodeMeaning,character-method sosBoundedBy sosBoundedBy-method
#' sosBoundedBy,list-method sosCoordinates sosCoordinates-method
#' sosCoordinates,list-method sosCoordinates,SosObservationOffering-method
#' sosId sosId-method sosId,list-method sosSrsName sosSrsName-method
#' sosFeatureIds sosFeatureIds-method sosFeatureIds,list-method
#' sosObservedProperties,list-method sosFeaturesOfInterest,list-method
#' sosGetCRS sosGetCRS-method sosGetCRS,SOS-method sosGetCRS,list-method
#' sosGetCRS,character-method sosGetCRS,SosObservationOffering-method sosName
#' sosName-method sosName,list-method sosName,OwsServiceProvider-method
#' sosName,OwsGetCapabilities-method sosName,OwsOperation-method
#' sosName,SosDescribeSensor-method sosName,SosGetObservation-method
#' sosName,SosGetObservationById-method sosTitle sosTitle-method
#' sosTitle,SOS-method sosAbstract sosAbstract-method sosAbstract,SOS-method
#' sosEncoders sosEncoders,SOS-method sosOperations sosOperations,SOS-method
#' sosOperations,OwsCapabilities-method
#' sosOperations,SosCapabilities_1.0.0-method
#' sosOperations,SosCapabilities_2.0.0-method sosGetDCP
#' sosGetDCP,SOS,character-method sosSwitchCoordinates
#' sosSwitchCoordinates-method sosSwitchCoordinates,SOS-method plot.SOS
#' plot,SOS,missing-method plot.SosObservationOffering
#' plot,SosObservationOffering,missing-method
#' as.SosObservationOffering.SpatialPolygons print.summary.SOS
#' print.summary.SosObservationOffering summary.SosObservationOffering
#' summary.SOS summary.SOS_versioned sosResult,data.frame-method sosUOM
#' sosUOM,list-method sosUOM,data.frame-method sosCapabilitiesUrl
#' sosCapabilitiesUrl-method sosCapabilitiesUrl,SOS-method SOS_Test
#' SOS_Test-class sosName,SosGetFeatureOfInterest_2.0.0-method
#' sosProcedures,SosObservationOffering_2.0.0-method
#' sosResponseFormats,SosObservationOffering_2.0.0-method SOS_2.0.0-class
#' print,SOS_2.0.0-method show,SOS_2.0.0-method sosBinding,SOS_2.0.0-method
#' sosUrl,SOS_2.0.0-method toString,SOS_2.0.0-method
#' @docType class
#' @usage SOS(url, binding = SosDefaultBinding(), version = sos100_version,
#' parsers = SosParsingFunctions(), encoders = SosEncodingFunctions(),
#' dataFieldConverters = SosDataFieldConvertingFunctions(), timeFormat =
#' sosDefaultTimeFormat, verboseOutput = FALSE, switchCoordinates = FALSE,
#' useDCPs = TRUE, dcpFilter = SosDefaultDCPs(), additionalKVPs = list(), ...)
#' 
#' SOS_Test(name = "test", binding = SosDefaultBinding(), version =
#' sos100_version, parsers = SosParsingFunctions(), encoders =
#' SosEncodingFunctions(), dataFieldConverters =
#' SosDataFieldConvertingFunctions(), timeFormat = sosDefaultTimeFormat,
#' verboseOutput = FALSE, switchCoordinates = FALSE, useDCPs = TRUE, dcpFilter
#' = SosDefaultDCPs(), additionalKVPs = list(), ...)
#' @param url See the corresponding slot description.
#' @param binding See the corresponding slot description.
#' @param version See the corresponding slot description.
#' @param parsers See the corresponding slot description.
#' @param encoders See the corresponding slot description.
#' @param dataFieldConverters See the corresponding slot description.
#' @param timeFormat See the corresponding slot description.
#' @param verboseOutput See the corresponding slot description.
#' @param switchCoordinates See the corresponding slot description.
#' @param useDCPs See the corresponding slot description.
#' @param dcpFilter See the corresponding slot description.
#' @param additionalKVPs See the corresponding slot description.
#' @param ... Additional parameters that are passed on to the
#' \code{\link{getObservation}} call that is done within this function.
#' @param name Name of the test SOS class.
#' @return The construction functions returns an object of class
#' \code{\link{SOS-class}}.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction function of the form \code{SOS(...)}.
#' 
#' Object from the class can be used in calls to function for metadata
#' retrieval of sensors (\code{link{describeSensor-methods}}) and observation
#' data queries (\code{link{getObservation-methods}} and
#' \code{link{getObservationById-methods}})
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See also creation function \code{\link{SOS}} and the package
#' vignette for general description of use.
#' @references Na, A., Priest, M. (Eds.), Sensor Observation Service, Open
#' Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0
#' 
#' The document is available for download at
#' \url{https://www.opengeospatial.org/standards/sos}.
#' @keywords classes
#' @examples
#' 
#' showClass("SOS")
#' showClass("SOS_2.0.0")
#' 
#' # create a SOS connection
#' mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
#'              binding = "KVP")
#' 
#' # create the URL to a GET request for GetCapabilities
#' sosCapabilitiesUrl(mySOS)
#' 
#' # access details of the SOS connection and it's metadata
#' sosContents(mySOS)
#' sosTime(mySOS)
#' sosFeaturesOfInterest(mySOS)
#' sosBinding(mySOS)
#' 
#' \dontrun{
#' # create a SOS connetion with a specific connection method and time format
#' mysos <- SOS(url = "http://mysos.org/sos",
#'     binding = "KVP", timeFormat = "%Y-%m-%d")
#' 
#' # turn on verbose output for all methods and functions
#' SOS(url = "http://mysos.org/sos", verboseOutput = TRUE)
#' 
#' # get the meaning of an exception code
#' sosExceptionCodeMeaning(ex@exceptionCode)
#' 
#' # create a CRS object from a URN CRS string
#' sosGetCRS("urn:ogc:def:crs:EPSG:4217")
#' 
#' # create a SOS for a specific binding at a specific endpoint
#' SOS(url = "http://localhost:8080/52n-sos/sos/pox", binding = "POX",
#' 		useDCPs = FALSE)
#' 
#' # create a SOS using only the DCPs from the capabilities that match a specific
#' # pattern with the default binding
#' SOS(url = "http://localhost:8080/52n-sos/sos/service",
#' 		dcpFilter = list("POX" = "/pox"))
#' }
#' 
#' 
#' @export SOS
SOS <- function(url, binding = SosDefaultBinding(),
                version = sos100_version,
                parsers = SosParsingFunctions(),
                encoders = SosEncodingFunctions(),
                dataFieldConverters = SosDataFieldConvertingFunctions(),
                timeFormat = sosDefaultTimeFormat,
                verboseOutput = FALSE,
                switchCoordinates = FALSE,
                useDCPs = TRUE,
                dcpFilter = SosDefaultDCPs(),
                additionalKVPs = list(),
                ...) {
  if (version == sos100_version) {
    .sos <- new("SOS_1.0.0",
                url = url,
                binding = binding,
                version = version,
                # dummy capabilities to be replaced below
                capabilities = new("OwsCapabilities", version = "NA",
                                   updateSequence = as.character(NA),
                                   owsVersion = sosDefaultGetCapOwsVersion),
                parsers = parsers,
                encoders = encoders,
                dataFieldConverters = dataFieldConverters,
                timeFormat = timeFormat,
                verboseOutput = verboseOutput,
                switchCoordinates = switchCoordinates,
                useDCPs = useDCPs,
                dcpFilter = dcpFilter,
                additionalKVPs = additionalKVPs)

    .caps <- getCapabilities(sos = .sos, verbose = verboseOutput, ...)
    if (!is(.caps, "OwsCapabilities")) {
      stop("ERROR: Did not receive a Capabilities response!")
    }

    .sos@capabilities <- .caps

    if (verboseOutput) cat("[SOS] Created new SOS:\n", toString(.sos), "\n")

    cat("[sos4R] Created SOS for URL", url, "\n")
    return(.sos)
  }
  else if (version == sos200_version) {
    .sos <- new("SOS_2.0.0",
                url = url,
                binding = binding,
                version = version,
                # dummy capabilities to be replaced below
                capabilities = new("OwsCapabilities", version = "NA",
                                   updateSequence = as.character(NA),
                                   owsVersion = sosDefaultGetCapOwsVersion),
                parsers = parsers,
                encoders = encoders,
                dataFieldConverters = dataFieldConverters,
                timeFormat = timeFormat,
                verboseOutput = verboseOutput,
                switchCoordinates = switchCoordinates,
                useDCPs = useDCPs,
                dcpFilter = dcpFilter,
                additionalKVPs = additionalKVPs)

    .caps <- getCapabilities(sos = .sos, verbose = verboseOutput, ...)
    if (!is(.caps, "OwsCapabilities")) {
      stop("ERROR: Did not receive a Capabilities response!")
    }

    .sos@capabilities <- .caps

    if (verboseOutput) cat("[SOS] Created new SOS:\n", toString(.sos), "\n")

    cat("[sos4R] Created SOS for URL", url, "\n")
    return(.sos)
  }

  stop("Service version not supported!")
}

SosFilter_Capabilities <- function(spatial = list(NA_character_),
                                   temporal = list(NA_character_),
                                   scalar = list(NA_character_),
                                   id = list(NA_character_)) {
  new("SosFilter_Capabilities", spatial = spatial, temporal = temporal,
      scalar = scalar, id = id)
}





#' Class and Construction Function for "SosCapabilities"
#' 
#' The service metadata document of a Sensor Observation Service.
#' 
#' This document provides clients with service metadata about a specific
#' service instance, including metadata about the tightly-coupled data served.
#' 
#' The portions of the GetCapabilities response document that are defined in
#' the OWS Common specification are not modified for SOS. The sections of the
#' response that are specific for the SOS are the Filter_Capabilities and the
#' Contents section.
#' 
#' @name SosCapabilities
#' @aliases SosCapabilities_1.0.0-class SosCapabilities SosCapabilities-class
#' SosCapabilities_2.0.0-class
#' @docType class
#' @usage SosCapabilities(version, updateSequence = NA, owsVersion = "1.1.0",
#' identification = NULL, provider = NULL, operations = NULL,
#' filterCapabilities = NULL, contents = NULL)
#' @param version The version of the service.
#' @param updateSequence Service metadata document version, value is
#' "increased" whenever any change is made in complete service metadata
#' document. This can be used to request a certain version of a metadata
#' document. Parameter is found in both request and reponse, but may not be
#' supported by a service.
#' @param owsVersion The used OWS specification version.
#' @param identification The identification section of a capabilities document,
#' object of class \code{OwsServiceIdentification}.
#' @param provider The provider section of a capabilities document, object of
#' class \code{OwsServiceProvider}.
#' @param operations A list of objects of class \code{OwsOperation} in a
#' \code{OperationsMetadata} object. The provider section of a capabilities
#' document.
#' @param filterCapabilities An object of class \code{SosFilter_Capabilities}
#' contaning the filter capabilities of a service.
#' @param contents The provider section of a capabilities document, object of
#' class \code{SosContents}.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction function of the form \code{SosCapabilities(...)} including the
#' parameter \code{"owsVersion"} for the respective version of the service
#' metadata document.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\linkS4class{SosFilter_Capabilities}},
#' \code{\linkS4class{SosContents}}, \code{\linkS4class{OwsCapabilities}}
#' @references Section 8.2.3 of the SOS specification: Na, A., Priest, M.
#' (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC
#' 06-009r6, Version: 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("SosCapabilities_1.0.0")
#' showClass("SosCapabilities_2.0.0")
#' 
#' @export SosCapabilities
SosCapabilities <- function(version,  updateSequence = NA, owsVersion = "1.1.0",
                            identification = NULL, provider = NULL, operations = NULL,
                            filterCapabilities = NULL, contents = NULL) {
  if (version == "1.0.0") {
    new("SosCapabilities_1.0.0",
        version = version, updateSequence = updateSequence,
        owsVersion = owsVersion,
        identification = identification,
        provider = provider, operations = operations,
        filterCapabilities = filterCapabilities, contents = contents)
  }
  else if (version == "2.0.0") {
    new("SosCapabilities_2.0.0",
        version = version, updateSequence = updateSequence,
        owsVersion = owsVersion,
        identification = identification,
        provider = provider, operations = operations,
        filterCapabilities = filterCapabilities, contents = contents)
  }
  else {
    new("OwsCapabilities",
        version = version, updateSequence = updateSequence,
        owsVersion = owsVersion)
  }
}

SosObservationOffering <- function(id,
                                   name = as.character(NA),
                                   time,
                                   procedure,
                                   observedProperty,
                                   featureOfInterest,
                                   responseFormat,
                                   intendedApplication = as.character(NA),
                                   resultModel = as.character(NA),
                                   responseMode = as.character(NA),
                                   boundedBy = list()) {
  new("SosObservationOffering",
      id = id,
      name = name,
      time = time,
      procedure = procedure,
      observedProperty = observedProperty,
      featureOfInterest = featureOfInterest,
      responseFormat = responseFormat,
      intendedApplication = intendedApplication,
      resultModel = resultModel,
      responseMode = responseMode,
      boundedBy = boundedBy)
}

SosContents <- function(observationOfferings) {
  new("SosContents", observationOfferings = observationOfferings)
}





#' Classes and Construction Functions for sos:eventTime elements.
#' 
#' Temporal query parameters for GetObservation requests.
#' 
#' Specifies the time period(s) for which observations are requested. This
#' allows a client to request observations from a specific instant, multiple
#' instances or periods of time in the past, present and future. The supported
#' range is listed in the selected offering capabilities. The objects of these
#' classes are used in the GetObservation (paramter in \link{GetObservation}).
#' 
#' A typical example in a POST request: \verb{ <eventTime> <ogc:TM_During>
#' <ogc:PropertyName>om:samplingTime</ogc:PropertyName> <gml:TimePeriod>
#' <gml:beginPosition>2006-11-05T17:18:58.000-06:00</gml:beginPosition>
#' <gml:endPosition>2006-11-05T21:18:59.000-06:00</gml:endPosition>
#' </gml:TimePeriod> </ogc:TM_During> </eventTime> }
#' 
#' In GET binding (\code{\link{SosBindings}}) the eventTime is simply omitted
#' for getting the latest observation.
#' 
#' It is recommended to use the creation functions as shown in the examples.
#' 
#' @name SosEventTime
#' @aliases SosEventTime-class show,SosEventTime-method
#' print,SosEventTime-method toString,SosEventTime-method SosEventTime
#' @docType class
#' @usage SosEventTime(temporalOps)
#' @param temporalOps An object of class \link{OgcBinaryTemporalOp-class} to be
#' wrapped by the sos:eventTime element.
#' @section Objects from the Classes: Objects can be created by calls to the
#' construction functions of the form \code{SosEventTime(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See also \code{\link{SosGetObservation-class}},
#' \code{\link{sosCreateEventTimeList-methods}}.
#' @references See SOS specification, Table 4: \dQuote{Parameters of
#' GetObservation Request}.
#' @keywords classes
#' @examples
#' 
#' showClass("SosEventTime")
#' 
#' # create SosEventTime for all times after the given time stamp
#' timePos <- GmlTimePosition(as.POSIXct("2010-01-01 12:00"))
#' tOps <- TM_After(time = GmlTimeInstant(timePosition = timePos))
#' time1 <- SosEventTime(tOps)
#' 
#' # encode it as XML
#' encodeXML(time1, sos = SOS_Test())
#' 
#' # encode it as KVP
#' encodeKVP(time1, sos = SOS_Test())
#' 
#' @export SosEventTime
SosEventTime <- function(temporalOps) {
  new("SosEventTime", temporalOps = temporalOps)
}

SosFeatureOfInterest <- function(objectIDs = list(NA), spatialOps = NULL) {
  new("SosFeatureOfInterest", objectIDs = objectIDs, spatialOps = spatialOps)
}

SosDescribeSensor <- function(
  service,
  version,
  procedure,
  outputFormat) {
  new("SosDescribeSensor",
      request = sosDescribeSensorName,
      service = service,
      version = version,
      procedure = procedure,
      outputFormat = outputFormat)
}

SosGetObservation <- function(
  service,
  version,
  offering,
  observedProperty,
  responseFormat,
  srsName = as.character(NA),
  eventTime = list(),
  procedure = as.character(NA),
  featureOfInterest = NULL,
  result = NULL,
  resultModel = as.character(NA),
  responseMode = as.character(NA),
  BBOX = as.character(NA),
  valueReferenceTemporalFilter = as.character(NA)
) {
  new("SosGetObservation",
      request = sosGetObservationName,
      service = service,
      version = version,
      offering = offering,
      observedProperty = observedProperty,
      responseFormat = responseFormat,
      srsName = srsName,
      eventTime = eventTime,
      procedure = procedure,
      featureOfInterest = featureOfInterest,
      result = result,
      resultModel = resultModel,
      responseMode = responseMode,
      BBOX = BBOX,
      valueReferenceTemporalFilter = valueReferenceTemporalFilter)
}

SosGetObservationById <- function(
  service,
  version,
  observationId,
  responseFormat,
  srsName = as.character(NA),
  resultModel = as.character(NA),
  responseMode = as.character(NA)) {
  new("SosGetObservationById",
      request = sosGetObservationByIdName,
      service = service,
      version = version,
      observationId = observationId,
      responseFormat = responseFormat,
      srsName = srsName,
      resultModel = resultModel,
      responseMode = responseMode)
}

#
# main internal request method ----
#
.sosRequest_1.0.0 <- function(sos, request, verbose = FALSE, inspect = FALSE) {
  # check the request for consistency with service description
  .checkResult <- checkRequest(service = sos, operation = request,
                               verbose = verbose)
  if (!.checkResult) {
    warning("Check returned FALSE! Turn on verbose option for possible details.",
            immediate. = TRUE)
  }

  # get encoding function for the respective method
  .encodingFunction <- sos@encoders[[sos@binding]]
  if (verbose) {
    .f <- functionBody(.encodingFunction)
    cat("[.sosRequest_1.0.0] Encoding Function (beginning of function body): ",
        substring(text = .f, first = 0, last = 60), " ... [",
        max((length(.f) - 60), 0), " more chrs].\n")
  }

  # encode!
  .encodedRequest = .encodingFunction(obj = request, sos = sos, verbose = verbose)

  if (sos@binding == .sosBindingKVP) {
    .dcp <- list("Get", "application/x-kvp", sos@url)

    if (sos@useDCPs) {
      if (verbose) cat("[.sosRequest_1.0.0] Using DCP from operation description.\n")

      .dcp <- sosGetDCP(sos, sosName(request), owsGetName)

      if (is.null(.dcp) || is.na(.dcp) || !length(.dcp)) {
        .dcp <- list("Get", "application/x-kvp", sos@url)
        if (verbose) cat("[.sosRequest_1.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request, using ",
                         .dcp, "\n")
      }

      if (is.list(.dcp) && length(.dcp) && is.list(.dcp[[1]])) {
        .dcp <- .sosFilterDCPs(dcp = .dcp,
                               pattern = sos@dcpFilter[[.sosBindingKVP]],
                               verbose = verbose)
        .dcp <- unlist(.dcp)
      }

      if (verbose) cat("[.sosRequest_1.0.0] Using DCP:", toString(.dcp), "\n")
    }
    else if (verbose) cat("[.sosRequest_1.0.0] Not using DCP from capabilities.\n", .dcp, "\n")

    if (isTRUE(grep(pattern = "[\\?]", x = .dcp[[owsDcpUrlIndex]]) > 0)) {
      if (verbose) cat("Given url already contains a '?', appending arguments!\n")
      .url = paste0(.dcp[[owsDcpUrlIndex]], .encodedRequest)
    }
    else .url = paste(.dcp[[owsDcpUrlIndex]], .encodedRequest, sep = "?")

    if (!is.na(sos@additionalKVPs) && length(sos@additionalKVPs) > 0) {
      .kvps <- sos@additionalKVPs

      if (verbose) cat("[.sosRequest_1.0.0] adding extra KVP parameters:\n\t", toString(.kvps))

      .kvpsString <- .encodeAdditionalKVPs(.kvps)
      .url <- paste(.url, .kvpsString, sep = "&")
    }

    if (inspect) cat("[.sosRequest_1.0.0] GET!\n[.sosRequest_1.0.0] REQUEST:\n\t", .url, "\n")

    if (verbose) cat("[.sosRequest_1.0.0] Do request...\n")

    .response = httr::GET(url = .url)
    .content <- .processResponse(.response, verbose)

    if (verbose) cat("[.sosRequest_1.0.0] ... done.\n")
  }
  else if (sos@binding == .sosBindingPOX) {
    if (inspect) {
      cat("[.sosRequest_1.0.0] POST!\n[.sosRequest_1.0.0] REQUEST:\n")
      print(.encodedRequest)
    }

    .dcp <- list("Post", "application/xml", sos@url)

    if (sos@useDCPs) {
      .dcp <- sosGetDCP(sos, sosName(request), owsPostName) #sos@url as fallback

      if (is.null(.dcp) || is.na(.dcp) || !length(.dcp)) {
        .dcp <- list("Post", "application/xml", sos@url)
        if (verbose) cat("[.sosRequest_1.0.0] Could not get DCP from operation description. This is OK for first GetCapabilities request. Using", .dcp, "\n")
      }
      else {
        if (verbose) cat("[.sosRequest_1.0.0] Got DCPs from capabilites:", toString(.dcp), "\n")
      }

      if (is.list(.dcp) && length(.dcp) && is.list(.dcp[[1]])) {
        .dcp <- .sosFilterDCPs(dcp = .dcp,
                               pattern = sos@dcpFilter[[.sosBindingPOX]],
                               verbose = verbose)
        .dcp <- unlist(.dcp)
      }

      if (verbose) cat("[.sosRequest_1.0.0] Using DCP:", toString(.dcp), "\n")
    }
    else if (verbose) cat("[.sosRequest_1.0.0] *NOT* using DCP from capabilities:", .dcp, "\n")

    .requestString <- toString(.encodedRequest)

    # using 'POST' for application/xml content
    if (verbose) cat("[.sosRequest_1.0.0] Do request...\n")

    .response <- httr::POST(url = .dcp[[owsDcpUrlIndex]],
                            httr::content_type_xml(),
                            httr::accept_xml(),
                            body = .requestString )
    .content <- .processResponse(.response, verbose)

    if (verbose) cat("[.sosRequest_1.0.0] ... done.")
  }
  else if (sos@binding == .sosBindingSOAP) {
    if (inspect) {
      print("[.sosRequest_1.0.0] SOAP! REQUEST:\n")
      print(.encodedRequest)
    }

    stop("[sos4R] ERROR: SOAP is not implemented for SOS 1.0.0.\n")
  }
  else {
    stop(paste0("Unsupported method, has to be one of '",
               SosSupportedBindings(), "' but is '", sos@binding, "'."))
  }

  if (verbose) {
    cat("[.sosRequest_1.0.0] RESPONSE:\n")
    print(.content)
  }

  return(.content)
}

setMethod(f = "sosRequest",
          signature = signature(sos = "SOS_1.0.0", request = "OwsServiceOperation",
                                verbose = "logical", inspect = "logical"),
          definition = function(sos, request, verbose, inspect) {
            .sosRequest_1.0.0(sos = sos,
                              request = request,
                              verbose = verbose,
                              inspect = inspect)
          }
)


#
# internal SOS operations ----
#
.getCapabilities_1.0.0 <- function(sos, verbose, inspect, sections,
                                   acceptFormats, updateSequence, owsVersion,	acceptLanguages) {
  if (verbose) {
    cat("[.getCapabilities_1.0.0] of", sosUrl(sos), "\n")
  }

  .gc <- OwsGetCapabilities(service = sosService,
                            acceptVersions = c(sosVersion(sos)),
                            sections = sections,
                            acceptFormats = acceptFormats,
                            updateSequence = updateSequence,
                            owsVersion = owsVersion,
                            acceptLanguages = acceptLanguages)
  if (verbose) cat("[.getCapabilities_1.0.0] REQUEST:\n", toString(.gc), "\n")

  .response = sosRequest(sos = sos,
                         request = .gc,
                         verbose = verbose,
                         inspect = inspect)

  if (inspect) {
    cat("[.getCapabilities_1.0.0] RESPONSE DOC:\n")
    print(.response)
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }
  else {
    .parsingFunction <- sosParsers(sos)[[sosGetCapabilitiesName]]
    .caps <- .parsingFunction(obj = .response, sos = sos)
    if (verbose) {
      cat("[.getCapabilities_1.0.0] DONE WITH PARSING!\n")
    }
    return(.caps)
  }
}

setMethod(f = "getCapabilities", signature = signature(sos = "SOS_1.0.0"),
          definition = function(sos, verbose, inspect, sections, acceptFormats,
                                updateSequence, owsVersion,	acceptLanguages) {
            return(.getCapabilities_1.0.0(sos = sos, verbose = verbose,
                                          inspect = inspect, sections = sections,
                                          acceptFormats = acceptFormats,
                                          updateSequence = updateSequence,
                                          owsVersion = owsVersion,
                                          acceptLanguages = acceptLanguages))
          }
)

#
#
#
.describeSensor_1.0.0 <- function(sos, procedure, outputFormat, verbose,
                                  inspect, saveOriginal) {
  if (verbose) cat("[.describeSensor_1.0.0] ", procedure, "@", sos@url, "\n")

  # check if multiple sensors
  if (length(procedure) > 1) {
    if (verbose) cat("[.describeSensor_1.0.0] multiple sensors: ", procedure,
                    "\n")

    .descriptions <- list()
    for (p in procedure) {
      .description <- .describeSensor_1.0.0(sos = sos, procedure = p,
                                            outputFormat = outputFormat, verbose = verbose,
                                            inspect = inspect, saveOriginal = saveOriginal)
      .descriptions <- c(.descriptions, .description)
    }

    return(.descriptions)
  }

  .ds <- SosDescribeSensor(service = sosService, version = sos@version,
                           procedure = procedure, outputFormat = outputFormat)
  if (verbose) cat("[.describeSensor_1.0.0] REQUEST:\n", toString(.ds), "\n")

  .response = sosRequest(sos = sos,
                         request = .ds,
                         verbose = verbose,
                         inspect = inspect)
  if (inspect) cat("[.describeSensor_1.0.0] RESPONSE:\n", toString(.response), "\n")

  .filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      .filename <- saveOriginal
      if (verbose) cat("Using saveOriginal parameter for file name:", .filename, "\n")
    }
    else if (is.logical(saveOriginal)) {
      if (saveOriginal) .filename <- paste(.cleanupFileName(procedure), ".xml", sep = "")
      if (verbose) cat("Generated file name:", .filename, "\n")
    }

    if (verbose) cat("[.describeSensor_1.0.0] Saving original document...", .filename, "in", getwd(), "\n")

    xml2::write_xml(x = .response, file = .filename)
    cat("[sos4R] Original document saved:", .filename, "\n")
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }
  else {
    .parsingFunction <- sosParsers(sos)[[sosDescribeSensorName]]
    .sml <- .parsingFunction(obj = .response, sos = sos, verbose = verbose)

    if (!is.null(.filename)) {
      .oldAttrs <- attributes(.sml)
      .newAttrs <- list(.filename)
      names(.newAttrs) <- list(sosAttributeFileName)
      if (verbose) cat("[.describeSensor_1.0.0] Appending new attributes",
                      toString(.newAttrs), "(names",
                      toString(names(.newAttrs)), ")\n")

      attributes(.sml) <- c(.oldAttrs, .newAttrs)
    }

    return(.sml)
  }
}

setMethod(f = "describeSensor",
          signature = signature(sos = "SOS_1.0.0", procedure  = "character"),
          definition = function(sos, procedure, outputFormat, verbose, inspect, saveOriginal) {
            .result <- .describeSensor_1.0.0(sos = sos, procedure = procedure,
                                             outputFormat = outputFormat, verbose = verbose,
                                             inspect = inspect, saveOriginal = saveOriginal)
            return(.result)
          }
)


#
#
#
setMethod(f = "getObservationById",
          signature = signature(sos = "SOS_1.0.0", observationId = "character"),
          definition = function(sos, observationId, responseFormat, srsName,
                                resultModel, responseMode, verbose, inspect, saveOriginal) {
            return(.getObservationById_1.0.0(sos = sos,
                                             observationId = observationId,
                                             responseFormat = responseFormat, srsName = srsName,
                                             resultModel = resultModel,
                                             responseMode = responseMode, verbose = verbose,
                                             inspect = inspect, saveOriginal = saveOriginal))
          }
)

.getObservationById_1.0.0 <- function(sos, observationId, responseFormat, srsName,
                                      resultModel, responseMode, verbose, inspect, saveOriginal) {
  if (verbose) cat("[.getObservationById_1.0.0] ID", observationId, "\n")

  .filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      .filename <- saveOriginal
      if (verbose) cat("[.getObservationById_1.0.0] Using saveOriginal parameter for file name:", .filename, "\n")
    }
    else if (is.logical(saveOriginal)) {
      if (saveOriginal) .filename <- paste(observationId,
                                          format(Sys.time(), sosDefaultFilenameTimeFormat),
                                          ".xml",
                                          sep = "_")
      if (verbose) cat("[.getObservationById_1.0.0] Generating file name:", .filename, "\n")
    }
  }

  .go <- SosGetObservationById(service = sosService,
                               version = sos@version,
                               observationId = observationId,
                               responseFormat =  responseFormat,
                               srsName = srsName,
                               resultModel = resultModel,
                               responseMode = responseMode)

  if (verbose) cat("[.getObservationById_1.0.0] REQUEST:\n", toString(.go), "\n")

  .response = sosRequest(sos = sos,
                         request = .go,
                         verbose = verbose,
                         inspect = inspect)
  if (inspect) cat("[.getObservationById_1.0.0] RESPONSE:\n", toString(.response), "\n")

  if (!is.null(.filename)) {
    xml2::write_xml(x = .response, file = .filename)
    cat("[sos4R] Original document saved:", .filename, "\n")
  }

  if (.isExceptionReport(.response)) {
    return(.handleExceptionReport(sos, .response))
  }
  else {
    .parsingFunction <- sosParsers(sos)[[sosGetObservationByIdName]]
    .obs <- .parsingFunction(obj = .response, sos = sos,
                             verbose = verbose)

    # remove list if only one element
    if (is.list(.obs) && length(.obs) == 1)
      .obs <- .obs[[1]]

    if (verbose) {
      cat("[.getObservationById_1.0.0] PARSED RESPONSE:\n")
      print(.obs)
    }

    if (!is.null(.filename)) {
      .oldAttrs <- attributes(.obs)
      .newAttrs <- list(.filename)
      names(.newAttrs) <- list(sosAttributeFileName)
      if (verbose) cat("[.getObservationById_1.0.0] Appending new attributes",
                      toString(.newAttrs), "(names",
                      toString(names(.newAttrs)), ")\n")

      attributes(.obs) <- c(.oldAttrs, .newAttrs)
    }

    return(.obs)
  }

  if (verbose) {
    cat("[.getObservationById_1.0.0] returning raw response string.\n")
  }

  return(.response)
}

#
#
#
.createGetObservation_1.0.0 <- function(sos, offeringId, observedProperty,
                                        responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                        result, resultModel, responseMode, BBOX, verbose, inspect,
                                        saveOriginal) {
  .go <- SosGetObservation(service = sosService, version = sos@version,
                           offering = offeringId, observedProperty = observedProperty,
                           responseFormat =  responseFormat, srsName = srsName,
                           eventTime = eventTime, procedure = procedure,
                           featureOfInterest = featureOfInterest, result = result,
                           resultModel = resultModel, responseMode = responseMode,
                           BBOX = BBOX)

  if (verbose) cat("[.createGetObservation_1.0.0] Done:\n", toString(.go), "\n")

  return(.go)
}


#
#
#
.getObservation_1.0.0 <- function(sos, offeringId, observedProperty,
                                  responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                  result, resultModel, responseMode, BBOX, verbose, inspect,
                                  saveOriginal) {

  .filename <- NULL
  if (!is.null(saveOriginal)) {
    if (is.character(saveOriginal)) {
      .filename <- saveOriginal
      if (verbose) cat("[.getObservation_1.0.0] Using saveOriginal parameter for file name:", .filename, "\n")
    }
    else if (is.logical(saveOriginal)) {
      if (saveOriginal) .filename <- paste(.cleanupFileName(offeringId),
                                           format(Sys.time(), sosDefaultFilenameTimeFormat),
                                           ".xml",
                                           sep = "_")
      if (verbose) cat("[.getObservation_1.0.0] Generated file name:", .filename, "\n")
    }
  }

  if (verbose) cat("[.getObservation_1.0.0] to ", sos@url, " with offering ", offeringId, "\n")

  .go <- .createGetObservation_1.0.0(sos, offeringId, observedProperty,
                                     responseFormat, srsName, eventTime,	procedure, featureOfInterest,
                                     result, resultModel, responseMode, BBOX, verbose, inspect,
                                     saveOriginal)

  if (verbose) cat("[.getObservation_1.0.0] REQUEST:\n\n", toString(.go), "\n")

  .response = sosRequest(sos = sos,
                         request = .go,
                         verbose = verbose,
                         inspect = inspect)
  if (verbose) cat("[sos4R] Received response (object size:", object.size(.response), "bytes), parsing ...\n")

  if (inherits(.response, "xml_document")) {
    if (verbose) cat("[.getObservation_1.0.0] Got XML document as response.\n")
    if (inspect) {
      cat("[.getObservation_1.0.0] RESPONSE DOC:\n")
      print(.response)
    }

    if (!is.null(.filename)) {
      xml2::write_xml(x = .response, file = .filename)
      if (verbose) cat("[.getObservation_1.0.0] Saved original document:", .filename, "\n")
    }

    if (.isExceptionReport(.response)) {
      return(.handleExceptionReport(sos, .response))
    }

    .parsingFunction <- sosParsers(sos)[[sosGetObservationName]]

    if (verbose) {
      cat("[.getObservation_1.0.0] Parsing with function ")
      print(.parsingFunction)
    }

    .obs <- .parsingFunction(obj = .response,
                             sos = sos,
                             verbose = verbose)

    # calculate result length vector
    if (inherits(.obs, "OmObservationCollection")) {
      if (verbose) cat("[.getObservationById_1.0.0] Got OmObservationCollection",
                      "... calculating length with sosResult()")

      .result <- sosResult(.obs, bind = FALSE, coordinates = FALSE)
      if (verbose) cat("[.getObservationById_1.0.0] result: ", toString(.result))

      .resultLength <- sapply(.result, nrow)
      if (length(.resultLength) == 0) # nothing
        .resultLength = 0
    }
    else .resultLength <- NA

    if (verbose) {
      cat("[.getObservation_1.0.0] PARSED RESPONSE:", class(.obs), "\n")
      cat("[.getObservation_1.0.0] Result length(s): ", toString(.resultLength), "\n")
    }

    if (is.list(.obs) && any(sapply(.obs, is.null))) {
      .countInfo <- paste("NO DATA, turn on 'verbose' for more information.")
    }
    else {
      .nonNulls <- Filter(Negate(function(x) is.null(unlist(x))), .resultLength)
      .sum <- sum(unlist(.nonNulls))
      .countInfo <- paste(.sum, "result values [", toString(.resultLength), "].")
    }

    .msg <- paste("[sos4R] Finished getObservation to", sos@url,
                  "\n\t--> received", length(.obs), "observation(s) having",
                  .countInfo , "\n")
    if (!is.null(.filename)) {
      .msg <- paste(.msg,
                    "[sos4R] Original document saved:", .filename, "\n")

      .oldAttrs <- attributes(.obs)
      .newAttrs <- list(.filename)
      names(.newAttrs) <- list(sosAttributeFileName)
      if (verbose) cat("[.getObservationById_1.0.0] Appending new attributes",
                      toString(.newAttrs), "(names",
                      toString(names(.newAttrs)), ")\n")

      attributes(.obs) <- c(.oldAttrs, .newAttrs)
    }
    cat(.msg)

    return(.obs)
  }
  else {# response is NOT an XML document:
    if (verbose) cat("[.getObservation_1.0.0] Did NOT get XML document as response, trying to parse with", responseFormat, "\n")

    if (is.na(responseFormat) || is.null(responseFormat)) {
      if (verbose) cat("[.getObservation_1.0.0] responseFormat is ",
                       responseFormat, " >>> returning response string...\n")
      return(.response)
    }

    if (mimeTypeCSV == responseFormat) {
      if (inspect) {
        cat("[.getObservation_1.0.0] CSV RESPONSE:\n")
        print(.response)
      }

      .parsingFunction <- sosParsers(sos)[[mimeTypeCSV]]
      .csv <- .parsingFunction(obj = .response, verbose = verbose)

      if (!is.null(.filename)) {
        .filename <- paste(file = .filename, ".csv", sep = "")
        write.csv(.csv, .filename)
      }

      .msg <- paste("[sos4R] Finished getObservation to", sos@url, "\n\t",
                    "--> received observations with dimensions",
                    toString(dim(.csv)), "\n")
      if (!is.null(.filename)) {
        .msg <- paste(.msg, "[sos4R] Original document saved:", .filename, "\n")

        .oldAttrs <- attributes(.csv)
        .newAttrs <- list(.filename)
        names(.newAttrs) <- list(sosAttributeFileName)
        if (verbose) cat("[.getObservation_1.0.0] Appending new attributes",
                        toString(.newAttrs), "(names",
                        toString(names(.newAttrs)), ")\n")

        attributes(.csv) <- c(.oldAttrs, .newAttrs)
      }
      cat(.msg)

      return(.csv)
    } # grep(pattern = mimeTypeCSV...

    # TODO Add other non-XML encodings here.
  } # else

  # not xml nor csv nore otherwise handled
  if (inspect) {
    cat("[.getObservation_1.0.0] UNKNOWN RESPONSE FORMAT; Response:\n'", .response, "'\n")
    warning("Unknown response format!")
  }

  if (!is.null(.filename)) {
    save(.response, file = .filename)
    cat("[sos4R] Saved original document:", .filename)
  }

  return(.response)
}

#
#
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_1.0.0",
                                offering = "SosObservationOffering"),
          definition = function(sos, offering, observedProperty, responseFormat, srsName,
                                eventTime,	procedure, featureOfInterest, result, resultModel,
                                responseMode, BBOX, verbose, inspect, saveOriginal) {
            .offeringId <- offering@id
            if (verbose)	cat("[getObservation] Requesting offering", .offeringId,
                             "by SosObservationOffering.\n")

            return(.getObservation_1.0.0(sos = sos, offeringId = .offeringId,
                                         observedProperty = observedProperty,
                                         responseFormat = responseFormat,
                                         srsName = srsName,
                                         eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         result = result,
                                         resultModel = resultModel,
                                         responseMode = responseMode,
                                         BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect,
                                         saveOriginal = saveOriginal))
          }
)

#
#
#
setMethod(f = "getObservation",
          signature = signature(sos = "SOS_1.0.0",
                                offering = "character"),
          definition = function(sos, offering, observedProperty = list(), responseFormat,
                         srsName, eventTime,	procedure, featureOfInterest, result,
                         resultModel, responseMode, BBOX, verbose, inspect,
                         saveOriginal) {
            if (verbose)	cat("[getObservation] Requesting offering", offering,
                             "by name.\n")

            .off <- sosOfferings(sos)[[offering]]

            if (length(observedProperty) == 0) {
              .obsProps <- sosObservedProperties(.off)
              if (verbose) cat("[getObservation] Got observation(s) from offering because none given:",
                               toString(.obsProps), "\n")
            }
            else {
              .obsProps <- observedProperty
            }

            return(.getObservation_1.0.0(sos = sos, offeringId = offering,
                                         observedProperty = .obsProps,
                                         responseFormat = responseFormat,
                                         srsName = srsName, eventTime = eventTime,
                                         procedure = procedure,
                                         featureOfInterest = featureOfInterest,
                                         result = result, resultModel = resultModel,
                                         responseMode = responseMode, BBOX = BBOX,
                                         verbose = verbose,
                                         inspect = inspect, saveOriginal = saveOriginal))
          }
)

#
# see: http://www.oostethys.org/best-practices/best-practices-get
#
setMethod(f = "encodeRequestKVP", signature = signature(obj = "SosDescribeSensor"),
          definition = function(obj, sos, verbose = FALSE) {

            if (sos@version == sos100_version) {
              return(.sosEncodeRequestKVPDescribeSensor_1.0.0(obj = obj,
                                                              sos = sos, verbose = verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)

.kvpBuildRequestBase <- function(sos, operation = sosGetCapabilitiesName) {
  paste(
    paste("service", .kvpEscapeSpecialCharacters(x = sosService), sep = "="),
    paste("version", .kvpEscapeSpecialCharacters(x = sos@version), sep = "="),
    paste("request", .kvpEscapeSpecialCharacters(x = operation), sep = "="),
    sep = "&")
}

.sosEncodeRequestKVPDescribeSensor_1.0.0 <- function(obj, sos,
                                                     verbose = FALSE) {
  # mandatory:
  .requestBase <- .kvpBuildRequestBase(sos, sosDescribeSensorName)
  .procedure <- paste("procedure", .kvpEscapeSpecialCharacters(x = obj@procedure), sep = "=")
  .format <- paste(
    "outputFormat",
    .kvpEscapeSpecialCharacters(x = gsub(obj@outputFormat,
                                         pattern = "&quot;",
                                         replacement = '"')),
    sep = "=")

  .kvpString <- paste(.requestBase, .procedure,
                      .format, sep = "&")

  if (verbose) cat("[.sosEncodeRequestKVPDescribeSensor_1.0.0] ", .kvpString)

  return(.kvpString)
}

setMethod(f = "encodeRequestKVP", signature = signature(obj = "SosGetObservation"),
          definition = function(obj, sos, verbose = FALSE) {
            if (sos@version == sos100_version) {
              return(.sosEncodeRequestKVPGetObservation_1.0.0(obj, sos,
                                                              verbose))
            } else if (sos@version == sos200_version) {
              return(.sosEncodeRequestKVPGetObservation_2.0.0(obj, sos,
                                                              verbose))
            }
            else {
              stop("Version not supported!")
            }
          }
)
.sosEncodeRequestKVPGetObservation_1.0.0 <- function(obj, sos,
                                                     verbose = FALSE) {
  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] encoding",
                   toString(obj), "\n")

  # required:
  .requestBase <- .kvpBuildRequestBase(sos, sosGetObservationName)
  .offering <- paste(sosKVPParamNameOffering,
                     .kvpEscapeSpecialCharacters(x = obj@offering), sep = "=")
  .observedProperty <- .kvpKeyAndValues(sosKVPParamNameObsProp,
                                        obj@observedProperty)

  .mandatory <- paste(.requestBase, .offering,
                      .observedProperty, sep = "&")

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0]",
                   "mandatory elements: ", .mandatory, "\n")

  # optional:
  .optionals = ""
  # is optional for GET
  if (!is.na(obj@responseFormat)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding response format ",
                     obj@responseFormat, "\n")
    .noHTMLquotes <- gsub(obj@responseFormat,
                          pattern = "&quot;",
                          replacement = '"')
    #.singleQuotes <- gsub(.noHTMLquotes,
    #                      pattern = "\"",
    #                      replacement = "'")
    .responseFormat <- paste(
      sosKVPParamNameResponseFormat,
      .kvpEscapeSpecialCharacters(x = .noHTMLquotes),
      sep = "=")
    .optionals <- paste(.optionals, .responseFormat, sep = "&")
  }

  if (!is.na(obj@srsName)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding SRS name ",
                     obj@srsName, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameSrsName,
                                          .kvpEscapeSpecialCharacters(x = obj@srsName),
                                          sep = "="),
                        sep = "&")
  }

  if (!length(obj@eventTime) == 0) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding event time",
                     toString(obj@eventTime), "\n")
    if (length(obj@eventTime) > 1)
      warning("Only first event time in the list is used for KVP!")

    .timeString <- encodeKVP(obj = obj@eventTime[[1]],
                             sos = sos, verbose = verbose)

    # if the eventTime is a latest request, it returns NA, the GET binding
    # says for the latest observation eventTime is omitted
    if (!is.na(.timeString)) {
      .optionals <- paste(.optionals, paste("eventTime",
                                            .kvpEscapeSpecialCharacters(x = .timeString),
                                            sep = "="),
                          sep = "&")
    }
    else {
      if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] ",
                      "encodeKVP returned NA for eventTime, omitting",
                      "parameter for request for latest observation.")
    }
  }

  if (!any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding procedures ",
                    obj@procedure, "\n")
    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(sosKVPParamNameProcedure, obj@procedure),
                        sep = "&")
  }

  if (!is.null(obj@featureOfInterest)) {
    .foiIDs <- obj@featureOfInterest@objectIDs

    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding features ",
                    toString(obj@featureOfInterest), "by IDs ", toString(.foiIDs), "\n")

    .optionals <- paste(.optionals,
                        .kvpKeyAndValues(sosKVPParamNameFoi, .foiIDs),
                        sep = "&")
    #		warning("'featureOfInterest' is not supported for 'GET' - parameter is discarded, use another method to include it!")
  }

  if (!is.null(obj@result)) {
    warning("'result' is not supported for 'GET' - parameter is discarded, use another method to include it!")
  }

  if (!is.na(obj@resultModel)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding result model ",
                     obj@resultModel, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameResultModel,
                                          .kvpEscapeSpecialCharacters(x = obj@resultModel),
                                          sep = "="),
                        sep = "&")
  }

  if (!is.na(obj@responseMode)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding response mode ",
                     obj@responseMode, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameResponseMode,
                                          .kvpEscapeSpecialCharacters(x = obj@responseMode),
                                          sep = "="),
                        sep = "&")
  }

  if (!is.na(obj@BBOX)) {
    if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0] Adding BBOX ",
                    obj@BBOX, "\n")
    .optionals <- paste(.optionals, paste(sosKVPParamNameBBOX,
                                          .kvpEscapeSpecialCharacters(x = obj@BBOX), sep = "="),
                        sep = "&")
  }

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0]",
                  "optional elements: ", .optionals, "\n")

  .kvpString <- paste(.mandatory, .optionals, sep = "")

  if (verbose) cat("[.sosEncodeRequestKVPGetObservation_1.0.0]",
                  "Finished KVP string creation:\n", .kvpString, "\n")

  return(.kvpString)
}

setMethod(f = "encodeRequestKVP", signature = signature(obj = "SosGetObservationById"),
          definition = function(obj, sos, verbose = TRUE) {
            stop("KVP encoding of operation 'GetObservationById' not supported!")
          }
)

#
# encode as XML
#
setMethod(f = "encodeRequestXML", signature = signature(obj = "SosGetObservation"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("[encodeRequestXML]", class(obj), "\n")
            }

            if (sos@version == sos100_version) {
              return(.sosEncodeRequestXMLGetObservation_1.0.0(obj = obj,
                                                              sos = sos,
                                                              verbose = verbose))
            }
            else if (sos@version == sos200_version) {
              stop(paste("XML request encoding for SOS 2.0 GetObservation",
                         " not implemented. Use KVP binding."))
            } else {
              stop("Version not supported!")
            }
          }
)
.sosEncodeRequestXMLGetObservation_1.0.0 <- function(obj, sos, verbose = FALSE) {
  xmlDoc <- xml2::xml_new_root(sosGetObservationName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos100Namespace,
                                service = obj@service,
                                version = sos@version,
                                "xmlns:xsi" = xsiNamespace,
                                "xmlns:ows" = owsNamespace,
                                "xmlns:sos" = sos100Namespace,
                                "xmlns:om" = omNamespace,
                                "xmlns:ogc" = ogcNamespace,
                                "xmlns:gml" = gmlNamespace))

  # required and optional are mixed - schema requires a particular order:
  xml2::xml_add_child(xmlDoc, sosOfferingName, obj@offering)

  if (!length(obj@eventTime) == 0) {
    eventTimeList <- lapply(X = obj@eventTime,
                             FUN = encodeXML,
                             sos = sos,
                             verbose = verbose)
    for (et in eventTimeList) {
      xml2::xml_add_child(xmlDoc, et)
    }
  }

  if (!any(sapply(obj@procedure, "is.na"), na.rm = TRUE)) {
    for (p in obj@procedure) {
      xml2::xml_add_child(xmlDoc, sosProcedureName, p)
    }
  }

  for (op in obj@observedProperty) {
    xml2::xml_add_child(xmlDoc, sosObservedPropertyName, op)
  }

  if (!is.null(obj@featureOfInterest)) {
    foi <- encodeXML(obj = obj@featureOfInterest,
                      sos = sos,
                      verbose = verbose)
    xml2::xml_add_child(xmlDoc, foi)
  }

  if (!is.null(obj@result)) {
    if (is.character(obj@result)) {
      result <- encodeXML(obj = obj@result,
                          sos = sos,
                          addNamespaces = TRUE,
                          verbose = verbose)
    }
    else {
      result <- encodeXML(obj = obj@result,
                          sos = sos,
                          verbose = verbose)
    }
    xml2::xml_add_child(xmlDoc, result)
  }

  if (!is.na(obj@responseFormat)) {
    rF <- gsub(obj@responseFormat, pattern = "&quot;", replacement = "\"")
    xml2::xml_add_child(xmlDoc, sosResponseFormatName, rF)
  }

  if (!is.na(obj@resultModel)) {
    xml2::xml_add_child(xmlDoc, sosResultModelName, obj@resultModel)
  }

  if (!is.na(obj@responseMode)) {
    xml2::xml_add_child(xmlDoc, sosResponseModeName, obj@responseMode)
  }

  if (!is.na(obj@srsName)) {
    xml2::xml_set_attr(x = xmlDoc, attr = "srsName", value = obj@updateSequence)
  }

  if (!is.na(obj@BBOX)) {
    warning("GetObservation contains BBOX, but that is not supported for 'POST' and ignored.",
            "This is also not in the SOS Specification - use featureOfInterest instead!")
  }

  return(xmlDoc)
}

setMethod(f = "encodeRequestXML", signature = signature(obj = "SosGetObservationById"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("[encodeRequestXML]", class(obj), "\n")
            }

            if (sos@version == sos100_version) {
              return(.sosEncodeRequestXMLGetObservationById_1.0.0(obj = obj,
                                                                  sos = sos))
            }
            else {
              stop("Version not supported!")
            }
          }
)
.sosEncodeRequestXMLGetObservationById_1.0.0 <- function(obj, sos) {
  xmlDoc <- xml2::xml_new_root(sosGetObservationByIdName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos100Namespace,
                                service = obj@service,
                                version = sos@version,
                                "xmlns:xsi" = xsiNamespace,
                                "xmlns:sos" = sos100Namespace))

  xml2::xml_add_child(xmlDoc, sosObservationIdName, obj@observationId)

  rF <- gsub(obj@responseFormat, pattern = "&quot;", replacement = "\"")
  xml2::xml_add_child(xmlDoc, sosResponseFormatName, rF)

  if (!is.na(obj@resultModel)) {
    xml2::xml_add_child(xmlDoc, sosResultModelName, obj@resultModel)
  }

  if (!is.na(obj@responseMode)) {
    xml2::xml_add_child(xmlDoc, sosResponseModeName, obj@responseMode)
  }

  if (!is.na(obj@srsName)) {
    xml2::xml_set_attr(x = xmlDoc, attr = "srsName", value = obj@updateSequence)
  }

  return(xmlDoc)
}


setMethod(f = "encodeRequestXML", signature = signature(obj = "SosDescribeSensor"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("[encodeRequestXML]", class(obj), "\n")
            }

            if (sos@version == sos100_version) {
              if (verbose) {
                cat("[encodeRequestXML] encoding vor SOS 1.0.0\n")
              }
              return(.sosEncodeRequestXMLDescribeSensor_1.0.0(obj = obj, sos = sos))
            }
            else {
              stop("[encodeRequestXML] Version not supported for operation DescribeSensor!")
            }
          }
)
.sosEncodeRequestXMLDescribeSensor_1.0.0 <- function(obj, sos) {
  xmlDoc <- xml2::xml_new_root(sosDescribeSensorName)
  xml2::xml_set_attrs(x = xmlDoc,
                      value = c(xmlns = sos100Namespace,
                                "xmlns:xsi" = xsiNamespace,
                                service = obj@service,
                                outputFormat = obj@outputFormat,
                                version = sos@version),
                      ns = SosAllNamespaces())

  xml2::xml_add_child(xmlDoc, "procedure", obj@procedure)
  return(xmlDoc)
}

#
# encoding functions for SOAP ----
#
setMethod("encodeRequestSOAP", "SosDescribeSensor",
          function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("ENCODE SOAP ", class(obj), "\n")
            }

            if (sos@version == sos100_version) {
              return(.sosEncodeRequestXMLDescribeSensor_1.0.0(obj, sos = sos))
            }
            else {
              stop("Version not supported!")
            }
          }
)
setMethod("encodeRequestSOAP", "SosGetObservation",
          function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("ENCODE SOAP ", class(obj), "\n")
            }
            stop("Method not implemented yet!")
          }
)
setMethod("encodeRequestSOAP", "SosGetObservationById",
          function(obj, sos, verbose = FALSE) {
            if (verbose) {
              cat("ENCODE SOAP ", class(obj), "\n")
            }
            stop("Method not implemented yet!")
          }
)

#
# encoding functions XML ----
#
setMethod(f = "encodeXML",
          signature = signature(obj = "SosEventTime", sos = "SOS"),
          function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeXML] SosEventTime", class(obj), "\n")

            .temporalOpsClass <- class(obj@temporalOps)
            if (!is.null(SosSupportedTemporalOperators()[[.temporalOpsClass]])) {
              # FIXME: https://github.com/r-lib/xml2/issues/239
              #eventTime <- xml2::xml_new_root(sosEventTimeName, xmlns = sos100Namespace)
              eventTime <- xml2::read_xml(paste0("<", sosEventTimeName,
                                                 " xmlns:", sos100NamespacePrefix, "=\"", sos100Namespace,
                                                 "\" />"))

              temporalOpsXML <- encodeXML(obj = obj@temporalOps, sos = sos, verbose = verbose)
              xml2::xml_add_child(eventTime, temporalOpsXML)

              return(eventTime)
            }
            else {
              stop(paste("temporalOps type not supported:",
                         .temporalOpsClass))
            }
          }
)

setMethod(f = "encodeXML",
          signature = signature(obj = "SosFeatureOfInterest", sos = "SOS"),
          function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeXML]", class(obj), "\n")

            # FIXME: https://github.com/r-lib/xml2/issues/239
            #foi <- xml2::xml_new_root(sosEventTimeName, xmlns = sos100Namespace)
            foi <- xml2::read_xml(paste0("<", sosFeatureOfInterestName,
                                         " xmlns:", sos100NamespacePrefix, "=\"", sos100Namespace,
                                         "\" />"))

            # switch between objectIDs and spatialOps
            if (!any(is.na(obj@objectIDs))) {
              for (id in obj@objectIDs) {
                xml2::xml_add_child(foi, sosObjectIDName, id)
              }
            }
            else if (!is.null(obj@spatialOps)) {
              spOp <- encodeXML(obj = obj@spatialOps, sos = sos, verbose = verbose)
              xml2::xml_add_child(foi, spOp)
            }

            return(foi)
          }
)

# to make just the time encoding interchangeable by users
setMethod(f = "encodeXML",
          signature = signature(obj = "POSIXt", sos = "SOS"),
          definition = function(obj, sos, verbose = FALSE) {
            if (verbose) cat("[encodeXML] POSIXt with value", toString(obj), "\n")

            formatted <- strftime(x = obj, format = sosTimeFormat(sos))

            if (verbose) cat("Formatted ", obj, " to ", formatted)

            return(formatted)
          }
)

setMethod(f = "encodeKVP",
          signature = signature(obj = "SosEventTime", sos = "SOS"),
          function(obj, sos, verbose = FALSE) {
            if (verbose) cat("ENCODE KVP ", class(obj), "\n")

            .temporalOpsKVP <- encodeKVP(obj = obj@temporalOps,
                                         sos = sos,
                                         verbose = verbose)
            return(.temporalOpsKVP)
          }
)

# to make just the time encoding interchangeable by users
setMethod(f = "encodeKVP",
          signature = signature(obj = "POSIXt", sos = "SOS"),
          definition = function(obj, sos, verbose) {
            if (verbose) cat("[encodeKVP] POSIXt with value", toString(obj), "\n")

            formatted <- strftime(x = obj, format = sosTimeFormat(sos))

            if (verbose) cat("Formatted ", obj, " to ", formatted, "\n")

            return(formatted)
          }
)

#
# check functions for requests ----
#
setMethod(f = "checkRequest",
          signature = signature(service = "SOS", operation = "SosDescribeSensor",
                                verbose = "logical"),
          definition = function(service, operation, verbose) {
            if (verbose) {
              cat("[checkRequest] Checking DescribeSensor... \n")
              cat(toString(operation), "\n")
            }

            # check if operation is for SOS and operation is DescribeSensor
            if (!(operation@service == sosService &&
                 operation@request == sosDescribeSensorName)) {
              stop("Wrong input for Method checkReuqest! Require classes 'SOS' as service and ''SosDescribeSensor' as operation.")
              return(FALSE)
            }

            # check if sensor is in listed in procedures
            .procedures = unique(unlist(sosProcedures(service)))
            .dsOperation <- sosOperation(service, sosDescribeSensorName)

            .procContained <- FALSE
            for (x in .procedures) {
              if (x == operation@procedure)
                .procContained <- TRUE
            }
            if (!.procContained)
              warning("Requested procedure ist not listed in capablities, service might return error!")


            # check if output format is supported by sos
            .oFSupported <- FALSE
            .supportedFormats <- .dsOperation@parameters[["outputFormat"]];
            .format <- gsub(operation@outputFormat, pattern = "\\&quot;",
                            replacement = '"')

            if (!any(sapply(.supportedFormats, "==", .format), na.rm = TRUE)) {
              warning(paste("Outputformat has to be one of",
                            paste(.supportedFormats, sep = ", ",
                                  collapse = "', '"), "'. Received format is '", .format, "'."))
            }
            else {
              .oFSupported <- TRUE
            }

            # check if binding is supported
            .bindingSupported <- any(sapply(SosSupportedBindings(),
                                            "==", service@binding))
            if (!.bindingSupported)
              warning("Requested method type ist not listed in capablities for this operation, service might return error!")

            if (verbose) {
              cat("[checkRequest] Checks: procedure contained =",
                  .procContained,
                  ", output format supported =", .oFSupported,
                  ", binding supported =", .bindingSupported, "\n")
            }

            return(.procContained && .oFSupported && .bindingSupported)
          })

setMethod(f = "checkRequest",
          signature = signature(service = "SOS", operation = "SosGetObservation",
                                verbose = "logical"),
          definition = function(service, operation, verbose) {
            # check if operation is for SOS and operation is DescribeSensor
            if (!(operation@service == sosService &&
                 operation@request == sosGetObservationName)) {
              stop("Wrong input! Require classes 'SOS' as service and 'GetObservation' as operation.")
              return(FALSE)
            }

            # TODO implement checkRequest for GetObservation

            # check if given responseFormat is supported by the service

            # check if temporal operator and operand are a valid combination according to filter capabilities

            return(TRUE)
          }
)

setMethod(f = "checkRequest",
          signature = signature(service = "SOS",
                                operation = "SosGetObservationById", verbose = "logical"),
          definition = function(service, operation, verbose) {
            # check if operation is for SOS and operation is DescribeSensor
            if (!(operation@service == sosService &&
                 operation@request == sosGetObservationByIdName)) {
              stop("Wrong input! Require classes 'SOS' as service and 'GetObservationById' as operation.")
              return(FALSE)
            }

            # TODO implement checkRequest for GetObservationById
            # see above!

            return(TRUE)
          }
)

#
# util functions for getting content from response ----
#
.processResponse <- function(response, verbose) {
  contentType <- httr::http_type(response)

  if (!httr::has_content(response)) {
    stop("Response has no content: ", toString(response),
         " | headers: ", paste(names(httr::headers(response)),
                               httr::headers(response),
                               sep = ": ",
                               collapse = "; "))
  }

  if (verbose) cat("[.processResponse] Response status: ", httr::status_code(response),
                   " | type: ", contentType, "\n")

  if (httr::status_code(response) == 405)
    warning("Response is HTTP 405 - Method Not Allowed: Please check if endpoint and binding match.")

  httr::stop_for_status(response, "sending request to SOS")

  if (contentType == "text/plain") {
    text <- httr::content(response)
    if (length(text) > 0 &
        regexpr("(<html>|<HTML>|<!DOCTYPE HTML|<!DOCTYPE html)", text) > 0) {
      stop(paste("[sos4R] ERROR: Got HTML response!:\n", text, "\n\n"))
    }

    xml <- xml2::read_xml(text)
    return(xml)
  }
  else if (contentType == "application/xml" || contentType == "text/xml") {
    xml <- httr::content(x = response, encoding = sosDefaultCharacterEncoding)
    return(xml)
  }
  else if (contentType == "text/csv") {
    if (!requireNamespace("readr", quietly = TRUE))
      stop("package readr required to handle text/csv format, please install")

    tibble <- httr::content(x = response, encoding = sosDefaultCharacterEncoding)
    return(tibble)
  }

  stop("Unsupported content type in response")
}
