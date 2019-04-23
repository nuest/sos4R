

#' Methods for Function checkRequest
#' 
#' A function to check request prior to sending them to a service. This
#' function is autmatically called during the request process and can be used
#' to check the request for consistency with ifself as well as with available
#' metadata, and also perform additional validity checks that might not be
#' possible with class validation.
#' 
#' 
#' @name checkRequest-methods
#' @aliases checkRequest checkRequest-methods
#' checkRequest,SOS,SosDescribeSensor,logical-method
#' checkRequest,SOS,SosGetObservationById,logical-method
#' checkRequest,SOS,SosGetObservation,logical-method
#' checkRequest,SOS,OwsGetCapabilities_1.1.0,logical-method
#' checkRequest,SOS,OwsGetCapabilities_2.0.0,logical-method
#' checkRequest,SOS_2.0.0,SosGetFeatureOfInterest_2.0.0,logical-method
#' checkRequest,SOS_2.0.0,SosGetObservation,logical-method
#' checkRequest,SOS_2.0.0,SosGetDataAvailability_1.0.0,logical-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(service = \"SOS\",
#' operation = \"DescribeSensor\", verbose = \"logical\")")}{ Checking a
#' DescribeSensor request that is send to a SOS. This method currently checks
#' the following elements: \itemize{ \item\code{operation@service} attribute
#' must be \verb{SOS} \item\code{operation@request} attribute must be
#' \verb{DescribeSensor} \item\code{operation@procedure} must be listed in the
#' given service's capabilities \item\code{operation@outputFormat} must be
#' supported by the operations capabilities description
#' \item\code{operation@binding} must be supported by the package
#' implementations. See \link{SosBindings}.  } } \item{list("signature(service
#' = \"SOS\", operation = \"SosGetObservationById\", verbose =
#' \"logical\")")}{Checking a GetObservationById request. \strong{Warning:
#' Function not implemented yet.}} \item{list("signature(service = \"SOS\",
#' operation = \"SosGetObservation\", verbose = \"logical\")")}{Checking a
#' GetObservation request. \strong{Warning: Function not implemented yet.}}
#' \item{list("signature(service = \"SOS\", operation =
#' \"OwsGetCapabilities_1.1.0\", verbose = \"logical\")")}{Checking a
#' GetCapabilities request. \strong{Warning: Function not implemented yet.}}
#' \item{list("signature(service = \"SOS\", operation =
#' \"OwsGetCapabilities_2.0.0\", verbose = \"logical\")")}{Checking a
#' GetCapabilities request. \strong{Warning: Function not implemented yet.}} }
#' @keywords methods
NULL





#' Constants in sos4R
#' 
#' The package \pkg{sos4R} comes with a set of constant character strings and
#' fixed supported features, for example for names of XML elements, XML
#' Namespace prefixes, or supported formats and models.
#' 
#' Most of these variables should be pretty self-explanatory.
#' 
#' Constants for names of XML elements start with a lowercase character string
#' of the namespace prefix (e.g. "gml"), a unique name of the element (where
#' parts like "type" and special characters may be left out, and other
#' descripte elements may be added for clarity), and end with "Name".
#' 
#' Examples: codegmlEnvelopeName, \code{ogcGeometryOperandLineStringName},
#' \code{ogcTempOpTMEqualsName}.
#' 
#' The \code{OwsExceptionsData()} function provides access to the fixed
#' exception codes, meanings and respective HTTP codes and messages.
#' 
#' The \strong{mime types} are used for automatic detection of the best fitting
#' parser.
#' 
#' @aliases Constants gmlBeginName gmlBeginPositionName gmlBoundedByName
#' gmlDescriptionName gmlDurationName gmlEndName gmlEndPositionName
#' gmlEnvelopeName gmlFeatureCollectionName gmlFeatureMemberName
#' gmlLowerCornerName gmlNameName gmlNamespacePrefix gmlPointName gmlPosName
#' gmlIdentifierName gmlRelatedTimeName gmlTimeInstantName gmlTimeIntervalName
#' gmlTimeLengthName gmlTimePeriodName gmlTimePositionName gmlUpperCornerName
#' ogcBBOXName ogcArithmeticOperatorsName ogcComparisonOpBetweenName
#' ogcComparisonOpEqualToName ogcComparisonOpGreaterThanName
#' ogcComparisonOpGreaterThanOrEqualToName ogcComparisonOpLessThenName
#' ogcComparisonOpLessThanOrEqualToName ogcComparisonOpIsLikeName
#' ogcComparisonOpIsNotEqualTo ogcComparisonOpIsNull ogcContainsName ogcEIDName
#' ogcFIDName ogcGeometryOperandName ogcGeometryOperandsName ogcIdCapabilities
#' ogcGeometryOperandEnvelopeName ogcGeometryOperandPolygonName
#' ogcGeometryOperandPointName ogcGeometryOperandLineStringName
#' ogcIntersectsName ogcLiteralName ogcLogicalOperatorsName ogcNamespacePrefix
#' ogcOverlapsName ogcPropertyNameName ogcScalarCapabilitiesName
#' ogcSpatialCapabilitiesName ogcSpatialOperatorName ogcSpatialOperatorsName
#' ogcSpatialOpBBOXName ogcSpatialOpContainsName ogcSpatialOpIntersectsName
#' ogcSpatialOpOverlapsName ogcSpatialOpBeyondName ogcSpatialOpCrossesName
#' ogcSpatialOpDWithinName ogcSpatialOpDisjointName ogcSpatialOpEqualsName
#' ogcSpatialOpTouchesName ogcSpatialOpWithinName ogcTempOpTMAfterName
#' ogcTempOpTMBeforeName ogcTempOpTMBeginsName ogcTempOpTMBegunByName
#' ogcTempOpTMContainsName ogcTempOpTMDuringName ogcTempOpTMEndedByName
#' ogcTempOpTMEndsName ogcTempOpTMEqualsName ogcTempOpTMMeetsName
#' ogcTempOpTMMetByName ogcTempOpTMOverlapsName ogcTempOpTTMOverlappedBy
#' ogcTemporalCapabilitiesName ogcTemporalOperandsName ogcTemporalOperandName
#' ogcTemporalOperatorsName ogcTemporalOperatorName omMeasurementName
#' omMemberName omObservationName omObservationCollectionName
#' omFeatureOfInterestName omProcedureName omObservedPropertyName
#' omResultTimeName omSamplingTimeName omResultName omCategoryObservationName
#' omCountObservationName omTruthObservationName omGeometryObservationName
#' omTemporalObservationName omComplexObservationName saSamplingPointName
#' saSamplingSurface saPositionName saSampledFeatureName saSamplingTimeName
#' owsServiceIdentificationName owsTitleName owsAbstractName owsKeywordsName
#' owsKeywordName owsServiceTypeName owsServiceTypeVersionName owsFeesName
#' owsAccessConstraintsName owsServiceProviderName owsOperationsMetadataName
#' owsOperationName owsDCPName owsHTTPName owsGetName owsPostName
#' owsParameterName owsAllowedValuesName owsValueName owsAnyValueName
#' owsRangeName owsMinimumValueName owsMaximumValueName owsSpacingName
#' owsConstraintName owsMetadataName owsExceptionName owsExceptionTextName
#' owsProfileName owsProviderNameName owsProviderSiteName owsServiceContactName
#' saSamplingPointName saSamplingSurface saPositionName saSampledFeatureName
#' saSamplingTimeName sosService sos100NamespacePrefix sosGetCapabilitiesName
#' sosDescribeSensorName sosGetObservationName sosGetObservationByIdName
#' owsExceptionReportName sosGetFeatureOfInterestName
#' sosIntendedApplicationName sosTimeName sosProcedureName
#' sosProcedureDescriptionFormat sosObservedPropertyName
#' sosFeatureOfInterestName sosFeatureOfInterestTypeName sosResultModelName
#' sosResponseFormatName sosResponseModeName sosObservationOfferingName
#' sosObservationOfferingListName sosContentsName sosFilterCapabilitiesName
#' sosCapabilitiesName sosEventTimeName sosObjectIDName sosResultName
#' sweCompositePhenomenonName sweBaseName sweComponentName sweDataArrayName
#' sweElementTypeName sweSimpleDataRecordName sweDataRecordName sweFieldName
#' sweTimeName sweQuantityName sweCategoryName sweBooleanName sweCountName
#' sweEncodingName sweTextBlockName sweValuesName sweValueName sweCodeSpaceName
#' sweTextName sweUomName sweCoordinateName sweLocationName swePositionName
#' sweVectorName xmlTextNodeName OwsExceptionsData ogcComparisonOperatorsName
#' ogcTempOpTMOverlappedBy owsNamespacePrefix sosDescribeFeatureTypeName
#' sosDescribeObservationTypeName sosDescribeResultModelName
#' sosGetFeatureOfInterestTimeName sosGetResultName sosInsertObservationName
#' sosRegisterSensorName mimeTypeCSV mimeTypeOM mimeTypeSML mimeTypeXML
#' mimeSubtypeOM smlSensorMLName sosAttributeFileName sos100_version
#' sos200_version sosKVPParamNameRequest sosKVPParamNameService
#' sosKVPParamNameVersion sosKVPParamNameOffering sosKVPParamNameObsProp
#' sosKVPParamNameFoi sosKVPParamNameResponseFormat sosKVPParamNameSrsName
#' sosKVPParamNameEventTime sosKVPParamNameProcedure sosKVPParamNameResultModel
#' sosKVPParamNameResponseMode sosKVPParamNameBBOX SosAllNamespaces
#' gmlNamespace omNamespace omNamespacePrefix ogcNamespace owsAcceptFormatsName
#' owsAcceptLanguagesName owsAcceptVersionsName owsExceptionReportNameOnly
#' owsLanguageName owsNamespace owsOutputFormatName owsSectionName
#' owsSectionsName owsVersionName sfNamespacePrefix sfSampledFeatureName
#' sfTypeName smlNamespace sos100Namespace sweLowerCornerName sweNamespace
#' sweNamespacePrefix sweUpperCornerName xlinkNamespace xsiNamespace
#' sos200ContentsName sos200FilterCapabilitiesName om20ResultMeasureTypeName
#' om20ResultTypeAttributeName om20OM_Observation om20PhenomenonTimeName
#' samsShapeName sosGetFeatureOfInterestResponseName
#' sosGetObservationResponseName sosObservationTypeName sosObservedAreaName
#' sosPhenomenonTimeName sosResultTimeName sweTextEncodingName
#' swesIdentifierName swesNameName swesObservablePropertyName swesOfferingName
#' swesProcedureDescriptionFormatName wmlMonitoringPointName fesNamespace
#' gml32Namespace om20Namespace om20NamespacePrefix saNamespace
#' saNamespacePrefix samsNamespace samsNamespacePrefix samsSamplingFeatureName
#' sf20Namespace sos200FeatureOfInterestTypeName sos200Namespace
#' sos200NamespacePrefix sos200ObservationOfferingListName
#' sos200ObservationOfferingName sos200ObservationTypeName
#' sos200ObservedAreaName sos200PhenomenonTimeName sos200ResponseFormatName
#' sos200ResponseModeName sos200ResultTimeName sos200contentsName
#' sosGDAMemberName sosGetDataAvailabilityName sosGetDataAvailabilityResponse
#' sosObservationIdName sosOfferingName swesNamespace swesNamespacePrefix
#' swesProcedureName
#' @references See also \code{\link{Defaults}} for default parameter settings.
#' 
#' Whiteside A. (Ed.), OGC Web Services Common Specification, Open Geospatial
#' Consortium Inc., OGC 06-121r3, Version: 1.1.0 with Corrigendum 1
#' @keywords constants XML
#' @examples
#' 
#' 
#' # example constants
#' sos100NamespacePrefix
#' gmlNameName
#' sweUomName
#' 
#' # Data frame holding OWS exception code information
#' OwsExceptionsData()
#' 
#' # Get all namespaces
#' SosAllNamespaces()
#' 
NULL





#' Default Parameter Settings and Handling Functions
#' 
#' These values are default parameters and handling functions for connections
#' and requests to, as well as response processing of ansers from, Sensor
#' Observation Services. These allow to simplify a SOS connection for the most
#' common use cases and non-expert users.
#' 
#' The default values are strongly related to what is actually implemented in
#' the package, but also often resemble the (hopefully) most common use cases.
#' 
#' Some defaults are accessed directly, others should be accessed using a
#' function. The latter is required for cases where a runtime evaluation is
#' needed, e.g. for default values of construction functions.
#' 
#' A special case are the functions to access the default functions for
#' specific purposes, which are the parsing functions, the encoding functions
#' and the field converting functions. See the examples on how to use them.
#' 
#' The function \code{SosDisabledParsers} can be used to use no parsing at all
#' (despite the parsing for the capabilities response, which is required for
#' establishing a connection to a SOS. This function is helpful to inspect the
#' unprocessed responses from a service.
#' 
#' The function \code{SosResetParsingFunctions} can be used to replace the
#' included parsing functions of a \code{SOS} object with the default ones.
#' This is even useful for development of the default parsing functions.
#' 
#' \bold{The default parameter values are:} \describe{
#' \item{sosDefaultCharacterEncoding}{\verb{\Sexpr[results=verbatim,stage=render]{sosDefaultCharacterEncoding}}}
#' % "UTF-8"
#' \item{sosDefaultDescribeSensorOutputFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultDescribeSensorOutputFormat}}}
#' % SosSupportedResponseFormats()[2]
#' \item{sosDefaultGetCapSections}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetCapSections}}}
#' % c("All")
#' \item{sosDefaultGetCapAcceptFormats}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetCapAcceptFormats}}}
#' % c("text/xml")
#' \item{sosDefaultGetCapOwsVersion}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetCapOwsVersion}}}
#' % "1.1.0"
#' \item{sosDefaultGetObsResponseFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultGetObsResponseFormat}}}
#' % SosSupportedResponseFormats()[1]
#' \item{sosDefaultTimeFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultTimeFormat}}}
#' % "%Y-%m-%dT%H:%M:%OS"
#' \item{sosDefaultFilenameTimeFormat}{\verb{\Sexpr[results=text,stage=render]{sosDefaultFilenameTimeFormat}}}
#' %
#' \item{sosDefaultTempOpPropertyName}{\verb{\Sexpr[results=text,stage=render]{sosDefaultTempOpPropertyName}}}
#' % "om:samplingTime"
#' \item{sosDefaultTemporalOperator}{\verb{\Sexpr[results=text,stage=render]{sosDefaultTemporalOperator}}}
#' % SosSupportedTemporalOperators()[[ogcTempOpTMDuringName]]
#' \item{sosDefaultSpatialOpPropertyName}{\verb{\Sexpr[results=text,stage=render]{sosDefaultSpatialOpPropertyName}}}
#' % "urn:ogc:data:location" }
#' 
#' The \bold{default parsing functions} can be replaced for a variety of XML
#' elements, so that you only need to replace the parts of the parsing that
#' really must be changed. Be aware that inclusion and exclusion are performed
#' after merging the given functions with the defaults!
#' 
#' \bold{Example Services:} This list contains a few SOS instances that were
#' tested (to different degress) with \pkg{sos4R}. The package authors do not
#' maintain these services, so no guarantee can be given that these are usable.
#' 
#' @aliases sosDefault Defaults sosDefaultCharacterEncoding SosDefaultBinding
#' SosDefaultDCPs sosDefaultDescribeSensorOutputFormat
#' sosDefaultGetCapAcceptFormats sosDefaultGetCapOwsVersion
#' sosDefaultGetCapSections sosDefaultGetObsResponseFormat
#' sosDefaultSpatialOpPropertyName sosDefaultTempOpPropertyName
#' sosDefaultTemporalOperator sosDefaultTemporalValueReference
#' sosDefaultTimeFormat sosDefaultFilenameTimeFormat
#' sosDefaultColumnNameFeatureIdentifier sosDefaultColumnNameLat
#' sosDefaultColumnNameLon sosDefaultColumnNameSRS sosDefaultColorPalette
#' sosDefaultReferenceFrameSensorDescription SosParsingFunctions
#' SosEncodingFunctions SosDisabledParsers SosDataFieldConvertingFunctions
#' SosResetParsingFunctions SosDefaults SosDefaults2 SosDefaultParsingOptions
#' @usage SosDefaultBinding()
#' 
#' SosParsingFunctions(..., include = character(0), exclude = character(0))
#' SosEncodingFunctions(..., include = character(0), exclude = character(0))
#' SosDataFieldConvertingFunctions(..., include = character(0), exclude =
#' character(0))
#' 
#' SosDisabledParsers()
#' 
#' SosDefaults()
#' 
#' SosResetParsingFunctions(sos)
#' 
#' SosDefaultDCPs()
#' 
#' SosDefaultParsingOptions()
#' @param \dots Named references to functions to be used for the respective
#' element during parsing, encoding oder conversion, e.g. \code{"myUnit" =
#' myUnitParser}.
#' @param include A list of names of elements whose functions shall be included
#' in the returned list, e.g. \code{include = c("GetObservation",
#' "DescribeSensor")}. This inclusion is done \bold{after} replacing the
#' default functions based on the \code{...} argument.
#' @param exclude A list of names of elements whose functions shall be excluced
#' in the returned list, e.g. \code{exclude = c("DescribeSensor")}. This
#' exclusion is done \bold{after} replacing the default functions based on the
#' \code{...} argument.
#' @param sos An object of class \code{SOS}.
#' @return The default value of the respective setting or parameter. This can
#' be a list, especially a named list of functions.
#' @references \code{\link{Constants}}
#' @keywords misc
#' @examples
#' 
#' # simple default values
#' show(sosDefaultCharacterEncoding)
#' show(sosDefaultDescribeSensorOutputFormat)
#' show(sosDefaultGetCapAcceptFormats)
#' show(sosDefaultGetCapOwsVersion)
#' show(sosDefaultGetCapSections)
#' show(sosDefaultGetObsResponseFormat)
#' show(sosDefaultSpatialOpPropertyName)
#' show(sosDefaultTempOpPropertyName)
#' show(sosDefaultTemporalOperator)
#' show(sosDefaultTimeFormat)
#' SosDefaultBinding()
#' 
#' \dontrun{
#' # usage of defaults in construction method for SOS class
#' sos <- SOS("http://mysos.com/sos", binding = SosDefaultBinding(),
#' 		timeFormat = sosDefaultTimeFormat)
#' }
#' 
#' # functions to disable all parsing
#' SosDisabledParsers()
#' 
#' # Replace a parsing function
#' myER <- function(xml) {
#' 	return("EXCEPTION!!!11")
#' }
#' SosParsingFunctions("ExceptionReport" = myER)
#' 
#' # use inclusion and exclusion, important: even the just added function needs to
#' # be included manually!
#' SosParsingFunctions("ExceptionReport" = myER,
#' 	include = c("GetObservation", "DescribeSensor", "ExceptionReport"))
#' SosParsingFunctions(exclude = c("GetObservation", "DescribeSensor"))
#' 
#' \dontrun{
#' # Replace an encoding function
#' myEncoding <- function(object, v) {
#' 	return(str(object))
#' }
#' 
#' sos = SOS(url = "http://mysos.com/sos",
#' 		encoders = SosEncodingFunctions("POST" = myPostEncoding))
#' 
#' # Use custom converting function and connection method. This mechanism works the
#' # same for encoders and decoders.
#' myConverters <- SosDataFieldConvertingFunctions(
#' 	"myNumericUnit" = sosConvertDouble,
#' mySos <- SOS(sos.url, binding = "KVP", dataFieldConverters = myConverters)
#' sosDataFieldConverters(mySos)
#' 
#' # inspecting XML using dummy parsing function
#' sos = SOS(url = "http://mysos.com/sos", parsers = SosDisabledParsers)
#' describeSensor(sos, sosProcedures(sos)[[1]])
#' }
#' 
#' # a list of example services
#' SosExampleServices()
#' 
#' # a named list of all defaults
#' SosDefaults()
#' 
#' # replace the parsing functions with the default ones
#' \dontrun{
#' sos <- SosResetParsingFunctions(sos)
#' }
#' 
#' # parsing options used by xml2
#' SosDefaultParsingOptions()
#' 
#' 
NULL





#' Method for a DescribeSensor Request to a SOS
#' 
#' This method sends a DescribeSensor request for a description of the given
#' procedure to the given Sensor Observation Service instance.
#' 
#' 
#' @name describeSensor-methods
#' @aliases describeSensor describeSensor-methods
#' describeSensor,SOS,character-method
#' describeSensor,SOS_1.0.0,character-method
#' describeSensor,SOS_2.0.0,character-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(sos = \"SOS\", procedure
#' = \"character\")")}{Method requests a description of the given procedure
#' from the given SOS.} }
#' @keywords methods
NULL





#' Class and Construction Function for "SosDescribeSensor"
#' 
#' The DescribeSensor Operatiosn of a Sensor Observation Service can be used to
#' retrieve metadata of procedures that are available from a SOS. This sensor
#' description is normally encoded in \verb{SensorML}.
#' 
#' Please also consult the specification for details on possible contents of
#' the request.
#' 
#' This functions should not be called directly, but instead using the function
#' \code{\link{describeSensor}}.
#' 
#' 
#' @name DescribeSensor
#' @aliases DescribeSensor SosDescribeSensor SosDescribeSensor-class
#' show,SosDescribeSensor-method print,SosDescribeSensor-method
#' toString,SosDescribeSensor-method
#' @docType class
#' @usage SosDescribeSensor(service, version, procedure, outputFormat)
#' @param service The service attribute of the request, e.g. \samp{SOS}.
#' @param version The version attribute of the request, e.g. \samp{1.0.0}.
#' @param procedure The value of the procedure elements in the request, e.g.
#' \samp{urn:procedure:42}.
#' @param outputFormat The value of the output format element in the request,
#' e.g. \samp{text/xml;subtype="sensorML/1.0.1"}
#' @return The value of the construction function is an object of class
#' \link{SosDescribeSensor-class}
#' @section Objects from the Class: Objects can be created by calling the
#' construction function of the form \code{DescribeSensor(...)}. The contain
#' the procedure identifier that is to be described by a service.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See Also \code{\link{SensorML}} and \link{describeSensor}.
#' @references See OGC 06-009r6 section 8.4, or the XSD schema file at
#' \url{http://schemas.opengis.net/sos/1.0.0/sosDescribeSensor.xsd}.
#' @keywords classes
#' @examples
#' 
#' showClass("SosDescribeSensor")
#' 
#' # example for construction function
#' describeSensorRequest <- SosDescribeSensor(service = "SOS", version = "1.0.0",
#' 	procedure = "urn:procedure:42", outputFormat = "text/xml")
#' print(describeSensorRequest)
#' 
#' # encode the request in XML
#' sos <- SOS_Test()
#' encodeRequestXML(describeSensorRequest, sos)
#' toString(encodeRequestXML(describeSensorRequest, sos))
#' 
#' # request a sensor description
#' mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
#'              binding = "KVP")
#' mySensor <- describeSensor(sos = mySOS,
#' 		procedure = sosProcedures(mySOS)[[1]],
#' 		outputFormat = 'text/xml; subtype="sensorML/1.0.1"', # space is needed!
#' 		)
#' 
NULL





#' Encode Classes as KVP
#' 
#' These methods convert a given object to a key-value-pair representation to
#' be used in GET requests.  The given instance of \code{SOS} is possibly used
#' for encoding sub-elements or accessing metadata which is required for the
#' encoding, like time stamp format.
#' 
#' 
#' @name encodeKVP-methods
#' @aliases encodeKVP encodeKVP-methods
#' encodeKVP,OgcBinaryTemporalOp,SOS-method encodeKVP,SosEventTime,SOS-method
#' encodeKVP,SosEventTime,ANY-method encodeKVP,POSIXt,SOS-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(obj =
#' \"OgcBinaryTemporalOp\", sos = \"SOS\")")}{Convert the given object to a KVP
#' representation.}
#' 
#' \item{list("signature(obj = \"SosEventTime\", sos = \"ANY\")")}{Convert the
#' given object to a KVP representation.}
#' 
#' \item{list("signature(obj = \"POSIXt\", sos = \"ANY\")")}{Convert the given
#' object to a string format suitable for KVP representation.} }
#' @seealso \code{\link{SosBindings}},
#' \url{https://en.wikipedia.org/wiki/Key_Value_Pair}
#' @keywords methods
NULL





#' Methods for Encoding Requests to SOS in KVP Format
#' 
#' These methods encode objects representing requests to a Sensor Observation
#' Service into a key-value-pair format which can be used in the GET binding,
#' see \link{SosBindings}.
#' 
#' 
#' @name encodeRequestKVP-methods
#' @aliases encodeRequestKVP encodeRequestKVP-methods
#' encodeRequestKVP,SosDescribeSensor-method
#' encodeRequestKVP,SosGetObservation-method
#' encodeRequestKVP,SosGetObservationById-method
#' encodeRequestKVP,OwsGetCapabilities-method
#' encodeRequestKVP,OwsGetCapabilities_1.1.0-method
#' encodeRequestKVP,OwsGetCapabilities_2.0.0-method
#' encodeRequestKVP,SosGetFeatureOfInterest_2.0.0-method
#' encodeRequestKVP,SosGetObservation_2.0.0-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(obj =
#' \"SosDescribeSensor\")")}{Encode a DescribeSensor request.}
#' \item{list("signature(obj = \"SosGetObservation\")")}{Encode a
#' GetObservation request.} \item{list("signature(obj =
#' \"SosGetObservationById\")")}{Encode GetObservationById request.}
#' \item{list("signature(obj = \"OwsGetCapabilities\")")}{Dispatching method,
#' checks the version attribute and forwards the encoding to the appropriate
#' method. This method should be called rather than calling the versioned
#' methods directly!} \item{list("signature(obj =
#' \"OwsGetCapabilities_1.1.0\")")}{Encode GetCapabilities request with OWS
#' version 1.1.0.} \item{list("signature(obj =
#' \"OwsGetCapabilities_2.0.0\")")}{Encode GetCapabilities request with OWS
#' version 2.0.0.} }
#' @keywords methods
NULL





#' Methods for Encoding Requests to SOS in SOAP Format
#' 
#' These methods encode objects representing requests to a Sensor Observation
#' Service into a SOAP message format to be used in the SOAP binding (see
#' \code{\link{SosBindings}}).
#' 
#' 
#' @name encodeRequestSOAP-methods
#' @aliases encodeRequestSOAP encodeRequestSOAP-methods
#' encodeRequestSOAP,SosDescribeSensor-method
#' encodeRequestSOAP,SosGetObservation-method
#' encodeRequestSOAP,SosGetObservationById-method
#' encodeRequestSOAP,OwsGetCapabilities-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(obj =
#' \"SosDescribeSensor\")")}{Encode a DescribeSensor operation.}
#' \item{list("signature(obj = \"SosGetObservation\")")}{Encode a
#' GetObservation operation.} \item{list("signature(obj =
#' \"SosGetObservationById\")")}{Encode a GetObservationById operation.}
#' \item{list("signature(obj = \"OwsGetCapabilities\")")}{Encode a
#' GetCapabilities operation.} }
#' @seealso \code{\link{SosBindings}} ,\code{\link{encodeXML}}
#' @keywords methods
NULL





#' Methods for Encoding Requests to SOS in XML Format
#' 
#' These methods encode objects representing requests to a Sensor Observation
#' Service into a XML format wich can be used in the POST binding, see
#' \code{\link{SosBindings}}.
#' 
#' 
#' @name encodeRequestXML-methods
#' @aliases encodeRequestXML encodeRequestXML-methods
#' encodeRequestXML,SosDescribeSensor-method
#' encodeRequestXML,SosGetObservation-method
#' encodeRequestXML,SosGetObservationById-method
#' encodeRequestXML,OwsGetCapabilities-method
#' encodeRequestXML,OwsGetCapabilities_1.1.0-method
#' encodeRequestXML,OwsGetCapabilities_2.0.0-method
#' encodeRequestXML,SosEventTime,SOS-method
#' encodeRequestXML,SosFeatureOfInterest,SOS-method
#' encodeRequestXML,XMLNode,SOS-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(obj =
#' \"SosDescribeSensor\")")}{Encode a DescribeSensor request.}
#' \item{list("signature(obj = \"SosGetObservation\")")}{Encode a
#' GetObservation request.} \item{list("signature(obj =
#' \"SosGetObservationById\")")}{Encode a GetObservationById request.}
#' \item{list("signature(obj = \"OwsGetCapabilities\")")}{Encode a
#' GetCapabilities request.} }
#' @seealso \code{\link{SosBindings}}, \code{\link{encodeXML}}
#' @references Annex A (normative) \dQuote{SOS Schema} in Na, A., Priest, M.
#' (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC
#' 06-009r6, Version: 1.0.
#' @keywords methods XML
NULL





#' Encode Classes as XML
#' 
#' These methods convert the given objects to XML representations for
#' \code{HTTP POST} requests. The given instance of \code{SOS} is possibly used
#' for encoding sub-elements or accessing metadata which is required for the
#' encoding, like time stamp format.
#' 
#' 
#' @name encodeXML-methods
#' @aliases encodeXML encodeXML-methods encodeXML,GmlDirectPosition,SOS-method
#' encodeXML,GmlEnvelope,SOS-method encodeXML,GmlLineString,SOS-method
#' encodeXML,GmlPointProperty,SOS-method encodeXML,GmlPoint,SOS-method
#' encodeXML,GmlPolygon,SOS-method encodeXML,GmlTimeInstantProperty,SOS-method
#' encodeXML,GmlTimeInstant,SOS-method encodeXML,GmlTimePeriod,SOS-method
#' encodeXML,GmlTimePosition,SOS-method encodeXML,OgcBBOX,SOS-method
#' encodeXML,OgcComparisonOps,SOS-method encodeXML,OgcContains,SOS-method
#' encodeXML,OgcIntersects,SOS-method encodeXML,OgcOverlaps,SOS-method
#' encodeXML,SosEventTime,SOS-method encodeXML,SosFeatureOfInterest,SOS-method
#' encodeXML,TM_After,SOS-method encodeXML,TM_Before,SOS-method
#' encodeXML,TM_During,SOS-method encodeXML,TM_Equals,SOS-method
#' encodeXML,XMLNode,SOS-method encodeXML,character,SOS-method
#' encodeXML,XMLInternalElementNode,SOS-method encodeXML,POSIXt,SOS-method
#' encodeXML,xml_document,SOS-method encodeXML,xml_node,SOS-method
#' @docType methods
#' @section Methods: \describe{
#' 
#' \item{list("signature(obj = \"GmlDirectPosition\", sos = \"SOS\")")}{Convert
#' the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlEnvelope\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlLineString\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlPointProperty\", sos = \"SOS\")")}{Convert
#' the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlPoint\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlPolygon\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlTimeInstantProperty\", sos =
#' \"SOS\")")}{Convert the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlTimeInstant\", sos = \"SOS\")")}{Convert
#' the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlTimePeriod\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"GmlTimePosition\", sos = \"SOS\")")}{Convert
#' the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"POSIXt\", sos = \"SOS\")")}{Converts the time
#' object to a string that is suitable to be used as the value of XML time
#' elements.}
#' 
#' \item{list("signature(obj = \"OgcBBOX\", sos = \"SOS\")")}{Convert the given
#' object to an XML representation.}
#' 
#' \item{list("signature(obj = \"OgcComparisonOps\", sos = \"SOS\")")}{Convert
#' the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"OgcContains\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"OgcIntersects\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"OgcOverlaps\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"SosEventTime\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"SosFeatureOfInterest\", sos =
#' \"SOS\")")}{Convert the given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"TM_After\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"TM_Before\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"TM_During\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"TM_Equals\", sos = \"SOS\")")}{Convert the
#' given object to an XML representation.}
#' 
#' \item{list("signature(obj = \"XMLNode\", sos = \"SOS\")")}{Convert the given
#' object to an XML representation.}
#' 
#' }
#' @seealso \code{\link{encodeRequestXML}}, \code{\link{encodeRequestSOAP}}
#' @references XML specification: \url{http://www.w3.org/XML/}.
#' @keywords methods
NULL





#' Class \code{"FoiOrNULL"}
#' 
#' Classes and construction functions for features of interest from the OGC
#' Observations and Measurements specification. A feature of interest is
#' considered to be the real world feature that is observed.
#' 
#' 
#' @name FoiOrNULL-class
#' @docType class
#' @section Objects from the Class: A virtual Class: No objects may be created
#' from it.
#' @keywords classes
#' @examples
#' 
#' showClass("FoiOrNULL")
#' 
NULL





#' Request Capabilities from a SOS
#' 
#' This method request the metadata description of a given Sensor Observation
#' Service, the Capabilities document.
#' 
#' 
#' @name getCapabilities-methods
#' @aliases getCapabilities getCapabilities-methods getCapabilities,SOS-method
#' getCapabilities,SOS_1.0.0-method
#' getCapabilities,SOS_1.0.0,verbose,inspect-method
#' getCapabilities,SOS_2.0.0-method
#' getCapabilities,SOS_2.0.0,verbose,inspect-method
#' @docType methods
#' @section Methods:
#' 
#' \describe{ \item{list("signature(sos = \"SOS\")")}{Request capabilities
#' description from a SOS.} \item{list("signature(inspect =
#' \"logical\")")}{Print out the sent and received documents to console.}
#' \item{list("signature(verbose = \"logical\")")}{Extensive debugging
#' information printed to console.} }
#' @keywords methods
NULL





#' Class and construction function for \code{"GetDataAvailability"} operation
#' 
#' See SOS 2.0 Hydrology profile specification, OGC 14-004r1, section 7.4,
#' requirement 12
#' 
#' 
#' @name SosGetDataAvailability_1.0.0-class
#' @aliases DataAvailabilityMember DataAvailabilityMember-class
#' print,DataAvailabilityMember-method show,DataAvailabilityMember-method
#' toString,DataAvailabilityMember-method SosGetDataAvailability_1.0.0
#' SosGetDataAvailability_1.0.0-class SosGetDataAvailability_1.0.0-class
#' encodeRequestKVP,SosGetDataAvailability_1.0.0-method
#' sosName,SosGetDataAvailability_1.0.0-method
#' toString,SosGetDataAvailability_1.0.0-method
#' getDataAvailability,SOS_2.0.0-method getDataAvailability
#' parseGetDataAvailabilityResponse
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to
#' construction functions:
#' 
#' \code{DataAvailabilityMember(...)}
#' 
#' \code{SosGetDataAvailability_1.0.0(...)}
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references OGC 14-004r1, section 7.4, requirement 12
#' @keywords classes
#' @examples
#' 
#' showClass("DataAvailabilityMember")
#' showClass("SosGetDataAvailability_1.0.0")
#' 
NULL





#' Function retrieving features of interest, i.e. the representations of the
#' real world features that are observed and for which observations are
#' provided.
#' 
#' This function retrieves , i.e. the representations of the real world
#' features that are observed and for which observations are provided, from a
#' Sensor Observation Service.
#' 
#' 
#' @aliases getFeatureOfInterest getFeatureOfInterest,SOS_2.0.0,ANY-method
#' getFeatureOfInterest,SOS_2.0.0,character-method
#' @usage getFeatureOfInterest(sos, featureOfInterest, verbose =
#' sos@verboseOutput, inspect = FALSE, saveOriginal = FALSE)
#' @param sos The Sensor Observation Service from which features of interest
#' should be retrieved
#' @param featureOfInterest identifier(s) of features of interest
#' @param verbose A boolean value indicating whether debug information is
#' printed out to the console during the execution
#' @param inspect A boolean value to enable printing of the sent request and
#' received response to the console
#' @param saveOriginal Save the received document to the current working
#' directory. If \code{TRUE} a filename is automatically generated, if a
#' character string is given it is used as the filename
#' @return An object of class \link{SosGetFeatureOfInterest_2.0.0-class}.
#' @examples
#' 
#' ##---- Should be DIRECTLY executable !! ----
#' ##-- ==>  Define data, use random,
#' ##--	or do  help(data=index)  for the standard data sets.
#' 
#' 
NULL





#' Request Observations
#' 
#' This method sends a GetObservation request to the given SOS.
#' 
#' It takes a variety of inputs, of which only the offering is mandatory for
#' GetObservation request, and the observationId for
#' GetObservationByIdRequests.
#' 
#' @name getObservation-methods
#' @aliases getObservation getObservation-methods
#' getObservation,SOS,character-method
#' getObservation,SOS,SosObservationOffering-method
#' getObservation,SOS_1.0.0,character-method
#' getObservation,SOS_1.0.0,SosObservationOffering-method getObservationById
#' getObservationById-methods getObservationById,SOS,character-method
#' getObservationById,SOS_1.0.0,character-method
#' getObservation,SOS_2.0.0,SosObservationOffering_2.0.0-method
#' getObservation,SOS_2.0.0,character-method
#' getObservationById,SOS_2.0.0,character-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(sos = \"SOS\", offering =
#' \"SosObservationOffering\")")}{Request observation data from the given SOS
#' for the given offering (either character identifier or an object of class
#' \code{SosObservationOffering}).}\item{ or }{Request observation data from
#' the given SOS for the given offering (either character identifier or an
#' object of class \code{SosObservationOffering}).}\item{list("signature(sos =
#' \"SOS\", offering = \"character\")")}{Request observation data from the
#' given SOS for the given offering (either character identifier or an object
#' of class \code{SosObservationOffering}).} \item{list("signature(sos =
#' \"SOS\", observationId = \"character\")")}{Request observation data from the
#' given SOS for the given observation identifier.} }
#' @keywords methods
#' @examples
#' 
#' \dontrun{
#' # request observations
#' mySOSpox <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/pox",
#'              binding = "POX", useDCPs = FALSE)
#' 
#' myOffering <- sosOfferings(mySOS)[["ws2500"]]
#' period <- sosCreateTimePeriod(sos = mySOS,
#' 		begin = as.POSIXct("2015/11/01"),
#' 		end = as.POSIXct("2015/11/02"))
#' eventTime <- sosCreateEventTimeList(period)
#' 
#' nov2015 <- getObservation(sos = mySOSpox,
#'                           offering = myOffering,
#'                           eventTime = eventTime)
#' 
#' # request observation by identifier and get the data
#' obsId <- getObservationById(sos = mySOSpox,
#'                             observationId = "http://www.52north.org/test/observation/1")
#' sosResult(obsId, coordinates = TRUE)
#' }
#' 
NULL





#' GetObservation and GetObservationById Request Objects
#' 
#' Classes (and their construction functions) to request observations from a
#' Sensor Observation Service.
#' 
#' Please consult the specification for details on possible contents of the
#' request.
#' 
#' @name GetObservation
#' @aliases GetObservation SosGetObservation SosGetObservation-class
#' show,SosGetObservation-method toString,SosGetObservation-method
#' print,SosGetObservation-method GetObservationById SosGetObservationById
#' SosGetObservationById-class show,SosGetObservationById-method
#' print,SosGetObservationById-method toString,SosGetObservationById-method
#' SosGetObservation_2.0.0 SosGetObservation_2.0.0-class
#' @docType class
#' @usage SosGetObservation(service, version, offering, observedProperty,
#' responseFormat, srsName = as.character(NA), eventTime = list(), procedure =
#' as.character(NA), featureOfInterest = NULL, result = NULL, resultModel =
#' as.character(NA), responseMode = as.character(NA), BBOX = as.character(NA),
#' valueReferenceTemporalFilter = as.character(NA))
#' SosGetObservationById(service, version, observationId, responseFormat,
#' srsName = as.character(NA), resultModel = as.character(NA), responseMode =
#' as.character(NA))
#' @param service The \verb{service} attribute of the request, e.g. \samp{SOS}.
#' @param version The \verb{version} attribute of the request, e.g.
#' \samp{1.0.0}.
#' @param observationId The value of the \verb{ObservationId} element in the
#' request, e.g. \samp{o_12345}, which is to be optained. This could have been
#' obtained by the client via a URL in a feed, alert, or some other
#' notification.
#' @param offering The \verb{offering} element value in the request, e.g.
#' \samp{"temperatures"}. All other parameters are depending on the selected
#' offering.
#' @param observedProperty A list of values for \verb{observedProperty}
#' elements in the request, e.g. \samp{"urn:property:AirTemperature"}. IDs of
#' phenomena are advertised in capabilities document.
#' @param responseFormat The \verb{responseFormat} element value in the
#' request, e.g. \samp{text/xml;subtype="om/1.0.0"}. ID of the output format to
#' be used for the requested data. The supported output formats are listed in
#' the selected offering capabilities.
#' @param srsName The \verb{srsName} attribute of the request, e.g.
#' \samp{urn:ogc:def:crs:EPSG:4326}.
#' @param eventTime A list of objects of class \link{SosEventTime-class} which
#' are added as \verb{eventTime} elements to the request. Allows a client to
#' request observations from a specific instant, multiple instances or periods
#' of time in the past, present and future. The supported range is listed in
#' the selected offering capabilities.
#' @param procedure A list of procedure identifiers added to the request as
#' \verb{procedure} elements.
#' @param featureOfInterest An object of class \link{SosFeatureOfInterest}
#' added to the request as the \verb{featureOfInterest} element, or
#' \code{NULL}. Specifies target feature for which observations are requested.
#' @param result An object of class \link{OgcComparisonOps-class} added to the
#' request as \verb{result} element, or \code{NULL}, or any element that can be
#' encoded using \code{encodeXML(...)} and then be added to an XML document
#' with \code{addChildren(...)}. Filtering: Only report observations where the
#' result matches this expression.
#' @param resultModel The \verb{resultModel} element of the request, e.g.
#' \samp{om:Measurement}, which is an identifier of the result model to be used
#' for the requested data. The resultModel values supported by a service are
#' listed in the contents section of the service metadata, identified as QName
#' values.
#' @param responseMode The \verb{responseMode} element of the request, e.g.
#' \samp{inline}, which allows the client to request the form of the response.
#' @param BBOX A bounding box to be used only with \verb{KVP} encoding in
#' request via \verb{HTTP GET}, in the format
#' \samp{minlon,minlat,maxlon,maxlat,srsURI?}, with the spatial reference
#' system being optional. This element is ignored for \verb{POST} requests, use
#' the parameter \code{featureOfInterest} instead, see
#' \code{\link{SosBindings}}.
#' @param valueReferenceTemporalFilter The property name used in a temporal
#' filter for SOS 2.0 KVP reuests, ignore for SOS 1.0.0.
#' @return An object of class \link{SosGetObservation-class} or
#' \link{SosGetObservationById-class} respectively.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction functions of the form \code{SosGetObservationById(...)} or
#' \code{SosGetObservationById(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\link{SosGetObservation-class}},
#' \code{\link{SosGetObservationById-class}}
#' @references See OGC 06-009r6 section 8.4, or the XSD schema file at
#' \url{http://schemas.opengis.net/sos/1.0.0/sosGetObservation.xsd}.
#' 
#' See OGC 06-009r6 section 10.1, or the XSD schema file at
#' \url{http://schemas.opengis.net/sos/1.0.0/sosGetObservationById.xsd}.
#' @keywords classes utitlities
#' @examples
#' 
#' showClass("SosGetObservation")
#' showClass("SosGetObservationById")
#' 
#' responseFormat <- "text/xml;subtype=&quot;om/1.0.0&quot;"
#' 
#' obsReq <- SosGetObservation(service = "SOS", version = "1.0.0",
#'                             offering = "temperatures",
#'                             observedProperty = list("urn:property:AirTemperature"),
#'                             responseFormat = responseFormat)
#' print(obsReq)
#' 
#' obsByIdReq <- SosGetObservationById(service = "SOS", version = "1.0.0",
#'                                     observationId = "o_12345",
#'                                     responseFormat = responseFormat)
#' print(obsByIdReq)
#' 
#' \dontrun{
#' sos <- SOS("http://mysos.net/sos")
#' encodeXML(obsByIdReq, sos = sos)
#' }
#' 
#' 
NULL





#' Classes and Construction Functions from the GML Namespace
#' 
#' Classes for \code{GML} elements and their respective construction functions.
#' See the referenced specification for details.
#' 
#' The \verb{"...OrNULL"} classes are used to model optional slots.
#' 
#' @aliases GmlDirectPosition-class GmlDirectPositionOrNULL-class
#' GmlEnvelope-class GmlFeature-class GmlFeatureCollection-class
#' GmlFeatureOrNULL-class GmlFeatureProperty-class
#' GmlFeatureOrGmlFeaturePropertyOrNULL-class GmlGeometry-class
#' GmlLineString-class GmlPoint-class GmlPointOrNULL-class
#' GmlPointProperty-class GmlPolygon-class GmlTimeGeometricPrimitive-class
#' GmlTimeInstant-class GmlTimeInstantOrNULL-class GmlTimeInstantProperty-class
#' GmlTimeInstantPropertyOrNULL-class GmlTimeInterval-class
#' GmlTimeIntervalOrNULL-class GmlTimeObject-class GmlTimeObjectOrNULL-class
#' GmlTimePeriod-class GmlTimePosition-class GmlTimePositionOrNULL-class
#' GmlTimePrimitive-class show,GmlDirectPosition-method show,GmlEnvelope-method
#' show,GmlFeatureCollection-method show,GmlFeatureProperty-method
#' show,GmlGeometry-method show,GmlPoint-method show,GmlPointProperty-method
#' show,GmlTimeInstant-method show,GmlTimeInterval-method
#' show,GmlTimePeriod-method show,GmlTimePosition-method
#' print,GmlDirectPosition-method print,GmlEnvelope-method
#' print,GmlFeatureCollection-method print,GmlFeatureProperty-method
#' print,GmlGeometry-method print,GmlPoint-method print,GmlPointProperty-method
#' print,GmlTimeInstant-method print,GmlTimeInstantProperty-method
#' print,GmlTimeInterval-method print,GmlTimePeriod-method
#' print,GmlTimePosition-method toString,GmlDirectPosition-method
#' toString,GmlEnvelope-method toString,GmlFeatureCollection-method
#' toString,GmlFeatureProperty-method toString,GmlGeometry-method
#' toString,GmlPoint-method toString,GmlPointProperty-method
#' toString,GmlTimeInstant-method toString,GmlTimeInstantProperty-method
#' toString,GmlTimeInterval-method toString,GmlTimePeriod-method
#' toString,GmlTimePosition-method GmlDirectPosition GmlDirectPositionLatLon
#' GmlEnvelope GmlFeatureCollection GmlPoint GmlPointProperty
#' GmlFeatureProperty GmlTimeInstant GmlTimeInstantProperty GmlTimeInterval
#' GmlTimePeriod GmlTimePosition GmlMeasure-class print,GmlMeasure-method
#' toString,GmlMeasure-method GmlMeasure show,GmlMeasure-method
#' sosCoordinates,GmlDirectPosition-method
#' sosCoordinates,GmlFeatureCollection-method
#' sosCoordinates,GmlFeatureProperty-method sosCoordinates,GmlPoint-method
#' sosCoordinates,GmlPointProperty-method sosId,GmlFeature-method
#' sosSrsName,GmlDirectPosition-method sosSrsName,GmlPoint-method
#' sosFeatureIds,GmlFeatureCollection-method
#' sosFeatureIds,GmlFeatureProperty-method
#' sosFeaturesOfInterest,GmlFeatureCollection-method
#' sosTime,GmlTimeInstant-method sosTime,GmlTimeInstantProperty-method
#' sosTime,GmlTimePeriod-method sosTime,GmlTimePosition-method
#' summary.GmlTimePeriod print.summary.GmlTimePeriod sosUOM,GmlMeasure-method
#' @usage GmlDirectPosition(pos, srsName = as.character(NA), srsDimension =
#' NA_integer_, axisLabels = as.character(NA), uomLabels = as.character(NA))
#' GmlDirectPositionLatLon(lat, lon, srsName = as.character(NA), srsDimension =
#' NA_integer_, axisLabels = as.character(NA), uomLabels = as.character(NA))
#' GmlEnvelope(lowerCorner, upperCorner, srsName = as.character(NA),
#' srsDimension = NA_integer_, axisLabels = as.character(NA), uomLabels =
#' as.character(NA)) GmlFeatureCollection(featureMembers, id =
#' as.character(NA)) GmlPoint(pos, id = as.character(NA), srsName =
#' as.character(NA), srsDimension = NA_integer_, axisLabels = as.character(NA),
#' uomLabels = as.character(NA)) GmlPointProperty(href = as.character(NA),
#' point = NULL) GmlFeatureProperty(href = as.character(NA), feature = NULL)
#' GmlTimeInstant(timePosition, id = as.character(NA), relatedTimes = list(NA),
#' frame = as.character(NA)) GmlTimeInstantProperty(href = as.character(NA),
#' time = NULL) GmlTimeInterval(interval, unit, radix = NA, factor = NA)
#' GmlTimePeriod(begin = NULL, beginPosition = NULL, end = NULL, endPosition =
#' NULL, duration = as.character(NA), timeInterval = NULL, id =
#' as.character(NA), relatedTimes = list(NA), frame = as.character(NA))
#' GmlTimePosition(time, frame = as.character(NA), calendarEraName =
#' as.character(NA), indeterminatePosition = as.character(NA))
#' GmlMeasure(value, uom)
#' @param axisLabels See corresponding slot description.
#' @param begin See corresponding slot description.
#' @param beginPosition See corresponding slot description.
#' @param calendarEraName See corresponding slot description.
#' @param duration See corresponding slot description.
#' @param end See corresponding slot description.
#' @param endPosition See corresponding slot description.
#' @param factor See corresponding slot description.
#' @param feature See corresponding slot description.
#' @param featureMembers See corresponding slot description.
#' @param frame See corresponding slot description.
#' @param href See corresponding slot description.
#' @param id See corresponding slot description.
#' @param indeterminatePosition See corresponding slot description.
#' @param interval See corresponding slot description.
#' @param lat Latitude coordinate parameter.
#' @param lon Longitue coordinate parameter.
#' @param lowerCorner See corresponding slot description.
#' @param point See corresponding slot description.
#' @param pos See corresponding slot description.
#' @param radix See corresponding slot description.
#' @param relatedTimes See corresponding slot description.
#' @param srsDimension See corresponding slot description.
#' @param srsName See corresponding slot description.
#' @param time See corresponding slot description.
#' @param timeInterval See corresponding slot description.
#' @param timePosition See corresponding slot description.
#' @param unit See corresponding slot description.
#' @param uomLabels See corresponding slot description.
#' @param upperCorner See corresponding slot description.
#' @param value See slot description.
#' @param uom See slot description.
#' @return The construction functions return an object of the respective class.
#' @section Objects from this classes: Objects can be created by calling the
#' according construction functions. e.g. in the form \code{GmlPoint(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references GML specification at
#' \url{https://www.opengeospatial.org/standards/gml}.
#' @keywords classes utilities
#' @examples
#' 
#' showClass("GmlDirectPosition")
#' showClass("GmlEnvelope")
#' showClass("GmlFeature")
#' showClass("GmlFeatureCollection")
#' showClass("GmlFeatureOrNULL")
#' showClass("GmlFeatureProperty")
#' showClass("GmlGeometry")
#' showClass("GmlLineString")
#' showClass("GmlPoint")
#' showClass("GmlPointProperty")
#' showClass("GmlPolygon")
#' showClass("GmlTimeGeometricPrimitive")
#' showClass("GmlTimeInstant")
#' showClass("GmlTimeInstantOrNULL")
#' showClass("GmlTimeInstantProperty")
#' showClass("GmlTimeInstantPropertyOrNULL")
#' showClass("GmlTimeInterval")
#' showClass("GmlTimeIntervalOrNULL")
#' showClass("GmlTimeObject")
#' showClass("GmlTimeObjectOrNULL")
#' showClass("GmlTimePeriod")
#' showClass("GmlTimePosition")
#' showClass("GmlTimePositionOrNULL")
#' showClass("GmlTimePrimitive")
#' 
#' # create direct position
#' pos1 <- GmlDirectPosition(pos = "7.0 52.0")
#' show(pos1)
#' 
#' # create envelope
#' env1 <- GmlEnvelope(upperCorner = pos1,
#'                     lowerCorner = GmlDirectPosition("6.0 51.0"))
#' print(env1)
#' 
#' # wrap elements in feature collection
#' GmlFeatureCollection(id = "001", featureMembers=list(pos1, env1))
#' 
#' # create point with ID
#' point1 <- GmlPoint(pos = pos1, id = "002")
#' 
#' # create point properties
#' GmlPointProperty(href = "http://link.to/point")
#' GmlPointProperty(point = point1)
#' 
#' # time interval of one day
#' GmlTimeInterval(interval = "1", unit = "d")
#' 
#' # referenced feature
#' GmlFeatureProperty(href = "http://link.to/feature")
#' 
#' # create a time position and wrap it into a time instant
#' timePos1 <- GmlTimePosition(time = as.POSIXct("2010-01-01"))
#' 
#' # create direct or referenced time instant
#' timeInst1 <- GmlTimeInstant(timePosition = timePos1)
#' timeInst1
#' 
#' GmlTimeInstantProperty(href = "http://link.to/timeInstant")
#' 
#' # create different variants of time periods
#' # one hour with time positions
#' GmlTimePeriod(beginPosition = timePos1,
#'               endPosition = GmlTimePosition(time = timePos1@time+3600))
#' 
#' # one week backwards from now
#' timePos <- GmlTimePosition(time = Sys.time()-(3600*24*7))
#' aWeekAgo <- GmlTimeInstantProperty(time = GmlTimeInstant(time = timePos))
#' timePos <- GmlTimePosition(time = Sys.time())
#' now <- GmlTimeInstantProperty(time = GmlTimeInstant(time = timePos))
#' GmlTimePeriod(begin = aWeekAgo, end = now)
#' 
NULL





#' Methods for the Namespace kml
#' 
#' Methods for handling responses in Keyhole Markup Language (KML) format.
#' 
#' At the current stage, the parsers simply returns an object from the package
#' \pkg{XML} with the KML document.
#' 
#' 
#' @aliases parseKML mimeTypeKML kmlName kml
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references \url{https://www.opengeospatial.org/standards/kml}
#' \url{https://developers.google.com/kml/documentation/}
#' @keywords methods misc
#' @examples
#' 
#' #
#' 
NULL





#' Class \code{"MonitoringPoint"}
#' 
#' A monitoring point is the feature of interest defined for WaterML
#' observations, i.e. a monitoring point represents the real world feature for
#' which observations are taken. This may be, for example, the position of a
#' stream flow sensor at a river.
#' 
#' 
#' @name MonitoringPoint-class
#' @aliases MonitoringPoint-class MonitoringPoint print,MonitoringPoint-method
#' show,MonitoringPoint-method toString,MonitoringPoint-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("MonitoringPoint", ...)}.
#' @keywords classes
#' @examples
#' 
#' showClass("MonitoringPoint")
#' 
NULL





#' Classes and Construction Functions for the OGC Namespace
#' 
#' These classes represent elements from the OpenGIS(R) Filter Encoding
#' Implementation Specification that are used in requests to Sensor Observation
#' Services.
#' 
#' These comprise spatial and temporal operations and operators which can be
#' encoded in different ways.
#' 
#' The \verb{...OrNULL} classes are used to model optional slots.
#' 
#' % TODO describe class hierarchy here.
#' 
#' @name OGC
#' @aliases OGC ogc OgcBBOX-class show,OgcBBOX-method OgcBinarySpatialOp
#' OgcBinarySpatialOp-class OgcBinaryTemporalOp OgcBinaryTemporalOp-class
#' OgcBinaryTemporalOpOrNULL-class OgcComparisonOps OgcComparisonOps-class
#' OgcComparisonOpsOrXMLOrNULL-class OgcContains-class show,OgcContains-method
#' OgcIntersects-class show,OgcIntersects-method OgcOverlaps-class
#' show,OgcOverlaps-method OgcSpatialOps OgcSpatialOps-class
#' OgcSpatialOpsOrNULL-class OgcBBOX OgcContains OgcIntersects OgcOverlaps
#' print,OgcBBOX-method print,OgcContains-method print,OgcIntersects-method
#' print,OgcOverlaps-method toString,OgcBBOX-method toString,OgcContains-method
#' toString,OgcIntersects-method toString,OgcOverlaps-method
#' @docType class
#' @usage OgcBBOX(propertyName = sosDefaultSpatialOpPropertyName, envelope)
#' OgcContains(propertyName = sosDefaultSpatialOpPropertyName, geometry = NULL,
#' envelope = NULL) OgcIntersects(propertyName =
#' sosDefaultSpatialOpPropertyName, geometry = NULL, envelope = NULL)
#' OgcOverlaps(propertyName = sosDefaultSpatialOpPropertyName, geometry = NULL,
#' envelope = NULL)
#' @param propertyName The value for the propertyName attribute.
#' @param geometry The geometry to be used in a spatial filter.
#' @param envelope The geometry to be used in a sptial filter.
#' @return The value of the construction functions is an object of the
#' respective class.
#' @note This implementation of the Filter Encoding Specification is not
#' complete.
#' @section Objects from the Class: Objects can be created by calls to the
#' respective construction functions of the form \code{OgcBBOX( ...)},
#' \code{OgcContains(...)}, or \code{OgcIntersects}.
#' 
#' The following classes are virtual, no objects may be created from them:
#' \code{OgcBinaryTemporalOp}, \code{OgcBinaryTemporalOpOrNULL},
#' \code{OgcComparisonOps}, codeOgcComparisonOpsOrNULL, \code{OgcSpatialOps},
#' \code{OgcSpatialOpsOrNULL}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references Vretanos, Panagiotis A. (Ed.), OpenGIS(R) Filter Encoding
#' Implementation Specification, OGC 04-095, Version: 1.1.0
#' 
#' Schemas: \url{http://schemas.opengis.net/filter/1.1.0/}
#' @keywords classes utilities
#' @examples
#' 
#' showClass("OgcBBOX")
#' showClass("OgcBinarySpatialOp")
#' showClass("OgcBinaryTemporalOp")
#' showClass("OgcBinaryTemporalOpOrNULL")
#' showClass("OgcComparisonOps")
#' showClass("OgcContains")
#' showClass("OgcOverlaps")
#' showClass("OgcSpatialOps")
#' showClass("OgcSpatialOpsOrNULL")
#' 
NULL





#' Classes for om:Observation Elements
#' 
#' Classes and construction functions for objects from the OGC Observations and
#' Measurements specification.
#' 
#' The class \code{OmObservationProperty} can be used to reference to an
#' (online) observation.
#' 
#' The \verb{...OrNULL} classes are used to model optional slots.
#' 
#' @name OmObservation-class
#' @aliases OmObservation OmObservation-class OmObservationOrNULL-class
#' show,OmObservation-method print,OmObservation-method
#' print,OmObservationProperty-method
#' sosCoordinates,OmObservationProperty-method toString,OmObservation-method
#' toString,OmObservationProperty-method OmObservationProperty-class
#' show,OmObservationProperty-method OmObservationProperty
#' sosProcedures,OmObservation-method names.OmObservation sosResult
#' sosResult,list-method sosResult,OmObservationProperty-method
#' sosResult,OmObservation-method as.data.frame.OmObservation
#' sosCoordinates,OmObservation-method
#' sosFeaturesOfInterest,OmObservation-method
#' sosFeatureIds,OmObservation-method
#' sosObservedProperties,OmObservation-method sosGetCRS,OmObservation-method
#' as.SpatialPointsDataFrame.OmObservation sosUOM,OmObservation-method
#' print.summary.OmObservation summary.OmObservation
#' @docType class
#' @usage OmObservation(samplingTime, procedure, observedProperty,
#' featureOfInterest, result, metadata = NA, resultTime = NULL, resultQuality =
#' NA, parameter = NA) OmObservationProperty(href = as.character(NA), obs =
#' NULL)
#' @param samplingTime See slot description.
#' @param procedure See slot description.
#' @param observedProperty See slot description.
#' @param featureOfInterest See slot description.
#' @param result See slot description.
#' @param metadata See slot description.
#' @param resultTime See slot description.
#' @param resultQuality See slot description.
#' @param parameter See slot description.
#' @param href See slot description.
#' @param obs See slot description.
#' @return The construction functions return an object of the respective class.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction functions of the form \code{OmObservation(...)} and
#' \code{OmObservationProperty(...)}.
#' 
#' The following classes are virtual, no objects may be created from them:
#' \code{OmObservationOrNULL-class}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See Also as \code{\link{OmMeasurement-class}}.
#' @references Cox, S. (Ed.), Observations and Measurements - Part 1 -
#' Observation schema, Open Geospatial Consortium Inc., OGC 07-022r1, Version:
#' 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("OmObservation")
#' showClass("OmObservationProperty")
#' showClass("OmObservationOrNULL")
#' 
#' OmObservationProperty(href = "http://link.to/myObservation")
#' 
#' # get result from an observation
#' \dontrun{
#' result <- observation@result
#' 
#' # the accessor method also works with lists of observations
#' result <- sosResult(observation)
#' resultList <- sosResult(observationList)
#' }
#' 
#' 
NULL





#' Class \code{"OmOM_Observation"}
#' 
#' Classes and construction functions for objects from the OGC Observations and
#' Measurements specification, version 2.0.
#' 
#' 
#' @name OmOM_Observation-class
#' @aliases OmOM_Observation-class OmOM_ObservationOrNULL-class
#' OmOM_Observation sosCoordinates,OmOM_Observation-method
#' sosFeatureIds,OmOM_Observation-method
#' sosFeaturesOfInterest,OmOM_Observation-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("OmOM_Observation", ...)}.
#' @keywords classes
#' @examples
#' 
#' showClass("OmOM_Observation")
#' 
NULL





#' Classes and Construction Functions for Elements of the OWS Namespace
#' 
#' These classes represent elements from the OGC Web Services Common
#' Specification and the OGC Web Services Common Standard.
#' 
#' \code{OwsServiceOperation} is the top class which is eventually put into the
#' request method, \code{sosRequest(...)}.
#' 
#' Classes ending in \verb{...OrNULL} are used to model optional slots.
#' 
#' @name OWS
#' @aliases OwsCapabilities_1.1.0-class OwsCapabilities_2.0.0-class
#' OwsCapabilities-class OwsContents-class OwsContentsOrNULL-class
#' OwsException-class OwsExceptionReport OwsExceptionReport-class
#' OwsGetCapabilities_1.1.0-class OwsGetCapabilities_2.0.0-class
#' OwsGetCapabilities-class OwsOperation-class OwsOperationsMetadata-class
#' OwsOperationsMetadataOrNULL-class OwsRange-class
#' OwsServiceIdentification-class OwsServiceIdentificationOrNULL-class
#' OwsServiceOperation-class OwsServiceProvider-class
#' OwsServiceProviderOrNULL-class print,OwsCapabilities-method
#' print,OwsCapabilities_1.1.0-method print,OwsCapabilities_2.0.0-method
#' print,OwsContents-method print,OwsException-method
#' print,OwsExceptionReport-method print,OwsGetCapabilities-method
#' print,OwsGetCapabilities_1.1.0-method print,OwsGetCapabilities_2.0.0-method
#' print,OwsOperation-method print,OwsOperationsMetadata-method
#' print,OwsRange-method print,OwsServiceIdentification-method
#' print,OwsServiceOperation-method print,OwsServiceProvider-method
#' print.summary.OwsRange show,OwsCapabilities_1.1.0-method
#' show,OwsCapabilities_2.0.0-method show,OwsCapabilities-method
#' show,OwsContents-method show,OwsException-method
#' show,OwsExceptionReport-method show,OwsGetCapabilities_1.1.0-method
#' show,OwsGetCapabilities_2.0.0-method show,OwsGetCapabilities-method
#' show,OwsOperation-method show,OwsOperationsMetadata-method
#' show,OwsRange-method show,OwsServiceIdentification-method
#' show,OwsServiceOperation-method show,OwsServiceProvider-method
#' toString,OwsCapabilities-method toString,OwsCapabilities_1.1.0-method
#' toString,OwsCapabilities_2.0.0-method toString,OwsContents-method
#' toString,OwsException-method toString,OwsExceptionReport-method
#' toString,OwsGetCapabilities-method toString,OwsGetCapabilities_1.1.0-method
#' toString,OwsGetCapabilities_2.0.0-method toString,OwsOperation-method
#' toString,OwsOperationsMetadata-method toString,OwsRange-method
#' toString,OwsServiceIdentification-method toString,OwsServiceOperation-method
#' toString,OwsServiceProvider-method OwsCapabilities OwsContents OwsException
#' OwsGetCapabilities OwsOperation OwsOperationsMetadata OwsRange
#' OwsServiceIdentification OwsServiceProvider
#' sosResult,OwsExceptionReport-method sosTitle,OwsServiceIdentification-method
#' sosAbstract,OwsServiceIdentification-method summary.OwsRange
#' @docType class
#' @usage OwsCapabilities(version, updateSequence = NA, owsVersion =
#' sosDefaultGetCapOwsVersion, identification = NULL, provider = NULL,
#' operations = NULL, contents = NULL, languages = NULL)
#' OwsException(exceptionCode, exceptionText = c(), locator = as.character(NA))
#' OwsExceptionReport(version, lang = as.character(NA), exceptions = list(NA))
#' OwsGetCapabilities(service, acceptVersions, sections =
#' sosDefaultGetCapSections, acceptFormats = sosDefaultGetCapAcceptFormats,
#' updateSequence = c(as.character(NA)), owsVersion =
#' sosDefaultGetCapOwsVersion, acceptLanguages = c(NA)) OwsOperation(name,
#' DCPs, parameters = list(NA), constraints = list(NA), metadata = list(NA))
#' OwsOperationsMetadata(operations, parameters = list(NA), constraints =
#' list(NA), extendedCapabilities = xml2::xml_missing()) OwsRange(minimumValue
#' = as.character(NA), maximumValue = as.character(NA), rangeClosure =
#' as.character(NA), spacing = as.character(NA))
#' OwsServiceProvider(providerName, providerSite = as.character(NA),
#' serviceContact = xml2::xml_missing()) OwsServiceIdentification(serviceType,
#' serviceTypeVersion, profile = c(NA), title, abstract = c(NA), keywords =
#' c(NA), fees = as.character(NA), accessConstraints = c(NA))
#' @param abstract Brief narrative description of this server, normally
#' available for display to a human.
#' @param acceptFormats Unordered character vector of zero or more response
#' formats desired by client, with preferred formats listed first.
#' @param acceptLanguages Unordered character vector of zero or more languages
#' desired by client, with preferred formats listed first. Only OWS 2.0.0!
#' @param acceptVersions Comma-separated prioritized sequence of one or more
#' specification versions accepted by client, with preferred versions listed
#' first.
#' @param accessConstraints Access constraints that should be observed to
#' assure the protection of privacy or intellectual property, and any other
#' restrictions on retrieving or using data from or otherwise using a server.
#' @param constraints Constraint on valid domain of a non-parameter quantity
#' that applies to an operation which a server implements.
#' @param contents The provider section of a capabilities document, object of
#' class \code{OwsContentsOrNULL}.
#' @param DCPs Information for a Distributed Computing Platform (DCP) supported
#' for an operation.
#' @param exceptionCode The code attribute of an OWS Exception, see
#' \code{\link{OwsExceptionsData}}.
#' @param exceptions The list of \code{OwsException} in a
#' \code{OwsExceptionReport}.
#' @param exceptionText The text element of an OWS Exception, see
#' \code{\link{OwsExceptionsData}}.
#' @param extendedCapabilities The possible contents of the
#' ExtendedCapabilities subsection are not specified by the SOS standard.
#' @param fees Fees and terms for using a server, including the monetary units
#' as specified in ISO 4217.
#' @param identification The identification section of a capabilities document,
#' object of class \code{OwsServiceIdentificationOrNULL}.
#' @param keywords Unordered list of one or more commonly used or formalised
#' word(s) or phrase(s) used to describe a server.
#' @param lang The code attribute of an OWS Exception.
#' @param languages The languages section of a capabilities document, currently
#' an object of class \code{XMLAbstractNode}.
#' @param locator The locator attribute of an OWS Exception, see
#' \code{\link{OwsExceptionsData}}.
#' @param maximumValue Maximum value of a range (numeric parameter).
#' @param metadata Metadata about an operation and its implementation.
#' @param minimumValue Minimum value of a range (numeric parameter).
#' @param name Name of an operation (request) (for example, GetCapabilities).
#' @param operations A list of objects of class \code{OwsOperation} in a
#' \code{OperationsMetadata} object. The provider section of a capabilities
#' document.
#' @param owsVersion The used OWS specification version.
#' @param parameters Parameter valid domain that applies to an operation which
#' a server implements.
#' @param profile Identifier of OGC Web Service (OWS) Application Profile.
#' @param providerName Unique identifier for service provider organization.
#' @param providerSite Reference to the most relevant web site of a service
#' provider.
#' @param provider The provider section of a capabilities document, object of
#' class \code{OwsServiceProviderOrNULL}.
#' @param rangeClosure Specifies which of minimum and maximum values are
#' included in this range; include when not default of \dQuote{closed} range.
#' Possible values are closed, open, open-closed, or closed-open.
#' @param sections Unordered character vector of zero or more names of sections
#' of service metadata document to be returned in service metadata document.
#' @param serviceContact Information for contacting service provider.
#' @param service Service type identifier text.
#' @param serviceType A service type name from registry of services.
#' @param serviceTypeVersion Version of a service type implemented by a server.
#' @param spacing Regular distance or spacing between allowed values in this
#' range; include when range is not continuous.
#' @param title Title of a server, normally used for display to a human.
#' @param updateSequence Service metadata document version, value is
#' "increased" whenever any change is made in complete service metadata
#' document. This can be used to request a certain version of a metadata
#' document. Parameter is found in both request and reponse, but may not be
#' supported by a service.
#' @param version The version of the document.
#' @section Objects from the Class: Objects can be created by calling the
#' construction functions, e.g. in the form \code{OwsCapabilities_1.1.0(...)},
#' \code{OwsContents(...)} or \code{OwsException(...)}.
#' 
#' The following classes are virtual and no objects may be created from it:
#' \code{OwsContentsOrNULL}, \code{OwsServiceIdentificationOrNULL},
#' \code{OwsServiceProviderOrNULL}, \code{OwsOperationsMetadataOrNULL}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references Whiteside A. (Ed.), OGC Web Services Common Specification, Open
#' Geospatial Consortium Inc., OGC 06-121r3, Version: 1.1.0 with Corrigendum 1
#' 
#' Whiteside A., Greenwood, J. (Eds.), OGC Web Services Common Standard, Open
#' Geospatial Consortium Inc., OGC 06-121r9, Version: 2.0.0
#' @keywords classes utilities
#' @examples
#' 
#' showClass("OwsCapabilities_1.1.0")
#' showClass("OwsCapabilities_2.0.0")
#' showClass("OwsCapabilities")
#' showClass("OwsContents")
#' showClass("OwsContentsOrNULL")
#' showClass("OwsException")
#' showClass("OwsExceptionReport")
#' showClass("OwsGetCapabilities_1.1.0")
#' showClass("OwsGetCapabilities_2.0.0")
#' showClass("OwsGetCapabilities")
#' showClass("OwsOperation")
#' showClass("OwsOperationsMetadata")
#' showClass("OwsRange")
#' showClass("OwsServiceIdentification")
#' showClass("OwsServiceIdentificationOrNULL")
#' showClass("OwsServiceOperation")
#' showClass("OwsServiceProvider")
#' showClass("OwsServiceProviderOrNULL")
#' 
NULL





#' Parsing Functions for XML Documents and Elements
#' 
#' The functions decode a given XML object to an representation, which can be
#' an object of a specific class, a list, a named character vector, \ldots{}
#' 
#' The naming of the functions follow the following rule: parse[optional:
#' namespace prefix][name of the XML element to be parsed]
#' 
#' Not all parsing function that have a \code{SOS} object or \code{verbose} in
#' their signature, but few actually use it at this points of development. Some
#' of the parsing functions are \strong{exchangeable} when creating a new SOS
#' connection. Please see the examples!
#' 
#' \code{parseOM} is a special function in the respect that it matches sub
#' parsing function depending on an objects \code{xmlName} from the list of the
#' given \code{SOS}'s parsing functions.
#' 
#' \code{parseNoParsing} is a convenience function that directly returns the
#' object without any changes.
#' 
#' \code{sosParse} allows parsing of files for all elements that have a parsers
#' registered with the given SOS.
#' 
#' @aliases parse parseCategoryObservation parseComplexObservation
#' parseComponent parseCompositePhenomenon parseCountObservation parseDataArray
#' parseElementType parseEncoding parseFeatureCollection parseField parseFOI
#' parseGeometryObservation parseMeasure parseMeasurement parseObservation
#' parseObservationCollection parseOM parseOwsException parseOwsExceptionReport
#' parseOwsOperation parseOwsRange parseOwsServiceIdentification
#' parseOwsServiceProvider parsePhenomenonProperty parsePoint parsePosition
#' parseResult parseSamplingPoint parseSensorML parseSosCapabilities
#' parseSosFilter_Capabilities parseSosObservationOffering
#' parseTemporalObservation parseTextBlock
#' parseTimeGeometricPrimitiveFromParent parseTimeInstant
#' parseTimeInstantProperty parseTimePeriod parseTimePosition parseTimeObject
#' parseTruthObservation parseValues parseObservationProperty sosParse
#' sosParse-methods sosParse,SOS_1.0.0,character-method
#' sosParse,SOS_1.0.0,character,logical-method parseNoParsing parseCSV
#' parseFile parseFile-method parseFile,SOS_1.0.0,character-method
#' parseFile,SOS_versioned,character-method parseSweCoordinate
#' parseSweCoordinate-method parseSweLocation parseSweLocation-method
#' parseSwePosition parseSwePosition-method parseSweVector
#' parseSweVector-method parseGetFeatureOfInterestResponse
#' parseGetObservationResponse parseMonitoringPoint parseObservation_2.0
#' parseSamsShape parseSosCapabilities100 parseSosCapabilities200
#' parseSosObservationOffering_200 parseSosObservedProperty
#' parseSwesObservableProperty parseTextEncoding parseTime
#' @usage parseFile(sos, file, verbose = FALSE, ...) parseCSV(obj, verbose =
#' FALSE) parseNoParsing(obj) parseCategoryObservation(obj, sos, verbose =
#' FALSE) parseComplexObservation(obj, sos, verbose = FALSE)
#' parseComponent(obj, verbose = FALSE) parseCompositePhenomenon(obj, verbose =
#' FALSE) parseCountObservation(obj, sos, verbose = FALSE) parseDataArray(obj,
#' sos, verbose = FALSE) parseElementType(obj, sos, verbose = FALSE)
#' parseEncoding(obj, sos, verbose = FALSE) parseFeatureCollection(obj, sos)
#' parseField(obj, sos, verbose = FALSE) parseFOI(obj, sos, verbose = FALSE)
#' parseGeometryObservation(obj, sos, verbose = FALSE) parseMeasure(obj)
#' parseMeasurement(obj, sos, verbose = FALSE) parseObservation(obj, sos,
#' verbose = FALSE) parseObservationCollection(obj, sos, verbose) parseOM(obj,
#' sos, verbose = FALSE) parseOwsException(obj) parseOwsExceptionReport(obj,
#' verbose = FALSE) parseOwsOperation(obj, namespaces = SosAllNamespaces())
#' parseOwsRange(obj) parseOwsServiceIdentification(obj, namespaces =
#' SosAllNamespaces()) parseOwsServiceProvider(obj)
#' parsePhenomenonProperty(obj, verbose = FALSE) parsePoint(obj, sos)
#' parsePosition(obj, sos) parseResult(obj, sos, verbose = FALSE)
#' parseSamplingPoint(obj, sos) parseSensorML(obj, sos, verbose = FALSE)
#' parseSosCapabilities(obj, sos) parseSosFilter_Capabilities(obj, sos)
#' parseSosObservationOffering(obj, sos) parseTemporalObservation(obj, sos,
#' verbose = FALSE) parseTextBlock(obj)
#' parseTimeGeometricPrimitiveFromParent(obj, sos) parseTimeInstant(obj, sos)
#' parseTimeInstantProperty(obj, sos) parseTimePeriod(obj, sos)
#' parseTimePosition(obj, sos) parseTimeObject(obj, sos, timeObjectMap =
#' list(), verbose = FALSE) parseTruthObservation(obj, sos, verbose = FALSE)
#' parseValues(values, fields, encoding, sos, verbose = FALSE)
#' @param obj The object to decode, normally objects of either
#' \verb{xml_document}.
#' @param sos An object of class \link{SOS-class}, which may be
#' utilized/required by some parsing functions to access other parsing
#' functions or encoding information.
#' @param verbose A boolean value indication whether information is printed out
#' to the console during the process - potentially a lot of output!
#' @param namespaces A vector of namespace prefixes and definitions to use.
#' @param format A character string defining the time format to be used in
#' \code{strptime}.
#' @param values The values to be parsed in \code{parseValues(...)}.
#' @param fields Field information in \code{parseValues(...)}, a named list.
#' @param file Name of the file to be parsed in \code{sosParse(...)}.
#' @param encoding Encoding information in \code{parseValues(...)}, an object
#' of class \code{SweTextBlock}.
#' @param timeObjectMap A named list of time objects passed on during parsing
#' (name is the id) to resolve in-document references.
#' @param ... Additional arguments that are parsed to
#' \code{xml2::read_xml(..)}.
#' @return An objects of a specific class depending on the parsing method and
#' the passed object, possibly even lists or named character vectors.
#' @section Warning : Functions might result in error if parsed an object of
#' the wrong type, because that is normally not checked.
#' 
#' Some of the functions are placeholders for future implementations!
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\link{SosParsingFunctions}}, \code{\link{sosParsers-methods}}
#' @keywords methods misc
#' @examples
#' 
#' # parsing a XML string to an exception report object
#' er.doc <- xml2::read_xml(paste0("<ows:ExceptionReport xmlns:ows=\"http://www.opengis.net/ows/1.1\"",
#'   " xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" version=\"1.0.0\"",
#'   " xsi:schemaLocation=\"http://schemas.opengis.net/ows/1.1.0/owsExceptionReport.xsd\">",
#'   "<ows:Exception exceptionCode=\"VersionNegotiationFailed\" locator=\"AcceptVersions\">",
#'   "<ows:ExceptionText>The parameter 'AcceptVersions' does not contain the version",
#'   " of this SOS: '1.0.0'</ows:ExceptionText></ows:Exception></ows:ExceptionReport>"))
#' er.parsed <- parseOwsExceptionReport(er.doc)
#' print(er.parsed)
#' str(er.parsed)
#' 
#' \dontrun{
#' # save and re-parse an observation from file
#' obsId <- getObservationById(sos = mySOS, observationId = "o_3508493",
#' 		saveOriginal = TRUE)
#' .files <- list.files(getwd())
#' .startWithO_ <- .files %in% grep("o_", .files, value=TRUE)
#' .observationFiles <- subset(.files, .startWithO_)
#' 
#' obsId <- parseFile(sos = mySOS, file = .observationFiles[[1]])
#' }
#' 
#' 
NULL





#' Classes and creation functions for Sampling Features
#' 
#' Sampling Feature classes.
#' 
#' \code{SamsShape} represents the geometry of a spatial sampling feature, that
#' can be used as a feature of interest. Currently, only points are supported.
#' 
#' 
#' @name SamsSamplingFeature-class
#' @aliases SamsSamplingFeature SamsSamplingFeature-class
#' print,SamsSamplingFeature-method show,SamsSamplingFeature-method
#' sosCoordinates,SamsSamplingFeature-method
#' toString,SamsSamplingFeature-method parseSams200SamplingFeature
#' SamsShape-class SamsShape print,SamsShape-method show,SamsShape-method
#' toString,SamsShape-method sosCoordinates,SamsShape-method
#' @docType class
#' @note Schema: http://schemas.opengis.net/sampling/2.0/samplingFeature.xsd
#' @section Objects from the Class: Objects can be created by calls to the
#' creation functions:
#' 
#' \code{SamsSamplingFeature(...)}
#' 
#' \code{SamsShape(...)}
#' @author Daniel Nuest
#' @references https://www.opengeospatial.org/standards/om
#' @keywords classes
#' @examples
#' 
#' showClass("SamsSamplingFeature")
#' showClass("SamsShape")
#' 
NULL





#' Classes of the Namespace sa
#' 
#' Classes and construction functions for elements from the OGC specification
#' \dQuote{Observations and Measurements - Part 2 - Sampling Features}.
#' 
#' 
#' @name SA
#' @aliases sa sampling features SaSamplingPoint SaSamplingPoint-class
#' show,SaSamplingPoint-method SaSamplingSurface SaSamplingSurface-class
#' show,SaSamplingSurface-method print,SaSamplingPoint-method
#' print,SaSamplingSurface-method toString,SaSamplingPoint-method
#' toString,SaSamplingSurface-method SaSamplingPoint
#' sosCoordinates,SaSamplingPoint-method sosFeatureIds,SaSamplingPoint-method
#' @docType class
#' @usage SaSamplingPoint(sampledFeatures, position, relatedObservation =
#' list(NA), relatedSamplingFeature = list(NA), surveyDetails = NA, id =
#' NA_character_)
#' @param sampledFeatures ~~
#' @param position ~~
#' @param relatedObservation ~~
#' @param relatedSamplingFeature ~~
#' @param surveyDetails ~~
#' @param id ~~
#' @return Construction functions: An object of the respective class.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction functions of the form \code{SaSamplingPoint(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references Simon Cox (Ed.), Observations and Measurements - Part 2 -
#' Sampling Features, OGC 07-002r3
#' @keywords classes
#' @examples
#' 
#' showClass("SaSamplingPoint")
#' 
#' # create sampling point
#' SaSamplingPoint(sampledFeatures = list("feature1", "feature2"),
#'                 position = GmlPointProperty(href = "http://link.to/point"))
#' 
NULL





#' Classes of the Namespace sml
#' 
#' Classes, construction functions, and accessor functions for elements from
#' the OGC specification \dQuote{OpenGIS(R) Sensor Model Language (SensorML)
#' Implementation Specification}.
#' 
#' The only class at the moment is \code{"SensorML"} which wraps an
#' \code{"XMLInternalDocument"} and some additional information. This strongly
#' depends on the SensorML Profile for Discovery to find the respective
#' paramters.
#' 
#' 
#' @name SML
#' @aliases SensorML-class show,SensorML-method print,SensorML-method
#' toString,SensorML-method SensorML sml sosId,SensorML-method
#' sosName,SensorML-method sosAbstract,SensorML-method
#' sosCoordinates,SensorML-method sosBoundedBy,SensorML-method
#' sosGetCRS,SensorML-method as.SensorML.SpatialPointsDataFrame plot.SensorML
#' plot,SensorML,missing-method
#' @docType class
#' @section Objects from the Class: Objects can be created by calls to the
#' construction method in the form \code{SensorML(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\link{DescribeSensor}}
#' @references Botts, M., Robin, A. (Eds.), OpenGIS(R) Sensor Model Language
#' (SensorML) Implementation Specification, Open Geospatial Consortium: 07-000.
#' 
#' Houbie, F., Skivee F., Robin A., Jirka S., Broering, A., Nuest D. (2009):
#' OGC(R) Catalogue Services Specification 2.0 - Extension Package for ebRIM
#' Application Profile: SensorML. OGC Discussion Paper. Open Geospatial
#' Consortium: 09-163.
#' \url{http://portal.opengeospatial.org/files/?artifact_id=37944}.
#' @keywords classes
#' @examples
#' 
#' showClass("SensorML")
#' 
#' mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
#'              binding = "KVP")
#' mySensor <- describeSensor(sos = mySOS,
#' 		procedure = sosProcedures(mySOS)[[1]],
#' 		outputFormat = 'text/xml; subtype="sensorML/1.0.1"', # space is needed!
#' 		)
#' class(mySensor)
#' print(mySensor)
#' 
#' sosId(mySensor)
#' sosName(mySensor)
#' sosBoundedBy(mySensor)
#' sosCoordinates(mySensor)
#' sosGetCRS(mySensor)
#' 
#' \dontrun{
#' plot(mySensor)
#' }
#' 
#' 
NULL





#' A client for the OGC Sensor Observation Service
#' 
#' \pkg{sos4R} is a client for Sensor Observation Services (SOS). It allows
#' users to retrieve metadata from SOS web service instances as specified by
#' the Open Geospatial Consortium (OGC) and subsequently to interactively
#' create requests for observation data based on the available sensors,
#' phenomena, observations, offerings etc.
#' 
#' \tabular{ll}{ Package: \tab sos4R\cr Type: \tab Package\cr Version: \tab
#' 0.3.0\cr Date: \tab 2019-03-30\cr License: \tab GPL-2\cr LazyLoad: \tab
#' yes\cr ByteCompile: \tab yes\cr Imports: \tab xml2, httr, sp, stringr,
#' methods\cr }
#' 
#' @name sos4R-package
#' @aliases sos4R-package sos4R sosCheatSheet release_questions
#' @docType package
#' @note The development of this software was gratefully supported by the
#' 52North Student Innovation Prize for Geoinformatics 2010.
#' 
#' To stay updated on all matters around \pkg{sos4R} go to the development blog
#' at \url{http://www.nordholmen.net/sos4r/}.
#' 
#' If you want to ask questions about using the software, please go to the to
#' the issue tracker at \url{https://github.com/52North/sos4R/issues}.
#' 
#' The most extensive documenation is contained in the \strong{package
#' vignette}.
#' @author Daniel Nuest <daniel.nuest@@uni-muenster.de>
#' @seealso See also the package vignette.
#' @references Na, A., Priest, M. (Eds.), 2007. Sensor Observation Service.
#' OpenGIS Implementation Standard, Version 1.0, OGC 06-009r6
#' @keywords package connection ts spatial database
#' @examples
#' 
#' 
#' \dontrun{
#' 
#' # Open the connection
#' sos = SOS(url = "<the service endpoint>")
#' 
#' # List offerings, procedures and observedProperties
#' names(sosOfferings(sos))
#' sosProcedures(sos)
#' sosObservedProperties(sos)
#' 
#' # Create time period (last 30 days)
#' tPeriod <- sosCreateEventTimeList(
#' 	time = sosCreateTimePeriod(
#' 		sos = pegelsos,
#' 		begin = Sys.time() - (3600 * 24 * 30),
#' 		end = Sys.time()))
#' 
#' # Request data for all observed properties and procedures of a certain offering
#' observation <- getObservation(sos = sos,
#' 		observedProperty = sosObservedProperties(sos),
#' 		offering = sosOfferings(sos)[[2]],
#' 		procedure = sosProcedures(sos),
#' 		eventTime = tPeriod)
#' 
#' # Inspect result
#' sosResult(observation)
#' str(sosResult(observation))
#' 
#' # Inspect attributes of the data fields
#' if(is.list(sosResult(observation))) {
#' 	attributes(sosResult(observation)[,1])
#' }
#' else {
#' 	attributes(sosResult(pegelObs)[,1])
#' }
#' 
#' # Use custom converting function and connection method. This mechanism works the
#' # same for encoders and decoders.
#' myConverters <- SosDataFieldConvertingFunctions(
#' 	"myNumericUnit" = sosConvertDouble)
#' mySos <- SOS(sos.url, binding = "KVP", dataFieldConverters = myConverters)
#' sosDataFieldConverters(mySos)
#' 
#' # get the cheat sheet
#' sosCheatSheet()
#' }
#' 
#' 
NULL





#' Bindings and Connecition Methods of OGC Sensor Observation Service
#' 
#' The SOS comes with three possible methods of transfering data, HTTP GET,
#' HTTP POST and SOAP.
#' 
#' The \strong{POST} binding is described in the official SOS specification and
#' should be the default method.
#' 
#' The \strong{GET} binding is described by OOTethys in a Best Practice
#' document:
#' \url{https://web.archive.org/web/20120616065001/http://www.oostethys.org/best-practices/best-practices-get}.
#' It contains some special encoding for bounding boxes, as the only spatial
#' filter, and time periods, as the only temporal filter.
#' 
#' The \strong{SOAP} binding is not official with regards to the spec, and also
#' not implemented yet.
#' 
#' The connection method can be changed on creation of a \code{SOS} object.
#' 
#' @aliases bindings SosBinding SosBindings GET POST KVP POX SOAP HTTP
#' @seealso \code{\link{SosSupportedBindings}}
#' @references Wikipedia page for HTTP request methods:
#' \url{https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol#Request_methods}.
#' @keywords constants XML
#' @examples
#' 
#' 
#' # HTTP connection methods supported by this sos4R implementation
#' supported <- SosSupportedBindings()
#' supported
#' 
#' \dontrun{
#' sos <- SOS("http://sosurl.org/", binding = "KVP")
#' }
#' 
#' 
NULL





#' Class and Construction Function of "SosContents"
#' 
#' \code{SosContents} models the sos:Contents section in a service metadata
#' document.
#' 
#' The SosContents section extends the generic ows:Contents elements. It
#' contains the \code{\linkS4class{SosObservationOffering}}s of a Sensor
#' Observation Service.
#' 
#' @name SosContents-class
#' @aliases SosContents-class SosContentsOrNULL-class show,SosContents-method
#' SosContents print,SosContents-method toString,SosContents-method
#' @docType class
#' @usage SosContents(observationOfferings)
#' @param observationOfferings A list of objects of class
#' \code{SosObservationOffering}.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction funtion in the form \code{SosContents(...)}.
#' 
#' The virtual class \code{SosContentsOrNULL} is used to model optional slots
#' in classes containing \code{SosContents}: No objects may be created from it.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\linkS4class{SosObservationOffering}},
#' \code{\linkS4class{OwsContents}}
#' @references See section 8.2.3.2, \dQuote{Contents Section}, of the SOS
#' specification.
#' @keywords classes
#' @examples
#' 
#' showClass("SosContents")
#' showClass("SosContentsOrNULL")
#' 
NULL





#' Convenience Functions for Request Parameter Creations
#' 
#' These methods can be seen as convenience functions or shortcuts to regularly
#' used parameters in GetObservation requests to a Sensor Observation Service.
#' The remove some complexity and target the most common cases, but also limit
#' flexibility.
#' 
#' 
#' @name sosCreate
#' @aliases sosCreate sosCreateBBOX
#' sosCreateBBOX,numeric,numeric,numeric,numeric-method sosCreateBBoxMatrix
#' sosCreateBBoxMatrix,numeric,numeric,numeric,numeric-method
#' sosCreateEventTimeList sosCreateEventTimeList-methods
#' sosCreateEventTimeList,GmlTimeGeometricPrimitive-method sosCreateEventTime
#' sosCreateEventTime-methods
#' sosCreateEventTime,GmlTimeGeometricPrimitive-method
#' sosCreateFeatureOfInterest sosCreateFeatureOfInterest-methods
#' sosCreateFeatureOfInterest,ANY-method sosCreateTimeInstant
#' sosCreateTimeInstant-methods sosCreateTimeInstant,SOS,POSIXt-method
#' sosCreateTimePeriod sosCreateTimePeriod-methods
#' sosCreateTimePeriod,SOS,POSIXt,POSIXt-method sosCreateTime
#' sosCreateTime-methods sosCreateTime,SOS,character-method
#' @docType methods
#' @usage sosCreateBBOX(lowLat, lowLon, uppLat, uppLon, srsName, srsDimension =
#' NA_integer_, axisLabels = NA_character_, uomLabels = NA_character_,
#' propertyName = sosDefaultSpatialOpPropertyName) sosCreateBBoxMatrix(lowLat,
#' lowLon, uppLat, uppLon) sosCreateFeatureOfInterest(objectIDs = list(NA),
#' spatialOps = NULL, bbox = NULL, srsName = NA_character_)
#' sosCreateEventTime(time, operator) sosCreateEventTimeList(time, operator)
#' sosCreateTimeInstant(sos, time, frame = as.character(NA), calendarEraName =
#' as.character(NA), indeterminatePosition = as.character(NA))
#' sosCreateTimePeriod(sos, begin, end, frame = as.character(NA),
#' calendarEraName = as.character(NA), indeterminatePosition =
#' as.character(NA), duration = as.character(NA), timeInterval = NULL)
#' sosCreateTime(sos, time, operator = sosDefaultTemporalOperator)
#' @param lowLat Minimum latitude for bounding box and bounding box matrix.
#' @param lowLon Minimum longitude for bounding box and bounding box matrix.
#' @param uppLat Maximum latitude for bounding box and bounding box matrix.
#' @param uppLon Maximum longitude for bounding box and bounding box matrix.
#' @param srsName Name of the spatial reference system for bounding box, e.g.
#' \samp{"urn:ogc:def:crs:EPSG:4326"}.
#' @param srsDimension Dimensions of the spatial reference system, e.g.
#' \samp{2}.
#' @param axisLabels Labels of the axes of a bounding box as an ordered
#' character vector.
#' @param uomLabels Unit of measurement labels as an ordered character vector
#' for the axes in a bounding box, e.g. \samp{"deg"}.
#' @param propertyName The spatial property name for the bounding box, e.g.
#' \samp{"urn:ogc:data:location"}
#' @param objectIDs Identifiers of a feature of interest list.
#' @param spatialOps An object of class \code{\link{OgcSpatialOps-class}} which
#' is inserted into the feature of interest element.
#' @param bbox Shortcut to add a feature of interest with a
#' \code{\link{GmlEnvelope-class}}, object must be a \code{matrix} as created
#' by \code{sosCreateBBoxMatrix(...)}.
#' @param time Object of class \code{"GmlTimeGeometricPrimitive"} for
#' \code{sosCreateEventTimeList}, or an object of class \code{POSIXt} for
#' \code{sosCreateTimePeriod},or an object of class \code{character} for
#' \code{sosCreateTime}.
#' @param operator The operator to be used for the time in
#' \code{sosCreateEventTimeList}, e.g. \samp{"TM_During"}.
#' @param sos An object of class \code{SOS-class} for which the element is
#' created. The \code{SOS} might for example be required for formatting
#' settings.
#' @param frame Provides a URI reference that identifies a description of the
#' reference system.
#' @param calendarEraName The name of the calendar era.
#' @param begin Object of class \code{POSIXt}.
#' @param end Object of class \code{POSIXt}.
#' @param indeterminatePosition Inexact temporal positions may be expressed
#' using the optional indeterminatePosition parameter. This takes one of the
#' following values: after, before, now, unknown.
#' @param duration Duration of an interval using ISO 8601 syntax for temporal
#' length.
#' @param timeInterval An object of class \code{"GmlTimeIntervalOrNULL"} to be
#' used in a \code{GmlTimePeriod-class}.
#' @return An object of the respective class, or a list in case of
#' \code{sosEventTimeList}.
#' @section Methods: \describe{ \item{list("signature(time =
#' \"GmlTimeGeometricPrimitive\")")}{Create sos:time based on the given
#' \code{GmlTimeGeometricPrimitive}.} \item{list("signature(sos = \"SOS\", time
#' = \"POSIXt\")")}{Create sos:time with time instant based on the given time.}
#' \item{list("signature(sos = \"SOS\", begin = \"POSIXt\", end =
#' \"POSIXt\")")}{Create sos:time with time interval based on the given begin
#' and end times.} }
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso These methods create object of the following classes:
#' \code{\link{GmlTimeInstant-class}}, \code{\link{GmlTimePeriod-class}},
#' \code{\link{SosEventTime-class}}, \code{\link{SosFeatureOfInterest-class}},
#' \code{\link{OgcBBOX-class}}, \code{\link{matrix-class}}.
#' @keywords utilities methods
#' @examples
#' 
#' # create a feature of interest based on identifiers
#' foiIDs <- list("urn:ogc:object:feature:1", "urn:ogc:object:feature:2")
#' foiObj <- sosCreateFeatureOfInterest(objectIDs = foiIDs[1:2])
#' print(foiObj)
#' 
#' # create a bounding box matrix and use it to create a spatial feature of interest
#' bboxMatrix <- sosCreateBBoxMatrix(lowLat = 50.0, lowLon = 7.0,
#'                                   uppLat = 53.0, uppLon = 10.0)
#' foiBBox <- sosCreateFeatureOfInterest(bbox = bboxMatrix,
#'                                       srsName = "urn:ogc:def:crs:EPSG:6.8:4326")
#' print(foiBBox)
#' 
#' # create a foi with a bounding box
#' bbox <- sosCreateBBOX(lowLat = 50.0, lowLon = 7.0, uppLat = 53.0, uppLon = 10.0,
#'                       srsName = "urn:ogc:def:crs:EPSG:6.8:4326",
#'                       srsDimension = as.integer(2), axisLabels = "lat,lon",
#'                       uomLabels = "deg,deg", propertyName = "bboxName")
#' foiBBox2 <- sosCreateFeatureOfInterest(spatialOps = bbox)
#' print(foiBBox2)
#' 
#' last.period <- sosCreateTimePeriod(sos = SOS_Test(),
#' 	begin = (Sys.time() - 3600 * 24 * 7), end = Sys.time())
#' 
#' period <- sosCreateTimePeriod(sos = SOS_Test(),
#' 		begin = as.POSIXct("2010/01/01"), end = as.POSIXct("2010/01/07"))
#' eventTime <- sosCreateEventTimeList(period)
#' 
#' sosCreateTime(sos = SOS_Test(), time = "2007-07-07 07:00::2008-08-08 08:00")
#' sosCreateTime(sos = SOS_Test(), time = "2007-07-07 07:00/2010-10-10 10:00")
#' 
#' sosCreateTime(sos = SOS_Test(), time = "::2007-08-05")
#' sosCreateTime(sos = SOS_Test(), time = "2007-08-05/")
#' 
#' 
NULL





#' Class and Construction Function for "SosFeatureOfInterest"
#' 
#' Element in a GetObservation request to a Sensor Observation service to
#' constrain the observations to be returned regarding the observed feature.
#' 
#' Specifies the feature for which observations are requested. This can either
#' be represented by a reference to a feature ID advertised in the capabilities
#' document or can be a spatial constraint.
#' 
#' @name SosFeatureOfInterest-class
#' @aliases SosFeatureOfInterest-class show,SosFeatureOfInterest-method
#' print,SosFeatureOfInterest-method toString,SosFeatureOfInterest-method
#' SosFeatureOfInterest SosFeatureOfInterestOrNULL-class
#' @docType class
#' @usage SosFeatureOfInterest(objectIDs = list(NA), spatialOps = NULL)
#' @param objectIDs A list of character identifiers of features in a SOS.
#' @param spatialOps An object of class \code{OgcSpatialOps} for spatial
#' filtering.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction function of the form \code{SosFeatureOfInterest(...)}.
#' 
#' \code{SosFeatureOfInterestOrNULL} is a virtual class to model optional slots
#' of containing elements: No objects may be created from it.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See also \code{\linkS4class{SosGetObservation}}, and the
#' convenience creation function
#' \code{\link{sosCreateFeatureOfInterest-methods}}.
#' @references See section 8.4.2 of the SOS specification: Na, A., Priest, M.
#' (Eds.), Sensor Observation Service, Open Geospatial Consortium Inc., OGC
#' 06-009r6, Version: 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("SosFeatureOfInterest")
#' showClass("SosFeatureOfInterestOrNULL")
#' 
NULL





#' Classes and Construction Functions for "SosFilter_Capabilities" Elements
#' 
#' Additional section in the service metadata document of a Sensor Observation
#' Service, which contains information about the supported filters.
#' 
#' The FilterCapabilities section is used to indicate what types of query
#' parameters are supported by the service. These capabilities refer to the
#' parameters of the GetObservation operation which is the only operation that
#' includes OGC filter-like expressions.
#' 
#' @name SosFilter_Capabilities-class
#' @aliases SosFilter_Capabilities-class show,SosFilter_Capabilities-method
#' print,SosFilter_Capabilities-method toString,SosFilter_Capabilities-method
#' SosFilter_Capabilities SosFilter_CapabilitiesOrNULL-class
#' @docType class
#' @usage SosFilter_Capabilities(spatial = list(NA_character_), temporal =
#' list(NA_character_), scalar = list(NA_character_), id = list(NA_character_))
#' @param spatial A character list of names of available spatial filters.
#' @param temporal A character list of names of available temporal filters.
#' @param scalar A character list of names of available scalar filters.
#' @param id A character list of names of available filters on identifiers.
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SosFilter_Capabilities", ...)}.
#' 
#' \code{SosFilter_CapabilitiesOrNULL} is virtual class: No objects may be
#' created from it.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\linkS4class{SosCapabilities}}
#' @references See section 8.2.3.1, \dQuote{FilterCapabilities Section}, the
#' SOS specification: Na, A., Priest, M. (Eds.), Sensor Observation Service,
#' Open Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("SosFilter_Capabilities")
#' showClass("SosFilter_CapabilitiesOrNULL")
#' 
NULL





#' Class \code{"SosGetFeatureOfInterest_2.0.0"}
#' 
#' Representation of a GetFeatureOfInterest operation request that needs to be
#' sent to a Sensor Observation Service to retrieve the features of interest,
#' i.e. the real world features that are observed and for which observations
#' are taken.
#' 
#' 
#' @name SosGetFeatureOfInterest_2.0.0-class
#' @aliases SosGetFeatureOfInterest_2.0.0-class SosGetFeatureOfInterest_2.0.0
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SosGetFeatureOfInterest_2.0.0", ...)}.
#' @keywords classes
#' @examples
#' 
#' showClass("SosGetFeatureOfInterest_2.0.0")
#' 
NULL





#' Methods for Function \code{sosObservableProperties} in Package \pkg{sos4R}
#' 
#' Methods for function \code{sosObservableProperties} in package \pkg{sos4R}.
#' The function allows to retrieve observable properties, e.g. air temperature,
#' wind speed, etc., listed in the capabilities of a Sensor Observation Service
#' and other classes.
#' 
#' 
#' @name sosObservableProperties-methods
#' @aliases sosObservableProperties sosObservableProperties-methods
#' sosObservableProperties,list-method
#' sosObservableProperties,OmObservation-method
#' sosObservableProperties,OmObservationCollection-method
#' sosObservableProperties,SOS-method
#' sosObservableProperties,SosObservationOffering_2.0.0-method
#' sosObservableProperties,SweCompositePhenomenon-method
#' sosObservableProperties,SwePhenomenonProperty-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(obj = \"list\")")}{List
#' of objects to retreive properties from.} \item{list("signature(obj =
#' \"OmObservation\")")}{Get observable properties from an object of class
#' \code{OmObservation}.} \item{list("signature(obj =
#' \"OmObservationCollection\")")}{Get observable properties from an object of
#' class \code{OmObservationCollection}, namely the items in the collection.}
#' \item{list("signature(obj = \"SOS\")")}{Get observable properties for a
#' whole connection to an SOS.} \item{list("signature(obj =
#' \"SosObservationOffering_2.0.0\")")}{Get observable properties from an
#' object of class \code{SosObservationOffering_2.0.0}, needed internally for
#' getting properties for a whole SOS.} \item{list("signature(obj =
#' \"SweCompositePhenomenon\")")}{Get observable properties from an object of
#' class \code{SweCompositePhenomenon}.} \item{list("signature(obj =
#' \"SwePhenomenonProperty\")")}{Get observable properties from an object of
#' class \code{SwePhenomenonProperty}.} }
#' @keywords methods
NULL





#' Classes and Related Functions for "SosObservationOffering"
#' 
#' \code{SosObservationOffering}s collect all metadata about a specific
#' offerign in a Sensor Observation Service.
#' 
#' ObservationOffering provides a mechanism for factoring groups of related
#' observations within a single service instance. A functionally equivalent
#' outcome could be obtained by factoring between different service instances.
#' 
#' @name SosObservationOffering-class
#' @aliases SosObservationOffering SosObservationOffering-class
#' show,SosObservationOffering-method sosTime,SosObservationOffering-method
#' sosBoundedBy,SosObservationOffering-method
#' print,SosObservationOffering-method toString,SosObservationOffering-method
#' sosName,SosObservationOffering-method
#' sosResultModels,SosObservationOffering-method
#' sosId,SosObservationOffering-method SosObservationOffering_2.0.0
#' SosObservationOffering_2.0.0-class show,SosObservationOffering_2.0.0-method
#' summary.SosObservationOffering_2.0.0
#' print.summary.SosObservationOffering_2.0.0
#' print,SosObservationOffering_2.0.0-method
#' toString,SosObservationOffering_2.0.0-method
#' sosName,SosObservationOffering_2.0.0-method
#' sosResultModels,SosObservationOffering_2.0.0-method
#' sosId,SosObservationOffering_2.0.0-method
#' @docType class
#' @usage SosObservationOffering(id, name = as.character(NA), time, procedure,
#' observedProperty, featureOfInterest, responseFormat, intendedApplication =
#' as.character(NA), resultModel = as.character(NA), responseMode =
#' as.character(NA), boundedBy = list())
#' @param boundedBy See the corresponding slot description.
#' @param featureOfInterest See the corresponding slot description.
#' @param id See the corresponding slot description.
#' @param intendedApplication See the corresponding slot description.
#' @param name See the corresponding slot description.
#' @param observedProperty See the corresponding slot description.
#' @param procedure See the corresponding slot description.
#' @param responseFormat See the corresponding slot description.
#' @param responseMode See the corresponding slot description.
#' @param resultModel See the corresponding slot description.
#' @param time See the corresponding slot description.
#' @return The construction functions return an object of the respective class,
#' e.g. \code{SosObservationOffering}.
#' @section Objects from the Class: Objects can be created by calls to the
#' construction functions of the form \code{SosObservationOffering(...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\linkS4class{SosContents}},
#' \code{\linkS4class{SosCapabilities}}
#' @references See section 6.3, \dQuote{Observation Offerings}, of the SOS
#' specification: Na, A., Priest, M. (Eds.), Sensor Observation Service, Open
#' Geospatial Consortium Inc., OGC 06-009r6, Version: 1.0
#' @keywords classes
#' @examples
#' 
#' showClass("SosObservationOffering")
#' showClass("SosObservationOffering_2.0.0")
#' 
#' # explore offerings of an SOS
#' mySOS <- SOS(url = "http://sensorweb.demo.52north.org/sensorwebtestbed/service/kvp",
#'              binding = "KVP")
#' offering1 <- sosOfferings(mySOS)[[1]]
#' 
#' sosId(offering1)
#' sosName(offering1)
#' sosTime(offering1)
#' sosBoundedBy(offering1)
#' 
NULL





#' Send Request to SOS
#' 
#' This is the main request function for sending and receiving requests
#' respectively documents from a Sensor Observation Service. It's intended for
#' internal use.
#' 
#' Please use the methods for the SOS operations as long as possible:
#' \code{\link{getCapabilities-methods}}, \code{\link{describeSensor-methods}},
#' \code{\link{getObservation-methods}}, and
#' \code{\link{getObservationById-methods}}.
#' 
#' 
#' @name sosRequest-methods
#' @aliases sosRequest sosRequest-methods
#' sosRequest,SOS_1.0.0,OwsServiceOperation,logical,logical-method
#' sosRequest,SOS_2.0.0,OwsServiceOperation,logical,logical-method
#' @docType methods
#' @section Methods: \describe{ \item{list("signature(sos = \"SOS\", request =
#' \"OwsServiceOperation\", verbose = \"logical\", inspect =
#' \"logical\")")}{Method sends the given operation to the given SOS
#' connection. \code{verbose} activates extensive debugging to the console.
#' \code{inspect} prints only the request and response documents to the
#' console.} }
#' @keywords methods
NULL





#' Functions to Access Supported Features of the Current sos4R Implementation
#' 
#' These functions can be used to access the supported parameters for a range
#' of settings of a SOS connection.
#' 
#' \strong{Supported features}, like connection methods and supported response
#' modes, are accessible by functions starting with "SosSupported". See the
#' examples section for a complete list of these functions.
#' 
#' It is encouraged to rather use these methods than manually set character
#' values for compatibility with future versions, e.g.
#' \code{SosSupportedBindings()[[1]]} instead of directly writing \code{"GET"}.
#' 
#' @aliases SosSupported SosSupportedComparisonOperators SosSupportedBindings
#' SosSupportedGeometryOperands SosSupportedResponseFormats
#' SosSupportedResponseModes SosSupportedResultModels
#' SosSupportedSpatialOperators SosSupportedTemporalOperators
#' SosSupportedServiceVersions SosSupportedOperations
#' @usage SosSupportedOperations(version = sos100_version)
#' SosSupportedComparisonOperators() SosSupportedBindings()
#' SosSupportedGeometryOperands() SosSupportedResponseFormats()
#' SosSupportedResponseModes() SosSupportedResultModels()
#' SosSupportedSpatialOperators() SosSupportedTemporalOperators()
#' SosSupportedServiceVersions()
#' @param version The SOS specification version.
#' @return A list of supported values for the respective parameter.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso See \code{\link{Defaults}} for default values of parameters.
#' @keywords utilities
#' @examples
#' 
#' # The supported operations of the specification
#' SosSupportedOperations()
#' 
#' # HTTP connection methods supported by this sos4R implementation
#' SosSupportedBindings()
#' myBinding <- SosSupportedBindings()[[1]]
#' myBinding
#' 
#' # Formats, modes and models that can be processed by this implementation
#' SosSupportedResponseFormats()
#' SosSupportedResultModels()
#' SosSupportedResponseModes()
#' 
#' # Operators and operands for filtering in a GetObservation request
#' SosSupportedTemporalOperators()
#' SosSupportedSpatialOperators()
#' SosSupportedGeometryOperands()
#' SosSupportedComparisonOperators()
#' 
NULL





#' Classes and Construction Functions for the SWE Namespace
#' 
#' These classes represent elements from the OpenGIS(R) Sensor Model Language
#' (SensorML) Implementation Specification that are used to model observation
#' data in responses from a Sensor Observation Service.
#' 
#' The \verb{...OrNULL} classes are used to model optional slots.
#' 
#' @name SWE
#' @aliases SweCompositePhenomenon-class show,SweCompositePhenomenon-method
#' SwePhenomenon-class show,SwePhenomenon-method SwePhenomenonOrNULL-class
#' SwePhenomenonProperty-class show,SwePhenomenonProperty-method
#' SwePhenomenonPropertyOrNULL-class SweTextBlock-class
#' show,SweTextBlock-method print,SweCompositePhenomenon-method
#' print,SwePhenomenon-method print,SwePhenomenonProperty-method
#' print,SweTextBlock-method toString,SweCompositePhenomenon-method
#' toString,SwePhenomenon-method toString,SwePhenomenonProperty-method
#' toString,SweTextBlock-method SweCompositePhenomenon SwePhenomenon
#' SwePhenomenonProperty SweTextBlock
#' sosObservedProperties,SweCompositePhenomenon-method
#' sosObservedProperties,SwePhenomenonProperty-method
#' @docType class
#' @usage SweCompositePhenomenon(id, name, description = as.character(NA),
#' dimension, components, base = NULL) SwePhenomenon(id, name, description =
#' as.character(NA)) SwePhenomenonProperty(href = as.character(NA), phenomenon
#' = NULL) SweTextBlock(tokenSeparator, blockSeparator, decimalSeparator, id =
#' as.character(NA))
#' @param id The character string to be used for the id attribute (mandatory).
#' @param name The character string to be used for the name element
#' (mandatory).
#' @param description The character string to be used for the description
#' element.
#' @param dimension The dimensions of a composite phenomenon (mandatory).
#' @param components The (sub-) components of a composite phenomenon
#' (mandatory).
#' @param base The (optional) base element for a composite phenomenon.
#' @param href A reference to an (online) object instead of a inline property.
#' @param phenomenon The inline phenomenon of a phenomenon property.
#' @param tokenSeparator The character to be used as the token seperator, often
#' \code{","}.
#' @param blockSeparator The character to be used as the block seperator, often
#' \code{";"}.
#' @param decimalSeparator The character to be used as the decimal seperator,
#' often \code{"."}.
#' @return The construction functions return an object of the respective class.
#' @section Objects from the Class: Objects can be created by calls to the
#' constrction functions of the form \code{SweCompositePhenomenon( ...)},
#' \code{SwePhenomenonProperty( ...)} and so forth.
#' 
#' The following classes are virtual, no objects may be created from them:
#' \code{SwePhenomenonOrNULL}, \code{SwePhenomenonPropertyOrNULL}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @references See section 9, SWE Common XML Encoding and Examples, of Botts,
#' M., Robin, A. (Eds.), OpenGIS(R) Sensor Model Language (SensorML)
#' Implementation Specification, Open Geospatial Consortium Inc., OGC 07-000
#' @keywords classes
#' @examples
#' 
#' showClass("SweCompositePhenomenon")
#' showClass("SwePhenomenon")
#' showClass("SwePhenomenonProperty")
#' showClass("SwePhenomenonPropertyOrNULL")
#' showClass("SweTextBlock")
#' 
NULL





#' Class \code{"SweTextEncoding"}
#' 
#' Representation of a text encoding defined in the OGC SWE Common
#' specification. It defines the token, block, and decimal seperators for a
#' text-encoded array of values.
#' 
#' 
#' @name SweTextEncoding-class
#' @aliases SweTextEncoding-class SweTextEncoding
#' @docType class
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("SweTextEncoding", ...)}.
#' @keywords classes
#' @examples
#' 
#' showClass("SweTextEncoding")
#' 
NULL





#' Classes and Construction Methods for Temporal Operator Classes
#' 
#' Classes for temporal operators from OpenGIS(R) Filter Encoding used in
#' filters in GetObservation requests.
#' 
#' 
#' @name TM_Operators
#' @aliases print,TM_After-method print,TM_Before-method print,TM_During-method
#' print,TM_Equals-method show,TM_After-method show,TM_Before-method
#' show,TM_During-method show,TM_Equals-method toString,TM_After-method
#' toString,TM_Before-method toString,TM_During-method
#' toString,TM_Equals-method TM_After-class TM_Before-class TM_During-class
#' TM_Equals-class TM_Operators-class TM_Operators TM_After TM_Before TM_During
#' TM_Equals
#' @docType class
#' @usage TM_After(propertyName = sosDefaultTempOpPropertyName, time)
#' TM_Before(propertyName = sosDefaultTempOpPropertyName, time)
#' TM_During(propertyName = sosDefaultTempOpPropertyName, time)
#' TM_Equals(propertyName = sosDefaultTempOpPropertyName, time)
#' @param propertyName The name of the property that is used to wrap the time.
#' @param time A time instant or period to be used as the temporal operand.
#' @return An object of the respective class, so \code{TM_After},
#' \code{TM_Before}, \code{TM_During} or \code{TM_Equals}
#' @section Objects from the Class: Objects can be created by calls of the form
#' \code{new("TM_After", ...)}.
#' @author Daniel Nuest \email{daniel.nuest@@uni-muenster.de}
#' @seealso \code{\link{SosGetObservation}}
#' @references Vretanos, Panagiotis A. (Ed.), OpenGIS(R) Filter Encoding
#' Implementation Specification, OGC 04-095, Version: 1.1.0
#' 
#' See the schema file: \url{http://schemas.opengis.net/sos/1.0.0/ogc4sos.xsd}.
#' @keywords classes utilities
#' @examples
#' 
#' showClass("TM_After")
#' showClass("TM_Before")
#' showClass("TM_During")
#' showClass("TM_Equals")
#' 
#' \dontrun{
#' # create times to use for operators
#' t1 <- sosCreateTimeInstant(sos = weathersos, time = Sys.time())
#' p1 <- sosCreateTimePeriod(sos = weathersos, begin = as.POSIXct("2010-03-01 12:15"),
#'                           end = as.POSIXct("2010-03-02 12:15"))
#' 
#' # create temporal operator
#' afterNow <- TM_After(time = t1)
#' print(afterNow)
#' encodeXML(t1, sos)
#' 
#' during <- TM_During(time = p1)
#' print(during)
#' }
#' 
#' 
NULL





#' Classes to model S3 classes of package \code{xml2}
#' 
#' Currently the main classes of the XML handling package \code{xml2} are only
#' S3 classes and thus cannot be used in slots for S4 classes. To remedy this,
#' classes can be registered with \code{setOldClass(..)}. This is done within
#' this package, but should really be part of \code{xml2}, see
#' https://github.com/r-lib/xml2/issues/248
#' 
#' 
#' @name xml_document-class
#' @aliases xml_document xml_document-class xml_node xml_node-class
#' @docType class
#' @author Daniel Nuest
#' @keywords classes
NULL



