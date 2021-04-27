#' ChEBI entry class.
#'
#' This is the entry class for ChEBI database.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::newInst()
#'
#' # Create a connector to ChEBI
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Get an entry
#' e <- conn$getEntry('15440')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @export ChebiEntry
#' @exportClass ChebiEntry
ChebiEntry <- methods::setRefClass("ChebiEntry",
    contains="BiodbXmlEntry"
)
