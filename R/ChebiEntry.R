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
#' @importFrom R6 R6Class
#' @export
ChebiEntry <- R6::R6Class("ChebiEntry",
    inherit=biodb::BiodbXmlEntry
)
