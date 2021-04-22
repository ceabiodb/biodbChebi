#' ChEBI connector class.
#'
#' This is the connector class for connecting to the ChEBI database through its
#' web services.
#'
#' @examples
#' # Create an instance with default settings:
#' mybiodb <- biodb::Biodb()
#'
#' # Create a connector
#' conn <- mybiodb$getFactory()$createConn('chebi')
#'
#' # Get an entry
#' e <- conn$getEntry('15440')
#'
#' # Convert an InChI KEY to a ChEBI identifier
#' conn$convInchiToChebi('YYGNTYWPHWGJRM-AAJYLUCBSA-N')
#'
#' # Terminate instance.
#' mybiodb$terminate()
#'
#' @import methods
#' @export ChebiConn
#' @exportClass ChebiConn
ChebiConn <- methods::setRefClass("ChebiConn",
    contains=c("BiodbRemotedbConn", "BiodbCompounddbConn"),
    fields=list(
        wsdl='ANY',
        ws.values='list' # Stores WSDL values
    ),

methods=list(

initialize=function(...) {

    callSuper(...)

    .self$initFields(wsdl=NULL, ws.values=list())
},

getEntryPageUrl=function(id) {
    # Overrides super class' method
    
    url <- c(.self$getPropValSlot('urls', 'base.url'), 'searchId.do')
    
    urls <- vapply(id, function(x) BiodbUrl$new(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')
    
    return(urls)
},

getEntryImageUrl=function(id) {
    # Overrides super class' method

    url <- c(.self$getPropValSlot('urls', 'base.url'), 'displayImage.do')
    
    urls <- vapply(id,
                   function(x) BiodbUrl$new(url=url,
                                        params=list(defaultImage='true',
                                          imageIndex=0, chebiId=x,
                                          dimensions=400))$toString(),
                  FUN.VALUE='')
    
    return(urls)
},


wsWsdl=function(retfmt=c('plain', 'parsed', 'request')) {
    ":\n\nRetrieves the complete WSDL from the web server.
    \nretfmt: The return format to use. 'plain' will return the value as it is
    returned by the server. 'parsed' will return an XML object. 'request' will
    return a BiodbRequest object representing the request that would have been
    sent. 
    \nReturned value: Depending on `retfmt` value.
    "

    retfmt <- match.arg(retfmt)

    # Build request
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'webservice')
    request <- .self$makeRequest(method='get', url=BiodbUrl$new(url=url,
                                                            params='wsdl'))
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt == 'parsed')
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

    return(results)
},

wsGetLiteEntity=function(search=NULL, search.category='ALL', stars='ALL',
                         max.results=10,
                         retfmt=c('plain', 'parsed', 'request', 'ids')) {
    ":\n\nCalls getLiteEntity web service and returns the XML result.
    Be careful when searching by mass
    (search.category='MASS' or 'MONOISOTOPIC MASS'), since the search is made
     in text mode, thus the number must be exactly written as it is stored in
     database, eventually padded with 0 in order to have exactly 5 digits after
     the decimal. An easy solution is to use wildcards to search a mass:
     '410;.718*'.
    See http://www.ebi.ac.uk/chebi/webServices.do for more details.
    \nsearch: The text or pattern to search.
    \nsearch.category: The search category. Call `getSearchCategories()`
    to get a full list of search categories.
    \nmax.results: The maximum of results to return.
    \nstars: How many starts the returned entities should have. Call
    `getStarsCategories() to get a full list of starts categories.`
    \nretfmt: The return format to use. 'plain' will return the results as
    given by the server, in a string. 'parsed' will return an XML object.
    'request' will return a BiodbRequest object representing the request as
    would have been sent. 'ids' will return a list of matched entity IDs.
    \nReturned value: Depending on `retfmt` value.
    "

    retfmt <- match.arg(retfmt)

    # Check parameters
    chk::chk_string(search)
    chk::chk_in(search.category, .self$getSearchCategories())
    chk::chk_number(max.results)
    chk::chk_gte(max.results, 0)
    chk::chk_in(stars, .self$getStarsCategories())

    # Build request
    params <- c(search=search,
                searchCategory=search.category,
                maximumResults=max.results,
                starsCategory=stars)
    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test/getLiteEntity')
    request <- .self$makeRequest(method='get',
                                 url=BiodbUrl$new(url=url, params=params),
                            encoding='UTF-8')
    if (retfmt == 'request')
        return(request)

    # Send request
    results <- .self$getBiodb()$getRequestScheduler()$sendRequest(request)

    # Parse
    if (retfmt != 'plain') {

        # Parse XML
        results <-  XML::xmlInternalTreeParse(results, asText=TRUE)

        if (retfmt == 'ids') {
            ns <- .self$getPropertyValue('xml.ns')
            results <- XML::xpathSApply(results, "//chebi:chebiId",
                                        XML::xmlValue, namespaces=ns)
            results <- sub('CHEBI:', '', results)
            if (length(grep("^[0-9]+$", results)) != length(results))
                biodb::error("Impossible to parse XML to get entry IDs.")
        }
    }

    return(results)
},

convIdsToChebiIds=function(ids, search.category, simplify=TRUE) {
    ":\n\nConverts a list of IDs (InChI, InChI Keys, CAS, ...) into a list of
    ChEBI IDs. Several ChEBI IDs may be returned for a single ID.
    \nsimplify: If set to TRUE and only one ChEBI ID has been found for each ID,
    then a character vector is returned. Otherwise a list of character vectors
    is returned.
    \nsearch.category: The search category. Call `getSearchCategories()`
    to get a full list of search categories.
    \nReturned value: Depending on the value of simplify.
    "

    chebi <- list()
    msg <- paste('Converting', search.category, 'IDs to ChEBI IDs.')

    # Loop on all cas IDs
    prg <- biodb::Progress$new(biodb=.self$getBiodb(), msg=msg,
                               total=length(ids))
    for (id in ids) {

        # Get ChEBI IDs for this ID
        if (is.na(id))
            x <- character()
        else
            x <- .self$wsGetLiteEntity(id, search.category=search.category,
                                       retfmt='ids')

        chebi <- c(chebi, list(x))

        # Send progress message
        prg$increment()
    }

    # Simplify
    if (simplify && all(vapply(chebi, length, FUN.VALUE=1L) < 2)) {
        chebi <- lapply(chebi, function(x) if (length(x) == 0) NA_character_
                                           else x)
        chebi <- unlist(chebi)
    }

    return(chebi)
},

convInchiToChebi=function(inchi, simplify=TRUE) {
    ":\n\nConverts a list of InChI or InChI KEYs into a list of ChEBI IDs.
    Several ChEBI IDs may be returned for a single InChI or InChI KEY.
    \nsimplify: If set to TRUE and only one ChEBI ID has been found for each ID,
    then a character vector is returned. Otherwise a list of character vectors
    is returned.
    \nReturned value: Depending on the value of simplify.
    "

    return(.self$convIdsToChebiIds(inchi, search.category='INCHI/INCHI KEY',
                                   simplify=simplify))
},

convCasToChebi=function(cas, simplify=TRUE) {
    ":\n\nConverts a list of CAS IDs into a list of ChEBI IDs.
    Several ChEBI IDs may be returned for a single InChI or InChI KEY.
    \nsimplify: If set to TRUE and only one ChEBI ID has been found for each ID,
    then a character vector is returned. Otherwise a list of character vectors
    is returned.
    \nReturned value: Depending on the value of simplify.
    "

    return(.self$convIdsToChebiIds(cas, search.category='REGISTRY NUMBERS',
                                   simplify=simplify))
},

.searchByMass=function(mass.field, mass.min, mass.max, max.results=0) {

    ids <- character()

    # Set search category
    if (mass.field == 'monoisotopic.mass')
        search.category <- 'MONOISOTOPIC MASS'
    else if (mass.field == 'molecular.mass')
        search.category <- 'MASS'
    else
        biodb::error('Unknown mass field "%s".', mass.field)
                           
    # Search for all masses in the range
    n <- floor(log10(mass.max - mass.min))
    if (n >= 0)
        n <- -1
    firstMass <- floor(mass.min * 10^-n) * 10^n
    lastMass <- ceiling(mass.max * 10^-n) * 10^n
    for (m in seq(firstMass, lastMass, 10^n)) {

        # Get entries matching integer mass
        x <- .self$wsGetLiteEntity(search=paste0(m, '*'),
                                   search.category=search.category,
                                   max.results=0, retfmt='ids')
        
        # Remove IDs that we already have
        x <- x[ ! x %in% ids]
        
        # Filter on mass range
        x <- .self$.filterIdsOnMassRange(x, mass.min, mass.max,
                                         mass.field,
                                         max.results - length(ids))
        
        # Add IDs
        ids <- c(ids, x)
        
        if ( ! is.na(max.results) && max.results > 0 && length(ids)
            >= max.results)
            break
    }

    # Remove duplicates
    ids <- ids[ ! duplicated(ids)]
    
    return(ids)
},

.doSearchForEntries=function(fields=NULL, max.results=0) {

    ids <- NULL

    if ( ! is.null(fields)) {

        # Search by name
        if ('name' %in% names(fields))
            ids <- .self$wsGetLiteEntity(search=fields$name,
                                         search.category="ALL NAMES",
                                         max.results=0, retfmt='ids')
        
        # Search by mass
        for (mass.field in c('monoisotopic.mass', 'molecular.mass'))
            if (mass.field %in% names(fields)) {
                rng <- do.call(Range$new, fields[[mass.field]])
                if (is.null(ids))
                    ids <- .self$.searchByMass(mass.field,
                                               mass.min=rng$getMin(),
                                               mass.max=rng$getMax(),
                                               max.results=max.results)
                else
                    ids <-  .self$.filterIdsOnMassRange(ids, mass.min=rng$getMin(),
                        mass.max=rng$getMax(), mass.field=mass.field,
                        limit=max.results)
            }
    }

    # Empty list if no search has been run
    if (is.null(ids))
        ids <- character()

    # Cut
    if (max.results > 0 && max.results < length(ids))
        ids <- ids[seq_len(max.results)]

    return(ids)
},

getWsdl=function() {
    ":\n\nGets the WSDL as an XML object.
    \nReturned value: The ChEBI WSDL as an XML object.
    "
    
    if (is.null(.self$wsdl))
        .self$wsdl <- .self$wsWsdl(retfmt='parsed')
    
    return(.self$wsdl)
},

getWsdlEnumeration=function(name) {
    ":\n\nExtracts a list of values from an enumeration in the WSDL.
    \nname: The name of the enumeration for which to retrieve the values.
    \nReturned value: A character vector listing the enumerated values.
    "
    
    if ( ! name %in% names(.self$ws.values)) {

        ns <- .self$getPropertyValue('xml.ns')

        # Get search categories
        expr <- paste0("//xsd:simpleType[@name='", name, "']//xsd:enumeration")
        res <- XML::xpathSApply(.self$getWsdl(), expr, XML::xmlGetAttr, 'value',
                                namespaces=ns)
        .self$ws.values[[name]] <- res
    }
    
    return(.self$ws.values[[name]])
},

getStarsCategories=function() {
    ":\n\nGets the list of allowed stars categories for the getLiteEntity web
    service.
    \nReturned value: Returns all the possible stars categories as a character
    vector.
    "
    
    return(.self$getWsdlEnumeration('StarsCategory'))
},

getSearchCategories=function() {
    ":\n\nGets the list of allowed search categories for the getLiteEntity web
    service.
    \nReturned value: Returns all the possible search categories as a character
    vector.
    "

    return(.self$getWsdlEnumeration('SearchCategory'))
},

.doGetEntryContentRequest=function(id, concatenate=TRUE) {

    url <- c(.self$getPropValSlot('urls', 'ws.url'), 'test',
             'getCompleteEntity')

    urls <- vapply(id, function(x) BiodbUrl$new(url=url,
                                            params=list(chebiId=x))$toString(),
                   FUN.VALUE='')

    return(urls)
},

.doGetEntryIds=function(max.results=NA_integer_) {
    return(.self$wsGetLiteEntity(search='1*', search.category='CHEBI ID',
                                 max.results=max.results, retfmt='ids'))
},

.filterIdsOnMassRange=function(ids, mass.min, mass.max, mass.field, limit=0) {

    retids <- character()
    msg <- paste0('Filtering ChEBI entries on mass range [', mass.min, ',
                  ', mass.max, '] and field "', mass.field, '".')

    # Loop on all IDs
    prg <- biodb::Progress$new(biodb=.self$getBiodb(), msg=msg,
        total=length(ids))
    for (id in ids) {

        # Print progress
        prg$increment()

        # Get entry
        e <- .self$getEntry(id, drop=TRUE)

        # Test mass
        if ( ! is.null(e)) {
            m <- e$getFieldValue(mass.field)
            if ( ! is.na(m) && m >= mass.min && m <= mass.max) {
                retids <- c(retids, id)

                # Stop if limit is reached
                if (limit > 0 && length(retids) >= limit)
                    break
            }
        }
    }

    return(retids)
}

))
