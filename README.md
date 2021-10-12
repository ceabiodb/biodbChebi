<!-- vimvars: b:markdown_embedded_syntax={'r':''} -->
# biodbChebi

[![Codecov test coverage](https://codecov.io/gh/pkrog/biodbChebi/branch/master/graph/badge.svg)](https://codecov.io/gh/pkrog/biodbChebi?branch=master)

An R Bioconductor package for accessing [ChEBI](https://www.ebi.ac.uk/chebi/)
online database, based on Bioconductor package/framework
[biodb](https://github.com/pkrog/biodb/).

## Introduction

*biodbChebi* is an extension package of the *biodb* package.
It allows to connect to ChEBI for retrieving entries, searching for entries by
name or mass, and convert CAS IDs or InChI to ChEBI IDs.

## Examples

Getting a single entry:
```r
bdb <- biodb::Biodb()
chebi <- bdb$getFactory()$createConn('chebi')
entries <- chebi$getEntry(c('2528', '7799', '15440'))
bdb$entriesToDataframe(entries)
```

Searching by name and mass:
```r
bdb <- biodb::Biodb()
chebi <- bdb$getFactory()$createConn('chebi')
ids <- chebi$searchCompound(name='aspartic', mass=133,
    mass.field='molecular.mass', mass.tol=0.3, max.results=3)
```

See the vignette for more examples.

## Installation

Install the latest stable version using Bioconductor:
```r
if (!requireNamespace("BiocManager", quietly = TRUE))
    install.packages("BiocManager")
BiocManager::install('biodbChebi')
```

## Documentation

See the introduction vignette:
```r
vignette('biodbChebi', package='biodbChebi')
```

## Citations

 * Hastings, J., de Matos, P., Dekker, A., Ennis, M., Harsha, B., Kale, N., Muthukrishnan, V., Owen, G., Turner, S., Williams, M., and Steinbeck, C. (2013) The ChEBI reference database and ontology for biologically relevant chemistry: enhancements for 2013. Nucleic Acids Res, <http://dx.doi.org/10.1093/nar/gks1146>.
