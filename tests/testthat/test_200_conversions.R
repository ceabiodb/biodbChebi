test_chebi_convCasToChebi <- function(conn) {

    # Values
    cas2chebi1 <- c('51-41-2'='18357', '87605-72-9'='10000')
    cas2chebi2 <- c(as.list(cas2chebi1), list('14215-68-0'=c('40356', '28037')))

    # Get ChEBI IDs
    ids <- conn$convCasToChebi(names(cas2chebi1))
    testthat::expect_is(ids, 'character')
    testthat::expect_identical(ids, unname(cas2chebi1))

    # Chech NA
    ids <- conn$convCasToChebi(NA_character_)
    testthat::expect_is(ids, 'character')
    testthat::expect_identical(ids, NA_character_)
    ids <- conn$convCasToChebi(c(names(cas2chebi1), NA_character_))
    testthat::expect_identical(ids, c(unname(cas2chebi1), NA_character_))
    
    # Check with multiple ChEBI IDs for one CAS ID
    ids <- conn$convCasToChebi(names(cas2chebi2))
    testthat::expect_is(ids, 'list')
    testthat::expect_identical(ids, unname(cas2chebi2))
}

test_chebi_convInchiToChebi <- function(conn) {

    # Build InChIs
    inchi <- paste0('InChI=1S/C15H24/c1-9(2)11-7-8-15(4)12-6-5-10(3)14(15)13(11)12/h5,9,11',
                '-14H,6-8H2,1-4H3/t11?,12?,13?,14?,15-/m0/s1')
    inchikey <- 'VLXDPFLIRFYIME-MWHZVNNOSA-N'
    testthat::expect_equal(conn$convInchiToChebi(inchi), '10341')
    testthat::expect_equal(conn$convInchiToChebi(inchikey), '10341')
}

# Set test context
biodb::testContext("Test conversions")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbChebi')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('chebi')

# Run tests
biodb::testThat('convCasToChebi() works.',
          test_chebi_convCasToChebi, conn=conn)
biodb::testThat('convInchiToChebi() works.',
          test_chebi_convInchiToChebi, conn=conn)

# Terminate Biodb
biodb$terminate()
