test.chebi.wsGetLiteEntity <- function(conn) {

	# Get Id
	id = "2571"
	entry.ids = conn$wsGetLiteEntity(search = id, search.category = 'CHEBI ID',
                                     retfmt = 'ids')
	testthat::expect_is(entry.ids, 'character')
	testthat::expect_length(entry.ids, 1)
	testthat::expect_identical(entry.ids, id)
}

# Set test context
biodb::testContext("Test web services")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbChebi')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('chebi')

# Run tests
biodb::testThat('Web service getLiteEntity works fine.',
          test.chebi.wsGetLiteEntity, conn=conn)

# Terminate Biodb
biodb$terminate()
