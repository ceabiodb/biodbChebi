test.chebi.encoding.issue.in.xml <- function(conn) {

	entry.ids <- conn$wsGetLiteEntity(search = "2571",
                                      search.category = 'CHEBI ID',
                                      retfmt = 'ids')
	testthat::expect_is(entry.ids, 'character')
	testthat::expect_length(entry.ids, 1)

    # +- sign (U+00b1) appears as <c2><b1> if encoding is not set to UTF-8:
    # "<chebiName>(<c2><b1>)-2-Heptanol</chebiName>" instead of
    # "<chebiName>(±)-2-Heptanol</chebiName>"
	entry <- conn$getEntry('2571')
}

# Set test context
biodb::testContext("Non regression tests")

# Instantiate Biodb
biodb <- biodb::createBiodbTestInstance(ack=TRUE)

# Load package definitions
defFile <- system.file("definitions.yml", package='biodbChebi')
biodb$loadDefinitions(defFile)

# Create connector
conn <- biodb$getFactory()$createConn('chebi')

# Run tests
biodb::testThat('ChEBI encoding issue in XML is handled.',
          test.chebi.encoding.issue.in.xml, conn=conn)

# Terminate Biodb
biodb$terminate()

