
context("PSets")

# in tests we used mocked availablePSets in case of lack of internet connection.
# to prepare new mock data: 
# saveRDS(PharmacoGx::availablePSets(canonical = FALSE), 
#         system.file("extdata", "data_for_unittests", "PSets.rds", package = "gDRimport")) 

read_mocked_PSets <- function(canonical = FALSE) {
  readRDS(
    system.file("extdata", "data_for_unittests", "PSets.rds", package = "gDRimport")
  )
}

test_that("getPSet works as expected", {
  pset <- testthat:::with_mock(
    `PharmacoGx::availablePSets` = read_mocked_PSets,
    suppressMessages(getPSet(
      "Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")
    ))
  )
  expect_equal(unname(dim(pset)), c(53, 46))
  expect_s4_class(pset, "PharmacoSet")
})


test_that("setEnvForPSet works as expected", {
  on.exit(gDRutils::reset_env_identifiers())
  setEnvForPSet()
  expect_equal(gDRutils::get_env_identifiers("drug"), "DrugName")
})

test_that(".extractDoseResponse works as expected", {
  pset <- testthat:::with_mock(
    `PharmacoGx::availablePSets` = read_mocked_PSets,
    suppressMessages(getPSet(
      "Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")
    ))
  )
  dt <- .extractDoseResponse(pset)
  expect_s3_class(dt, "data.table")
  expect_equal(names(dt), c("rn", "ReadoutValue", "Concentration", "clid", "DrugName", 
                            "Duration"))
  expect_equal(dim(dt), c(34684, 6))
})

test_that(".extractDoseResponse, .removeNegatives, and .createPseudoData work as expected", {
  pset <- testthat:::with_mock(
    `PharmacoGx::availablePSets` = read_mocked_PSets,
    suppressMessages(getPSet(
      "Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")
    ))
  )
  dt <- .extractDoseResponse(pset)
  expect_s3_class(dt, "data.table")
  expect_equal(names(dt), c("rn", "ReadoutValue", "Concentration", "clid", "DrugName", 
                            "Duration"))
  expect_equal(dim(dt), c(34684, 6))
  
  dt_positive <- .removeNegatives(dt)
  expect_equal(dim(dt_positive), c(34516, 6))
  
  dt_with_pseudodata <- .createPseudoData(dt_positive)
  expect_equal(dim(dt_with_pseudodata), c(34516, 7))
})

test_that("convert_pset_to_df works as expected", {
  on.exit(gDRutils::reset_env_identifiers())
  pset <- testthat:::with_mock(
    `PharmacoGx::availablePSets` = read_mocked_PSets,
    suppressMessages(getPSet(
      "Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")
    ))
  )
  dt <- convert_pset_to_df(pset)
  checkmate::assert_data_table(dt)
  expect_equal(dim(dt), c(34516, 7))
  expect_equal(names(dt), c("Barcode", "ReadoutValue", "Concentration", "Clid", "DrugName", 
                            "Duration", "ReferenceDivisionTime"))
})
