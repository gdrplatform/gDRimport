test_that("getPSet works as expected", {
  pset <- suppressMessages(getPSet("Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")))
  expect_equal(unname(dim(pset)), c(53, 46))
  expect_s4_class(pset, "PharmacoSet")
})

test_that("setEnvForPSet works as expected", {
  setEnvForPSet()
  expect_equal(gDRutils::get_env_identifiers("drug"), "DrugName")
  gDRutils::reset_env_identifiers()
})

test_that(".extractDoseResponse works as expected", {
  pset <- suppressMessages(getPSet("Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")))
  dt <- .extractDoseResponse(pset)
  expect_s3_class(dt, "data.table")
  expect_equal(names(dt), c("rn", "ReadoutValue", "Concentration", "clid", "DrugName", 
                            "Duration"))
  expect_equal(dim(dt), c(34684, 6))
})

test_that(".extractDoseResponse, .removeNegatives, and .createPseudoData work as expected", {
  pset <- suppressMessages(getPSet("Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")))
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
  pset <- suppressMessages(getPSet("Tavor_2020", psetDir = system.file("extdata/pset", package = "gDRimport")))
  dt <- convert_pset_to_df(pset)
  expect_s3_class(dt, "data.frame")
  expect_equal(dim(dt), c(34516, 7))
  expect_equal(names(dt), c("Barcode", "ReadoutValue", "Concentration", "Clid", "DrugName", 
                            "Duration", "ReferenceDivisionTime"))
})
