context("correction")

test_that("fix_typos_with_reference", {

a <- c("GNUMBER 2", "Concentration  3")
b <- c("gnumber_2_3", "my_concentration_2")
c <- c("gnumber2", "my_concentration3")
d <- c(" gnumber_2", "GNUmber_3", " Concentration ", "concentration_3 ")

aobs <- as.character(fix_typos_with_reference(a, ref = get_expected_template_sheets("optional")))
aref <- as.character(gDRutils::get_env_identifiers(c("drug2", "concentration3"), FALSE))
expect_equal(aref, aobs)

bobs <- as.character(fix_typos_with_reference(b, ref = get_expected_template_sheets("optional"), method = "grepl"))
bref <- as.character(gDRutils::get_env_identifiers(c("drug2", "concentration2"), FALSE))

cobs <-
  as.character(
    fix_typos_with_reference(
      c,
      ref = get_expected_template_sheets("optional"),
      method = "grepl",
      fix_underscores = TRUE
    )
  )
cref <- as.character(gDRutils::get_env_identifiers(c("drug2", "concentration3"), FALSE))
expect_equal(cref, cobs)

dobs <- as.character(fix_typos_with_reference(d, ref = get_expected_template_sheets()))
dref <- as.character(gDRutils::get_env_identifiers(c("drug2", "drug3", "concentration", "concentration3"), FALSE))
expect_equal(dref, dobs)
})

test_that("fix_typos_with_reference", {

a <- c("GNUMBER 2", "Concentration  3")
b <- c("gnumber_2_3", "my_concentration_2")
c <- c("gnumber2", "my_concentration3")
d <- c(" gnumber_2", "GNUmber_3", " Concentration ", "concentration_3 ")
e <- c("Gnomber", "Concuntration")

aobs <- as.character(fix_typos_with_reference(a, ref = get_expected_template_sheets("optional")))
aref <- as.character(gDRutils::get_env_identifiers(c("drug2", "concentration3"), FALSE))
expect_equal(aref, aobs)

bobs <- as.character(fix_typos_with_reference(b, ref = get_expected_template_sheets("optional"), method = "grepl"))
bref <- as.character(gDRutils::get_env_identifiers(c("drug2", "concentration2"), FALSE))

cobs <-
  as.character(
    fix_typos_with_reference(
      c,
      ref = get_expected_template_sheets("optional"),
      method = "grepl",
      fix_underscores = TRUE
    )
  )
cref <- as.character(gDRutils::get_env_identifiers(c("drug2", "concentration3"), FALSE))
expect_equal(cref, cobs)

dobs <- as.character(fix_typos_with_reference(d, ref = get_expected_template_sheets()))
dref <- as.character(gDRutils::get_env_identifiers(c("drug2", "drug3", "concentration", "concentration3"), FALSE))
expect_equal(dref, dobs)

eobs <- as.character(fix_typos_with_reference(e, ref = get_expected_template_sheets(), method = "adist"))
eref <- as.character(gDRutils::get_env_identifiers(c("drug", "concentration"), FALSE))
expect_equal(eref, eobs)
})


test_that("correct_template_sheets works as expected", {
  tfiles <- list("template1.xlsx" = c("Gnumber", "Concentration", "Media"),
                 "template2.xlsx" = c("Gnomber", "Concentration"),
                 "template3.xlsx" = c("Gnomber", "Concentration", "Gnumber_2", "Concentration_2"),
                 "template_untr.xlsx" = c("Gnumber", "Media"))
  tfiles_empty <- lapply(tfiles, function(x) {
    listData <- lapply(x, function(y) data.frame(x = runif(12)))
    names(listData) <- x
    listData
  })
  lapply(names(tfiles_empty), function(x) {
    openxlsx::write.xlsx(tfiles_empty[[x]], file.path(tempdir(), x))
  })
  correctedList <- correct_template_sheets(file.path(tempdir(), names(tfiles)))
  
  tfilesCorrect <- list("template1.xlsx" = c("Gnumber", "Concentration", "Media"),
                 "template2.xlsx" = c("Gnumber", "Concentration"),
                 "template3.xlsx" = c("Gnumber", "Concentration", "Gnumber_2", "Concentration_2"),
                 "template_untr.xlsx" = c("Gnumber", "Media"))
  testthat::expect_identical(lapply(unname(correctedList), unname), unname(tfilesCorrect))
})
