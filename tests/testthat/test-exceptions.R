library(testthat)

test_that("check get_exception_data works correctly", {
  obs <- gDRimport::get_exception_data(1)
  expect_equal(NROW(obs), 1)
  expect_equal(colnames(obs),
               c("status_code", "title", "sprintf_text", "type", "input_type"))
  expect_equal(obs$sprintf_text,
               "There were errors loading manifest. Check error message below:\n```\n%s\n```")

  expect_true(all(dim(gDRimport::get_exception_data()) >= 5))

  obs <- gDRimport::get_exception_data(10)
  expect_is(obs, "data.table")

  obs <- gDRimport::get_exception_data()
  expect_gt(NROW(obs), 1)
  expect_is(obs, "data.table")

  expect_error(
    gDRimport::get_exception_data("two"),
    "Assertion on 'status_code' failed: Must be of type 'number'",
    fixed = TRUE
  )

  expect_error(
    gDRimport::get_exception_data(0),
    paste0(
      "Assertion on \\'toString\\(status_code\\)\\' failed:.+but is \\'0\\'"
    )
  )
})

test_that("the codes added for validate_gdr_input are in the table", {
  obs <- gDRimport::get_exception_data()
  expect_true(all(as.character(40:49) %in% obs$status_code))
  expect_identical(unique(obs$type[obs$status_code %in% as.character(40:49)]), "error")
})

test_that(".exception_code_from_message finds the code of every message in the table", {
  tbl <- gDRimport::get_exception_data()
  for (i in seq_len(NROW(tbl))) {
    n_args <- lengths(regmatches(tbl$sprintf_text[i], gregexpr("%s", tbl$sprintf_text[i], fixed = TRUE)))
    msg <- do.call(sprintf, c(list(tbl$sprintf_text[i]), as.list(rep("file one.xlsx", n_args))))
    expect_identical(.exception_code_from_message(msg), as.integer(tbl$status_code[i]), label = msg)
  }
  expect_identical(.exception_code_from_message("an error that is not in the table"), NA_integer_)
})
