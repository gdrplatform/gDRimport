context("assert_utils")

test_that("assert_utils", {
  td1 <- get_test_data1()
  expect_true(is_readable_v(td1$m_file))
  expect_true(is_readable_v(td1$r_files))
  err_msg1 <-
    "Following path(s) with no read permission found: '/non/existent/file'"
  expect_error(is_readable_v(c(td1$m_file, "/non/existent/file")), err_msg1, fixed = TRUE)
})