context("assert_utils")

test_that("assert_utils", {
  td1 <- get_test_data()
  expect_true(is_readable_v(manifest_path(td1)))
  expect_true(is_readable_v(result_path(td1)))
  err_msg1 <-
    "Following path(s) with no read permission found: '/non/existent/file'"
  expect_error(is_readable_v(c(manifest_path(td1), "/non/existent/file")), err_msg1, fixed = TRUE)
  
  td2 <- get_test_Tecan_data()
  expect_true(is_readable_v(td2$m_file))
  expect_true(is_readable_v(td2$r_files))
  expect_true(is_readable_v(td2$t_files))
  err_msg1 <-
    "Following path(s) with no read permission found: '/non/existent/file'"
  expect_error(is_readable_v(c(td2$m_file, "/non/existent/file")), err_msg1, fixed = TRUE)
  
})
