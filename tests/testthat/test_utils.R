test_that("check_pred() works", {
  expect_error(check_pred("a"))
  expect_true(is.matrix(check_pred(array(1:4, dim = c(2L, 2L)))))
  expect_true(is.matrix(check_pred(iris[1:4])))
  expect_equal(check_pred(1:4), 1:4)
})

test_that("fix_names() works", {
  expect_equal(colnames(fix_names(cbind(1:10))), "y")
  expect_equal(colnames(fix_names(cbind(1:5, 1:5))), c("y1", "y2"))
  expect_equal(colnames(fix_names(cbind(a = 1:5))), "a")
})


