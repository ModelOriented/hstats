test_that("prepare_pred() works", {
  expect_equal(prepare_pred(iris[1:4]), data.matrix(iris[1:4]))
  expect_error(prepare_pred(iris["Species"]))
  expect_equal(prepare_pred(iris$Sepal.Width), iris$Sepal.Width)
  expect_equal(prepare_pred(iris["Sepal.Width"]), iris$Sepal.Width)
})

test_that("prepare_w() works", {
  w1 <- prepare_w(iris$Sepal.Length, X = iris)
  w2 <- prepare_w("Sepal.Length", X = iris)
  expect_equal(w1$w, iris$Sepal.Length)
  expect_equal(w2$w, iris$Sepal.Length)  
})

test_that("prepare_by() works", {
  by1 <- prepare_by(iris$Species, X = iris)
  by2 <- prepare_by("Species", X = iris)
  expect_equal(by1$BY, iris$Species)
  expect_equal(by2$BY, iris$Species)  
  expect_equal(by1$by_name, "Group")
  expect_equal(by2$by_name, "Species")
})

test_that("prepare_y() works", {
  # "Vector" interface
  expect_equal(prepare_y(iris[1:4], X = iris)$y, data.matrix(iris[1:4]))
  expect_equal(prepare_y(iris["Species"], X = iris)$y, iris$Species)
  expect_equal(prepare_y(iris$Sepal.Width, X = iris)$y, iris$Sepal.Width)
  expect_equal(prepare_y(iris["Sepal.Width"], X = iris)$y, iris$Sepal.Width)
  
  # "Column names" interface
  cn <- colnames(iris[1:4])
  out <- prepare_y(cn, X = iris)
  expect_equal(out$y, data.matrix(iris[1:4]))
  expect_equal(out$y_names, cn)
  
  out <- prepare_y("Species", X = iris)
  expect_equal(out$y, iris$Species)
  expect_equal(out$y_names, "Species")
  
  out <- prepare_y("Sepal.Width", X = iris)
  expect_equal(out$y, iris$Sepal.Width)
  expect_equal(out$y_names, "Sepal.Width")
})
