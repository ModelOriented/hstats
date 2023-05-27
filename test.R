library(collapse)

rle3 <- function(x) {
  is_vec <- is.vector(x)
  is_fac <- is.factor(x)
  grp <- collapse::GRP(x, sort = FALSE)
  values <- grp[[4L]]
  if (is_vec || is_fac) {
    values <- values[[1L]]
    if (is_fac) {
      values <- factor(values, levels = values)
    }
  }
  list(values = values, lengths = grp[[3L]])
}

ir <- iris[4:5]
microbenchmark(rle3(ir), rle2(ir))


# What does collapse do? -> turning g into data.frame does not change anything
g <- GRP(iris$Species, sort = FALSE)
x <- iris$Sepal.Length
fmean(x, g)  # Vector with groups as names

x <- as.matrix(iris$Sepal.Length)
fmean(x, g)  # Matrix with unnamed column and groups as rownames

x <- as.matrix(iris[1:2])
fmean(x, g)  # Matrix with two named columns and groups as rownames

# What does collapse do?
g <- GRP(iris$Petal.Width, sort = FALSE)
x <- iris$Sepal.Length
fmean(x, g)  # Vector with groups as names

x <- as.matrix(iris$Sepal.Length)
fmean(x, g)  # Matrix with unnamed column and groups as rownames

x <- as.matrix(iris[1:2])
fmean(x, g)  # Matrix with two named columns and groups as rownames
