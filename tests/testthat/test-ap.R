context("apercu")

# Creation of objects
v <- c(1:20)
names(v) <- letters[1:20]
m <- matrix(1:100, 10, 10)
dimnames(m) <- list(letters[1:10], letters[1:10])
df <- as.data.frame(m)
li <- lapply(1:10, function(x) {u <- matrix((1:100)*x, 10,10); dimnames(u) <- list(letters[1:10], letters[1:10])
return(u)})
lis <- li[1:3]
names(li) <- letters[1:10]
a <- array(c(1:1000), c(10,10,10))
dimnames(a) <- list(letters[1:10], letters[1:10], letters[1:10])
a2 <- array(1:10000, c(10,10,10,10))
dimnames(a2) <- list(letters[1:10], letters[1:10], letters[1:10], letters[1:10])
a3 <- array(1:100000, c(10,10,10,10,10))
dimnames(a3) <- list(letters[1:10], letters[1:10], letters[1:10], letters[1:10], letters[1:10])
sm <- matrix(1:4, 2, 2)
sDf <- as.data.frame(sm)
library(pls)
data("gasoline")



test_that("works on vectors", {
  expect_equal(ap(v)$apercu, v[1:5])
  expect_equal(ap(v)$dimensions, NULL)
  expect_equal(ap(v)$classes, "integer")
})

test_that("works on matrices", {
  expect_equal(ap(m)$apercu, m[1:5,1:5])
  expect_equal(ap(m)$dimensions, c(10,10))
  expect_equal(ap(m)$classes, "matrix")
})

test_that("works on small matrices", {
  expect_equal(ap(sm)$apercu, sm[1:2,1:2])
  expect_equal(ap(sm)$dimensions, c(2,2))
  expect_equal(ap(sm)$classes, "matrix")
})

test_that("works on data.frames", {
  expect_equal(ap(df)$apercu, df[1:5,1:5])
  expect_equal(ap(df)$dimensions, c(10,10))
  expect_equal(ap(df)$classes,
               list(object="data.frame", elements="integer"))
})

test_that("works on small data.frames", {
  expect_equal(ap(sDf)$apercu, sDf[1:2,1:2])
  expect_equal(ap(sDf)$dimensions, c(2,2))
  expect_equal(ap(sDf)$classes,
               list(object="data.frame", elements="integer"))
})

test_that("works on data.frames with AsIs Matrices in it", {
  expect_equal(ap(gasoline)$apercu,
               data.frame(octane=gasoline$octane[1:5],
                          NIR=I(gasoline$NIR[1:5,1:5])))
  expect_equal(ap(gasoline)$dimensions,
               list(octane=NULL, NIR=c(60,401)))
  expect_equal(ap(gasoline)$classes, list(octane="numeric", NIR="AsIs"))
})

test_that("works on lists", {
  expect_equal(ap(li)$apercu, lapply(li[1:5], function(x) x[1:5,1:5]))
  expect_equal(ap(li)$dimensions, c(10,10))
  expect_equal(ap(li)$classes, "matrix")
})

test_that("works on small lists", {
  expect_equal(ap(lis)$apercu, lapply(lis, function(x) x[1:5,1:5]))
  expect_equal(ap(lis)$dimensions, c(10,10))
  expect_equal(ap(lis)$classes, "matrix")
})

test_that("works on arrays of 3 dimensions", {
  expect_equal(ap(a)$apercu, a[1:5,1:5,1:5])
  expect_equal(ap(a)$dimensions, c(10,10,10))
  expect_equal(ap(a)$classes, "array")
})

test_that("works on arrays of 4 dimensions", {
  expect_equal(ap(a2)$apercu, a2[1:5,1:5,1:5,1:5])
  expect_equal(ap(a2)$dimensions, c(10,10,10,10))
  expect_equal(ap(a2)$classes, "array")
})

test_that("works on arrays of 5 dimensions", {
  expect_equal(ap(a3)$apercu, a3[1:5,1:5,1:5,1:5,1:5])
  expect_equal(ap(a3)$dimensions, c(10,10,10,10,10))
  expect_equal(ap(a3)$classes, "array")
})

