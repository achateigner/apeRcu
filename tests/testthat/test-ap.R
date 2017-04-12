context("ap")

## TODO: Rename context
## TODO: Add more tests
# Creation of objects
v <- c(1:20)
names(v) <- letters[1:20]
m <- matrix(1:100, 10, 10)
dimnames(m) <- list(letters[1:10], letters[1:10])
df <- as.data.frame(m)
li <- lapply(1:10, function(x) {u <- matrix((1:100)*x, 10,10); dimnames(u) <- list(letters[1:10], letters[1:10])
return(u)})
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



test_that("works on vector", {
  expect_equal(ap(v)$apercu, v[1:5])
})
