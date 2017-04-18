# What ap does
*Apercu* (ap) is a R package made to give you a quick view (an **aperçu** in
French) of any R object. It works (or is supposed to) on vectors, matrices,
data frames (even with matrices inside), lists and arrays. Since the first
versions, it is now able to select data from these objects. And finally, when
you want more details, it also gives you the dimension and classes of the object.

# How to install it
To install it, in R, you currectly just need to:
```{r}
library(devtools)
install_github("achateigner/apercu")
library(ap)
```

# Example usage
Creation of a vector, a matrix, a data frame, a list, and 3 arrays of 3, 4 and 5
dimensions:
```{r}
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
```

Automatic aperçu of the objects
```{r}
ap(v)
ap(m)
ap(df)
ap(li)
ap(a)
ap(a2)
ap(a3)
```

if the size of the object is very small :
```{r}
sm <- matrix(1:4, 2, 2)
sDf <- as.data.frame(sm)
ap(sm)
ap(sDf)
```

Specific aperçu of the objects
```{r}
ap(v, list(1:2))
ap(m, list(c(1,3), c(1:5)))
ap(m, list(c(1,3,1:5))) # outputs a vector as only one dimension is given
ap(df, list(c(1,3,5), 1:10)) # outputs lines 1, 3 and 5, and columns 1 to 10
ap(li, list(c(1:3,5),c(4,6,9), c(3,6))) # the result is different between a list and an array
ap(a, list(c(1:3,5),c(4,6,9), c(3,6))) # as the dimensions of both are not in the same order
ap(a, list(1,3,5)) # outputs the element a[1,3,5]
ap(a, list(c(1,3,5))) # outputs the element a[c(1,3,5),1,1]
ap(a2, list(c(1:4),c(3:5),c(2:8),c(3:4)))
ap(a3, list(c(1:4),c(3:5),c(2:3),c(3:4), c(7:10)))
```

To print also the dimensions and classes
```{r}
print(ap(v), printAll = T)
print(ap(m), printAll = T)
print(ap(df), printAll = T)
print(ap(li), printAll = T)
print(ap(a), printAll = T)
print(ap(a2), printAll = T)
print(ap(a3), printAll = T)
```

It also works with data frames with a matrix in it:
```{r}
library(pls)
data("gasoline")
ap(gasoline)
ap(gasoline, list(1:10, list(1:10,1:10)))
```

# How to set up the dev environment
If you want to fork my work, nothing special is required (just citing my work).
Here are the tests that are done on the package:
```{r}
context("ap")

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
```

# How to ship a change
If you want to contribute to this work, fill free to send a pull request.

# Change log (generated by katip https://github.com/lab2023/katip)

#### 
 * [3ac1bf2](../../commit/3ac1bf2) - __(Aurelien Chateigner)__ Added changelog and Readme
 * [4e69c7a](../../commit/4e69c7a) - __(Aurelien Chateigner)__ Added the fittingLimits function, resolved the problem of small data frames in objects. The library is now in version 0.2.1
 * [c0cfd24](../../commit/c0cfd24) - __(Aurelien Chateigner)__ goes to beta version : 0.2.0
 * [2e62d95](../../commit/2e62d95) - __(Aurelien Chateigner)__ added the compatibility with small objects, finished the tests
 * [5ab6cf4](../../commit/5ab6cf4) - __(Aurelien Chateigner)__ Started to ad testthat tests
 * [adeed99](../../commit/adeed99) - __(Aurelien Chateigner)__ resolved the problem of slow execution on a df with AsIs matrix inside
 * [a0e4ab1](../../commit/a0e4ab1) - __(Aurelien Chateigner)__ several corrections...
 * [42ebaba](../../commit/42ebaba) - __(Aurelien Chateigner)__ corrected my é
 * [bc1a3a0](../../commit/bc1a3a0) - __(Aurelien Chateigner)__ The print is now working properly, it also works again with all the objects, and one can chose to print all or not. Version 0.1.2
 * [c47cc94](../../commit/c47cc94) - __(Aurelien Chateigner)__ divided the ap to have smaller and more generic functions, aperWrapper, apercu, classDeterminer. Finally, added a specific print and ap class to only output the apercu as default, and the dimensions and classes when print(apObject, all=T).
 * [0be4354](../../commit/0be4354) - __(Aurelien Chateigner)__ corrected the case when elements of data frame have several classes
 * [ac5cb2d](../../commit/ac5cb2d) - __(Aurelien Chateigner)__ 1st commit


# License and author info
Author and maintainer: Aurelien Chateigner <aurelien.chateigner@gmail.com>  
License: CC BY-SA 4.0  
Special thanks to: Vincent Segura, Facundo Muñoz and Thibaud Chauvin for tests
and improvements.
