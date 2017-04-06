#' @title Apercu of your object
#'
#' @description Some says that size doesn't matter, but when it comes to data
#' it does. The goal is to print an "aperçu", a short view of an object.
#'
#' @details The goal is to print an "aperçu", a short view of a vector, a
#' matrix, a data.frame, a list or an array. By default, it prints the first 5
#' elements of each dimension. By default, the number of columns is equal to
#' the number of lines. If you want to control the selection of the elements,
#' you can pass a list, with each element being a vector giving the selection
#' for each dimension. This function provides a simpler way of using the '['
#' function, with the same speed and flexibility.
#' @param object A vector, matrix, data frame, list or array
#' @param limitsList A list with each element being the selection of each dimension
#' @return A quick look or a selection of the data
#' @examples
#' # Creation of a vector, a matrix, a data frame, a list and 3 arrays of 3, 4
#' # and 5 dimensions:
#' v <- c(1:20)
#' names(v) <- letters[1:20]
#' m <- matrix(1:100, 10, 10)
#' dimnames(m) <- list(letters[1:10], letters[1:10])
#' df <- as.data.frame(m)
#' li <- lapply(1:10, function(x) {
#'     u <- matrix((1:100)*x, 10,10)
#'     dimnames(u) <- list(letters[1:10], letters[1:10])
#'     return(u)
#' })
#' names(li) <- letters[1:10]
#' a <- array(c(1:1000), c(10,10,10))
#' dimnames(a) <- list(letters[1:10], letters[1:10], letters[1:10])
#' a2 <- array(1:10000, c(10,10,10,10))
#' dimnames(a2) <- list(letters[1:10], letters[1:10], letters[1:10],
#'     letters[1:10])
#' a3 <- array(1:100000, c(10,10,10,10,10))
#' dimnames(a3) <- list(letters[1:10], letters[1:10], letters[1:10],
#'     letters[1:10], letters[1:10])
#'
#'
#' # Automatic aperçu of the objects
#' ap(v)
#' ap(m)
#' ap(df)
#' ap(li)
#' ap(a)
#' ap(a2)
#' ap(a3)
#'
#' # if the size of the object is very small :
#' sm <- matrix(1:4, 2, 2)
#' sDf <- as.data.frame(sm)
#' ap(sm)
#' ap(sDf)
#'
#' # Specific aperçu of the objects
#' ap(v, list(1:2))
#' ap(m, list(c(1,3), 1:5))
#' ## outputs a vector as only one dimension is given
#' ap(m, list(c(1,3,1:5)))
#' ## outputs lines 1, 3 and 5, and columns 1 to 10
#' ap(df, list(c(1,3,5), 1:10))
#' ## the result is different between a list and an array
#' ## as the dimensions of both are not in the same order
#' ap(li, list(c(1:3,5),c(4,6,9), c(3,6)))
#' ap(a, list(c(1:3,5),c(4,6,9), c(3,6)))
#' ## outputs the element a[1,3,5]
#' ap(a, list(1,3,5))
#' ## outputs the element a[c(1,3,5),1,1]
#' ap(a, list(c(1,3,5)))
#' ap(a2, list(1:4,3:5,2:8,3:4))
#' ap(a3, list(1:4,3:5,2:3,3:4,7:10))
#'
#' # It also works with data frames with a matrix in it:
#' library(pls)
#' data("gasoline")
#' ap(gasoline)
#' @export
ap <- function(object, limitsList=limitsLister(object)) {
  apercu <- function(o, l=lapply(seq_along(dim(o)), function(x) 1:5)){
    if(is.list(l) & length(l) == 0){
      if(is.matrix(o)){
        l=list(c(1:5), c(1:5))
      } else l=list(c(1:5))
    }
    do.call(`[`, c(list(o), l))
  }

  if (is.list(object) & !is.data.frame(object)) {
    if(is.list(limitsList) & length(limitsList) == 0) {
      limitsList=list(c(1:5))
      lapply(object[c(limitsList[[1]])], function(x) apercu(x))
    } else {
      lapply(object[c(limitsList[[1]])],
             function(x) apercu(x, limitsList[2:length(limitsList)]))
    }
  } else if(is.data.frame(object) & sum(unlist(sapply(object,
                                               function(x) class(x) == "AsIs")))){
    if(is.list(limitsList) & length(limitsList) == 0) {
      limitsList = lapply(object, function(x) lapply(seq_along(dim(x)),
                                                       function(y) 1:5))
    } else if(is.list(limitsList) & length(limitsList) > 0){
      lm2 <- limitsLister(object)
      if (all.equal(limitsList, lm2)) {
        limitsList[sapply(object, function(x) class(x) == "AsIs")] <- list(list(1:5,1:5))
      }
      limitsList <- lapply(limitsList, function(x){
        if (is.list(x)) {
          x
        } else {
            list(x)
        }
      })
    }
    res <- as.data.frame(lapply(seq_along(object),
                                function(x){
                                  ap(object[[x]], limitsList[[x]])
                                }))
    names(res) <- names(object)
    return(res)
  } else {
    apercu(object, limitsList)
  }
}

#' limitsLister
#' @param object The object for which the dimensions should be checked
#' @return A list of the dimensions if none is given
limitsLister <- function(object){
  lapply(seq_along(dim(object)), function(x){
    if (dim(object)[x] > 5) {
     1:5
    } else {
     1:dim(object)[x]
    }
  })
}
