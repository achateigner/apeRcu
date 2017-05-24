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
#' @param limitsList A list with each element being the selection of each
#' dimension
#' @param pA Boolean, to only print the aperçu (FALSE) or to also print the
#' dimensions and classes (TRUE)
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
#' # To print also the dimensions and classes
#' ap(v, pA = TRUE)
#' ap(m, pA = TRUE)
#' ap(df, pA = TRUE)
#' ap(li, pA = TRUE)
#' ap(a, pA = TRUE)
#' ap(a2, pA = TRUE)
#' ap(a3, pA = TRUE)

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
#' ap(gasoline, list(1:10, list(1:10,1:10)))
#' @export
ap <- function(object, limitsList = limitsLister(object), pA = FALSE) {
  if (is.list(object) & !is.data.frame(object)) {
    if(is.list(limitsList) & length(limitsList) == 0) {
      limitsList=list(c(1:5))
      if (length(object) < 5){limitsList=list(c(1:length(object)))}
      res <- lapply(object[c(limitsList[[1]])], function(x) ap(x))
      res2 = list()
      res2$apercu <- lapply(res, function(x) x$apercu)
      res2$dimensions <- lapply(res, function(x) x$dimensions)
      res2$classes <- lapply(res, function(x) x$classes)
      if (length(unique(res2$dimensions)) == 1 &
          length(unique(res2$classes)) == 1){
        res3 = list()
        res3$apercu <- res2$apercu
        res3$dimensions <- unlist(unique(res2$dimensions))
        res3$classes <- unlist(unique(res2$classes))
        res2 <- res3
      }
      class(res2) <- c("ap", class(res2))
      return(res2)
    } else {
        resultat <- lapply(object[c(limitsList[[1]])],
                           function(x) aperWrapper(x, limitsList[2:length(limitsList)]))
      if (pA == FALSE){
        resultat
      } else if (pA == TRUE){
        print(resultat, printAll = TRUE)
      }
    }
  } else if(is.data.frame(object)) {
    if (any(sapply(object, function(x) inherits(x, "AsIs")))) {
      if(is.list(limitsList) & length(limitsList) == 0) {
        limitsList = lapply(object, function(x) lapply(seq_along(dim(x)),
                                                       function(y) 1:5))
      } else if(is.list(limitsList) & length(limitsList) > 0){
        lm2 <- limitsLister(object)
        if (isTRUE(all.equal(limitsList, lm2))) {
          limitsList <- lapply(object, function(x) {
            a <- limitsLister(x)
            if(length(a) == 0) a <- list(1:5) else a
          })
        }
        limitsList <- lapply(limitsList, function(x){
          if (is.list(x)) {
            x
          } else {
            list(x)
          }
        })
      }
      res <- lapply(seq_along(object),
                    function(x){
                      ap(object[[x]], limitsList[[x]])
                    })
      names(res) <- names(object)
      res2 = list()
      res2$apercu <- as.data.frame(lapply(res, function(x) x$apercu))
      res2$dimensions <- lapply(res, function(x) x$dimensions)
      res2$classes <- lapply(res, function(x) x$classes)
      if (length(unique(res2$dimensions)) == 1 &
          length(unique(res2$classes)) == 1){
        res3 = list()
        res3$apercu <- res2$apercu
        res3$dimensions <- unlist(unique(res2$dimensions))
        res3$classes <- unlist(unique(res2$classes))
        res2 <- res3
      }
      class(res2) <- c("ap", class(res2))
      return(res2)
    } else {
      limitsList <- lapply(seq_along(dim(object)),
                           function(x) {
                             if(dim(object)[x] < 5){
                               1:dim(object)[x]
                             } else {
                               limitsList[[x]]
                             }
                           })
      resultat <- aperWrapper(object, limitsList)
      if (pA == FALSE){
        resultat
      } else if (pA == TRUE){
        print(resultat, printAll = TRUE)
      }
    }
  } else {
    resultat <- aperWrapper(object, limitsList)
    if (pA == FALSE){
      resultat
    } else if (pA == TRUE){
      print(resultat, printAll = TRUE)
    }
  }
}

#' limitsLister
#' @param objet The object for which the dimensions should be checked
#' @return A list of the dimensions if none is given
#' @keywords internal
limitsLister <- function(objet){
  lapply(seq_along(dim(objet)), function(x){
    if (dim(objet)[x] > 5) {
      1:5
    } else {
      1:dim(objet)[x]
    }
  })
}

#' apercu
#' @param o the object for which I need to have the apercu
#' @param l the limits
#' @param c the classes
#' @return The apercu
#' @keywords internal
apercu <- function(o, l=lapply(seq_along(dim(o)), function(x) 1:5)){
  if(is.list(l) & length(l) == 0){
    if(is.matrix(o)){
      l=list(c(1:5), c(1:5))
    } else if(is.null(dim(o))){
      if (length(o) < 5){
        l=list(c(1:length(o)))
      } else{
        l=list(c(1:5))
      }
    } else{
      l=list(c(1:5))
    }
  }
  l <- fittingLimits(o, l)
  do.call(`[`, c(list(o), l))
}

#' aperWrapper
#' @param ob the object sent to apercu
#' @param li the limits
#' @return the apercu object, of class ap
#' @keywords internal
aperWrapper <- function(ob, li=lapply(seq_along(dim(ob)), function(x) 1:5)){
  resultat <- apercu(ob, li)
  dims <- dim(ob)
  cl <- classDeterminer(ob)
  result <- list(apercu=resultat, dimensions=dims, classes=cl)
  class(result) <- c("ap", class(result))
  return(result)
}

#' classDeterminer
#' @param obj the object for which the classes have to be determined
#' @return the classes of the object and its elements if list
#' @keywords internal
classDeterminer <- function(obj){
  if (is.list(obj)){
    cla <- lapply(obj, class)
    if (all(cla[[1]] == cla)){
      cla <- list(object = class(obj), elements = cla[[1]])
    } else {
      cla <- list(object = class(obj), elements = unlist(cla))
    }
  } else{
    cla <- class(obj)
  }
  return(cla)
}


#' print.ap
#' @param x the ap class object that has to be printed
#' @param printAll should all the elements be printed ?
#' @return prints the object, only the apercu or with dimensions and classes
#' @export
#' @keywords internal
print.ap <- function(x, printAll = FALSE, ...){
  if (!inherits(x, "ap"))
    stop("Argument 'x' must be an object of class \"ap\".")
  if (!isTRUE(printAll)) {
    print(x$apercu)
  } else {
    class(x) <- "list"
    print(x)
  }
}

#' fittingLimits
#' @param ob object with dimensions
#' @param lim limits that we have to check
#' @return the corrected limits
#' @keywords internal
fittingLimits <- function(ob, lim){
  if (is.null(dim(ob))){
    if(length(ob) <= 5){
      exportList <- list(1:length(ob))
    } else{
      exportList <- lim
    }
  } else {
    exportList <- list()
    for (x in seq_along(dim(ob))) {
      if(dim(ob)[x] <= 5){
        exportList[[x]] <- 1:dim(ob)[x]
      } else {
        if (length(lim) >= x){
          exportList[[x]] <- lim[[x]]
        }
      }
    }
  }
  return(exportList)
}
