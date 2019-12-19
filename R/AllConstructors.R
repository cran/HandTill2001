#' A Constructor for Objects of Class \code{bincap}
#'
#' \code{bincap(\dots)} is an alias to \code{new("bincap", \dots)}.
#'
#' There is no casting or conversion of data. \code{bincap(\dots)} is just an
#' alias to \code{new("bincap", \dots)}.
#'
#' @param response Object of class \code{factor}.
#' @param predicted Object of class \code{numeric}.
#' @param true Object of class \code{character}.
#' @return An object of class \code{bincap}.
#' @seealso \code{\link[=bincap-class]{class?HandTill2001::bincap}}
#' @keywords ui-constructor
#' @examples
#'
#' library(HandTill2001)
#' data(ht01.twoclass)
#' str(ht01.twoclass$observed)
#' message("note that ht01.twoclass$observed is not a factor; we have to convert it.")
#' bincap(
#'   response = as.factor(ht01.twoclass$observed),
#'   predicted = ht01.twoclass$predicted,
#'   true = c("1")
#' )
#' @export
bincap <- function(response, predicted, true = "1") {
  new <- methods::new(
    Class = "bincap", response = response,
    predicted = predicted, true = true
  )
  return(new)
}




#' A Constructor for Objects of Class \code{multcap}
#'
#' \code{multcap(\dots)} is an alias to \code{new("multcap", \dots)}.
#'
#' There is no casting or conversion of data. \code{multcap(\dots)} is just
#' an alias to \code{new("multcap", \dots)}.
#'
#' @param response Object of class \code{factor}.
#' @param predicted Object of class \code{matrix}.
#' @return An object of class \code{multcap}.
#' @seealso \code{\link[=multcap-class]{class?HandTill2001::multcap}}
#' @keywords ui-constructor
#' @examples
#'
#' library(HandTill2001)
#' data(ht01.multipleclass)
#' str(ht01.multipleclass$observed)
#' message("note that ht01.multipleclass$observed is a factor; we do not have to convert it.")
#' multcap(
#'   response = ht01.multipleclass$observed,
#'   predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
#' )
#' @export
multcap <- function(response, predicted) {
  new <- methods::new(
    Class = "multcap", response = response,
    predicted = predicted
  )
  return(new)
}
