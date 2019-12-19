#' Methods for Function \code{auc} in Package \pkg{HandTill2001}
#'
#' Calculate area under curve of the receiver operating characteristic for
#' two or more prediction classes.
#'
#' Depending on whether \code{object} is of class \code{bincap} or of class
#' \code{multcap}, a binary class or multiple class AUC is calculated.
#'
#' @param object An object of \code{class} \emph{bincap} or \emph{multcap}.
#' @name auc-methods
#' @aliases auc-methods auc auc,bincap-method auc,multcap-method
#' @docType methods
#' @return An object of class \code{"numeric"}.
#' @section Methods: \describe{
#'
#' \item{signature(object = "bincap")}{ calculates the AUC statistic
#' for a binary class response following \cite{Hand and Till (2001)}, Eq. (3). }
#'
#' \item{signature(object = "multcap")}{ calculates the AUC statistic
#' for a multiple class response following \cite{Hand and Till (2001)}, Eq.
#' (7). } }
#' @seealso \code{\link[=bincap-class]{class?bincap}},
#' \code{\link[=multcap-class]{class?multcap}}
#' @references \cite{David J. Hand and Robert J. Till (2001). A Simple
#' Generalisation of the Area Under the ROC Curve for Multiple Class
#' Classification Problems. \emph{Machine Learning} \bold{45}(2), p. 171--186.
#' DOI:
#' \href{http://dx.doi.org/10.1023/A:1010920819831}{10.1023/A:1010920819831}}.
#' @keywords methods auc
#' @export
#' @examples
#' data(ht01.twoclass, package = "HandTill2001")
#' message(" == AUC for a binary class response")
#' message(" == == HandTill2001 result:")
#' HandTill2001::auc(HandTill2001::bincap(
#'   response = as.factor(ht01.twoclass[["observed"]]),
#'   predicted = ht01.twoclass[["predicted"]],
#'   true = "1"
#' ))
#' \dontrun{
#' message(" == == ROCR result:")
#' ROCR::performance(ROCR::prediction(
#'   labels = ht01.twoclass[["observed"]],
#'   predictions = ht01.twoclass[["predicted"]]
#' ),
#' measure = "auc"
#' )@y.values
#' }
setMethod(
  f = "auc",
  signature(object = "bincap"),
  function(object) {
    n0 <- sum(as.numeric(object@response == object@true))
    n1 <- sum(as.numeric(object@response != object@true))
    s0 <- sum(rank(object@predicted,
      ties.method = "average",
      na.last = TRUE
    )
    * (object@response
      == object@true))
    return(
      (s0 - n0 * (n0 + 1) / 2) / (n0 * n1)
    )
  }
)

#' @rdname auc-methods
#' @export
#' @examples
#' data(ht01.multipleclass, package = "HandTill2001")
#' message(" == AUC for a multiple class response")
#' predicted <- as.matrix(ht01.multipleclass[, levels(ht01.multipleclass[["observed"]])])
#' HandTill2001::auc(HandTill2001::multcap(
#'   response = ht01.multipleclass[["observed"]],
#'   predicted = predicted
#' ))
setMethod(
  f = "auc",
  signature(object = "multcap"),
  function(object) {
    return(mean(
      utils::combn(levels(object@response), 2,
        function(levels,
                 response,
                 predicted) {
          df <- as.data.frame(predicted) # factor and matrix -> need data frame
          df[["obs"]] <- response
          dfs <- subset(df, get("obs") %in% levels)
          t <- levels[1]
          aij <- auc(methods::new("bincap",
            response = factor(dfs[, "obs"]) # to drop non-ocurring levels
            , predicted = dfs[, t],
            true = t
          ))
          t <- levels[2]
          aji <- auc(methods::new("bincap",
            response = factor(dfs[, "obs"]) #  to drop non-ocurring levels
            , predicted = dfs[, t],
            true = t
          ))
          Aij <- mean(c(aij, aji))
          return(Aij)
        },
        response = object@response,
        predicted = object@predicted
      ),
      na.rm = TRUE
    ))
  }
)
