#' Multiple Class Area under ROC Curve
#'
#' A very lean package implementing merely \eqn{M} given by \cite{Hand and Till
#' (2001)}, Eq. (7).
#'
#' \eqn{M} given by \cite{Hand and Till (2001)} defines a multiple class
#' version of the area under curve of the receiver operating characteristic.
#'
#' @name HandTill2001-package
#' @aliases HandTill2001-package HandTill2001
#' @docType package
#' @seealso \code{help(package="HandTill2001")}, especially
#' \code{\link[=auc-methods]{?HandTill2001::auc}}; various packages that calculate
#' binary class AUC (\code{\link[ROCR:performance]{ROCR}}) or multiple class AUC
#' (pROC, \code{\link[caTools:caTools-package]{caTools}}).
#' @references \cite{David J. Hand and Robert J. Till (2001). A Simple
#' Generalisation of the Area Under the ROC Curve for Multiple Class
#' Classification Problems. \emph{Machine Learning} \bold{45}(2), p. 171--186.
#' DOI:
#' \doi{10.1023/A:1010920819831}}.
#' @keywords AUC ROC
#' @examples
#'
#' library(HandTill2001)
#' data(ht01.multipleclass)
#' auc(
#'   multcap(
#'     response = ht01.multipleclass$observed,
#'     predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
#'   )
#' )
NULL
