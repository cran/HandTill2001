
#'Multiple Class Area under ROC Curve
#'
#'A very lean package implementing merely \eqn{M} given by \cite{Hand and Till
#'(2001)}, Eq. (7).
#'
#'\eqn{M} given by \cite{Hand and Till (2001)} defines a multiple class version
#'of the area under curve of the receiver operating characteristic.
#'
#'@name HandTill2001-package
#'@aliases HandTill2001-package HandTill2001
#'@docType package
#'@author Andreas Dominik Cullmann [aut, cre], Edgar Kublin [ctb]
#'
#'Maintainer: Andreas Dominik Cullmann <r-package_handtill2001@@arcor.de>
#'@seealso \code{help(package="HandTill2001")}, especially
#'\code{"\link[=auc-methods]{methods?auc}"}; various packages that calculate
#'two class AUC (\code{"\link[ROCR:performance]{ROCR}"}) or multiple class AUC
#'(\code{"\link[pROC:pROC-package]{pROC}"},
#'\code{"\link[caTools:caTools-package]{caTools}"}).
#'@references \cite{David J. Hand and Robert J. Till (2001). A Simple
#'Generalisation of the Area Under the ROC Curve for Multiple Class
#'Classification Problems. \emph{Machine Learning} \bold{45}(2), p. 171--186.
#'DOI:
#'\href{http://dx.doi.org/10.1023/A:1010920819831}{10.1023/A:1010920819831}}.
#'@keywords AUC, ROC
#'@examples
#'
#'library(HandTill2001)
#'data(ht01.multipleclass)
#'auc( 
#'    multcap(
#'        response = ht01.multipleclass$observed
#'        , predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
#'        )
#'    )
#'
NULL





#'ht01.multipleclass data in Package \pkg{HandTill2001}
#'
#'multiple class data and probability predictions thereof.
#'
#'multiple class data ('observed': \code{MASS::fgl$type}) and probability
#'predictions (\code{predict(fgl.rp4)}, cf. Venables and Ripley (2002), p. 264
#'and \sQuote{Source}) from \code{rpart::rpart}.
#'
#'@name ht01.multipleclass
#'@docType data
#'@format A data frame with 214 observations on the following 7 variables.
#' \describe{
#'   \item{\code{observed}}{a factor with levels \code{Con} \code{Head} \code{Tabl} \code{Veh} \code{WinF} \code{WinNF}}
#'   \item{\code{WinF}}{a numeric vector}
#'   \item{\code{WinNF}}{a numeric vector}
#'   \item{\code{Veh}}{a numeric vector}
#'   \item{\code{Con}}{a numeric vector}
#'   \item{\code{Tabl}}{a numeric vector}
#'   \item{\code{Head}}{a numeric vector}
#' }
#'@references Venables, W. N and Ripley, B. D. (2002), \emph{Modern Applied
#'Statistics with S} (4th edition). Springer, ISBN 0-387-95457-0
#'@source \preformatted{
#'
#'## From: Forensic glass example 
#'
#'## Venables and Ripley(2002) pp. 261--265 
#'
#'library(MASS);library(rpart);data(fgl);set.seed(123)
#'
#'fgl.rp4 <- rpart(type ~ ., data = fgl, cp = 0.03
#'                         , parms = list(split = "information"))
#'
#'ht01.multipleclass <- data.frame(observed = fgl$type,predict(fgl.rp4))
#'
#'write.table(ht01.multipleclass, file = "ht01.multipleclass.txt")
#'
#' }
#'@keywords datasets
#'@examples
#'
#'library(HandTill2001)
#'data(ht01.multipleclass)
#'str(ht01.multipleclass)
#'
NULL





#'ht01.twoclass data in Package \pkg{HandTill2001}
#'
#'two class data and probability predictions thereof.
#'
#'two class data ('observed': \code{MASS::birthwt$low}) and probability
#'predictions
#'
#'(\code{predict(birthwt.step2, type = "response")}, cf. Venables and Ripley
#'(2002), pp. 195f and \sQuote{Source}) from \code{stats::glm}.
#'
#'@name ht01.twoclass
#'@docType data
#'@format A data frame with 189 observations on the following 2 variables.
#' \describe{
#'   \item{\code{observed}}{a numeric vector}
#'   \item{\code{predicted}}{a numeric vector}
#' }
#'@references Venables, W. N and Ripley, B. D. (2002), \emph{Modern Applied
#'Statistics with S} (4th edition). Springer, ISBN 0-387-95457-0
#'@source \preformatted{
#'
#'## From: A two class data example 
#'
#'## Venables and Ripley pp. 194--199
#'
#'library(MASS); data("birthwt"); attach(birthwt)
#'
#'race <-(factor(race, labels = c("white", "black", "other")))
#'
#'ptd <- factor(ptl > 0)
#'
#'ftv <- factor(ftv)
#'
#'levels(ftv)[-(1:2)] <- "2+"
#'
#'bwt <- data.frame(low = factor(low), age, lwt, race, smoke = (smoke > 0)
#'
#'                  , ptd, ht = (ht > 0), ui = (ui > 0), ftv)
#'
#'detach(birthwt)
#'
#'birthwt.glm <- glm(low ~ ., family = binomial(link=logit), data = bwt)
#'
#'birthwt.step2 <- stepAIC(birthwt.glm
#'
#'                         , ~ .^2 + I(scale(age)^2) + I(scale(lwt)^2)
#'
#'                         , trace = F )
#'
#'ht01.twoclass <- data.frame(observed = bwt$low
#'
#'                            , predicted = predict(birthwt.step2
#'
#'                                , type = "response"))
#'
#'write.table(ht01.twoclass, file = "ht01.twoclass.txt")
#'
#'}
#'
#'@keywords datasets
#'@examples
#'
#'library(HandTill2001)
#'data(ht01.twoclass)
#'str(ht01.twoclass)
#'
NULL

