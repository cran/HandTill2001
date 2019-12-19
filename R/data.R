#' Example Data for Multiple Classes
#'
#' Multiple class data and probability predictions thereof.
#'
#' Multiple class data ('observed': \code{MASS::fgl$type}) and probability
#' predictions (\code{predict(fgl.rp4)}, cf. Venables and Ripley (2002), p. 264
#' and \sQuote{Source}) from \code{rpart::rpart}.
#'
#' @name ht01.multipleclass
#' @docType data
#' @format A data frame with 214 observations on the following 7 variables.
#' \describe{
#'  \item{observed}{a factor with levels
#'                    \code{Con} \code{Head} \code{Tabl} \code{Veh}
#'                    \code{WinF} \code{WinNF}}
#'  \item{WinF}{a numeric vector}
#'  \item{WinNF}{a numeric vector}
#'  \item{Veh}{a numeric vector}
#'  \item{Con}{a numeric vector}
#'  \item{Tabl}{a numeric vector}
#'  \item{Head}{a numeric vector}
#' }
#' @references Venables, W. N and Ripley, B. D. (2002), \emph{Modern Applied
#' Statistics with S} (4th edition). Springer, ISBN 0-387-95457-0
#' @source \preformatted{ ## From: Forensic glass example Venables and Ripley
#' (2002) pp. 261--265 library(MASS);library(rpart);data(fgl);set.seed(123)
#' fgl.rp4 <- rpart(type ~ ., data = fgl, cp = 0.03 , parms = list(split =
#' "information")) ht01.multipleclass <- data.frame(observed = fgl$type,
#' predict(fgl.rp4)) write.table(ht01.multipleclass, file =
#' "ht01.multipleclass.txt") }
#' @keywords datasets
#' @examples
#'
#' library(HandTill2001)
#' data(ht01.multipleclass)
#' str(ht01.multipleclass)
NULL

#' Example Data for Binary Classes
#'
#' Binary class data and probability predictions thereof.
#'
#' Binary class data ('observed': \code{MASS::birthwt$low}) and probability
#' predictions
#' (\code{predict(birthwt.step2, type = "response")}, cf. Venables and Ripley
#' (2002), pp. 195f and \sQuote{Source}) from \code{stats::glm}.
#'
#' @name ht01.twoclass
#' @docType data
#' @format A data frame with 189 observations on the following 2 variables.
#' \describe{ \item{observed}{a numeric vector}
#' \item{predicted}{a numeric vector} }
#' @references Venables, W. N and Ripley, B. D. (2002), \emph{Modern Applied
#' Statistics with S} (4th edition). Springer, ISBN 0-387-95457-0
#' @source \preformatted{ ## From: A binary class data example Venables and
#' Ripley pp. 194--199 library(MASS); data("birthwt"); attach(birthwt) race <-
#' (factor(race, labels = c("white", "black", "other"))) ptd <- factor(ptl > 0)
#' ftv <- factor(ftv) levels(ftv)[-(1:2)] <- "2+" bwt <- data.frame(low =
#' factor(low), age, lwt, race, smoke = (smoke > 0) , ptd, ht = (ht > 0), ui =
#' (ui > 0), ftv) detach(birthwt) birthwt.glm <- glm(low ~ .,
#' family=binomial(link=logit), data=bwt) birthwt.step2 <- stepAIC(birthwt.glm,
#' ~ .^2 + I(scale(age)^2) + I(scale(lwt)^2), trace = F ) ht01.twoclass <-
#' data.frame(observed = bwt$low , predicted = predict(birthwt.step2 , type =
#' "response")) write.table(ht01.twoclass, file = "ht01.twoclass.txt") }
#' @keywords datasets
#' @examples
#'
#' library(HandTill2001)
#' data(ht01.twoclass)
#' str(ht01.twoclass)
NULL
