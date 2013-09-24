\name{bincap}
\alias{bincap}
\title{a ui-constructor for Class \code{"bincap"} in Package \pkg{HandTill2001}}
\usage{
  bincap(response, predicted, true = "1")
}
\arguments{
  \item{response}{Object of class \code{"factor"}.}

  \item{predicted}{Object of class \code{"numeric"}.}

  \item{true}{Object of class \code{"character"}.}
}
\value{
  An object of class \code{"bincap"}
}
\description{
  \code{bincap(\dots{})} is an alias to \code{new("bincap",
  \dots{})}.
}
\details{
  There is no casting or conversion of data.
  \code{bincap(\dots{})} is just an alias to
  \code{new("bincap",\dots{})}.
}
\examples{
library(HandTill2001)
data(ht01.twoclass)
str(ht01.twoclass$observed)
message("note that ht01.twoclass$observed is not a factor; we have to convert it.")
bincap(
   response = as.factor(ht01.twoclass$observed)
   , predicted = ht01.twoclass$predicted
   , true = c("1")
   )
}
\author{
  Andreas Dominik Cullmann
}
\seealso{
  \code{"\link[=bincap-class]{class?bincap}"}
}
\keyword{ui-constructor}

