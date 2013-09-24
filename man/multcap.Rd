\name{multcap}
\alias{multcap}
\title{a ui-constructor for Class \code{"multcap"} in Package \pkg{HandTill2001}}
\usage{
  multcap(response, predicted)
}
\arguments{
  \item{response}{Object of class \code{"factor"}.}

  \item{predicted}{Object of class \code{"matrix"}.}
}
\value{
  An object of class \code{"bincap"}
}
\description{
  \code{multcap(\dots{})} is an alias to
  \code{new("multcap", \dots{})}.
}
\details{
  There is no casting or conversion of data.
  \code{multcap(\dots{})} is just an alias to
  \code{new("multcap", \dots{})}.
}
\examples{
library(HandTill2001)
data(ht01.multipleclass)
str(ht01.multipleclass$observed)
message("note that ht01.multipleclass$observed is a factor; we do not have to convert it.")
multcap(
    response = ht01.multipleclass$observed
   , predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
   )
}
\author{
  Andreas Dominik Cullmann
}
\seealso{
  \code{"\link[=multcap-class]{class?multcap}"}
}
\keyword{ui-constructor}

