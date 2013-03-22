\docType{class}
\name{bincap-class}
\alias{bincap-class}
\title{Class \code{"bincap"} in Package \pkg{HandTill2001}}
\description{
  S4 class for a two class response and corresponding
  (predicted) probabilities.
}
\note{
  No defaults are set. Especially, you have to explicitly
  initialize \code{true}, there is no trying to guess it
  from the \code{levels} of \code{response}.
}
\section{Objects from the Class}{
  Objects can be created by calls of the form
  \code{new("bincap", ...)}. They are used to store a two
  class response (one of the two \code{levels} of which is
  supposed to be \code{true}), the information which of the
  two \code{levels} of the two class response is thought of
  as 'true'/'positive'/'present' (the other one would then
  be thought of as 'false'/'negative'/'absence') and the
  predicted probabilities that \code{response} is
  \code{true}.
}
\examples{
showClass("bincap")
}
\author{
  Andreas Dominik Cullmann
}
\seealso{
  \code{"\link[=cap-class]{class?cap}"} ,
  \code{"\link[=multcap-class]{class?multcap}"} ,
  \code{"\link[=bincap]{?bincap}"}
}
\keyword{classes}

