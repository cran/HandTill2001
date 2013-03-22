#'a ui-constructor for Class \code{"bincap"} in Package \pkg{HandTill2001}
#'
#'\code{bincap(\dots{})} is an alias to \code{new("bincap", \dots{})}.
#'
#'There is no casting or conversion of data. \code{bincap(\dots{})} is just an
#'alias to \code{new("bincap",\dots{})}.
#'
#'@export
#'@param response Object of class \code{"factor"}.
#'@param predicted Object of class \code{"numeric"}.
#'@param true Object of class \code{"character"}.
#'@return An object of class \code{"bincap"}
#'@author Andreas Dominik Cullmann
#'@seealso \code{"\link[=bincap-class]{class?bincap}"}
#'@keywords ui-constructor
#'@examples
#'
#'library(HandTill2001)
#'data(ht01.twoclass)
#'str(ht01.twoclass$observed)
#'message("note that ht01.twoclass$observed is not a factor; we have to convert it.")
#'bincap(
#'    response = as.factor(ht01.twoclass$observed)
#'    , predicted = ht01.twoclass$predicted
#'    , true = c("1")
#'    )
#'
#'
bincap <-
function (response, predicted, true = "1") 
{
    return(new(Class = "bincap", response = response, predicted = predicted, 
        true = true))
}


#'a ui-constructor for Class \code{"multcap"} in Package \pkg{HandTill2001}
#'
#'\code{multcap(\dots{})} is an alias to \code{new("multcap", \dots{})}.
#'
#'There is no casting or conversion of data. \code{multcap(\dots{})} is just an
#'alias to \code{new("multcap", \dots{})}.
#'
#'@export
#'@param response Object of class \code{"factor"}.
#'@param predicted Object of class \code{"matrix"}.
#'@return An object of class \code{"bincap"}
#'@author Andreas Dominik Cullmann
#'@seealso \code{"\link[=multcap-class]{class?multcap}"}
#'@keywords ui-constructor
#'@examples
#'
#'library(HandTill2001)
#'data(ht01.multipleclass)
#'str(ht01.multipleclass$observed)
#'message("note that ht01.multipleclass$observed is a factor; we do not have to convert it.")
#'multcap(
#'     response = ht01.multipleclass$observed
#'    , predicted = as.matrix(ht01.multipleclass[, levels(ht01.multipleclass$observed)])
#'    )
#'
#'
multcap <-
function (response, predicted) 
{
    return(new(Class = "multcap", response = response, predicted = predicted))
}
