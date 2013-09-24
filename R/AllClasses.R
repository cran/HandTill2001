#'Class \code{"cap"} in Package \pkg{HandTill2001}
#'
#'A virtual class for \code{"bincap"} and \code{"multcap"}.
#'
#'
#'@name cap-class
#' @export
#'@docType class
#'@section Objects from the Class: A virtual Class: No objects may be created
#'from it.
#'@author Andreas Dominik Cullmann
#'@seealso \code{"\link[=bincap-class]{class?bincap}"},
#'\code{"\link[=multcap-class]{class?multcap}"}
#'@keywords classes
setClass(Class = "cap"
	 , representation = representation(
					   response = "factor"
					   , "VIRTUAL"
					   )
	 , prototype = prototype(response = factor())
	 , validity = function(object){
	 }
	 )

#'Class \code{"bincap"} in Package \pkg{HandTill2001}
#'
#'S4 class for a two class response and corresponding (predicted)
#'probabilities.
#'
#'
#'@name bincap-class
#' @export
#'@docType class
#'@note No defaults are set. Especially, you have to explicitly initialize
#'\code{true}, there is no trying to guess it from the \code{levels} of
#'\code{response}.
#'@section Objects from the Class: Objects can be created by calls of the form
#'\code{new("bincap", ...)}. They are used to store a two class response (one
#'of the two \code{levels} of which is supposed to be \code{true}), the
#'information which of the two \code{levels} of the two class response is
#'thought of as 'true'/'positive'/'present' (the other one would then be
#'thought of as 'false'/'negative'/'absence') and the predicted probabilities
#'that \code{response} is \code{true}.
#'@author Andreas Dominik Cullmann
#'@seealso \code{"\link[=cap-class]{class?cap}"} ,
#'\code{"\link[=multcap-class]{class?multcap}"} ,
#'\code{"\link[=bincap]{?bincap}"}
#'@keywords classes
#'@examples
#'
#'showClass("bincap")
#'
setClass(Class = "bincap"
	 , contains = "cap"
	 , representation = representation(
					   predicted = "numeric"
					   , true = "character"
					   )
	 , prototype = prototype(predicted = numeric(), true = character() )
	 , validity = function(object){
	   if(length(object@response) != length(object@predicted))
	     return("response and predicted must have the same number of observations.")
	   if(any(object@predicted < 0, na.rm = TRUE) || any(object@predicted > 1, na.rm = TRUE))
	     return("probabilities should be in [0,1].")
	   if(length(object@true) > 1)
	     return("give a single character for the 'true'/'presence' class.")
	   if(length(levels(object@response)) > 2)
	     return("response has to be a two class factor.")
	 }
	 )
#'Class \code{"multcap"} in Package \pkg{HandTill2001}
#'
#'S4 class for a multiple class response and corresponding (predicted)
#'probabilities.
#'
#'
#'@name multcap-class
#' @export
#'@docType class
#'@section Objects from the Class: Objects can be created by calls of the form
#'\code{new("multcap", ...)}. They are used to store a multiple class response
#'and the predicted probabilities for each of the \code{levels(response)}.
#'@author Andreas Dominik Cullmann
#'@seealso \code{"\link[=cap-class]{class?cap}"} ,
#'\code{"\link[=bincap-class]{class?bincap}"} ,
#'\code{"\link[=multcap]{?multcap}"}
#'@keywords classes
#'@examples
#'
#'showClass("multcap")
#'
setClass(Class = "multcap"
	 , contains = "cap"
	 , representation = representation(
					   predicted = "matrix"
					   )
	 , prototype = prototype(predicted = matrix(nrow = 0, ncol = 0))
	 , validity = function(object){
	   p <- object@predicted 
	   r <- object@response
	   if(! isTRUE(all.equal(as.character(unique(r)), levels(r))))
	     warning(paste("found extraneous factor level(s) '"
			   , paste(setdiff(levels(r),as.character(unique(r))), collapse=', ')
			   ,"' of response.\n"
			   , "You may want to work around this by 'response <- factor(response)'."
			   , sep = ''))

	   if(! all(levels(r) %in% dimnames(p)[[2]]))
	     warning(paste("found factor level(s) '"
			   , paste(setdiff(levels(r), dimnames(p)[[2]]), collapse=', ')
			   ,"' of response unmatched by predicted."
			   , sep = ''))
	   if(! all(unique(r) %in% dimnames(p)[[2]], na.rm = TRUE))
	     return(paste("found value(s) '"
			  , setdiff(as.character(unique(r)), dimnames(p)[[2]])
			  , "' of response unmatched by predicted.\n"
			  , "You may want to add column(s) filled with NA to predicted."
			  , sep = '')
	   )
	   if(! all(dimnames(p)[[2]] %in% levels(r)))
	     return(paste("found column(s) '"
			  , paste(setdiff(dimnames(p)[[2]], levels(r)), collapse=', ')
			  ,"' of predicted unmatched by levels(response)."
			  , sep = '')
	   )
	   if(length(r) != nrow(p))
	     return("response and predicted must have the same number of observations.")
	   if(any(p < 0, na.rm = TRUE) || any(p > 1, na.rm = TRUE))
	     return("probabilities should be in [0,1].")
	   if(! isTRUE(all.equal(rep(1,nrow(p)) ,as.numeric(rowSums(p, na.rm = TRUE)))))
	     return("row sums of predicted must be 1.")
	 }
	 )


