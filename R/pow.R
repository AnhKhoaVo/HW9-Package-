#' pow
#' 
#' This is a function to do Box-cox and Link
#' 
#' @param x 
#' @param a
#' @export
pow <- function(x, a=2) x^a

#' @export
reciprocal <- function(x) 1/x

#Link function
#' @export
link <- function(x){
	if (any(is.na(x))) stop("Error! One of them is NA")
	paste0(x,x)
}

#Box-cox Transformation
#' @export
boxcox <- function (x,lambda){
	if (lambda == 0) {y = log(x)}
	if (lambda != 0) {y = (x^(lambda-1)/lambda)}
	return(y)
}

#Inverse Box-cox Transformation
#' @export
reverse_boxcox <- function(x, lambda){
		if (lambda == 0) {y = exp(x)}
	  if (lambda !=0) {y=((lambda*x + 1)^(1/lambda))}
	  return(y)
}
