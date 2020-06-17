


#' benvo Print Method
#'
#' @export
#'
#' @param object benvo object
#' @param pars optional vector of
#' BEF strings with which to subset printed summary
setMethod(f = "show",
	signature = "Benvo",
	definition = function(object){
		print_benvo(object)
	}
	)


print_benvo <- function(object){


	cat("Subject Data:")
	cat("\n")
	cat("---------------------------:")
	cat("\n")
	cat("Observations: ", nrow(object@subject_data))
	cat("\n")
	cat("Columns: ", ncol(object@subject_data))
	cat("\n")
	cat("\n")

	cat("BEF Data:")
	cat("\n")
	cat("---------------------------:")
	cat("\n")
	cat("Number of Features: ", length(object@bef_data))
	cat("\n")
	cat("Features: ")
	cat("\n")
	prettydf <- data.frame(Name = object@bef_names,Measures = object@components)
	print(prettydf)
}


#' Number of Built Environment Features
#'
#' @export
#' @param x a benvo object
#' 
setGeneric("Num_BEF",function(x){
		   standardGeneric("Num_BEF")
})

num_BEF <- function(x)
	return(length(x@bef_names))


#' Extract Subject Design Matrix
#'
#' @export
#'
#' @param x benvo object
#' @param formula similar to \code{\link[stats]{lm}}.
#'
setGeneric("subject_design",function(x) standardGeneric("subject_design"))


subject_design <- function(formula,x){

	mf <- match.call(expand.dots=FALSE)
	mf[[1L]] <- as.name("model.frame")
	mf$formula <- formula
	mf$data = x@subject_data
	mf$drop.unused.levels <- T
	mf <- eval(mf,parent.frame()) ## evaluate in this environment with current Benvo object
	mt <- attr(mf,"terms")
	if(is.empty.model(mt))
	  stop("No intercept or predictors specified.",.call=F)

	y <- model.response(mf,"numeric")
	X <-  model.matrix(mt,mf)
	out <- list(y=y,X=X)
	return(out)
}
