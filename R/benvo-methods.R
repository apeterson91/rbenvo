#' Benvo Methods
#'
#'
#' @name benvo-methods
#'
#' @param x a benvo object
#'
#'
NULL 


#' @rdname benvo-methods
#' @export
bef_names <- function(x) UseMethod("bef_names")

#' @describeIn bef_names BEF name attribute
#' @export
bef_names.benvo <- function(x) return(attr(x,'bef_names'))

#' @export
#' @rdname benvo-methods
components <- function(x) UseMethod("components")

#' @describeIn components BEF components attribute
#' @export
components.benvo <- function(x){
	return(attr(x,'components'))
}

#' @rdname benvo-methods
#' @param term bef_name string
#' @export
component_lookup <- function(x,term) UseMethod("component_lookup")

#' @describeIn component_lookup lookups component based on bef name
#' @export
component_lookup.benvo <- function(x,term){

	ix <- which(bef_names(x) == term)

	components <- attr(x,'components')

	out <- switch(components,
				  "Distance" = "Distance",
				  "Time" = "Time",
				  "Distance-Time" = c("Distance","Time"))

	return(out)
}

#' @rdname benvo-methods
num_BEF <- function(x) UseMethod("num_BEF")

#' @export
#' @describeIn num_BEF number of BEF data frames
num_BEF.benvo <- function(x){
	return(length(x$bef_data))
}

#' Subject Design Matrix
#'
#' @param formula similar to \code{\link[stats]{lm}}.
#' @param x benvo object
#' @param ... other arguments passed to the model frame
#' @keywords internal
#'
subject_design <- function(x,formula,...) UseMethod("subject_design")


#' Extract Subject Design Matrix
#'
#' @export
#' @describeIn subject_design  method
#' @importFrom stats is.empty.model model.response model.matrix
#'
subject_design.benvo <- function(x,formula){

	mf <- match.call(expand.dots=FALSE)
	mf[[1L]] <- as.name("model.frame")
	mf$formula <- formula
	mf$data = x$subject_data
	mf$drop.unused.levels <- T
	mf <- eval(mf,parent.frame()) ## evaluate in this environment with current Benvo object
	mt <- attr(mf,"terms")
	if(is.empty.model(mt))
	  stop("No intercept or predictors specified.",.call=F)

	y <- model.response(mf,"numeric")
	X <-  model.matrix(mt,mf)
	out <- list(y=y,X=X,model_frame=mf)
	return(out)

}

#' Longitudinal design dataframe
#'
#' For use with \code{\link[lme4]{glmer}} type formulas/models
#' @param formula similar to \code{\link[lme4]{glmer}}.
#' @param x benvo object
#' @param ... other arguments passed to the model frame
#'
longitudinal_design <- function(x,formula,...) UseMethod("longitudinal_design")


#'
#' @export
#' @describeIn longitudinal_design  method
#' @importFrom lme4 glmerControl
#'
longitudinal_design <- function(x,formula,...){

  design <- function(formula){
	  mf <- match.call(expand.dots = TRUE)
	  mf[[1]] <- quote(lme4::glFormula)
	  mf$control <- glmerControl(check.nlev.gtreq.5 = "ignore",
	                             check.nlev.gtr.1 = "stop",
	                             check.nobs.vs.rankZ = "ignore",
	                             check.nobs.vs.nlev = "ignore",
	                             check.nobs.vs.nRE = "ignore" )

	  mf$data = x$subject_data
	  mf$formula <- formula
	  mf <- eval(mf,parent.frame())
	  y <- mf$fr[,as.character(mf$formula[2L])]
	  X <-  mf$X
	  out <- list(y=y,X=X,glmod=mf)
	}

	return(design(formula))

}


#' Join BEF and subject data within a benvo
#'
#' @export
#' @details Joins the subject dataframe within a benvo to the supplied BEF dataframe keeping the selected component
#' @param x benvo object
#' @param term string of bef name to join on in bef_data
#' @param component one of c("Distance","Time","Distance-Time") indicating which column(s) of the bef dataset should be returned
#' @param tibble boolean value of whether or not to return a data.frame or tibble
#' @param NA_to_zero replaces NA values with zeros - potentially useful when constructing design matrices
#'
joinvo <- function(x,term,component = "Distance",tibble = F,NA_to_zero = F) UseMethod("joinvo")

#'
#' @export
#' @importFrom stats quantile median
#' @describeIn joinvo method
#'
joinvo.benvo <- function(x,term,component = "Distance",tibble = F,NA_to_zero = F){


	stopifnot(component %in% c("Distance","Time","Distance-Time"))
	Distance <- Time <- NULL

	ix <- term_check(x,term)
	id <- get_id(x)
	component_check(x,term,component)


	jdf <- dplyr::right_join(x$bef_data[[ix]],x$subject_data[,id, drop=F], by=id)

	if(NA_to_zero){
		col <- switch(component,
			   "Distance" = "Distance",
			   "Time" = "Time",
			   "Distance-Time" = c("Distance","Time"))
		jdf <- jdf %>% dplyr::mutate_at(col,function(x) tidyr::replace_na(x,0))
	}

	if(tibble)
		return(tibble::as_tibble(jdf))
	else
		return(jdf)
}


#' Is a longitudinal benvo
#'
#' @keywords internal
#' @export
#'
is.longitudinal <- function(object) UseMethod("is.longitudinal")

#'
#' @describeIn is.longitudinal returns true if longitudinal
#' @export
#'
is.longitudinal.benvo <- function(object) return(attr(object,"longitudinal"))

#' @rdname benvo-methods
#' @export
is.benvo <- function(x) inherits(x,"benvo")



## Internal ----------------------------

get_id <- function(x)  return(attr(x,"id"))

create_unique_ID_mat <- function(id_one,id_two = NULL){
	tmp <- paste0(id_one,"_",id_two)
	lvls <- unique(tmp)
	new_id <- factor(tmp,levels=lvls)
	Matrix::fac2sparse(new_id)
}

.printfr <- function(x, digits, ...) {
  print(format(round(x, digits), nsmall = digits), quote = FALSE, ...)
}

term_check <- function(x,term){

	stopifnot(length(term)==1)
	nms <- bef_names(x)
	ix <- which(nms == term )
	if(!length(ix))
		stop("Term is not a member of this benvo")
	return(ix)
}

sf_check <-function(x,term){
	stopifnot(attr(x,"bef_sf")[term])
}

component_check <- function(x, term, component){

	ix <- term_check(x,term)
	comp <- attr(x,"components")[ix]
	if(comp == "Distance-Time"){
		if(!(component %in% c("Distance","Time")))
			stop(paste0(term," is associated with components Distance and Time, not ",component))
	}
	else if(!(component %in% comp))
		stop(paste0(term," is associated with component, ", comp, " not ",component))


}

