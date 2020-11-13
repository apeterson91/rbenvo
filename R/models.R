#' Subject Design Matrix
#'
#' @export
#' @param formula similar to \code{\link[stats]{lm}}.
#' @param x benvo object
#' @param ... optional arguments 
#' @keywords internal
#'
subject_design <- function(x,formula,...) UseMethod("subject_design")


#' Extract Subject Design Matrix
#'
#' @export
#' @describeIn subject_design  method
#' @importFrom stats is.empty.model model.response model.matrix
#'
subject_design.benvo <- function(x,formula,...){

	mf <- match.call(expand.dots=FALSE)
	m <- match(c("formula","data"),names(mf),0L)
	mf <- mf[c(1L,m)]
	id <- get_id(x)
	mf$drop.unused.levels <- TRUE
	mf[[1L]] <- quote(stats::model.frame)
	mf$formula <- formula
	mf$data = if(subject_has_sf(x)) sf::st_drop_geometry(x$subject_data) else x$subject_data
	mf$data <- mf$data %>% dplyr::arrange_at(id)
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
#' @export
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

	id <- get_id(x)
  design <- function(formula){
	  mf <- match.call(expand.dots = TRUE)
	  mf[[1]] <- quote(lme4::glFormula)
	  mf$control <- glmerControl(check.nlev.gtreq.5 = "ignore",
	                             check.nlev.gtr.1 = "stop",
	                             check.nobs.vs.rankZ = "ignore",
	                             check.nobs.vs.nlev = "ignore",
	                             check.nobs.vs.nRE = "ignore" )

	  mf$data = if(subject_has_sf(x)) sf::st_drop_geometry(x$subject_data) else x$subject_data
	  mf$data <- mf$data %>% dplyr::arrange_at(id)
	  mf$formula <- formula
	  mf <- eval(mf,parent.frame())
	  y <- mf$fr[,as.character(mf$formula[2L])]
	  X <-  mf$X
	  out <- list(y=y,X=X,glmod=mf)
	}

	return(design(formula))

}
