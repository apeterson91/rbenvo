
#' benvo Print Method
#'
#' @export
#' @param object benvo object
#'
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
	if(object@longitudinal){
		cat("Num Subjects: ", length(unique(object@subject_data[,joining_ID(object)[1]])))
		cat("\n")
	}
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

#' Benvo BEF Summary Generic
#'
setGeneric("bef_summary", function(x) standardGeneric("bef_summary"))

#' benvo BEF Summary Method
#'
#' @export
#' @param x a benvo object
#'
setMethod(f = "bef_summary",
	signature="Benvo",
	definition = function(x){

		BEF <- Measure <- NULL

	dfr <- purrr::map_dfr(1:(num_BEF(x)),function(y) {x@bef_data[[y]] %>% 
						  dplyr::mutate(BEF = x@bef_names[y] ) }) %>%
		tidyr::gather(dplyr::matches("Distance|Time"),key = "Distance_Time",value="Measure") %>%
		dplyr::group_by(BEF,Distance_Time) %>%
		dplyr::summarise(Lower = quantile(Measure,0.025,na.rm=T),
						 Median = median(Measure,na.rm=T),
						 Upper = quantile(Measure,0.975,na.rm=T),
						 Number = dplyr::n(),
						 Num_NA = sum(is.na(Measure))
		)
	print(dfr)
	return(invisible(dfr))
})

#' Return the joining ID of the benvo
#'
setGeneric("joining_ID",function(x) standardGeneric("joining_ID"))


#' benvo joining ID
#'
#' @export
#' @param x benvo object
#'
setMethod("joining_ID","Benvo",function(x){

	if(x@longitudinal)
		return(c("ID","Measurement"))
	else
		return(c("ID"))

})

#' BEF component look up
#'
setGeneric("component_lookup",function(x,bef_name) standardGeneric("component_lookup"))

#' Component look up
#'
#' returns the measure (Distance/Time) that's associated with the input BEF
#' @export
#' @param x benvo object
#' @param bef_name bef_name string
#'
setMethod("component_lookup","Benvo",function(x,bef_name){

	ix <- which(x@bef_names == bef_name)
	out <- switch(x@components[ix],
		   "Distance" = "Distance",
		   "Time" = "Time",
		   "Distance-Time" = c("Distance","Time"))
	return(out)

})

#' Number of BEF data frames
#'
setGeneric("num_BEF",function(x) standardGeneric("num_BEF") )

#' Number of Built Environment Features
#'
#' @export
#' @param x a benvo object
#'
setMethod("num_BEF","Benvo",function(x){
	return(length(x@bef_names))})


#' Subject Design Matrix
#'
setGeneric("subject_design",function(x,formula,...) standardGeneric("subject_design"))


#' Extract Subject Design Matrix
#'
#' @export
#' @param formula similar to \code{\link[stats]{lm}}.
#' @param x benvo object
#' @param ... other arguments passed to the model frame
#' @importFrom stats is.empty.model model.response model.matrix
#'
setMethod("subject_design","Benvo",function(x,formula,...){

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

})

#' Longitudinal design dataframe
#'
setGeneric("longitudinal_design",function(x,formula,...) standardGeneric("longitudinal_design"))


#' Extract Longitudinal Design Matrices
#'
#' For use with \code{\link[lme4]{glmer}} type formulas/models
#' @export
#' @param formula similar to \code{\link[lme4]{glmer}}.
#' @param x benvo object
#' @param ... other arguments passed to the model frame
#' @importFrom lme4 glmerControl
#'
setMethod("longitudinal_design","Benvo",function(x,formula,...){

  design <- function(formula){
	  mf <- match.call(expand.dots = TRUE)
	  mf[[1]] <- quote(lme4::glFormula)
	  mf$control <- glmerControl(check.nlev.gtreq.5 = "ignore",
	                             check.nlev.gtr.1 = "stop",
	                             check.nobs.vs.rankZ = "ignore",
	                             check.nobs.vs.nlev = "ignore",
	                             check.nobs.vs.nRE = "ignore" )

	  mf$data = x@subject_data
	  mf$formula <- formula
	  mf <- eval(mf,parent.frame())
	  y <- mf$fr[,as.character(mf$formula[2L])]
	  X <-  mf$X
	  out <- list(y=y,X=X,glmod=mf)
	}

	return(design(formula))

})


#' Join bef and subject data in benvos
#'
setGeneric("joinvo",function(x,bef_name,component = "Distance",tibble = F) standardGeneric("joinvo"))

#' Join bef and subject data in benvos
#'
#' @export
#' @importFrom stats quantile median
#' @param x benvo object
#' @param bef_name string of bef data to join on in bef_data
#' @param component one of c("Distance","Time","Distance-Time") indicating which column(s) of the bef dataset should be returned
#' @param tibble boolean value of whether or not to return a data.frame or tibble
#'
setMethod("joinvo","Benvo", function(x,bef_name,component = "Distance",tibble = F){


	Distance <- Time <- ID <- NULL
	stopifnot(component %in% c("Distance","Time","Distance-Time"))
	ix <- which(x@bef_names == bef_name)
	stopifnot(grepl(component,x@components[ix]))
	col <- switch(component,
		   "Distance" = "Distance",
		   "Time" = "Time",
		   "Distance-Time" = c("Distance","Time"))
	if(length(ix)!=1)
		stop("only one BEF name may be supplied")

	jdf <- merge(x@bef_data[[ix]],x@subject_data,all.y = T, by.x = joining_ID(x), by.y= joining_ID(x))

	to_keep <- c(joining_ID(x),intersect(union(col,colnames(x@subject_data)),colnames(jdf)))
	jdf %>% dplyr::select(!!to_keep) -> jdf

	if(tibble)
		return(tibble::as_tibble(jdf))
	else
		return(jdf)
})

#' Pointrange plot
#'
#' @export
#' @param x benvo object
#' @param BEF BEF specification
#' @param component one of c("Distance","Time") indicating which column(s) of the bef dataset should be returned
#'
setGeneric("plot_pointrange", function(x,BEF,component)  standardGeneric("plot_pointrange") )


#' Pointrange plot
#'
#' @export
#' @param x benvo object
#' @param BEF BEF specification
#' @param component one of c("Distance","Time") indicating which column(s) of the bef dataset should be returned
#'
setMethod("plot_pointrange","Benvo",function(x,BEF,component){

	Distance <- Lower <- Median <- Upper <- ID <- Measure <-  NULL
	jdf <- joinvo(x,BEF,component,tibble=T)

	if(x@longitudinal)
		jdf %>% dplyr::group_by(ID,Measurement) %>%
			dplyr::summarise(Lower = quantile(Distance,0.025,na.rm=T),
							 Median = median(Distance,na.rm=T),
							 Upper = quantile(Distance,0.975,na.rm=T)) %>%
		ggplot2::ggplot(ggplot2::aes(x=ID,y=Median))  +
		ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper)) +
		ggplot2::ylab(component) + ggplot2::theme(strip.background=ggplot2::element_blank()) +  
		  ggplot2::coord_flip() + ggplot2::facet_wrap(~Measurement)-> p
	  else{
		jdf %>% dplyr::group_by(ID) %>%
			dplyr::summarise(Lower = quantile(Distance,0.025,na.rm=T),
							 Median = median(Distance,na.rm=T),
							 Upper = quantile(Distance,0.975,na.rm=T)) %>%
		ggplot2::ggplot(ggplot2::aes(x=ID,y=Median))  +
		ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper)) +
		ggplot2::ylab(component) + 
		  ggplot2::coord_flip() -> p
	  }

	return(p)
})
