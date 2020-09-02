
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
#' @param x a benvo object
#' @keywords internal
#'
setGeneric("bef_summary", function(x) standardGeneric("bef_summary"))

#' benvo BEF Summary Method
#'
#' @export
#' @describeIn bef_summary
#'
setMethod(f = "bef_summary",
	signature="Benvo",
	definition = function(x){

		BEF <- Measure <- Distance_Time <- Measurement <- NULL

	dfr <- purrr::map_dfr(1:(num_BEF(x)),function(y) {x@bef_data[[y]] %>% 
						  dplyr::mutate(BEF = x@bef_names[y] ) }) %>%
		tidyr::gather(dplyr::matches("Distance|Time"),key = "Distance_Time",value="Measure") 
	if(x@longitudinal)
		dfr <- dfr %>% dplyr::group_by(BEF,Distance_Time,Measurement)
	else
		dfr <- dfr %>% dplyr::group_by(BEF,Distance_Time)
	dfr <- dfr %>% dplyr::summarise(Lower = quantile(Measure,0.025,na.rm=T),
						 Median = median(Measure,na.rm=T),
						 Upper = quantile(Measure,0.975,na.rm=T),
						 Number = dplyr::n(),
						 Num_NA = sum(is.na(Measure))
						)
	print(dfr)
	return(invisible(dfr))
})

#' Between - Within Construction Generic
#'
#' @export
#' @param x benvo object
#' @param M matrix to construct between/within measures
#' @keywords internal
#' 
setGeneric("bwinvo",function(x,M) standardGeneric("bwinvo"))


#' Between - Within Construction 
#'
#' @describeIn bwinvo between within decomposition of longitudinal matrix M according to benvo subject-bef data
#' @export 
#' 
setMethod("bwinvo","Benvo",function(x,M){
	
	stopifnot(x@longitudinal)
	smat <- Matrix::fac2sparse(x@subject_data$ID)
	if(nrow(M)!=ncol(smat))
		stop("rows in M are inappropriate")
	num <- apply(smat,1,sum)
	Xb <- apply((smat %*% M) ,2, function(x) x/num)
	Xb <- as.matrix(Matrix::t(smat) %*% Xb)
	Xw <- as.matrix(M - Xb)
	return(list(between = Xb, within = Xw))
})

#' Return the joining ID of the benvo
#'
#' @param x benvo object 
#' @keywords internal
#'
setGeneric("joining_ID",function(x) standardGeneric("joining_ID"))


#' benvo joining ID
#'
#' @export
#' @describeIn joining_ID  return appropriate benvo joining ID
#'
setMethod("joining_ID","Benvo",function(x){

	if(x@longitudinal)
		return(c("ID","Measurement"))
	else
		return(c("ID"))

})

#' BEF component look up
#'
#' @keywords internal
#' @param x benvo object
#' @param bef_name bef_name string
#'
setGeneric("component_lookup",function(x,bef_name) standardGeneric("component_lookup"))

#' Component look up
#'
#' @export
#' @describeIn component_lookup returns the measure (Distance/Time) that's associated with the input BEF
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
#' @param x a benvo object
#' @keywords internal
#'
setGeneric("num_BEF",function(x) standardGeneric("num_BEF") )

#' Number of Built Environment Features
#'
#' @export
#' @describeIn num_BEF 
#'
setMethod("num_BEF","Benvo",function(x){
	return(length(x@bef_names))})


#' Subject Design Matrix
#'
#' @param formula similar to \code{\link[stats]{lm}}.
#' @param x benvo object
#' @param ... other arguments passed to the model frame
#' @keywords internal
#'
setGeneric("subject_design",function(x,formula,...) standardGeneric("subject_design"))


#' Extract Subject Design Matrix
#'
#' @export
#' @describeIn subject_design  method
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
	out <- list(y=y,X=X,model_frame=mf)
	return(out)

})

#' Longitudinal design dataframe
#'
#' For use with \code{\link[lme4]{glmer}} type formulas/models
#' @param formula similar to \code{\link[lme4]{glmer}}.
#' @param x benvo object
#' @param ... other arguments passed to the model frame
#'
setGeneric("longitudinal_design",function(x,formula,...) standardGeneric("longitudinal_design"))


#'
#' @export
#' @describeIn longitudinal_design  method
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

#' Aggregate Matrix to Subject or Subject - Measurement Level
#' 
#' @param x benvo object
#' @param M matrix to aggregate
#' @param stap_term relevant stap term
#' @param component one of c("Distance","Time","Distance-Time") indicating which column(s) of the bef dataset should be returned
#'
setGeneric("aggrenvo",function(x,M,stap_term,component) standardGeneric("aggrenvo"))


#'
#' @export
#' @describeIn aggrenvo method
#' 
setMethod("aggrenvo","Benvo",function(x,M,stap_term,component){	

	ID <- Measurement <- . <- NULL
	jndf <- joinvo(x,stap_term,component,NA_to_zero = F)
	if(component=="Distance-Time")
		component_ <- c("Distance") ## Fine to use just one since zero exposure variable will equate to zero exposure in the other
	else
		component_ <- component
	if(x@longitudinal){
		jndf$BENVO_NEWID_ <- paste0(jndf$ID,"_",jndf$Measurement)
		lvls <- unique(jndf$BENVO_NEWID_)
		jndf$BENVO_NEWID_ <- factor(jndf$BENVO_NEWID_, levels = lvls)
		AggMat <- Matrix::fac2sparse(jndf$BENVO_NEWID_)
		zeromat <- jndf %>% dplyr::group_by(ID,Measurement) %>% 
		dplyr::summarise_at(component_,function(x) 1*all(!is.na(x))) %>% 
		dplyr::pull(component_) %>% diag(.)
	}
	else{
		AggMat <- Matrix::fac2sparse(jndf[,joining_ID(x)])
		zeromat <- jndf %>% dplyr::group_by(ID) %>% 
				dplyr::summarise_at(component_,function(x) 1*all(!is.na(x))) %>% 
				dplyr::pull(component_) %>% diag(.)
	}
	stopifnot(nrow(M) == ncol(AggMat))
	X <- as.matrix(zeromat %*% (AggMat %*% M))
	return(X)
})

#' Join BEF and subject data within a benvo
#'
#' @details Joins the subject dataframe within a benvo to the supplied BEF dataframe keeping the selected component
#' @param x benvo object
#' @param bef_name string of bef data to join on in bef_data
#' @param component one of c("Distance","Time","Distance-Time") indicating which column(s) of the bef dataset should be returned
#' @param tibble boolean value of whether or not to return a data.frame or tibble
#' @param NA_to_zero replaces NA values with zeros - potentially useful when constructing design matrices
#'
setGeneric("joinvo",function(x,bef_name,component = "Distance",tibble = F,NA_to_zero = F) standardGeneric("joinvo"))

#'
#' @export
#' @importFrom stats quantile median
#' @describeIn joinvo method
#'
setMethod("joinvo","Benvo", function(x,bef_name,component = "Distance",tibble = F,NA_to_zero = F){


	##TODO: Make option for subject design data to be constructed from joined data
	stopifnot(component %in% c("Distance","Time","Distance-Time"))
	Distance <- Time <- ID <- NULL

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
	if(NA_to_zero)
		jdf <- jdf %>% dplyr::mutate_at(col,function(x) tidyr::replace_na(x,0))

	if(tibble)
		return(tibble::as_tibble(jdf))
	else
		return(jdf)
})

#' Pointrange plot
#' 
#' Plots the sorted distribution intervals of distances and times across subjects
#'
#' @export
#' @param x benvo object
#' @param BEF BEF specification
#' @param component one of c("Distance","Time") indicating which column(s) of the bef dataset should be returned
#' @param p The probability of distances/times that should be included in interval
#'
setGeneric("plot_pointrange", function(x,BEF,component, p = 0.95)  standardGeneric("plot_pointrange") )


#'
#' @export
#' @describeIn plot_pointrange method
#'
setMethod("plot_pointrange","Benvo",function(x,BEF,component, p = 0.95){

	Distance <- Lower <- Median <- Upper <- ID <- Measure <-  Measurement <- NULL
	jdf <- joinvo(x,BEF,component,tibble=T)

	l <- .5 - (p/2)
	u <- .5 + (p/2)

	if(x@longitudinal)
		jdf %>% dplyr::group_by(ID,Measurement) %>%
			dplyr::summarise(Lower = quantile(Distance,l,na.rm=T),
							 Median = median(Distance,na.rm=T),
							 Upper = quantile(Distance,u,na.rm=T)) %>%
		ggplot2::ggplot(ggplot2::aes(x=forcats::fct_reorder(ID,Median),y=Median))  +
		ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper)) +
		ggplot2::ylab(component) + ggplot2::theme(strip.background=ggplot2::element_blank()) +  
		  ggplot2::coord_flip() + ggplot2::facet_wrap(~Measurement)-> p
	  else{
		jdf %>% dplyr::group_by(ID) %>%
			dplyr::summarise(Lower = quantile(Distance,0.025,na.rm=T),
							 Median = median(Distance,na.rm=T),
							 Upper = quantile(Distance,0.975,na.rm=T)) %>%
		ggplot2::ggplot(ggplot2::aes(x=forcats::fct_reorder(ID,Median),y=Median))  +
		ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper)) +
		ggplot2::ylab(component) + 
		  ggplot2::coord_flip() -> p
	  }

	return(p)
})
