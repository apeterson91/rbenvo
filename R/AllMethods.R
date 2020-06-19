
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

#' benvo Summary Method
#'
#' @param x a benvo object
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

	dfr <- purrr::map_dfr(1:(num_BEF(x)),function(y) {x@bef_data[[y]] %>% dplyr::mutate(BEF = x@bef_names[y] ) }) %>% 
		tidyr::gather(dplyr::matches("Distance|Time"),key = "Space_Time",value="Measure") %>% 
		dplyr::group_by(BEF) %>% 
		dplyr::summarise(Lower = quantile(Measure,0.025),
						 Median = median(Measure),
						 Upper = quantile(Measure,0.975),
						 Number = dplyr::n())
	print(dfr)
	return(invisible(dfr))
})

#' benvo joining ID
#'
#' @export
#' @param x benvo object
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


#' Number of Built Environment Features
#'
#' @export
#' @param x a benvo object
#'
setGeneric("num_BEF",function(x) standardGeneric("num_BEF") )

#' Number of Built Environment Features
#'
#' @export
#' @param x a benvo object
#'
setMethod("num_BEF","Benvo",function(x){
	return(length(x@bef_names))})


#' Extract Subject Design Matrix
#'
#' @export
#' @param x benvo object
#' @param formula similar to \code{\link[stats]{lm}}.
#' @param ... other arguments passed to the model frame
#' @importFrom stats is.empty.model model.response model.matrix
#'
setGeneric("subject_design",function(formula,x,...) standardGeneric("subject_design"))


#' Extract Subject Design Matrix
#'
#' @export
#' @param x benvo object
#' @param formula similar to \code{\link[stats]{lm}}.
#' @param ... other arguments passed to the model frame
#'
setMethod("subject_design","Benvo",function(formula,x,...){

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


#' Join bef and subject data
#'
#' @export
#' @importFrom stats quantile median
#' @param x benvo object
#' @param bef_name string of bef data to join on in bef_data
#' @param tibble boolean value of whether or not to return a data.frame or tibble
#'
setGeneric("joinvo",function(x,bef_name,tibble = F) standardGeneric("joinvo"))

#' Join bef and subject data in benvos
#'
#' @export
#' @param x benvo object
#' @param bef_name string of bef data to join on in bef_data
#' @param tibble boolean value of whether or not to return a data.frame or tibble
#' 
setMethod("joinvo","Benvo", function(x,bef_name,tibble = F){


	Distance <- Time <- ID <- NULL
	ix <- which(x@bef_names == bef_name)
	if(length(ix)!=1)
		stop("only one BEF name may be supplied")

	jdf <- merge(x@bef_data[[ix]],x@subject_data,all.y = T, by.x = joining_ID(x), by.y= joining_ID(x))

	to_keep <- c(joining_ID(x),intersect(union(c("Distance","Time"),colnames(x@subject_data)),colnames(jdf)))
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
#'
setGeneric("plot_pointrange", function(x,BEF)  standardGeneric("plot_pointrange") )


#' Pointrange plot
#'
#' @export
#' @param x benvo object
#' @param BEF BEF specification
#'
setMethod("plot_pointrange","Benvo",function(x,BEF){

	Distance <- Lower <- Median <- Upper <- ID <- Measure <-  NULL
	jdf <- joinvo(x,BEF,tibble=T)

	if(x@longitudinal)
		jdf %>% dplyr::group_by(ID) %>%
			dplyr::summarise(Lower = quantile(Distance,0.025,na.rm=T),
							 Median = median(Distance,na.rm=T),
							 Upper = quantile(Distance,0.975,na.rm=T)) %>%
		ggplot2::ggplot(ggplot2::aes(x=ID,y=Median))  +
		ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper)) +
		  ggplot2::coord_flip() + ggplot2::facet_wrap(~Measurement)-> p
	  else
		jdf %>% dplyr::group_by(ID) %>%
			dplyr::summarise(Lower = quantile(Distance,0.025,na.rm=T),
							 Median = median(Distance,na.rm=T),
							 Upper = quantile(Distance,0.975,na.rm=T)) %>%
		ggplot2::ggplot(ggplot2::aes(x=ID,y=Median))  +
		ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper)) +
		  ggplot2::coord_flip() -> p 


	return(p)
})
