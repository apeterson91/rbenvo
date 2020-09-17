#' Benvo plots
#'
#' Variety of plotting functions for benvo objects
#'
#' @export
#' @param x benvo object
#' @param plotfun one of c("pointrange","map")
#' @param ... extra arguments for plotfun
#'
plot.benvo <- function(x, plotfun = "pointrange", ... ){

	p <- switch(plotfun,
				"pointrange" = plot_pointrange(x,...),
				"map" = plot_map(x,...))

	return(p)
}

#' Plot Pointrange
#'
#' @export
#' @param x benvo object
#' @param term name of BEF to plot. If NULL plots the first component listed in the Benvo.
#' @param component one of c("Distance","Time") indicating which measure to use. Defaults to Distance if both measures are available, otherwise uses the only option.
#' @param p The probability of distances/times that should be included in interval
#'
plot_pointrange <- function(x, term = NULL,component = NULL, p = 0.95){

	Distance <- Lower <- Median <- Upper <- Measure <- .data <- NULL

	if(is.null(term)){
		ix <- 1
		term <- bef_names(x)[1]
	}else{
		term_check(x,term)
	}
	if(is.null(component)){
		component <- component_lookup(x,term)
		if(length(component)==2)
			component <- component[1]
	}
	else
		stopifnot(component %in% component_lookup(x,term))

	jdf <- joinvo(x,term,component,tibble=T)
	id <- get_id(x)

	l <- .5 - (p/2)
	u <- .5 + (p/2)

	jdf %>%
		dplyr::mutate_at(id,factor) %>%
		dplyr::group_by_at(id) %>%
		dplyr::summarise(Lower = quantile(Distance,l,na.rm=T),
						 Median = median(Distance,na.rm=T),
						 Upper = quantile(Distance,u,na.rm=T)) %>%
	ggplot2::ggplot(ggplot2::aes(x=forcats::fct_reorder(.data[[id]],Median),y=Median))  +
	ggplot2::geom_pointrange(ggplot2::aes(ymin=Lower,ymax=Upper),alpha=0.4) +
	ggplot2::xlab(id) +
	ggplot2::ylab(component) +
	ggplot2::theme(strip.background=ggplot2::element_blank(),
				   axis.text.y = ggplot2::element_blank(),
				   axis.ticks.y = ggplot2::element_blank()) +
	  ggplot2::coord_flip()  -> p
	if(is.longitudinal(x)){
		measurement <- id[2]
		p <- p + ggplot2::facet_wrap(~{measurement})
	}

	return(p)
}


#' Spatial Plot of benvo
#'
#' @export
#' @param x benvo object
#' @param term BEF term
#'
plot_map <- function(x,term){

	geometry <- Class <- NULL
	term_check(x,term)
	sf_check(x,term)
	rbdf <- rbind(x$subject_data %>% dplyr::select(geometry) %>% dplyr::mutate(Class = "subjects"),
				  x$bef_data[[term]] %>% dplyr::select(geometry) %>% dplyr::mutate(Class = term))
	p <- ggplot2::ggplot(data=rbdf,) + ggplot2::geom_sf(inherit.aes=F,ggplot2::aes(color=Class))
	return(p)
}

# TODO:  Finish temporal plot after incorporating date/time column specifications
# Temporal plot of benvo
#plot_timeline <- function(x,term){

#}
