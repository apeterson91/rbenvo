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
				"map" = plot_map(x,...),
				"time" = plot_timeline(x,...))

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
		component_check(x,term, component)

	jdf <- joinvo(x,term,component)
	id <- get_id(x)

	l <- .5 - (p/2)
	u <- .5 + (p/2)

	jdf %>%
		dplyr::mutate_at(id,factor) %>%
		dplyr::group_by_at(id) %>%
		dplyr::summarise_at(component,
		                    list(Lower = ~ quantile(.,l,na.rm = TRUE),
		                         Median = ~ median(.,na.rm = TRUE),
		                         Upper = ~ quantile(.,u,na.rm = TRUE))) %>%
	ggplot2::ggplot(ggplot2::aes(x=forcats::fct_reorder(.data[[id[1]]],Median),y=Median))  +
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
#' Provides a plot of benvo subjects and (one) BEF's locations
#' @export
#' @param x benvo object
#' @param term BEF term
#'
plot_map <- function(x,term = NULL){

	if(is.longitudinal(x))
		warning("Longitudinal structure will ignored for this plot")
	if(is.null(term)){
		ix <- 1
		term <- bef_names(x)[1]
	}else{
		term_check(x,term)
	}
	if(!all(c(subject_has_sf(x),bef_has_sf(x,term) ) ) )
		stop("No sf data to plot")

	geometry <- Class <- NULL
	bef_id <- bef_id_lookup(x,term)
	rbdf <- rbind(x$subject_data %>%
				  dplyr::select(geometry) %>%
				  dplyr::mutate(Class = "subjects"),
				  x$bef_data[[pre_bef(term)]] %>%
					  dplyr::select(geometry) %>%
					  dplyr::mutate(Class = term))
	bbx <- sf::st_bbox(rbdf)
	names(bbx) <- c("left","bottom","right","top")
	mp <- ggmap::get_stamenmap(bbx,color='bw')

	p <- ggmap::ggmap(mp) + 
		ggplot2::geom_sf(data= rbdf, inherit.aes = FALSE,
						 ggplot2::aes(color=Class,shape=Class))
	return(p)
}

#' Temporal Plot of benvo
#'
#' Provides a plot of benvo subjects temporal exposure over time.
#' @export
#' @param x benvo object
#' @param ... currently ignored
#'
plot_timeline <- function(x,...){


	#  To Pass R CMD CHECK
	bdf <- Date_Value <- .data <- Date_Meaning <- NULL

	date_cols <- get_date_cols(x)
	id <- get_id(x)
	p <- x$subject_data %>%
		tidyr::gather_(date_cols,
					  key = "Date_Meaning",
					  value="Date_Value") %>%
		ggplot2::ggplot(ggplot2::aes(x = Date_Value,
							group=.data[[id]],
							color=Date_Meaning,
							y = .data[[id]])) +
		ggplot2::geom_line() + ggplot2::geom_point() +
		ggplot2::xlab("Date") +
		ggplot2::theme(axis.text.y=ggplot2::element_blank(),
					   legend.title = ggplot2::element_blank())

	return(p)
}


#ggmap_bbox <- function(map) {
#  if (!inherits(map, "ggmap")) stop("map must be a ggmap object")
#  # Extract the bounding box (in lat/lon) from the ggmap to a numeric vector,
#  # and set the names to what sf::st_bbox expects:
#  map_bbox <- setNames(unlist(attr(map, "bb")),
#                       c("ymin", "xmin", "ymax", "xmax"))
#
#  # Coonvert the bbox to an sf polygon, transform it to 3857,
#  # and convert back to a bbox (convoluted, but it works)
#  bbox_3857 <- st_bbox(st_transform(st_as_sfc(st_bbox(map_bbox, crs = 4326)), 3857))
#
#  # Overwrite the bbox of the ggmap object with the transformed coordinates
#  attr(map, "bb")$ll.lat <- bbox_3857["ymin"]
#  attr(map, "bb")$ll.lon <- bbox_3857["xmin"]
#  attr(map, "bb")$ur.lat <- bbox_3857["ymax"]
#  attr(map, "bb")$ur.lon <- bbox_3857["xmax"]
#  map
#}
