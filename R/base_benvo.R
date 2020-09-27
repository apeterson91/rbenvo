#' Base Benvo
#'
#' When building a benvo iteratively the base benvo allows you to start with no bef data constructed a priori and build up from
#' subject data that contains spatial and/or temporal raw data in the form of \code{\link[sf]{sf}} structures or \code{\link[lubridate]{Date}} columns.
#' @param subject_data data.frame containing subject level covariates.
#' @param by optional key
#' @param ... optional arguments for specifying date-time columns see \code{\link{set_datetime_cols}}
#' @return a benvo with attribute base = TRUE
#'
base_benvo <- function(subject_data,
					   by,...){


	## To pass R CMD Check
	date_cols <- NULL
	subject_sf <- inherits(subject_data,'sf')
	datetime_cols <- do.call(set_datetime_cols,args=list(...))
	measurement_date <- start_date_col <- stop_date_col <- NULL
	for(i in names(datetime_cols))
		assign(i,datetime_cols[i])
	subject_dt <- check_datetime_cols(datetime_cols,subject_data)

	if(!all(by %in% colnames(subject_data)))
		stop(by," is not in subject data")

	checks <- sapply(datetime_cols,function(x){ lubridate::is.Date(subject_data[,x,drop=T])  })

	if(any(!checks))
		subject_data %>%
			dplyr::mutate_at(date_cols[which(checks==F)],
							 lubridate::as_date) -> subject_data

	 out <- list(subject_data=subject_data,
				 sub_bef_data = list(),
				 bef_data=list())

	structure(out,
			  id = by,
			  bef_sf = NULL,
			  subject_sf = subject_sf,
			  bef_dt = NULL,
			  subject_dt = subject_dt,
			  longitudinal = length(by)>1,
			  bef_names = NULL,
			  components = NULL,
			  measurement_date = measurement_date,
			  start_date_col = start_date_col,
			  stop_date_col = stop_date_col,
			  active = "subject",
			  base = TRUE,
			  class = "benvo")
}

#' Add Built Environment Feature to Benvo
#'
#' @param x benvo or base benvo
#' @param bef_data 'tidy' data frame containing date/spatial information for one unique bef
#' @param bef_id unique bef_id column name
#' @param d_function function for calculating distance. Default is \code{\link[sf]{st_distance}}
#' @export
#'
add_BEF <- function(x,
					bef_data,
					bef_id,
					d_function = sf::st_distance){

	# To Pass R CMD CHECK
	.data <- NULL
	bef_name <- deparse(substitute(bef_data))
	check <- FALSE

	if(subject_has_sf(x) && sf_check(bef_data)){
	  check <- TRUE
		distances <- d_function(x$subject_data,bef_data)
		colnames(distances) <- bef_data[,bef_id,drop=TRUE]
		distances <- dplyr::as_tibble(distances)
		id <- get_id(x)
		##TODO: handle case in which length(id)==2
		distances %>% mutate(TEMP_ID_ =  sf::st_drop_geometry(x$subject_data)[,get_id(x),drop=TRUE]) %>%
		  tidyr::gather(-dplyr::one_of("TEMP_ID_"),
						key={{bef_id}},
						value = "Distance") ->  df
		colnames(df)[1:length(id)] <- get_id(x)
		component <- "Distance"
	}
	if(has_subject_dt(x) && has_dt(x,bef_data)){
	  check <- TRUE
		date_cols <- get_date_cols(x)
		bef_cols <- date_cols[2:3]
		measure_date <- date_cols[1]
		start_date <- date_cols[2]
		stop_date <- date_cols[3]
		bef_date_cols <- paste0(date_cols[2:3],"_",bef_name)
		bef_start <- bef_date_cols[1]
		bef_stop <- bef_date_cols[2]
		if(subject_has_sf(x))
		  sdf <- sf::st_drop_geometry(x$subject_data[,c(get_id(x),date_cols)])
		else
		  sdf <- x$subject_data[,c(get_id(x),date_cols)]

		times <- tidyr::crossing(sdf,
		                         bef_data %>%
		                           dplyr::rename_at(bef_cols,function(nm) paste0(nm,"_",bef_name)) %>%
		                           dplyr::select_at(c(bef_start,bef_stop,bef_id))) %>%
			dplyr::filter(.data[[measure_date]] > .data[[start_date]],
			              .data[[measure_date]] > .data[[bef_start]],
			              .data[[bef_stop]] > .data[[start_date]]) %>%
			dplyr::mutate(Time = pmin(.data[[measure_date]] - .data[[start_date]],
			                                .data[[measure_date]] - .data[[bef_start]],
			                                .data[[stop_date]] - .data[[start_date]],
			                                .data[[bef_stop]] - .data[[start_date]],
			                                .data[[bef_stop]] - .data[[ bef_start ]]),
				   exposure_lag = .data[[measure_date]] - pmin(.data[[ bef_stop]],.data[[stop_date]]))
		if(subject_has_sf(x) && sf_check(bef_data)){
			df <- df %>% dplyr::inner_join(times)
			component <- "Distance-Time"
		}else{
			component <- "Time"
			df <- times
		}
	}
	if(!check)
	  stop("Must be datetime or sf information in order to add benvo.
	       If you'd like to create a benvo using your own calculations see ?benvo.")


	sub_bef_df <- list(df)
	names(sub_bef_df) <- bef_name
	bef_data <- list(bef_data)
	names(bef_data) <- pre_bef(bef_name)
	attributes <- list(bef_names = bef_name,
	                   bef_sf = sapply(pre_bef(bef_name),function(z) sf_check(bef_data[[z]])),
	                   bef_dt = sapply(pre_bef(bef_name),function(z) has_dt(x,bef_data[[z]])),
	                   components = component,
					   bef_id  = sapply(bef_name,function(z) bef_id))

	base <- attr(x,"base")
	out <- update_benvo_add(x,
							bef_data,
							sub_bef_df,
							attributes,
							base = base)
	return(out)
}


update_benvo_add <- function(x,bef_data,sub_bef_data,
							 attributes,base = FALSE){

	for(i in names(attributes))
		attr(x,i) <- c(attr(x,i),attributes[[i]])
	if(base)
		attr(x,"base") <- FALSE

	x$sub_bef_data <- c(x$sub_bef_data,sub_bef_data)
	x$bef_data <- c(x$bef_data,bef_data)
	return(x)
}


## Internal  ------------------------------------------

has_dt <- function(.data,bef_data){

	date_cols <- get_date_cols(.data)[2:3]
	if(all(date_cols %in% colnames(bef_data)))
		return(TRUE)
	else
		return(FALSE)
}

