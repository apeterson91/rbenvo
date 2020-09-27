#' @export
#' @importFrom dplyr filter
filter.benvo <- function(.data, ...) {

	d_tmp <- get_active(.data)
	if(active(.data) == 'subject')
		.data$subject_data <- dplyr::filter(d_tmp, ...)
	else if(bef_is_active(.data))
		.data$bef_data[[active(.data)]] <- dplyr::filter(d_tmp,...)
	else
		.data$sub_bef_data[[active(.data)]] <- dplyr::filter(d_tmp,...)
	.data
}


#' @export
dplyr::filter

#' @importFrom dplyr top_n
#' @export
dplyr::top_n

