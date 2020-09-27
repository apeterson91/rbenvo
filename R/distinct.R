

#' @importFrom dplyr distinct
#' @export
distinct.benvo <- function(.data,...){

	tmp_df <- get_active(.data)
	if(active(.data) == 'subject')
		.data$subject_data <- dplyr::distinct(tmp_df,...)
	else if(bef_is_active(.data))
		.data$bef_data[[active(.data)]] <- dplyr::distinct(tmp_df,...)
	else
		.data$sub_bef_data[[active(.data)]] <- dplyr::distinct(tmp_df,...)
	check_id_components(.data)
	.data
}



#' @export
dplyr::distinct
