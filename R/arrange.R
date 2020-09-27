#' @export
#' @importFrom dplyr arrange
arrange.benvo <- function(.data, ...) {

	tmp_df <- get_active(.data)
	if(active(.data) == 'subject')
		.data$subject_data <- dplyr::arrange(tmp_df,...)
	else if(bef_is_active(.data))
		.data$bef_data[[active(.data)]] <- dplyr::arrange(tmp_df,...)
	else
		.data$subj_bef_data[[active(.data)]] <- dplyr::arrange(tmp_df,...)
	.data
}

#' @export
dplyr::arrange
