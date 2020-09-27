
#' @importFrom dplyr mutate
#' @export
mutate.benvo <- function(.data,...){

	tmp_df <- get_active(.data)
	if(active(.data) == 'subject')
		.data$subject_data <- dplyr::mutate(tmp_df,...)
	else if(bef_is_active(.data))
		.data$bef_data[[active(.data)]] <- dplyr::mutate(tmp_df,...)
	else
		.data$sub_bef_data[[active(.data)]] <- dplyr::mutate(tmp_df,...)
	check_id_components(.data)
	.data
}


#' @export
dplyr::mutate

#' @importFrom dplyr transmute
#' @export
dplyr::transmute

#' @importFrom dplyr mutate_all
#' @export
dplyr::mutate_all

#' @importFrom dplyr mutate_at
#' @export
dplyr::mutate_at

#' @importFrom dplyr n
#' @export
dplyr::n


# Internal ----------------------------------------

check_id_components <- function(x){

	id <- get_id(x)
	check_one <- id %in% colnames(x$subject_data)
	check_two <- id %in% Reduce(intersect,lapply(x$sub_bef_data,colnames))
	if(!(check_one && check_two))
		stop("id key ",paste0(paste0("'",id,"'",sep=", "))," is no longer present, this operation results in an invalid benvo")
	check_three <- purrr::map_lgl(bef_names(x),function(y){ component_lookup(x,y) %in% colnames(x$sub_bef_data[[y]]) })
	if(!all(check_three))
		stop("Component ",paste0(components(x)[which(check_three==F)])," has been removed from ", bef_names(x)[which(check_three==F)], " resulting in an invalid benvo")

}
