#' @export
#' @importFrom dplyr select
select.benvo <- function(.data, ...) {
  d_tmp <- get_active(.data)
  d_tmp <- dplyr::select(d_tmp,...)
  if(active(.data)=='subject')
	  .data$subject_data <- d_tmp
  else if(bef_is_active(.data))
	  .data$bef_data[[active(.data)]] <- d_tmp
  else
	  .data$sub_bef_data[[active(.data)]] <- d_tmp
  check_id_components(.data)
  .data
}

#' @export
dplyr::select

#' @importFrom dplyr contains
#' @export
dplyr::contains

#' @importFrom dplyr ends_with
#' @export
dplyr::ends_with

#' @importFrom dplyr everything
#' @export
dplyr::everything

#' @importFrom dplyr matches
#' @export
dplyr::matches

#' @importFrom dplyr num_range
#' @export
dplyr::num_range

#' @importFrom dplyr one_of
#' @export
dplyr::one_of

#' @importFrom dplyr starts_with
#' @export
dplyr::starts_with
