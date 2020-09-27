#' benvo Print Method
#'
#' @export
#' @param x benvo object
#' @param ... ignored
#'
print.benvo <- function(x,...){

	cat("Active df: ", active(x))
	cat("\n")
	print(get_active(x))

	 

	

}




#' benvo BEF Summary Generic
#'
#' @param object a benvo object
#' @param ... ignored
#' @export
#'
summary.benvo <- function(object,...){

		cat("Subject Data:")
		cat("\n")
		cat("---------------------------:")
		cat("\n")
		cat("Observations: ", nrow(object$subject_data))
		cat("\n")
		cat("Columns: ", ncol(object$subject_data))
		cat("\n")
		if(is.longitudinal(object)){
			cat("Num Subjects: ", length(unique(object$subject_data[,get_id(object)[1]])))
			cat("\n")
		}
		cat("\n")

		if(num_BEF(object)>0){
			cat("BEF Data:")
			cat("\n")
			cat("---------------------------:")
			cat("\n")
			cat("Number of Features: ", num_BEF(object))
			cat("\n")
			cat("Features: ")
			cat("\n")
			prettydf <- data.frame(Name = bef_names(object),
								   Measures = attr(object,'components'))
			rownames(prettydf) <- NULL
			print(prettydf)
			return(invisible(prettydf))
		}
}
