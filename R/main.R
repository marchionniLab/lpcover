#' Cover a proportion of a given binary set with the smallest number of features 
#'
#' Function for computing the minimal covering for a given binary data matrix given 
#' a minimum proportion of samples to cover
#'
#' @param mat A binary data matrix with each column corresponding to a sample and each row 
#' corresponding to a feature.
#'
#' @return A list.
#'
#' @export
#'
#' @examples
#'

computeCovering <- function(mat, alpha=0.05, maxsol=100, J=1){
	getCovering(mat=mat, alpha=alpha, maxsol=maxsol, J=J)
}

# ==================================================================================================





