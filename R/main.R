#' Cover a proportion of a given binary set with the smallest number of features 
#'
#' Function for computing the minimal covering for a given binary data matrix given 
#' a minimum proportion of samples to cover
#'
#' @param mat A binary data matrix with each column corresponding to a sample and each row 
#' corresponding to a feature.
#'
#' @param alpha A value in the [0, 1) range indicating what proportion of samples to be 
#' considered as outlier. By default alpha = 0.05, indicating 95% of samples to be covered.
#' 
#' @param maxsol The number of optimal solutions to be returned. Default is 100.
#'
#' @param J The number of times each sample is to be covered. By default J=1, indicating
#' that each sample is to be covered with at least one feature.
#'
#' @return A list.
#'
#' @export
#'
#' @examples
#'

computeMinimalCovering <- function(mat, alpha=0.05, maxsol=100, J=1){
  
  # check: 
  
  # 0 <= alpha < 1
  
  # maxsol, integer, > 0
  
  # J > 0, integer
  
  
	getCovering(mat=mat, alpha=alpha, maxsol=maxsol, J=J)
}

# ==================================================================================================





