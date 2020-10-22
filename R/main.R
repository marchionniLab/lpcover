#' Cover a proportion of a given binary set with the smallest number of features 
#'
#' Function for computing the minimal covering for a given binary data matrix given 
#' a minimum proportion of samples to cover
#'
#' @param mat A binary data matrix with each column corresponding to a sample and each row 
#' corresponding to a feature.
#'
#' @param alpha A value in the 0 <= alpha < 1 range indicating what proportion of samples to be 
#' considered as outlier. By default alpha = 0.05, indicating 95% of samples to be covered.
#' 
#' @param maxsol The number of optimal solutions to be returned. Default is 100.
#'
#' @param J The number of times each sample is to be covered. By default J=1, indicating
#' that each sample is to be covered with at least one feature.
#'
#' @param solver A character string indicating whether to use gurobi or lpSolve.
#'
#' @return A list with items "obj": the objective returned by the optimization (as a vector), 
#' "sol": a character matrix of solutions(each column a solution), "r": a list where each element contains vectors of 
#' results obtained for x and lamba vectors, and "result": the direct output returned by the 
#' optimization (by either gurobi or lpSolve).
#'
#' @keywords cover, optimize
#' @export
#'
#' @examples
#' optim.out = computeMinimalCovering(mat=mat, alpha=0.05, maxsol=1, J=1, solver="lpSolve")
#' 

computeMinimalCovering <- function(mat, alpha=0.05, maxsol=100, J=1, solver=""){
  
  # check: 
  
  # 0 <= alpha < 1
  if(alpha < 0 || alpha >= 1)
    stop("ERROR: alpha should be in the [0, 1) interval")
  
  # maxsol, integer, > 0
  if( ! (maxsol > 0 && (maxsol %% 1 == 0)) )
    stop("ERROR: maxsol should be a positive integer")
  
  # J > 0, integer
  if( ! (J > 0 && (J %% 1 == 0)) )
    stop("ERROR: J should be a positive integer")
  
  # mat should be binary, no missing values
  if(! is.matrix(mat))
    stop("ERROR: mat should be a matrix")
  
  if(any(is.na(mat)))
    stop("ERROR: mat cannot have missing values")
  
  if(! all(sort(unique(as.vector(mat))) == c(0, 1)))
    stop("ERROR: mat must be binary")
  
	getCovering(mat=mat, alpha=alpha, maxsol=maxsol, J=J, solver=solver)
  
}

# ==================================================================================================





