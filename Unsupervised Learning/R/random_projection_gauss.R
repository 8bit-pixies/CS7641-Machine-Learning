#' random projections

#' starting with a matrix A, with n records, and m features
#' we want to reduce it to E, which is nxk features
#' 
#' what we need to do is produce a matrix R which is m by k
#' and fill it with eps, -1, 1 with probability 2/3, 1/6, 1/6
#' 
#' Then we will just go ahead and multy AxR to find E, there are 
#' various shortcuts to make it really fast! but lets not worry about that
#' 
#' this code is a mix of the sklearn code `random_projection.py` and also here: 
#' http://www.cs.ucsc.edu/~optas/papers/jl.pdf

johnson_loindenstrauss_min_dim <- function(n_samples, eps=0.1) {
  denominator = (eps ** 2 / 2) - (eps ** 3 / 3)
  # the min function is required in this case, due to the theorem 
  # which says that we require k0 to be less than k. 
  return (min(floor(4 * log(n_samples) / denominator),n_samples))
}

gaussian_random_proj <- function(row, col) {
  mat <- rnorm(row*col, 0, 1/(row**0.5))
  dim(mat) <- c(row, col)
  return(mat)
}

gaussian_random_projection <- function(A, n_features=NULL, eps=0.1) {
  # convert to matrix if the format is a dataframe.
  if (is.data.frame(A)) {
    #check_numeric types
    if (sum(sapply(A, is.numeric)) != length(names(A))){
      warning("Not all columns are numeric. Non-numeric columns will be ignored.")
    }
    A <- as.matrix(A[, sapply(A, is.numeric)])        
  }
  
  get_dim <- dim(A)
  if (is.null(n_features)){
    n_features = johnson_loindenstrauss_min_dim(get_dim[2]) # we want to reduce the number of features!
  }
  R = gaussian_random_proj(get_dim[2], n_features)
  
  return(list(A=A, R=R, RP=A %*% as.matrix(R)))
}

rowA <- 4
colA <- 4
A <- rnorm(rowA*colA)
dim(A) <- c(rowA, colA)

E <- gaussian_random_projection(A, n_features=2)

