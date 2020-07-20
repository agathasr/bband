#' Pointwise band
#'
#' \code{band_PW}  return a matrix with pointwise band.
#'
#' @param post  Matrix with dimension n.time by n.size containing posterior values for n.time times and n.size posterior sample.
#' @param alpha Value inside the interval (0,1) in order to get a credibility level of 100(1 - alpha)%.
#' @param interval Choice between "equal" and "hpd". The "equal" considers equal tails (each tail has alpha/2 area) and "hpd" considers the HPD interval.
#'
#' @return A list with the following components:
#'    \item{PW}{A matrix with n.time rows and 2 columns with pointwise band.}
#'    \item{alpha}{The information about the credibility level of 100(1 - alpha)%.}
#' @export
#'
band_PW <- function(post,alpha,interval){
  if(interval=='equal'){
    Int <- aux_equal_Int(post,alpha)
  } else if(interval=='hpd'){
    Int <- aux_hpd_Int(post,alpha) ### POR QUE PRECISO COLCOAR AS.MATRIX??
  } else{
    print("Interval sould be equal or hpd.")
  }
  return(list(PW=Int,alpha=alpha))
}
