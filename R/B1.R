#' B1 band
#'
#' \code{band_B1}  return a matrix with B1 band.
#'
#' @param post  Matrix with dimension n.time by n.size containing posterior values for n.time times and n.size posterior sample.
#' @param alpha Value inside the interval (0,1) in order to get a credibility level of 100(1 - alpha)%.
#' @param interval Choice between "equal" and "hpd". The "equal" considers equal tails (each tail has alpha/2 area) and "hpd" considers the HPD interval.
#'
#' @return A list with the following components:
#'    \item{B1}{A matrix with n.time rows and 2 columns with B1 band.}
#'    \item{alpha}{The information about the credibility level of 100(1 - alpha)%.}
#'    \item{interval}{If "equal" or "hpd".}
#' @export
#'
band_B1 <- function(post,alpha,interval){
  if(interval=='equal'){
    Int <- aux_equal_Int(post,alpha)
  } else if(interval=='hpd'){
    Int <- aux_hpd_Int(post,alpha)
  } else{
    print("Interval sould be equal or hpd.")
  }
  inside_interval <- function(i,k) {(post[i,k]>=Int[i,1] & post[i,k]<=Int[i,2])==1}
  obj_1 <- 1:dim(post)[1]
  obj_2 <- 1:dim(post)[2]
  fori <- function(i){
    aux <- purrr::map_dbl(obj_2,inside_interval, i = i)
    aux1 <- which(aux==1)
  }
  C <- purrr::map(obj_1,fori)
  aj <- function(caux){
    out <- aux_IntCont(post,caux)
    return(out)
  }
  exit1 <- purrr::map(C, aj)
  exit <- purrr::flatten_dbl(purrr::map(exit1,function(a)(sum(a[,2]-a[,1]))))
  B1 <- exit1[[which.min(exit)]]
  return(list(B1=B1,alpha=alpha,interval=interval))
}
