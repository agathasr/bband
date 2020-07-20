#' B2 band
#'
#' \code{band_B2}  return a matrix with B2 band.
#'
#' @param post  Matrix with dimension n.time by n.size containing posterior values for n.time times and n.size posterior sample.
#' @param alpha Value inside the interval (0,1) in order to get a credibility level of 100(1 - alpha)%.
#' @param density_aux Posterior density evaluated in each MCMC values of the parameters.
#'
#' @return A list with the following components:
#'    \item{B2}{A matrix with n.time rows and 2 columns with B2 band.}
#'    \item{alpha}{The information about the credibility level of 100(1 - alpha)%.}
#' @export
#'
band_B2 <- function(post,alpha,density_aux){
  post_al <- quantile(sort(density_aux),prob=alpha)
  C2 <- which(density_aux>=post_al)
  ID <- 1:dim(post)[2]
  id.t <- 1:dim(post)[1]
  B2 <- cbind(purrr::map_dbl(id.t,function(i){min(post[i,ID%in%C2])}),
              purrr::map_dbl(id.t,function(i){max(post[i,ID%in%C2])}))
  return(list(B2=B2,alpha=alpha))
}
