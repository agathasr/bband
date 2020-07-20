#' This function returns the pointwise interval with credibility level of 100(1 - alpha)% and equal tails (each tail has alpha/2 area).
aux_equal_Int <- function(post_matrix,alpha){
  out <- cbind(t(apply(post_matrix,1,function(x) quantile(sort(x),prob=c(alpha/2,1-alpha/2)))) )
  return(out)
}

#' This function returns the pointwise HPD interval with credibility level of 100(1 - alpha)%.
aux_hpd_Int <- function(post_matrix,alpha){
  out <- cbind(t(apply(post_matrix,1,function(x) TeachingDemos::emp.hpd(x,conf=1-alpha))) )
  return(out)
}

#' This function calculates the band.
aux_IntCont <- function(post_matrix,int){
  ID <- 1:dim(post_matrix)[1]
  out <- cbind(t(apply(post_matrix,1,function(x)
    {c(min(x[ID%in%int]),max(x[ID%in%int]))})))
  return(out)
}
