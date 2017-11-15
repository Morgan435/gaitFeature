#' Resample curves to t=0,...,100 using splines.
#' @param x data.frame with kinematics data in the wide format.
#' @param ... column names (bare) of the kinematics data. It should be specified \link[dplyr]{select} style.
#' @param plot logical. If \code{TRUE} a plot of the observed values superimposed with the inteporalted values are generated. For large dataset the plot might be slow to generate and hard to interpret visually.
#' @return same as \code{x} but \code{...} replaced by columns 0,1,...,100.
#' @details The resampling (intepolation) is done using periodic cubic spline. In particular, \link{splinefun} is the workhorse function.
#' @examples
#' X=kinematics%>%
#' filter(joint=="Knee",plane=="sag")%>%
#'   select(curve_id,`0`,`20`,`40`,`60`,`80`,`100`)
#' z=resamp101(X,`0`:`100`,plot=T)
#' @export resamp101
#' @importFrom dplyr one_of

resamp101=function(x,...,plot=FALSE){
  # browser()
  suppressWarnings({
    X=x%>%
      select(...)
    t0=seq(0,100,l=ncol(X))
    y=t(apply(X,1,function(d)splinefun(x=t0,y=d,method="periodic")(x=0:100)))
    colnames(y)=0:100
    todrop=colnames(x%>%select(...))
    z=x%>%
      select(-one_of(todrop))%>%
      cbind(y)

    if(plot){
      matplot(x=t0,y=x%>%select(...)%>%t,pch=1,xlab="t",ylab="angle",main="points: observed\n lines: intepolated")
      matplot(x=0:100,y=z%>%select(`0`:`100`)%>%t,type="l",lty=1,add=T)
    }
  })


  z

}
