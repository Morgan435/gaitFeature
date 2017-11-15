#' Reconstruct Left Pelvis curves from Right curves
#'
#' @param data dataframe with columns curve_id, side, 0,1,...,100, and whatever is specified by the \code{ofs} argument.
#' @param ofs the column name (quoted) of \code{data} which specifies the time point of opposite foot strike.
#' @param flipped logical. If TRUE the Right curves are flipped about the 0 degree axis.
#' This is needed when reconstructing the coronal and transverse plane pelvis curves, but not for the saggital curves.
#' @return same as data but the right curves are reconstructed to look like the left curves.
#' @details
#' Since there is only 1 pelvis for each person, the information conveyed by the left and right curve should be the same.
#' However, the left and right curves do not typically look the same.
#' Therefore, it \emph{might} be useful to homogenize them so we are only looking at 1 set of patterns.
#'
#' \code{PelR2L} reconstruct the left curves from the right curves.
#' It basically cuts out the second part of the curve, from the time of opposite (left) foot strike to the end of the gait cycle (t=100),
#' and paste it before the original beginning of the curve (t=0:t[opposite foot off]).
#' For the coronal and transverse plane, the whole curve can be flipped vertically about the 0 axis. i.e. multiplied by -1.
#'
#' @examples
#' ## make some fake opposite foot strike time point
#' kinematics$ofs=sample(58:62,nrow(kinematics),replace=T)
#' homogenized=PelR2L(subset(kinematics,joint=="Pel"&plane=="sag"),"ofs")
#' matplot(t(homogenized%>%select(`0`:`100`)),type="l",lty=1,col=ifelse(temp$side=="R","blue","red"))
#' matplot(t(subset(kinematics,joint=="Pel"&plane=="sag")%>%select(`0`:`100`)),type="l",lty=1,col=ifelse(temp$side=="R","blue","red"))
#' PelR2L(subset(kinematics,joint=="Pel"&plane=="cor"),"ofs",flipped=TRUE)
#' @export


PelR2L=function(data,ofs,flipped=F){
  # browser()
  if(is.null(data[[ofs]]))stop("data must have a column with name specified by argument ofs (opposite foot strike)")
  if(is.null(data$side))stop("data must have column 'side' with two levels: L, R")
  if(!all(unique(data$side)%in%c("L","R")))stop("data d must have column 'side' with two levels: L, R")

  if(max(data[[ofs]])<1)data[[ofs]]=data[[ofs]]*100
  data[[ofs]]=round(data[[ofs]])

  R=data%>%
    filter(side=="R")%>%
    select(curve_id,`0`:`100`,ofs)

  R2=lapply(1:nrow(R),function(i){
    R1=R[i,]%>%
      select(curve_id,get(as.character(R[i,ofs]+1)):`100`,`0`:get(as.character(R[i,ofs])))%>%
      as.matrix
  })%>%
    Reduce(rbind,.)%>%
    as.data.frame%>%
    setNames(c("curve_id",0:100))

  if(flipped)R2=R2%>%mutate_at(vars(`0`:`100`),`-`)

  L=data%>%
    filter(side=="L")%>%
    select(curve_id,`0`:`100`)

  data%>%
    select(-(`0`:`100`))%>%
    inner_join(rbind(L,R2))

}
