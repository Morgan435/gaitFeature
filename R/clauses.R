#' Feature clauses
#'
#' Convenient functions to integrate a clause into a feature definition.
#' @param x dataframe of gait kinematics values. Must have columns curve_id, 0, 1, ..., 100
#' @param ... the columns of x which \code{.f} should apply to. It should really be the time domain. E.g. `60`:`100` (including the back tick).
#' @param .f a function to summarize the kinematics. See
#' @param .dir a single string. Either \code{>(=)},\code{<(=)},\code{\%o\%}, or \code{\%w\%}. This represents the comparative direction of the clause. The last two values represent 'outside' and 'within' respectively.
#' @param .c numeric. The comparative threshold. See \code{Details}.
#' @param TC a matrix of typical feature curves. Each row represent each curve, and the columns must be 0,1,...,100. See \code{Details}.
#' @param cond a custom condition statement. See \code{Details}.
#' @param k numeric, the number of standard deviation used to form the comparative threshold. Ignored if \code{.c} is specified.
#' @param clDesc a character string which describes this clause. It must begin with "Cl:" which stands for 'Clause:'. If \code{NULL} (default) it is automatically constructed as \code{Cl:f(...).dir.c}
#' @param featurename Character string. The human readable name of the feature.
#' @param fname Character string. A machine friendly version of featurename. It is recommended that you do not use any special character in this field.
#' @details A feature consists of n>=1 clause(s). Each clause usually works by comparing some statistics between the curve under consideration and a reference set.
#' Specifically, each clause works as followed:
#' \enumerate{
#'   \item Select the kinematics at the relevant time points as specified by \code{...}, call this \code{x[...]}.
#'   \item Apply \code{.f} to \code{x[...]} and the reference curve.
#'   \item If \code{.c} is not specified, compute it. (e.g. mean(reference curve stats)+\code{k}standard deviation).
#'   \item If \code{.f(x[...]) .dir .c}, then it passess the clauses, thus raising a positive flag.
#' }
#'
#' The \code{unistat} function stands for univariate statistics clause. This should be the most frequently used clause.
#'
#' The \code{corr} function  stands for correlation clause. A set of target curve, \code{TC}, with which the curve under consideration will be correlated against, needs to be provided.
#' The \code{.c} in this context becomes the correlation threshold so it should be between -1 and 1.
#'
#' The \code{custom} function allows user to specify other clauses that could not be (easily) specified by either \code{unistat} or \code{corr}.
#' This is useful for timing clause. e.g. \code{custom(`60`:`100`,cond="t[which.max(angle)]>75")}.
#'
#' If these convenience functions are used, then the result should always be passed to the \code{finish} function which formats the output properly.
#' For example, if a feature consist of multiple clauses, the \code{finish} function will check that each clauses are passed (i.e. positive) in order to classify the feature as being detected.
#'
#' @examples
#' library(dplyr)
#' .dtEPS=function(df){
#'   df%>%
#'   filter(joint=="FootProgress"&plane=="tra")%>%
#'   unistat(`60`:`100`,.f=.ROM,.dir=">",k=2)%>%
#'   corr(`0`:`100`,TC=.tc()$ett)%>%
#'   finish(featurename = "External Foot Progression (Wave) in Swing only.",fname = "EPS")
#' }
#'
#' .dtEPS(kinematics)
#'
#' @return the same as \code{x} but with an added column which indicate whether the row passes the criterion, and an inherited class as specified by the \code{class} argument.
#' @importFrom dplyr %>% select select_ summarize summarize_at summarize_ mutate mutate_at mutate_if mutate_ group_by ungroup filter left_join inner_join starts_with full_join num_range
#' @importFrom tidyr gather spread
#' @export unistat
#' @export corr
#' @export custom
#' @export finish

unistat=function(x,...,.f,.dir,.c,k=1,clDesc=NULL){
  # browser()
  if(!is.null(clDesc)){
    if(substr(clDesc,1,3)!="Cl:")stop("clDesc should begins exactly with 'Cl:'")
  }

  if(is.null(clDesc))clDesc=sprintf("Cl:%s(%s)%s%s",
                                        deparse(substitute(.f)),
                                        gsub("\`","",deparse(substitute(...))),
                                        .dir,
                                        ifelse(missing(.c),paste0(k,"sd"),.c))


  if(missing(.c)){
    if(.dir=="==")stop("direction == can only work with absolute threshold .c but not # s.d. k.")
    ss=getRC(x$joint[1],x$plane[1])%>%
      select(...)%>%
      apply(1,.f)
    ll=mean(ss)-k*sd(ss)
    ul=mean(ss)+k*sd(ss)
    if(grepl("<",.dir)).c=ll
    if(grepl(">",.dir)).c=ul
  }
  crit=switch(.dir,
              "%w%"=paste0("angle<=(",ul,")&angle>=(",ll,")"),
              "%o%"=paste0("angle>(",ul,")|angle<(",ll,")"),
              paste0("angle",.dir,"(",.c,")"))


  # browser()
  pass=x%>%
    group_by(curve_id)%>%
    select(curve_id,...)%>%
    gather("t","angle",-curve_id,convert=TRUE)%>%
    summarize_at("angle",.f)%>%
    mutate_(pass=crit)%>%
    ungroup%>%
    select(curve_id,pass)

  colnames(pass)[colnames(pass)=="pass"]=clDesc

  x%>%inner_join(pass,by="curve_id")
}


#' @describeIn unistat correlation clause
corr=function(x,...,TC,.dir=">=",.c=0.8,clDesc=NULL){

  if(!is.null(clDesc)){
    if(substr(clDesc,1,3)!="Cl:")stop("clDesc should begins exactly with 'Cl:'")
  }

  crit=paste0("angle",.dir,"(",.c,")")

  if(is.null(clDesc))clDesc=sprintf("Cl:corr(%s)%s%s",
                                        gsub("\`","",deparse(substitute(...))),
                                        .dir,
                                        .c)
  # browser()
  pass=x%>%
    group_by(curve_id)%>%
    select(curve_id,...)%>%
    gather("t","angle",-curve_id,convert=TRUE)%>%
    summarize(angle=max(apply(TC,1,cor,angle)))%>%
    mutate_(pass=crit)%>%
    ungroup%>%
    select(curve_id,pass)

  colnames(pass)[colnames(pass)=="pass"]=clDesc

  x%>%inner_join(pass,by="curve_id")

}

#' @describeIn unistat custom clause
custom=function(x,...,cond,clDesc){
  if(!is.character(cond))stop("argument 'cond' must be a string.")
  pass=x%>%
    group_by(curve_id)%>%
    select(curve_id,...)%>%
    gather("t","angle",-curve_id,convert=TRUE)%>%
    summarize_(pass=cond)%>%
    ungroup%>%
    select(curve_id,pass)

  if(missing(clDesc))clDesc=paste0("Cl:",cond)

  colnames(pass)[colnames(pass)=="pass"]=clDesc

  x%>%inner_join(pass,by="curve_id")
}

#' @describeIn unistat finish
finish=function(x,featurename,fname){
  # browser()
  if(length(unique(x$joint))>1)stop("Feature should apply to 1 joint only.")
  if(length(unique(x$plane))>1)stop("Feature should apply to 1 plane only.")

  out=x%>%
    mutate(detected=x%>%
    select(starts_with("Cl:"))%>%
    apply(1,prod)%>%
    unlist(use.names=F))%>%
    select(curve_id,starts_with("Cl:"),!!fname:=detected)

  attr(out,"featurename")=featurename
  attr(out,"fname")=fname
  attr(out,"joint")=x$joint[1]
  attr(out,"plane")=x$plane[1]

  out
}

