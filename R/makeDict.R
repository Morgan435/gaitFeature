#' Construct Feature Dictionary
#' @param fns an option character vector of feature definition functions. This list is the basis of the feature dictionary. If \code{NULL} all functions which begins with \code{.dt} is included.
#' @param fn a feature definition function, either as string or actual function object.
#' @details \code{parseF} works by scanning the body of the feature definition function (e.g. \code{body(.dtAIR)}), and look for any \code{joint}, \code{plane}, \code{fname}, and \code{featurename} specification.
#' @examples
#' makeFDict()
#' makeFDict(c(".dtAIR",".dtDblBump"))
#'
#' parseF(.dtAIR)
#' @export makeFDict
#' @export parseF



makeFDict=function(fns=NULL){
  if(is.null(fns))fns=ls(pattern = "^.dt",all.names=T,name =  "package:gaitFeature")

  for(f in fns)if(!exists(f))stop(sprintf("%s does not exist!",f))

  parsed=lapply(fns,parseF)
  names(parsed)=fns

  dplyr::bind_rows(parsed,.id="fn")
}

#' @rdname makeFDict
parseF=function(fn){
  # browser()
  body=as.character(body(fn)[[2]])

  filter=body[[grep("filter",body)]]
  finish=body[[grep("finish",body)]]
  joint=gsub('.*joint[= \"]+([^\"]+)\".*',"\\1",filter)
  plane=gsub('.*plane[= \"]+([^\"]+)\".*',"\\1",filter)
  fname=gsub('.*fname[= \"]+([^\"]+)\".*','\\1',finish)
  featurename=gsub('.*featurename[= \"]+([^\"]+)\".*','\\1',finish)
  list(joint=joint,plane=plane,fname=fname,featurename=featurename)

  # temp=suppressMessages(do.call(fn,list(kinematics)))
  # list(joint=attr(temp,"joint"),
  #      plane=attr(temp,"plane"),
  #      fname=attr(temp,"fname"),
  #      featurename=attr(temp,"featurename"))
}
