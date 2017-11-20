#' Get reference curve
#' @param .joint joint name
#' @param .plane plane name
#' @return a dataframe of referene curve filtered to the specified joint and plane.
#' @seealso rc
#' @examples
#' getRC("Ankle","sag")
#' @export

getRC=function(.joint,.plane){
  .rc()%>%
    filter(joint==.joint&plane==.plane)%>%
    select(`0`:`100`)
}
