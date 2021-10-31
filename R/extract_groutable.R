#' Transform the result of compareGroups to a data frame for further use
#'
#' @param groutable the result of createTable in compareGroups package
#'
#' @return a data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' groutable(data = iris, variable = 5)  |> extract_groutable ()
extract_groutable <- function(groutable){
  stopifnot(inherits(groutable, "createTable"))
  res <- groutable$descr  |> as.data.frame()
  return(res)
}
