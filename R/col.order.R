#' Sorted by descending mean for each column
#'
#' @param data a data frame containing the grouping information in the first column
#' @param group NA or a numeric value indicate which variable containing the grouping inf.
#'
#' @return a ordered data frame
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' col.order(data = iris, group = 5)
#' col.order(data = iris[1:4], group = NA)
col.order <- function(data, group = 1){
  data <- as.data.frame(data)
  if (is.numeric(group)) {
    data1 <- data[, -group]
    if (all(sapply(data1, is.numeric))) {
      ind <- order(colMeans(x = data1), decreasing = T)
      data1 <- data1[, ind]
      data <- cbind(data[, group, drop = FALSE], data1)
    }else{
      stop("All the variables must be numeric!")
    }
  }else if (is.na(group)) {
    if (all(sapply(data, is.numeric))) {
      ind <- order(colMeans(x = data), decreasing = T)
      data <- data[, ind]
    }else{
      stop("All the variables must be numeric!")
    }
  }
  return(data)
}
