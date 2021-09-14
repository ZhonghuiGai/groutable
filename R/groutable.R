#' This is a wrapper function of compareGroups package
#'
#' @param data a data frame
#' @param variable A numeric to indicate which variable to be used for grouping
#' @param writeTab A boolean value to indicate whether to save the result or not
#' @param method 1 - forces analysis as "normal-distributed";2 - forces analysis as "continuous non-normal";
#' 3 - forces analysis as "categorical";4 - NA, which performs a Shapiro-Wilks test to decide between normal or non-normal.
#' @param digits show the number of digits
#' @param write.p.adjust logical value indicates whether write the adjustd p values
#' @param file.name the input file name, a character
#'
#' @return a statistical table
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' groutable(data = iris, variable = 5)
#' groutable(data = iris[1:100,], variable = 5)
#' groutable(data = droplevels(iris[1:100,]), variable = 5)
#' groutable(data = droplevels(iris[1:100,]), variable = 5, digits = 2)
#' groutable(data = droplevels(iris[1:100,]), variable = 5, digits = 2, method = 4)
#' data <- iris
#' data$Species <- as.character(data$Species)
#' groutable(data = droplevels(data[1:100,]), variable = 5, digits = 2, method = 4)
groutable <- function(data, variable = 1,
                      method = 1,
                      writeTab = FALSE,
                      digits = 3,
                      write.p.adjust = FALSE,
                      file.name = NULL){
  data <- as.data.frame(data)
  name <- names(data)
  show.ratio <- nlevels(data[, variable]) == 2
  formula <- as.formula(paste0(name[variable], "~."))
  t1 <- compareGroups::compareGroups(formula = formula, data = data,
                                     method = method,
                                     riskratio = FALSE,
                                     max.ylev = 7)
  t2 <- compareGroups::createTable(t1, digits = digits, digits.ratio = 3, digits.p = 3,
                                   show.ratio = show.ratio,
                                   show.all = FALSE, #Default value
                                   show.p.overall = TRUE, #Default value
                                   show.p.mul = TRUE,
                                   show.p.trend = FALSE, #Default value
                                   show.descr = TRUE, #Default value
                                   show.p.ratio = show.ratio,
                                   show.ci = FALSE, #Default value
                                   type = 2, # Default value
                                   hide.no = "no",
                                   q.type = c(1,3),
                                   sd.type = 2) # 1-mean (SD), 2-mean ± SD.
  p.value <- compareGroups::getResults(t1, what = "p.overall")
  p.value <- as.data.frame(p.value)
  p.value$p.adjust <- p.adjust(p.value$p.value, method = "BH")
  if(writeTab){
    compareGroups::export2xls(t2, file = paste(name[variable], file.name,
                                               "compare_table.xlsx", sep = "_"),
                              which.table = "descr")
    if (write.p.adjust) {
      write.table(p.value, file = paste(name[variable],
                                        "compare_table_pvalue.txt", sep = "_"),
                  sep = "\t", quote = FALSE)
    }
  }
  return(t2)
}
