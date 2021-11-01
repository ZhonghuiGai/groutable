#' A swapped function of DT::datatable to show data frame in Viewer
#'
#' @param data a data frame
#' @param pageLength the length of page
#' @param filter a logical value to show the filter option
#'
#' @return a table showed in Viewer
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' iris  |> dt2()
#' groutable(data = iris, variable = 5)  |> extract_groutable ()  |> ying_dt()
#' groutable(data = iris, variable = 5)  |> extract_groutable ()  |> dt2()
dt2 <- function(data, pageLength = NULL, filter = "none"){
  if (is.null(pageLength)) {
    options <- list()
  } else {
    options <- list(
      initComplete = DT::JS(paste0("function(settings, json) {$(this.api().table().header()).css({'font-size':'", 15, "'});}")),
      searchHighlight = TRUE,
      paging = FALSE,
      pageLength = pageLength)
  }
  data  |>
    DT::datatable(class = "compact cell-border stripe", rownames = FALSE,
                  escape = FALSE, filter = filter, style = "auto",
                  plugins = "ellipsis", options = options)  |>
    DT::formatStyle(0:length(data), fontSize = 15, lineHeight = "90%",
                    whiteSpace = "pre-wrap")
}
