#' Show data frame in Viewer, adapted from yingtools2
#'
#' Use to peruse a dataframe within RStudio. Utilizes \code{DT} package.
#'
#' If data frame is grouped (i.e. \code{group_by} in dplyr), the rows will be sorted and shaded by group.
#'
#' @param data dataframe to be viewed.
#' @param fontsize numeric controlling font size in the table, measured in px. Default is 11.
#' @param maxchars max number of characters before adding an ellipsis \code{...}. Default is 250.
#' @param whiteSpace CSS property sets how white space inside an element is handled. Default is "pre-wrap".
#' @param pageLength number of rows to display per page (Default \code{Inf}, show all rows)
#' @param maxrows numeric controlling max number of rows to display. The purpose is to prevent \code{DT} from handling excessively large data frames. Default is 1000.
#' @param rownames whether or not to show row names (passed directly to \code{\link[DT:datatable]{DT::datatable}}).
#' @param class the CSS class(es) of the table (passed directly to \code{\link[DT:datatable]{DT::datatable}}).
#' @param escape whether to escape HTML entities in the table (passed directly to \code{\link[DT:datatable]{DT::datatable}}).
#'
#' @export
#' @author Zhonghui Gai
#' @return A javascript-style datatable, which displays in the Rstudio viewer.
#' @examples
#' groutable(data = iris, variable = 5)  |> extract_groutable ()  |> dt()
#' kableExtra::kable_styling(kableExtra::kable(groutable(data = iris, variable = 5)  |>
#' extract_groutable (), "html"))
dt <- function(data, fontsize = 15, pageLength = Inf,
               maxchars = 250, maxrows = 500,
               rownames = FALSE, escape = FALSE,
               class = "compact cell-border stripe",
               whiteSpace = "pre-wrap") {
  requireNamespace(c("DT", "forcats"), quietly = TRUE)
  fontsize <- paste0(fontsize, "px")
  n.cols <- ncol(data)
  index_col <- n.cols + rownames

  pal <- c("white", "seashell", "aliceblue")
  indices <- seq_along(pal)
  clrs.rgb <- paste0("rgb(", apply(col2rgb(pal), 2 , function(x) paste(x, collapse=",")), ")")
  data$index_ <- data  |>  dplyr::group_indices()  |>  factor()  |>
    forcats::fct_inorder()  |> as.numeric()
  data <- data  |>  arrange(index_)  |>
    mutate(index_ = ((index_-1) %% length(pal)) + 1)  %>%
    select(!!!groups(.), -index_, everything())  |>  ungroup()
  add <- function(l,...) {
    if (is.list(l)) {
      c(l, list(...))
    } else {
      c(l, ...)
    }
  }
  plugins <- c()
  options <- list()
  columnDefs <- list()
  ## ellipsis
  plugins <- add(plugins, "ellipsis")
  columnDefs <- add(columnDefs, list(
    targets = 1:n.cols,
    render = DT::JS("$.fn.dataTable.render.ellipsis( ", maxchars, " ,true, true)")
  ))
  ## header font size
  options <- add(options,
                 initComplete = DT::JS(paste0("function(settings, json) {$(this.api().table().header()).css({'font-size':'",
                                            fontsize, "'});}")))
  options <- add(options, searchHighlight = TRUE)
  options <- add(options, paging = !is.infinite(pageLength),
                 pageLength = pmin(pageLength, maxrows))
  ## make index invisible
  columnDefs <- add(columnDefs, list(
    targets = index_col,
    visible = FALSE
  ))
  options <- add(options, columnDefs = columnDefs)
  output <- data  |>
    filter(row_number() <= maxrows)  |>
    DT::datatable(plugins = plugins, class = class, options = options,
                  escape = escape, rownames = rownames)  |>
    DT::formatStyle(0:length(data), fontSize = fontsize, lineHeight = "95%",
                    whiteSpace = whiteSpace)
  if (nrow(data)>0) {
    output <- output  |>
      DT::formatStyle("index_", target = "row",
                      backgroundColor = DT::styleEqual(indices, clrs.rgb))
  }
  return(output)
}

##' @importFrom magrittr %>%
##' @export
magrittr::`%>%`
