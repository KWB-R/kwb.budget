# to_cost_matrices -------------------------------------------------------------

#' Summarise all Cost Tables
#'
#' @param costs_by_wp list of cost tables, as returned by
#'   \code{kwb.budget:::get_all_cost_sheets} in element \code{by_wp}
#' @importFrom kwb.utils countOrSum selectColumns
#' @export
to_cost_matrices <- function(costs_by_wp)
{
  columns <- names(costs_by_wp)
  first_two <- columns[1:2]

  lapply(
    X = stats::setNames(nm = setdiff(columns, first_two)),
    FUN = function(column) {
      costs_by_wp %>%
        kwb.utils::selectColumns(c(first_two, column)) %>%
        kwb.utils::countOrSum(by = first_two, sum.up = column)
    }
  )
}
