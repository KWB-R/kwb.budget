##' Helper function: append zero costs
##' @param x x
##' @return data frame with zero costs
##' @export
##' @importFrom kwb.utils noFactorDataFrame safeRowBind
##'
append_zero_costs <- function(x) {
  kwb.utils::safeRowBind(x, kwb.utils::noFactorDataFrame(
    partner = unique(x$partner), wp = 1:7, cost = 0
  ))
}



##' Get Costs by Work Package
##' @param costs list with "costs" from multiple partner Excelsheets
##'
##' @return data frame with costs per work package
##' @export
##'
##' @importFrom kwb.utils defaultIfNA getAttribute rbindAll
##' @importFrom dplyr group_by summarise
##' @importFrom rlang .data
##'
get_costs_by_work_package <- function(costs)
{

  collect_lines_with_work_package <- function(x, name) {
    result <- lapply(x, kwb.utils::getAttribute, name)
    result <- lapply(result, append_zero_costs)
    result <- kwb.utils::rbindAll(result)
    result$cost <- kwb.utils::defaultIfNA(result$cost, 0)
    result[! is.na(result$wp), ]
  }

  personnel <- collect_lines_with_work_package(costs, "personnel")
  equipment <- collect_lines_with_work_package(costs, "equipment")
  consumables <- collect_lines_with_work_package(costs, "consumables")
  subcontracting <- collect_lines_with_work_package(costs, "subcontracting")

  sum_by_work_package <- function(x) {
    x %>%
      dplyr::group_by(.data$partner, .data$wp) %>%
      dplyr::summarise(cost = sum(.data$cost))
  }

  sum_pm_by_work_package <- function(x) {
    x %>%
      dplyr::group_by(.data$partner, .data$wp) %>%
      dplyr::summarise(person_months = sum(.data$person_months, na.rm = TRUE))
  }

  kwb.utils::mergeAll(by = c("partner", "wp"), list(
    personnel = sum_pm_by_work_package(personnel),
    personnel = sum_by_work_package(personnel),
    equipment = sum_by_work_package(equipment),
    consumables = sum_by_work_package(consumables),
    subcontracting = sum_by_work_package(subcontracting)
  ))
}
