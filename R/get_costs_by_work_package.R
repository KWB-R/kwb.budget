##' Helper function: append zero costs
##' @param x x
#'  @param n_work_packages number of work packages in EXCEL template
#' (default: 7, as used for DWC)
##' @return data frame with zero costs
##' @export
##' @importFrom kwb.utils noFactorDataFrame safeRowBind
##'
append_zero_costs <- function(x, n_work_packages) {
  kwb.utils::safeRowBind(x, kwb.utils::noFactorDataFrame(
    partner = unique(x$partner), wp = seq_len(n_work_packages), cost = 0
  ))
}

##' Get Costs by Work Package
##' @param costs_list list with "costs" from multiple partner Excelsheets
#'  @param n_work_packages number of work packages in EXCEL template
#' (default: 7, as used for DWC)
##' @return data frame with costs per work package
##' @export
##' @importFrom kwb.utils defaultIfNA getAttribute rbindAll
##' @importFrom dplyr group_by summarise
##' @importFrom rlang .data
##'
get_costs_by_work_package <- function(costs_list, n_work_packages = 7)
{
  # costs_list must be a list
  stopifnot(is.list(costs_list))

  # All list elements must be data frames
  stopifnot(all(sapply(costs_list, inherits, "data.frame")))

  collect_lines_with_work_package <- function(x, name) {
    result <- lapply(x, kwb.utils::getAttribute, name)
    result <- lapply(result, append_zero_costs, n_work_packages)
    result <- kwb.utils::rbindAll(result)
    result$cost <- kwb.utils::defaultIfNA(result$cost, 0)
    result[! is.na(result$wp), ]
  }

  #x <- costs_list
  #name = "personnel"

  personnel <- collect_lines_with_work_package(costs_list, "personnel")
  equipment <- collect_lines_with_work_package(costs_list, "equipment")
  consumables <- collect_lines_with_work_package(costs_list, "consumables")
  subcontracting <- collect_lines_with_work_package(costs_list, "subcontracting")

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
