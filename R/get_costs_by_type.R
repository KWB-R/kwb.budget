#' Helper function: prepare costs by type
#'
#' @param costs_overview  costs data
#'
#' @return df with costs by type (e.g. SME, research, ..)
#' @export
#' @importFrom dplyr group_by summarize mutate arrange desc
#'
get_costs_by_type <- function (costs_overview)
{
  # create summary by participant type
  cost_data_by_type <- costs_overview %>%
    dplyr::rename(Type = .data$partner_type) %>%
    dplyr::group_by(.data$Type) %>%
    dplyr::summarize(
      Total_funded_cost = round(sum(.data$Total_funded_cost), digits = 2)
    ) %>%
    dplyr::mutate(
      Total_funded_cost_p = round(
        100 * .data$Total_funded_cost / sum(.data$Total_funded_cost),
        digits = 2
      ),
      Type = as.character(.data$Type)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Total_funded_cost)) %>%
    as.data.frame()

  # show funded costs by type
  rbind(
    cost_data_by_type,
    c(
      "Total",
      sum(cost_data_by_type$Total_funded_cost),
      sum(cost_data_by_type$Total_funded_cost_p)
    )
  ) %>%
    dplyr::mutate(
      Total_funded_cost = as.numeric(.data$Total_funded_cost),
      Total_funded_cost_p = as.numeric(.data$Total_funded_cost_p)
    )
}


#' Helper function: prepare costs by sector
#'
#' @param costs_overview  costs data
#'
#' @return df with costs by type (e.g. health, water, it, ..)
#' @export
#' @importFrom dplyr group_by summarize mutate arrange desc
#'
get_costs_by_sector <- function (costs_overview)
{
  # create summary by participant type
  cost_data_by_sector <- costs_overview %>%
    dplyr::rename(sector = .data$partner_sector) %>%
    dplyr::group_by(.data$sector) %>%
    dplyr::summarize(
      Total_funded_cost = round(sum(.data$Total_funded_cost), digits = 2)
    ) %>%
    dplyr::mutate(
      Total_funded_cost_p = round(
        100 * .data$Total_funded_cost / sum(.data$Total_funded_cost),
        digits = 2
      ),
      sector = as.character(.data$sector)
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Total_funded_cost)) %>%
    as.data.frame()

  # show funded costs by sector
  rbind(
    cost_data_by_sector,
    c(
      "Total",
      sum(cost_data_by_sector$Total_funded_cost),
      sum(cost_data_by_sector$Total_funded_cost_p)
    )
  ) %>%
    dplyr::mutate(
      Total_funded_cost = as.numeric(.data$Total_funded_cost),
      Total_funded_cost_p = as.numeric(.data$Total_funded_cost_p)
    )
}
