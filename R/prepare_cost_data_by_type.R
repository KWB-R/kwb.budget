#' Helper function: prepare costs by type
#'
#' @param costs_data  costs data
#'
#' @return df with costs by type (e.g. SME, research, ..)
#' @export
#' @importFrom dplyr group_by summarize mutate
#'
prepare_cost_data_by_type <- function (costs_data){

  # create summary by participant type
  cost_data_by_type <- costs_data %>%
    dplyr::group_by(Type) %>%
    dplyr::summarize(Total_funded_cost = round(sum(.data$Total_funded_cost), 2)) %>%
    dplyr::mutate(Total_funded_cost_p = round(100 * .data$Total_funded_cost / sum(.data$Total_funded_cost), 2),
                  Type = as.character(.data$Type)) %>%
    as.data.frame()

  # show funded costs by type
  cost_data_by_type <- rbind(cost_data_by_type,
                             c("Total",
                               sum(cost_data_by_type$Total_funded_cost),
                               sum(cost_data_by_type$Total_funded_cost_p))
  ) %>%
    dplyr::mutate(Total_funded_cost = as.numeric(.data$Total_funded_cost),
                  Total_funded_cost_p = as.numeric(.data$Total_funded_cost_p))

  cost_data_by_type

}
