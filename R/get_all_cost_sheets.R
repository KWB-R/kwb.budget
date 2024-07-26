# get_all_cost_sheets ----------------------------------------------------------
get_all_cost_sheets <- function(costs_list, partner_info, n_work_packages)
{
  # Use fake data on partners if partner_info is missing or has errors
  if (is.null(partner_info) || kwb.utils::isTryError(partner_info)) {
    partner_info <- fake_partner_info(n_partners = length(costs_list))
  }

  # Generate overview with one row per partner
  costs_overview <- costs_list %>%
    kwb.utils::rbindAll() %>%
    dplyr::left_join(partner_info, by = "partner_id")

  # Generate detail view with one row per partner and work package
  # (direct costs by WP)
  costs_by_wp_and_partner <- list_to_costs_by_wp_and_partner(
    costs_list = costs_list,
    costs_overview = costs_overview,
    n_work_packages = n_work_packages
  )

  list(
    overview = costs_overview,
    overview_and_pm_per_wp = get_person_month_costs(
      costs_overview = costs_overview,
      costs_by_wp = costs_by_wp_and_partner
    ),
    by_wp_and_partner = costs_by_wp_and_partner,
    by_wp = list_to_costs_by_wp(costs_by_wp_and_partner),
    by_type = get_costs_by_type(costs_overview),
    by_sector = get_costs_by_sector(costs_overview),
    by_country = get_costs_by_country(costs_overview)
  )
}

# fake_partner_info ------------------------------------------------------------
fake_partner_info <- function(n_partners = 10L)
{
  indices <- seq_len(n_partners)

  data.frame(
    partner_id = indices,
    pic = 1e6 + indices,
    country = "de",
    partner_name_short = LETTERS[indices],
    partner_name_legal = "",
    partner_sector = "water",
    partner_type = "research institute",
    funding_rate = 1,
    contact_admin_name = "",
    contact_admin_email = "",
    comments = "",
    partner_description = ""
  )
}

# list_to_costs_by_wp_and_partner-----------------------------------------------
list_to_costs_by_wp_and_partner <- function(
    costs_list, costs_overview, n_work_packages
)
{
  costs_overview <- costs_overview %>%
    kwb.utils::selectColumns(c(
      "partner_id",
      "partner_name_short",
      "partner_type",
      "partner_sector",
      "country",
      "Reimbursement_rate"
    ))

  get_costs_by_work_package(costs_list, n_work_packages) %>%
    merge(costs_overview, by.x = "partner", by.y = "partner_id") %>%
    # Add indirect and total costs
    dplyr::rename(
      partner_id = .data$partner,
      partner = .data$partner_name_short
    ) %>%
    dplyr::mutate(
      Direct_cost = .data$cost.personnel +
        .data$cost.equipment +
        .data$cost.consumables +
        .data$cost.subcontracting,
      Indirect_cost = 0.25 * (
        .data$Direct_cost - .data$cost.subcontracting
      ),
      Total_cost = .data$Direct_cost + .data$Indirect_cost,
      Total_funded_cost = .data$Reimbursement_rate * .data$Total_cost
    ) %>%
    kwb.utils::moveColumnsToFront(c(
      "partner_id",
      "partner",
      "partner_type",
      "country"
    )) %>%
    move_columns_right(c(
      "Reimbursement_rate",
      "Total_funded_cost"
    ))
}

# get_person_month_costs -------------------------------------------------------
get_person_month_costs <- function(costs_overview, costs_by_wp)
{
  merge(
    x = prepare_cost_data_short(costs_overview),
    y = get_person_months_by_wp(costs_by_wp),
    by.x = "partner_name_short",
    by.y = "partner",
    all = TRUE
  ) %>%
    move_columns_right("Total_funded_cost")
}

# prepare_cost_data_short ------------------------------------------------------
prepare_cost_data_short <- function(costs_overview)
{
  # reduce table size
  costs_overview %>%
    kwb.utils::removeColumns(c(
      "pic_number",
      "partner_name",
      "author_name",
      "author_email",
      "contact_name",
      "contact_email",
      "Participant"
    )) %>%
    move_columns_right(c(
      "Reimbursement_rate",
      "Total_funded_cost"
    )) %>%
    kwb.utils::moveColumnsToFront(c(
      "filename",
      "partner_id",
      "partner_name_short",
      "partner_type",
      "country"
    ))
}

# get_person_months_by_wp ------------------------------------------------------
#' @importFrom tidyr spread
get_person_months_by_wp <- function(costs_by_wp)
{
  costs_by_wp %>%
    kwb.utils::selectColumns(c(
      "partner",
      "wp",
      "person_months.personnel"
    )) %>%
    tidyr::spread(
      key = .data$wp,
      value = .data$person_months.personnel
    )
}

# list_to_costs_by_wp-----------------------------------------------------------
list_to_costs_by_wp <- function(costs_by_wp_and_partner)
{
  costs_by_wp_and_partner %>%
    dplyr::group_by(.data$wp) %>%
    dplyr::summarise(
      Total_cost = sum(.data$Total_cost),
      Total_funded_cost = sum(.data$Total_funded_cost)
    ) %>%
    dplyr::mutate(
      Total_funded_cost_p = round(
        100 * .data$Total_funded_cost / sum(.data$Total_funded_cost),
        digits = 2
      )
    ) %>%
    as.data.frame()
}

# get_costs_by_country ---------------------------------------------------------
get_costs_by_country <- function(costs_overview)
{
  costs_by_country <- costs_overview %>%
    dplyr::group_by(.data$country) %>%
    dplyr::summarise(
      Total_cost = sum(.data$Total_cost),
      Total_funded_cost = sum(.data$Total_funded_cost),
      n = dplyr::n()
    ) %>%
    dplyr::mutate(
      Total_funded_cost_p = round(
        100 * .data$Total_funded_cost / sum(.data$Total_funded_cost),
        digits = 2
      )
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Total_funded_cost)) %>%
    kwb.utils::selectColumns(c(
      "country",
      "n",
      "Total_cost",
      "Total_funded_cost",
      "Total_funded_cost_p"
    )) %>%
    as.data.frame()

  # show funded costs by type
  rbind(costs_by_country, c(
    "Total",
    sum(costs_by_country$n),
    sum(costs_by_country$Total_cost),
    sum(costs_by_country$Total_funded_cost),
    sum(costs_by_country$Total_funded_cost_p)
  )) %>%
    dplyr::mutate(
      n = as.numeric(.data$n),
      Total_cost = as.numeric(.data$Total_cost),
      Total_funded_cost = as.numeric(.data$Total_funded_cost),
      Total_funded_cost_p = as.numeric(.data$Total_funded_cost_p)
    )
}
