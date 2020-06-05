# list_partner_budget_versions -------------------------------------------------

#' List the Partner's File Versions (Except Current)
#'
#' @export
#' @importFrom kwb.nextcloud list_file_versions
#' @export
#'
list_partner_budget_versions <- function()
{
  kwb.nextcloud::list_file_versions(
    path = "proposals/h2020_covid/60_Budget/10_Filled_out_forms",
    pattern = "\\.xlsx$"
  )
}
