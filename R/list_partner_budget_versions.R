# list_partner_budget_versions -------------------------------------------------

#' List the Partner's File Versions (Except Current)
#'
#' @export
#' @importFrom kwb.nextcloud list_files get_version_info
#' @export
#'
list_partner_budget_versions <- function()
{
  path <- "proposals/h2020_covid/60_Budget/10_Filled_out_forms"

  file_info <- kwb.nextcloud::list_files(path, full_info = TRUE)

  version_info <- kwb.nextcloud::get_version_info(file_info$fileid)

  columns_x <- c("fileid", "file", "lastmodified", "etag")
  columns_y <- c("fileid", "version", "href")

  result <- merge(
    kwb.utils::selectColumns(file_info, columns_x),
    kwb.utils::selectColumns(version_info, columns_y),
    by = "fileid"
  )

  kwb.utils::moveColumnsToFront(result, columns = c(
    "fileid", "file", "version"
  ))
}
