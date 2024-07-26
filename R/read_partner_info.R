# read_partner_info ------------------------------------------------------------

#' Read Information on Project Partners from (Downloaded) Excel file
#'
#' Either nextcloud_path or local_path must be given with local_path taking
#' precedence over nextcloud_path.
#'
#' @param nextcloud_path path to Excel file on nextcloud
#' @param sheet name of Excel sheet
#' @param local_path if given, the Excel file is read from this (local) path
#' @param columns vector of column names. If given, only these columns are
#'   returned. The default is this vector of required column names:
#'  \code{c("partner_id", "partner_name_short", "partner_name_legal", "pic",
#'  "funding_rate")}
#' @return data frame with attribute "local_path" being set to the path to the
#'   local Excel file from which the data were read
#' @export
read_partner_info <- function(
    nextcloud_path, sheet, local_path = NULL,
    columns = c(
      "partner_id",
      "partner_name_short",
      "partner_name_legal",
      "pic",
      "funding_rate"
    )
)
{
  if (is.null(local_path)) {
    local_path <- kwb.nextcloud::download_files(paths = nextcloud_path)
  }

  local_path %>%
    kwb.utils::safePath() %>%
    openxlsx::read.xlsx(sheet = sheet) %>%
    kwb.utils::selectColumns(columns) %>%
    structure(local_path = local_path)
}
