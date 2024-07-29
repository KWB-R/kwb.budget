#' Create an Excel File for Project Partner Information
#'
#' @param n_partners number of partners
#' @param file_name name of Excel file. Default: "project_partners_tmp.xlsx".
#' @param sheet_name name of Excel sheet. Default: "Partners-PIC-Main contact".
#' @importFrom openxlsx write.xlsx
#' @export
create_partner_template_xls <- function(
    n_partners = 7L,
    file_name = "project_partners_tmp.xlsx",
    sheet_name = "Partners-PIC-Main contact"
)
{
  partner_info_fake <- fake_partner_info(n_partners = 5L)
  file <- file.path(tempdir(), file_name)
  data <- list(partner_info_fake)
  names(data) <- sheet_name
  openxlsx::write.xlsx(data, file)
  file
}
