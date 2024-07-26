#' Create partners budget files (for DWH)
#'
#' @param partner_info data frame as returned by \code{\link{read_partner_info}}
#' @param path_budget_template path to budget template EXCEL file
#' @param prefix prefix given to all created files (default: "")
#' @param target_dir target directory where to save the budget files (default:
#' file.path(dirname(path_budget_template), "10_Filled_out_forms"))
#' @param set_values should metadata from partners EXCEL file be set or just the
#' template budget EXCEL file copied
#' @param overwrite should existing EXCEL files be overwritten (default: TRUE)
#' @return paths to created Excel files
#' @export
#' @importFrom fs dir_create file_copy
#' @importFrom openxlsx loadWorkbook read.xlsx writeData saveWorkbook
#'
create_partners_budget_files <- function(
    partner_info,
    path_budget_template,
    prefix = "",
    target_dir = file.path(dirname(path_budget_template), "10_Filled_out_forms"),
    set_values = FALSE,
    overwrite = TRUE
)
{
  if (set_values) {
    wb <- path_budget_template %>%
      kwb.utils::safePath() %>%
      openxlsx::loadWorkbook()
  }

  if (!dir.exists(target_dir)) {
    message("Creating ", target_dir)
    dir.create(target_dir, recursive = TRUE)
  }

  sapply(seq_len(nrow(partner_info)), function(index) {

    metadata <- kwb.utils::createAccessor(partner_info[index, ])

    budget_file_name <- sprintf(
      "%spartner-budget_%02d_%s.xlsx",
      prefix,
      metadata("partner_id"),
      metadata("partner_name_short")
    )

    target_file <- file.path(target_dir, budget_file_name)

    if (set_values) {

      wb <- openxlsx::loadWorkbook(path_budget_template)

      message(
        "Renaming template and add partner metadata ",
        "(DANGER: cell protection is lost!): ",
        target_file
      )

      openxlsx::writeData(
        wb = wb,
        sheet = "Summary",
        x = c(
          metadata("pic"),
          metadata("partner_name_legal"),
          metadata("partner_name_short"),
          metadata("funding_rate")
        ),
        startCol = "C",
        startRow = 5
      )

      openxlsx::saveWorkbook(wb, file = target_file, overwrite = overwrite)

    } else {

      message(sprintf("Renaming budget-template to: %s", target_file))
      fs::file_copy(path_budget_template, target_file, overwrite = overwrite)
    }

    target_file
  })
}
