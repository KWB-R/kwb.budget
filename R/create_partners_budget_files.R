#' Create partners budget files (for DWH)
#'
#' @param path_partners path to partners budget metadata EXCEL file
#' @param path_budget_template path to budget template EXCEL file
#' @param project_shortname proposal acronym (default: "DWH")
#' @param target_dir target directory where to save the budget files (default:
#' file.path(dirname(path_budget_template), "10_Filled_out_forms"))
#' @param set_values should metadata from partners EXCEL file be set or just the
#' template budget EXCEL file copied
#' @param overwrite should existing EXCEL files be overwritten (default: TRUE)
#' @return budget excel files for each project partner
#' @export
#' @importFrom fs dir_create file_copy
#' @importFrom openxlsx loadWorkbook read.xlsx writeData saveWorkbook
#'
create_partners_budget_files <- function(path_partners,
                                         path_budget_template,
                                         project_shortname = "DWH",
                                         target_dir = file.path(dirname(path_budget_template),
                                                                "10_Filled_out_forms"),
                                         set_values = FALSE,
                                         overwrite = TRUE) {



  partner_infos <- openxlsx::read.xlsx(xlsxFile = path_partners,
                                       sheet = "Partners-PIC-Main contact")

  fs::dir_create(target_dir)

  if (set_values)  wb <- openxlsx::loadWorkbook(path_budget_template)

  message(sprintf("Creating target directory: %s", target_dir))
  sapply(seq_len(nrow(partner_infos)), function(index) {

    metadata <-  partner_infos[index,]

    budget_file_name <- sprintf("%s_partner-budget_%02d_%s.xlsx",
                                project_shortname,
                                metadata$partner_id,
                                metadata$partner_name_short)

    target_file <- file.path(target_dir, budget_file_name)


    if (set_values) {
      wb <- openxlsx::loadWorkbook(path_budget_template)
      message(sprintf("Renaming template and add partner metadata (DANGER: cell protection is lost!): %s",
                      target_file))
      openxlsx::writeData(wb = wb,
                          sheet = "Summary",
                          x = c(metadata$pic,
                                metadata$partner_name_legal,
                                metadata$partner_name_short,
                                metadata$funding_rate),
                          startCol = "C",
                          startRow = 5)



      openxlsx::saveWorkbook(wb, file = target_file, overwrite = overwrite)
    } else {
      message(sprintf("Renaming budget-template to: %s", target_file))
      fs::file_copy(path_budget_template, target_file, overwrite = overwrite)
    }


  })

  target_dir
}
