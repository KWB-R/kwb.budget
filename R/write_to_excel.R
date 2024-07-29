# write_to_excel ---------------------------------------------------------------
write_to_excel <- function(costs, file, overwrite = TRUE)
{
  wb <- openxlsx::createWorkbook()

  for (sheet in names(costs)) {
    content <- costs[[sheet]]
    cols <- seq_len(ncol(content))
    openxlsx::addWorksheet(wb, sheet)
    openxlsx::writeData(wb, sheet = sheet, x = content)
    openxlsx::setColWidths(wb, sheet = sheet, cols = cols, widths = "auto")
  }

  openxlsx::saveWorkbook(wb = wb, file = file, overwrite = overwrite)

  invisible(file)
}
