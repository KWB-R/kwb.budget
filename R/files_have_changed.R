# files_have_changed -----------------------------------------------------------
files_have_changed <- function(file_info_new, file_info_old)
{
  columns <- c("fileid", "file", "lastmodified")

  file_comparsion <- dplyr::full_join(
    file_info_new[columns],
    file_info_old[columns],
    by = "fileid"
  ) %>%
    dplyr::mutate(
      msg = dplyr::if_else(
        is.na(.data$file.x) & ! is.na(.data$file.y),
        sprintf("DELETED: %s", .data$file.y),
        dplyr::if_else(
          ! is.na(.data$file.x) & is.na(.data$file.y),
          sprintf("ADDED: %s", .data$file.x),
          dplyr::if_else(
            .data$file.x == .data$file.y,
            "",
            sprintf("UPDATED: %s", .data$file.x)
          )
        )
      )
    )

  file_updated <-
    file_comparsion$lastmodified.x != file_comparsion$lastmodified.y |
    is.na(file_comparsion$lastmodified.x) |
    is.na(file_comparsion$lastmodified.y)

  if (any(file_updated)) {
    message(
      "The following files were updated:\n\n",
      paste(file_comparsion$msg[which(file_updated)], collapse = "\n")
    )
    return(TRUE)
  }

  FALSE
}
