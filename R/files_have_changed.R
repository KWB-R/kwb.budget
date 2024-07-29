# files_have_changed -----------------------------------------------------------
files_have_changed <- function(file_info_new, file_info_old)
{
  if (identical(file_info_new, file_info_old)) {
    return(FALSE)
  }

  columns <- c("fileid", "file", "lastmodified")

  file_info <- dplyr::full_join(
    file_info_new[columns],
    file_info_old[columns],
    by = "fileid",
    suffix = c(".new", ".old")
  )

  old_given <- !is.na(file_info$file.old)
  new_given <- !is.na(file_info$file.new)

  mod_x <- file_info$lastmodified.x
  mod_y <- file_info$lastmodified.y

  deleted <- !new_given & old_given
  added <- new_given & !old_given
  updated <- new_given & old_given & mod_x != mod_y

  status_text <- character(nrow(file_info))
  status_text[deleted] <- paste("DELETED:", file_info$file.old[deleted])
  status_text[added] <- paste("ADDED:", file_info$file.new[added])
  status_text[updated] <- paste("UPDATED:", file_info$file.old[updated])

  has_changed <- nzchar(status_text)

  if (any(has_changed)) {
    message(
      "The following files were updated:\n",
      paste(status_text[has_changed], collapse = "\n")
    )
    return(TRUE)
  }

  FALSE
}
