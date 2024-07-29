# download_matching_files ------------------------------------------------------

#' Download Files from Nextcloud that Match a File Name Pattern
#'
#' @param path Nextcloud path
#' @param pattern file name pattern
#' @export
download_matching_files <- function(path, pattern)
{
  file_info <- kwb.nextcloud::list_files(
    path = path,
    recursive = TRUE,
    full_info = TRUE
  )

  matches_pattern <- grepl(pattern, file_info$file)

  if (!any(matches_pattern)) {
    message(sprintf(
      "No file matches pattern '%s'. Available files: %s",
      pattern,
      paste("'", file_info$file, "'", collapse = ", ")
    ))
    return()
  }

  kwb.nextcloud::download_files(file_info$href[matches_pattern])
}
