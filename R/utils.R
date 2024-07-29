# move_columns_right -----------------------------------------------------------
move_columns_right <- function(data, columns)
{
  kwb.utils::selectColumns(data, c(setdiff(names(data), columns), columns))
}


# read_or_write_csv ------------------------------------------------------------

#' Read CSV File or Write CSV File if File does not exist
#'
#' @param file path to CSV file
#' @param data optional data to be written to CSV file in case that file does
#'   not exist
#' @importFrom kwb.utils createDirectory
#' @importFrom readr read_csv write_csv
#' @export
read_or_write_csv <- function(file, data = NULL)
{
  if (file.exists(file)) {
    readr::read_csv(file)
  } else {
    stopifnot(!is.null(data))
    kwb.utils::createDirectory(dirname(file), dbg = FALSE)
    readr::write_csv(data, file)
    data
  }
}

# remove_error_elements --------------------------------------------------------
remove_error_elements <- function(x)
{
  # Check for errors
  has_error <- sapply(x, kwb.utils::isTryError)

  if (any(has_error)) {
    message("Removing ", sum(has_error), " elements with errors")
  }

  # Exclude elements that caused errors
  x[!has_error]
}

# upload_files -----------------------------------------------------------------

#' Upload Files to Nextcloud
#'
#' @param files path(s) to local file(s)
#' @param target_path Nextcloud target path
#' @importFrom kwb.nextcloud upload_file
#' @export
upload_files <- function(files, target_path)
{
  files_there <- kwb.nextcloud::list_files(target_path)

  skip <- basename(files) %in% files_there

  if (any(skip)) {
    message(
      "Excluding ", sum(skip), " files from upload in order not to overwrite ",
      "existing files:\n",
      paste("-", basename(files[skip]), collapse = "\n")
    )
  }

  for (file in files[!skip]) {
    kwb.nextcloud::upload_file(file, target_path)
  }
}

