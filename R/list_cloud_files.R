if (FALSE)
{
  file_info <- kwb.budget:::list_cloud_folder(
    folder = "proposals/bmbf_digital/Previous-projects/Budget/10_Filled_out_forms",
    user = Sys.getenv("NEXTCLOUD_USER"),
    password = Sys.getenv("NEXTCLOUD_PASSWORD")
  )

  View(file_info)

  sum(file_info$size) == file_info$bytes[1]
}

# list_cloud_folder ------------------------------------------------------------
list_cloud_folder <- function(folder, user, password)
{
  dictionary <- list(
    dav = "https://<user>:<password>@cloud.kompetenz-wasser.de/remote.php/dav",
    url_files = "<dav>/files/<user>/<folder>",
    url_versions = "<dav>/versions/<user>/<fileid>"
  )

  strings <- kwb.utils::resolve(
    dictionary,
    user = user,
    password = password,
    folder = folder
  )

  response <- httr::VERB("PROPFIND", strings$url_files)

  content <- httr::content(response, as = "parsed")

  x_all <- xml2::as_list(content)

  to_numeric <- function(xx) as.numeric(kwb.utils::defaultIfNULL(xx, "0"))

  get_file_info <- function(x) {
    kwb.utils::noFactorDataFrame(
      id = gsub('"', '', x$propstat$prop$getetag[[1]]),
      #path = x$href[[1]],
      filename = basename(x$href[[1]]),
      #status = x$propstat$status[[1]],
      modified = x$propstat$prop$getlastmodified[[1]],
      bytes = to_numeric(x$propstat$prop$`quota-used-bytes`[[1]]),
      size = to_numeric(x$propstat$prop$getcontentlength[[1]])
    )
  }

  do.call(rbind, lapply(unname(x_all$multistatus), get_file_info))
}
