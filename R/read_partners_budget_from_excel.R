#' Read Partners Budget From Excel Files
#'
#' @param files full path to EXCEL files
#' @param number_of_work_packages number of work packages in EXCEL template
#' (default: 7, as used for DWC)
#' @param run_parallel should import be performed using multiple CPU cores or
#' only run on a single core (default: TRUE)
#' @return list with imported EXCEL budget files data
#' @export
#' @importFrom kwb.utils noFactorDataFrame renameAndSelect removeColumns toLookupTable catAndRun
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#'
read_partners_budget_from_excel <- function (files,
                                             number_of_work_packages = 7,
                                             run_parallel = TRUE) {

  if(run_parallel) {
    ncores <- parallel::detectCores() - 1
    cl <- parallel::makeCluster(ncores)

    msg <- sprintf("Importing %d budget files from partners", length(files))

    kwb.utils::catAndRun(messageText = msg,
                         expr = parallel::parLapply(cl, files, function(file) {

                           try(kwb.budget::read_partner_budget_from_excel(file,
                                                                          number_of_work_packages = number_of_work_packages)
                           )}))

    parallel::stopCluster(cl)

  } else {
    lapply(seq_along(files), function(i) {

      file <- files[i]
      message(sprintf("Reading '%s' (%d/%d)...", basename(file), i, length(files)))
      try(kwb.budget::read_partner_budget_from_excel(file,
                                                     number_of_work_packages = number_of_work_packages))


    })
  }}
