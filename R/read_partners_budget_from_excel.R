#' Read Partners Budget From Excel Files
#'
#' @param files full path to EXCEL files
#' @param n_work_packages number of work packages in EXCEL template
#' (default: 7, as used for DWC)
#' @param run_parallel should import be performed using multiple CPU cores or
#' only run on a single core (default: TRUE)
#' @return list with imported EXCEL budget files data
#' @export
#' @importFrom kwb.utils noFactorDataFrame renameAndSelect removeColumns
#' @importFrom kwb.utils toLookupTable catAndRun
#' @importFrom parallel detectCores makeCluster parLapply stopCluster
#'
read_partners_budget_from_excel <- function(
    files,
    n_work_packages = 7,
    run_parallel = TRUE
)
{
  budgets <- if (run_parallel) {

    ncores <- parallel::detectCores() - 1L

    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))

    kwb.utils::catAndRun(
      sprintf("Importing %d budget files from partners", length(files)),
      expr = parallel::parLapply(cl, files, function(file) {
        try(read_partner_budget_from_excel(
          file, n_work_packages = n_work_packages
        ))
      })
    )

  } else {

    lapply(files, function(file) {

      message(sprintf(
        "Reading '%s' (%d/%d)...", basename(file),
        which(file == files),
        length(files)
      ))

      try(read_partner_budget_from_excel(
        file, n_work_packages = n_work_packages
      ))

    })
  }

  stats::setNames(budgets, basename(files))
}
