library(kwb.budget)

# Define paths and other global constants --------------------------------------
project <- match.arg("ailiner", c("dwc", "dwh", "ailiner"))

GRAMMAR <- list(
  BUDGET = "<PROJECT>/60_budget",
  PARTNERS = "<PROJECT>/20_partners/<FILE_NAME_PARTNERS>",
  BUDGET_FORMS = "<BUDGET>/filled_out_forms",
  BUDGET_TEMPLATE = "<BUDGET>/<FILE_NAME_TEMPLATE>",
  BUDGET_SUMMARY = "<BUDGET>/summary_files",
  FILE_INFO = "<BUDGET_SUMMARY>/<FILE_NAME_FILE_INFO>",
  FILE_NAME_FILE_INFO = "<PRJ>_file-info.csv",
  FILE_NAME_TEMPLATE = "AI_Liner_Partner_Budget_Example.xlsx",
  FILE_NAME_PARTNERS = "partners_test.xlsx"
)

GRAMMAR_PROJECT <- kwb.utils::resolve(
  GRAMMAR,
  PRJ = "TT"
)

PATHS_CLOUD <- kwb.utils::resolve(
  GRAMMAR_PROJECT,
  PROJECT = "proposals_external/2024_EU_Twin_Transition/20_stage_2"
)

PATHS_LOCAL <- kwb.utils::resolve(
  GRAMMAR_PROJECT,
  PROJECT = "~/Projekte/2024_EU_Twin_Transition/stage_2"
)

PARTNER_INFO_SHEET_NAME <- "Partners-PIC-Main contact"

PARTNER_INFO_COLUMNS <- c(
  "partner_id",
  "partner_name_short",
  "partner_type",
  "partner_sector",
  "country"
)

N_WORK_PACKAGES <- 7L

# List files on Nextcloud ------------------------------------------------------
if (FALSE)
{
  kwb.nextcloud::list_files(PATHS_CLOUD$BUDGET, full_info = FALSE)
  kwb.nextcloud::list_files(PATHS_CLOUD$BUDGET_FORMS, full_info = FALSE)
}

# List file versions on Nextcloud (if there are any) ---------------------------
if (FALSE)
{
  (version_info <- kwb.nextcloud::get_file_versions(
    file_info = kwb.nextcloud::list_files(
      PATHS_CLOUD$BUDGET_FORMS, full_info = TRUE
    )
  ))
}

# Download files ---------------------------------------------------------------
if (FALSE)
{
  local_files <- kwb.budget::download_matching_files(
    path = PATHS_CLOUD$BUDGET_FORMS,
    pattern = "partner-budget_\\d\\d"
  )

  kwb.utils::hsOpenWindowsExplorer(dirname(local_files[1L]))
}

# Create and Upload an Excel File for Project Partner Information --------------
if (FALSE)
{
  file <- kwb.budget::create_partner_template_xls(n_partners = 5L)
  kwb.utils::hsOpenWindowsExplorer(file)
  kwb.budget::upload_files(file, dirname(PATHS_CLOUD$PARTNERS))
}

# Create Budget Files per Partner ----------------------------------------------
if (FALSE)
{
  # Download and read Excel file with metadata about the project partners
  partner_info <- kwb.budget::read_partner_info(
    nextcloud_path = PATHS_CLOUD$PARTNERS,
    sheet = PARTNER_INFO_SHEET_NAME,
    columns = NULL
  )

  # Get budget template
  path_budget_template <- kwb.nextcloud::download_files(
    paths = PATHS_CLOUD$BUDGET_TEMPLATE
  )

  # Create one file per partner with partner metadata
  output_files <- kwb.budget::create_partners_budget_files(
    partner_info = partner_info,
    path_budget_template = path_budget_template,
    target_dir = file.path(tempdir(), "abc"),
    set_values = TRUE
  )

  # Open folder locally in Windows Explorer
  #kwb.utils::hsOpenWindowsExplorer(dirname(output_files[1L]))

  upload_files(files = output_files, target_path = PATHS_CLOUD$BUDGET_FORMS)

  # Manually modify EXCEL files

  # - Add more advanced rules (e.g. WP leader get XX PM for XX)
  # - Add cell protection (otherwise R script will crash if someone deletes cells/rows) and
  # - Upload files to folder "<CLOUD_BUDGET>/10_Filled_out_forms"
  # - Send email to partners with instructions (save under same name, ...)
}

# Download And Analyse Budget Files from Nextcloud -----------------------------
if (FALSE)
{
  # List budget files that are available on Nextcloud
  cloud_budget_files <- kwb.nextcloud::list_files(
    path = PATHS_CLOUD$BUDGET_FORMS,
    full_info = TRUE
  )

  # Download budget files from Nextcloud
  local_budget_files <- kwb.nextcloud::download_files(
    hrefs = cloud_budget_files$href
  )

  local_file_info <- PATHS_LOCAL$FILE_INFO

  file_info_old <- if (file.exists(local_file_info)) {
    readr::read_csv(local_file_info)
  } else {
    kwb.utils::createDirectory(dirname(local_file_info), dbg = FALSE)
    readr::write_csv(cloud_budget_files, file = local_file_info)
    cloud_budget_files
  }

  file_info_latest <- cloud_budget_files

  if (!check_if_updated(file_info_latest, file_info_old)) {

    message(
      "Going to sleep, because I have nothing to do! (budget files on ",
      "the cloud have not changed since last execution!)"
    )

  } else {

    # Get metadata about the project partners
    partner_info <- try(kwb.budget::read_partner_info(
      nextcloud_path = PATHS_CLOUD$PARTNERS,
      sheet = PARTNER_INFO_SHEET_NAME,
      columns = NULL # PARTNER_INFO_COLUMNS
    ))

    # Get information on costs from input files and create all different cost
    # views as a list of data frames

    # Filter for Excel files
    budget_files <- grep("\\.xlsx$", local_budget_files, value = TRUE)

    if (FALSE) {
      kwb.budget::read_partner_budget_from_excel(
        file = budget_files[1L],
        n_work_packages = 7L
      )
    }

    costs_list <- kwb.budget::read_partners_budget_from_excel(
      budget_files,
      n_work_packages = N_WORK_PACKAGES,
      run_parallel = FALSE
    )

    # There are warnings: "No data found on worksheet.", why?
    costs_list <- remove_error_elements(costs_list)

    costs <- kwb.budget:::get_all_cost_sheets(
      costs_list,
      partner_info = partner_info,
      n_work_packages = N_WORK_PACKAGES
    )

    # Create and upload summary

    # Write costs to an Excel file
    write_costs_to_excel(costs, file = PATHS_LOCAL$BUDGET_SUMMARY)

    ## 2) if successful -> upload new file-info.csv
    readr::write_csv(file_info_latest, path = PATHS_LOCAL$FILE_INFO)

    # Upload updated files to Nextcloud
    upload_files(
      files = c(PATHS_LOCAL$BUDGET_SUMMARY, PATHS_LOCAL$FILE_INFO),
      target_path = PATHS_CLOUD$BUDGET_SUMMARY
    )

  }

  ### test: open directory in explorer
  #kwb.utils::hsOpenWindowsExplorer(normalizePath(tdir_root))
  #kwb.utils::hsOpenWindowsExplorer(normalizePath(LOCAL_BUDGET_SUMMARY))

}

# ANALYSIS ---------------------------------------------------------------------
if (FALSE)
{
  # get total costs by WP
  costs_by_wp_summary <- costs$by_wp

  par(mfrow = c(2,2))

  plot(
    costs_by_wp_summary$wp,
    costs_by_wp_summary$Total_cost,
    pch = 16, las = 1,
    xlab = "WP",
    ylab = "",
    yaxt = "n",
    main = "",
    col = "blue",
    cex.main = 0.7
  )

  points(
    costs_by_wp_summary$wp,
    costs_by_wp_summary$Total_funded_cost,
    col = "green"
  )

  grid()

  axis(2, las = 2, cex = 0.4)


  barplot(
    costs$Total_funded_cost, names.arg =  costs$partner_short_name, las = 2
  )

  grid()

  labels = paste0(
    "Requested grant €:",
    round(sum(costs_by_wp_summary$Total_funded_cost), 0),
    "\n Max. grant €:",
    round(sum(costs_by_wp_summary$Total_cost), 0)
  )

  plot(
    1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n",
    frame.plot = FALSE
  )

  text(1, 1, labels = labels)

  budget_merge_country <- budget_merge %>%
    dplyr::group_by(Country.y) %>%
    dplyr::summarise(
      Total_cost = sum(Total_cost),
      Total_funded_cost = sum(Total_funded_cost)
    ) %>%
    as.data.frame()

  budget_merge_type <- budget_merge %>%
    group_by(Type) %>%
    summarise(
      Total_cost = sum(Total_cost),
      Total_funded_cost = sum(Total_funded_cost)
    ) %>%
    as.data.frame()

  library(gridExtra)
  library(grid)

  grid.table(budget_merge_country)
  grid.table(budget_merge_type)

  cost_matrices <- to_cost_matrices(costs_by_wp)

  print(budget)
}

# path_defined -----------------------------------------------------------------
path_defined <- function(name)
{
  name %in% names(PATHS)
}

# remove_error_elements --------------------------------------------------------
remove_error_elements <- function(x)
{
  # Check for errors
  has_error <- sapply(x, kwb.utils::isTryError)

  if (any(has_error)) {
    kwb.utils::printIf(TRUE, has_error)
    print(table(has_error))
    message("Removing ", sum(has_error), " elements with errors")
  }

  # Exclude elements that caused errors
  x[!has_error]
}

# write_costs_to_excel ---------------------------------------------------------
write_costs_to_excel <- function(costs, file, overwrite = TRUE)
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

# upload_files -----------------------------------------------------------------
upload_files <- function(files, target_path)
{
  files_there <- kwb.nextcloud::list_files(target_path)

  skip <- basename(files) %in% files_there

  if (any(skip)) {
    message(
      "Excluding ", sum(skip), " files from upload in order not to overwrite ",
      "files:\n",
      paste("-", basename(files[skip]), collapse = "\n")
    )
  }

  for (file in files[!skip]) {
    kwb.nextcloud::upload_file(file, target_path)
  }
}

# check_if_updated -------------------------------------------------------------
check_if_updated <- function(file_info_latest, file_info_old)
{
  is_updated <- FALSE

  select_cols <- c("fileid", "file", "lastmodified")

  file_comparsion <- dplyr::full_join(
    file_info_latest[select_cols],
    file_info_old[select_cols],
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

    message(sprintf(
      "The following files were updated:\n\n%s",
      paste(file_comparsion$msg[which(file_updated)], collapse = "\n")
    ))

    is_updated <- TRUE
  }

  is_updated
}

# to_cost_matrices -------------------------------------------------------------
to_cost_matrices <- function(costs_by_wp)
{
  all_columns <- names(costs_by_wp)
  first_two <- all_columns[1:2]

  lapply(
    X = stats::setNames(nm = setdiff(all_columns, first_two)),
    FUN = function(column) {
      costs_by_wp %>%
        kwb.utils::selectColumns(c(first_two, column)) %>%
        kwb.utils::countOrSum(by = first_two, sum.up = column)
    }
  )
}
