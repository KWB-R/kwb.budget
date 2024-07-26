library(kwb.budget)

# Define paths and other global constants --------------------------------------
project <- match.arg("twin", c("covid", "twin"))

PATHS <- if (project == "covid") {

  kwb.utils::resolve(list(
    NEXTCLOUD_PROJECT = "proposals/h2020_covid",
    NEXTCLOUD_BUDGET = "<NEXTCLOUD_PROJECT>/60_Budget",
    NEXTCLOUD_BUDGET_TEMPLATE = "<NEXTCLOUD_BUDGET>/DWH_partner-budget_template.xlsx",
    NEXTCLOUD_BUDGET_FORMS = "<NEXTCLOUD_BUDGET>/10_Filled_out_forms",
    NEXTCLOUD_SUMMARY = "<NEXTCLOUD_BUDGET>/20_Summary_Files",
    NEXTCLOUD_PARTNER_INFO = "<NEXTCLOUD_PROJECT>/30_Partners/DWH_Partners-LOI-EAB_List_V2.xlsx",
    LOCAL_PROJECT = "Y:/PLANNING_PROJECTS/_Archiv-rejected-proposals/2020_EU_H2020_DWH/h2020_covid",
    LOCAL_PARTNER_INFO = "~/../Downloads/DWH_Partners-LOI-EAB_List(5).xlsx",
    LOCAL_PARTNER_INFO = "<LOCAL_PROJECT>/30_Partners/DWH_Partners-LOI-EAB_List_V2.xlsx",
    LOCAL_BUDGET_TEMPLATE = "<LOCAL_PROJECT>/60_Budget/DWH_partner-budget_template.xlsx",
    LOCAL_SUMMARY = "<NEXTCLOUD_SUMMARY>/DWC_partner-budget.xlsx",
    LOCAL_FILE_INFO = "<NEXTCLOUD_SUMMARY>/file-info.csv"
  ))

} else if (project == "twin") {

  kwb.utils::resolve(list(
    NEXTCLOUD_PROJECT = "proposals_external/2024_EU_Twin_Transition/20_stage_2",
    NEXTCLOUD_BUDGET = "<NEXTCLOUD_PROJECT>/60_budget",
    NEXTCLOUD_BUDGET_FORMS = "<NEXTCLOUD_BUDGET>/10_Filled_out_forms",
    NEXTCLOUD_SUMMARY = "<NEXTCLOUD_BUDGET>/20_Summary_Files",
    NEXTCLOUD_PARTNER_INFO = "<NEXTCLOUD_BUDGET>/AI_Liner_Partner_Budget_KWB_V2.xlsx",
    LOCAL_BUDGET_TEMPLATE = "<LOCAL_PROJECT>/60_Budget/DWH_partner-budget_template.xlsx",
    LOCAL_PROJECT = "Y:/PLANNING_PROJECTS/_Archiv-rejected-proposals/2020_EU_H2020_DWH/h2020_covid",
    LOCAL_PARTNER_INFO = "<LOCAL_PROJECT>/30_Partners/DWH_Partners-LOI-EAB_List_V2.xlsx"
  ))
}

PATTERN_FILLED_OUT <- "^10_Filled_out_forms"
PATTERN_BUDGET_FILES <- "partner-budget_\\d\\d"

PARTNER_INFO_SHEET_NAME <- "Partners-PIC-Main contact"

PARTNER_INFO_COLUMNS <- c(
  "partner_id",
  "partner_name_short",
  "partner_type",
  "partner_sector",
  "country"
)

# List files on Nextcloud ------------------------------------------------------
if (FALSE)
{
  kwb.nextcloud::list_files(PATHS$NEXTCLOUD_BUDGET, full_info = FALSE)
}

# List file versions on Nextcloud (if there are any) ---------------------------
if (FALSE)
{
  # List the Partner's File Versions (Except Current)
  kwb.nextcloud::list_file_versions(
    path = PATHS$NEXTCLOUD_BUDGET_FORMS,
    pattern = "\\.xlsx$"
  )

  # Any available version of an xlsx file below budget folder
  version_info <- kwb.nextcloud::list_file_versions(
    path = PATHS$NEXTCLOUD_BUDGET,
    recursive = TRUE
  )

  version_info
}

# Download files ---------------------------------------------------------------
if (FALSE)
{
  local_files <- kwb.budget::download_matching_files(
    path = PATHS$NEXTCLOUD_BUDGET_FORMS,
    pattern = PATTERN_BUDGET_FILES
  )

  kwb.utils::hsOpenWindowsExplorer(dirname(local_files[1L]))
}

# Create Budget Files per Partner ----------------------------------------------
if (FALSE)
{
  # Download and read Excel file with metadata about the project partners
  partner_info <- if (path_defined("LOCAL_PARTNER_INFO")) {
    # Due to bug in OnlyOffice Document Server (changes are not written back when editing)
    # https://help.nextcloud.com/t/documents-not-being-saved-when-editing-nextcloud-18-local-community-document-server-plugin/71382/4
    kwb.budget::read_partner_info(
      local_path = PATHS$LOCAL_PARTNER_INFO,
      sheet = PARTNER_INFO_SHEET_NAME
    )
  } else {
    kwb.budget::read_partner_info(
      nextcloud_path = PATHS$NEXTCLOUD_PARTNER_INFO,
      sheet = PARTNER_INFO_SHEET_NAME
    )
  }

  # Get the local path (to where the file was downloaded)
  path_partners <- kwb.utils::getAttribute(partner_info, "local_path")

  # Get budget template
  path_budget_template <- if (path_defined("LOCAL_BUDGET_TEMPLATE")) {
    PATHS$LOCAL_BUDGET_TEMPLATE
  } else {
    kwb.nextcloud::download_files(
      paths = PATHS$NEXTCLOUD_BUDGET_TEMPLATE,
      target_dir = dirname(path_partners)
    )
  }

  # Create one file per partner with partner metadata
  output_files <- kwb.budget::create_partners_budget_files(
    partner_info = partner_info,
    path_budget_template,
    target_dir = tempdir(), # file.path(dirname(path_partners), "10_Filled_out_forms"),
    set_values = TRUE
  )

  # Open folder locally in Windows Explorer
  kwb.utils::hsOpenWindowsExplorer(dirname(output_files[1L]))

  # Manually modify EXCEL files

  # - Add more advanced rules (e.g. WP leader get XX PM for XX)
  # - Add cell protection (otherwise R script will crash if someone deletes cells/rows) and
  # - Upload files to folder "<NEXTCLOUD_BUDGET>/10_Filled_out_forms"
  # - Send email to partners with instructions (save under same name, ...)
}

# Download And Analyse Budget Files from Nextcloud -----------------------------
if (FALSE)
{
  # 1) Download budget files from Nextcloud
  tdir_root <-  kwb.nextcloud:::create_download_dir("nextcloud_")
  tdir_summary <- file.path(tdir_root, "20_Summary_Files")

  cloud_budget_files <- kwb.nextcloud::list_files(
    #path = "proposals/h2020_covid/60_Budget",
    path = PATHS$NEXTCLOUD_BUDGET,
    recursive = TRUE,
    full_info = TRUE
  )

  local_budget_files <- kwb.nextcloud::download_files(
    hrefs = cloud_budget_files$href,
    target_dir = tdir_root
  )

  # Get partner metadata (for DWH proposal)

  partner_info <- try(kwb.budget::read_partner_info(
    nextcloud_path = PATHS$NEXTCLOUD_PARTNER_INFO,
    sheet = PARTNER_INFO_SHEET_NAME,
    columns = PARTNER_INFO_COLUMNS
  ))

  #kwb.utils::hsOpenWindowsExplorer(path.expand(tdir_root))

  file_info_old_path <- file.path(tdir_summary, "file-info.csv")

  # 3) Upload file metadata to cloud (only once!)
  if (!file.exists(file_info_old_path)) {

    # fs::dir_create(dirname(path_local_file_info))
    #
    # readr::write_csv(x = file_info_latest, path = path_local_file_info)

    #### Make sure not to overwrite by chance
    # kwb.nextcloud::upload_file(
    #   file = path_local_file_info,
    #   target_path = "proposals/h2020_covid/60_Budget/20_Summary_Files"
    # )

  } else {

    file_info_old <- if (file.exists(file_info_old_path)) {
      readr::read_csv(file_info_old_path)
    } # else NULL, implicitly

    keep_file <- grepl(PATTERN_FILLED_OUT, cloud_budget_files$file)
    file_info_latest <- cloud_budget_files[keep_file, ]

    if (check_if_updated(file_info_latest, file_info_old)) {

      # Filter for budget XLSX files, create and upload summary
      base_dir <- file.path(tdir_root, "10_filled_out_forms")
      base_dir <- tdir_root

      budget_files <- dir(base_dir, pattern = "\\.xlsx$", full.names = TRUE)
      target_dir <- tdir_root

      # Get information on costs from input files and create all different cost
      # views as a list of data frames
      costs <- get_all_cost_sheets(
        costs_list = read_costs_from_input_files(
          budget_files = budget_files,
          n_work_packages = n_work_packages
        ),
        partner_info = partner_info,
        n_work_packages = n_work_packages
      )

      # Write costs to an Excel file
      write_costs_to_excel(costs, file = LOCAL_SUMMARY)

      ## 2) if successful -> upload new file-info.csv
      readr::write_csv(file_info_latest, path = LOCAL_FILE_INFO)

      # Upload updated files to Nextcloud
      upload_files(
        files = c(LOCAL_SUMMARY, LOCAL_FILE_INFO),
        target_path = PATHS$NEXTCLOUD_SUMMARY
      )

    } else {

      message(
        "Going to sleep, because I have nothing to do! (budget files on ",
        "the cloud have not changed since last execution!)"
      )
    }

    ### test: open directory in explorer
    #kwb.utils::hsOpenWindowsExplorer(normalizePath(tdir_root))
    #kwb.utils::hsOpenWindowsExplorer(normalizePath(LOCAL_SUMMARY))
  }

}

# ANALYSIS ---------------------------------------------------------------------
if (FALSE)
{
  # get total costs by WP
  costs_by_wp_summary

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
    group_by(Country.y) %>%
    summarise(
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

# read_costs_from_input_files --------------------------------------------------
read_costs_from_input_files <- function(budget_files, n_work_packages = 6L)
{
  # kwb.budget::read_partner_budget_from_excel(
  #   file = budget_files[1L],
  #   n_work_packages = 7L
  # )

  costs_list <- kwb.budget::read_partners_budget_from_excel(
    budget_files,
    n_work_packages = n_work_packages,
    run_parallel = FALSE # false = slower but with more debug messages
  )

  # There are warnings: "No data found on worksheet.", why?

  # Check for errors
  has_error <- sapply(costs_list, kwb.utils::isTryError)

  if (any(has_error)) {
    kwb.utils::printIf(TRUE, has_error)
    print(table(has_error))
    message("Removing ", sum(has_error), " elements with errors")
  }

  # Exclude elements that caused errors
  costs_list[! has_error]
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
  for (file in files) {
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
