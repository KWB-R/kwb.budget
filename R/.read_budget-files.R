library(kwb.budget)

# List versions on the cloud ---------------------------------------------------
if (FALSE)
{
  kwb.budget::list_partner_budget_versions()

  # Any available version of an xlsx file below budget folder
  version_info <- kwb.nextcloud::list_file_versions(
    "proposals/h2020_covid/60_Budget",
    pattern = "\\.xlsx$",
    recursive = TRUE
  )

  version_info
}

# Create Budget Files per Partner ----------------------------------------------
if (FALSE)
{
  # 1) Get partner metadata
  partner_info <- read_partner_info()
  path_partners <- kwb.utils::getAttribute(partner_info, "path_partners")

  # 3) Get budget template
  path_budget_template <- kwb.nextcloud::download_files(
    paths = "proposals/h2020_covid/60_Budget/DWH_partner-budget_template.xlsx",
    target_dir = dirname(path_partners)
  )

  # 3) Create one file per partner with partner metadata
  output_dir <- kwb.budget::create_partners_budget_files(
    path_partners,
    path_budget_template,
    target_dir = file.path(dirname(path_partners), "10_Filled_out_forms"),
    set_values = TRUE
  )

  # 4) Open folder locally in Windows Explorer
  kwb.utils::hsOpenWindowsExplorer(output_dir)

  # 5) Manually modify EXCEL files

  # - Add more advanced rules (e.g. WP leader get XX PM for XX)
  # - Add cell protection (otherwise R script will crash if someone deletes cells/rows) and
  # - Upload to files to folder  "proposals/h2020_covid/60_Budget/10_Filled_out_forms"
  # - Send email to partners with instructions (save under same name, ...)
}

# Download And Analyse Budget Files from Nextcloud -----------------------------
if (FALSE)
{
  # 1) Download budget forms from Nextcloud

  #path <- "proposals/bmbf_digital/Previous-projects/Budget"
  path <- "proposals/h2020_covid/60_Budget/10_Filled_out_forms"

  # 2) Generate budget files metadata (in order to detect updates, required for
  #    automation)
  # List files recursively (only xlsx or csv files)
  file_info_latest <- kwb.nextcloud::list_files(
    path = path,
    pattern = "(xlsx|csv)$",
    recursive = TRUE,
    full_info = TRUE
  )

  # Check the result
  #View(file_info_latest)

  # 3) Upload file metadata to cloud (only once!)
  tdir_root <-  kwb.nextcloud:::create_download_dir("nextcloud_")
  tdir_forms <- file.path(tdir_root, "10_filled_out_forms")
  tdir_summary <- file.path(tdir_root, "20_Summary_Files")

  dir.create(tdir_forms)
  dir.create(tdir_summary)

  is_this_the_first_time <- FALSE

  if (is_this_the_first_time) {

    path_local_file_info <- file.path (tdir_summary, "file-info.csv")

    fs::dir_create(dirname(path_local_file_info))

    readr::write_csv(x = file_info_latest, path = path_local_file_info)

    kwb.nextcloud::upload_file(
      file = path_local_file_info,
      target_path = "proposals/h2020_covid/60_Budget/20_Summary_Files"
    )

  } else {

    file_info_old_path <-  kwb.nextcloud::download_files(
      paths = "proposals/h2020_covid/60_Budget/20_Summary_Files/file-info.csv",
      target_dir = tdir_summary
    )

    file_info_old <- readr::read_csv(file = file_info_old_path)

    select_cols <- c("fileid", "file", "lastmodified")

    file_comparsion <- dplyr::full_join(
      file_info_latest[select_cols],
      file_info_old[select_cols],
      by = "fileid"
    ) %>%
      dplyr::mutate(
        msg = dplyr::if_else(
          is.na(.data$file.x) & ! is.na(.data$file.y),
          sprintf("ADDED: %s", .data$file.y),
          dplyr::if_else(
            ! is.na(.data$file.x) & is.na(.data$file.y),
            sprintf("DELETED: %s", .data$file.x),
            sprintf("UPDATED: %s", .data$file.x)
          )
        )
      )

    file_updated <- file_comparsion$lastmodified.x != file_comparsion$lastmodified.y |
      is.na(file_comparsion$lastmodified.x) |
      is.na(file_comparsion$lastmodified.y)

    if (any(file_updated)) {

      message(sprintf(
        "The following files were updated:\n\n%s",
        paste(file_comparsion$msg[which(file_updated)], collapse = "\n")
      ))

      ### 1) write functions and add code for analysing (code below)

      # Provide the full paths by prepending the root path
      full_paths <- file.path(
        kwb.utils::getAttribute(file_info_latest, "root"),
        file_info_latest$file
      )

      # Download the corresponding files to a temp folder below ~/../Downloads
      system.time(
        downloaded_files <- kwb.nextcloud:::download_files(
          #href = file_info_latest$href,
          paths =  full_paths,
          target_dir = tdir_forms
        )
      )

      ## Filter out only budget XLSX files
      budget_files <- dir(
        dirname(downloaded_files[1]), "DWH_partner-budget_[0-9][0-9].*xlsx$",
        full.names = TRUE
      )

      ### START HERE (if you like an easy start)

      budget_files <- download_partner_budget_files()

      #kwb.utils::hsOpenWindowsExplorer(dirname(budget_files[1]))

      # Get information on costs from input files
      costs_list <- read_costs_from_input_files(budget_files)

      # Get all different cost views as a list of data frames
      costs <- get_all_cost_sheets(costs_list)

      ### Define path to xlsx output file
      xls_file <- file.path(tdir_summary, "DWC_partner-budget.xlsx")
      workbook <- create_workbook_with_sheets(costs)
      openxlsx::saveWorkbook(workbook, xls_file, overwrite = TRUE)

      kwb.utils::hsOpenWindowsExplorer(normalizePath(xls_file))
      ## 2) if successfull -> upload new file-info.csv

      # fs::dir_create(dirname(path_local_file_info))
      # readr::write_csv(x = file_info_latest, path = path_local_file_info)
      # kwb.nextcloud::upload_file(
      #   file = path_local_file_info,
      #   target_path = "proposals/h2020_covid/60_Budget/20_Summary_Files"
      # )

      kwb.nextcloud::upload_file(
        file = xls_file,
        target_path = "proposals/h2020_covid/60_Budget/20_Summary_Files"
      )

    } else {

      message(
        "Going to sleep, because I have nothing to do! (budget files on ",
        "the cloud have not changed since last execution!)"
      )
    }

    ### test: open directory in explorer

    #kwb.utils::hsOpenWindowsExplorer(normalizePath(tdir_root))
  }
}

# ANALYSIS ---------------------------------------------------------------------
if (FALSE)
{
  # get total costs by WP
  costs_by_wp_summary <- costs_by_wp_data %>%
    group_by(wp) %>%
    summarise(
      Total_cost = sum(Total_cost),
      Total_funded_cost = sum(Total_funded_cost)
    ) %>%
    as.data.frame()

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

# get_all_cost_sheets ----------------------------------------------------------
get_all_cost_sheets <- function(costs_list)
{
  costs <- list()

  # Generate overview with one row per partner
  costs$overview <- list_to_costs_overview(costs_list)

  # Generate detail view with one row per partner and work package
  # (direct costs by WP)
  costs$by_wp <- list_to_costs_by_wp(costs_list, costs$overview)

  # Merge with simplified table withz costs
  costs$short <- get_person_month_costs(costs$overview, costs$by_wp)

  # Prepare table with costs by company type
  costs$by_type <- kwb.budget::get_costs_by_type(costs$overview)

  # Prepare table with costs by country
  costs$by_country <- get_costs_by_country(costs$overview)

  costs[c("overview", "short", "by_wp", "by_type", "by_country")]
}

# create_workbook_with_sheets -----------------------------------------------
create_workbook_with_sheets <- function(sheet_contents)
{
  wb <- openxlsx::createWorkbook()

  sheets <- names(sheet_contents)

  for (sheet in sheets) {

    openxlsx::addWorksheet(wb, sheet)

    content <- sheet_contents[[sheet]]

    openxlsx::writeData(wb, sheet = sheet, x = content)

    cols <- seq_len(ncol(content))

    openxlsx::setColWidths(wb, sheet = sheet, cols = cols, widths = "auto")
  }

  wb
}

# list_to_costs_overview -------------------------------------------------------
list_to_costs_overview <- function(costs_list)
{
  # Get partner metadata (for DWH proposal)
  partner_info <- read_partner_info(c(
    "partner_id", "partner_name_short", "partner_type", "country"
  ))

  # Transform in dataframe
  kwb.utils::rbindAll(costs_list) %>%
    dplyr::left_join(partner_info, by = "partner_id")
}

# list_to_costs_by_wp ----------------------------------------------------------
list_to_costs_by_wp <- function(costs_list, costs_overview)
{
  costs_overview <- kwb.utils::selectColumns(costs_overview, c(
    "partner_id", "partner_name_short", "partner_type", "country",
    "Reimbursement_rate"
  ))

  kwb.budget::get_costs_by_work_package(costs_list, n_work_packages = 6) %>%
    merge(costs_overview, by.x = "partner", by.y = "partner_id") %>%
    # Add indirect and total costs
    dplyr::rename(partner_id = partner, partner = partner_name_short) %>%
    dplyr::mutate(
      Direct_cost = .data$cost.personnel +
        .data$cost.equipment +
        .data$cost.consumables +
        .data$cost.subcontracting,
      Indirect_cost = 0.25 * (
        .data$Direct_cost - .data$cost.subcontracting
      ),
      Total_cost = .data$Direct_cost + .data$Indirect_cost,
      Total_funded_cost = .data$Reimbursement_rate * .data$Total_cost
    ) %>%
    kwb.utils::moveColumnsToFront(c(
      "partner_id", "partner", "partner_type", "country"
    )) %>%
    dplyr::select(- .data$Reimbursement_rate, .data$Reimbursement_rate) %>%
    dplyr::select(- .data$Total_funded_cost, .data$Total_funded_cost)
}

# get_person_months_by_wp ------------------------------------------------------
get_person_months_by_wp <- function(costs_by_wp)
{
  costs_by_wp %>%
    dplyr::select(.data$partner, .data$wp, .data$person_months.personnel) %>%
    tidyr::spread(.data$wp, .data$person_months.personnel)
}

# get_person_month_costs -------------------------------------------------------
get_person_month_costs <- function(costs_overview, costs_by_wp)
{
  # Prepare simplified table with costs
  costs_short <- prepare_cost_data_short(costs_overview)

  # Prepare table with person month for each wp
  pm_data_by_wp <- get_person_months_by_wp(costs_by_wp)

  merge(
    costs_short, pm_data_by_wp,
    by.x = "partner_name_short",
    by.y = "partner",
    all = TRUE
  ) %>%
    dplyr::select(-Total_funded_cost, Total_funded_cost)
}

# get_costs_by_country ---------------------------------------------------------
get_costs_by_country <- function(costs_overview)
{
  costs_overview %>%
    dplyr::group_by(country) %>%
    dplyr::summarise(
      Total_cost = sum(Total_cost),
      Total_funded_cost = sum(Total_funded_cost),
      n = dplyr::n()
    ) %>%
    dplyr::arrange(dplyr::desc(.data$Total_funded_cost)) %>%
    as.data.frame()
}

# read_costs_from_input_files --------------------------------------------------
read_costs_from_input_files <- function(budget_files)
{
  costs_list <- setNames(
    object = kwb.budget::read_partners_budget_from_excel(
      budget_files,
      n_work_packages = 6,
      run_parallel = FALSE # false = slower but with more debug messages
    ),
    basename(budget_files)
  )

  # There are warnings: "No data found on worksheet.", why?

  # Check for errors
  has_error <- sapply(costs_list, inherits, "try-error")

  if (any(has_error)) {

    kwb.utils::printIf(TRUE, has_error)

    print(table(has_error))

    message("Removing ", sum(has_error), " elements with errors")
  }

  # Exclude elements that caused errors
  costs_list[! has_error]
}

# to_cost_matrices -------------------------------------------------------------
to_cost_matrices <- function(costs_by_wp)
{
  to_cost_matrix <- function(x, column) {
    input <- kwb.utils::selectColumns(x, c(names(x)[1:2], column))
    kwb.utils::countOrSum(
      input, by = names(input)[1:2], sum.up = names(input)[3]
    )
  }

  cost_columns <- kwb.utils::toNamedList(names(costs_by_wp)[-(1:2)])

  lapply(cost_columns, to_cost_matrix, x = costs_by_wp)
}

# prepare_cost_data_short ------------------------------------------------------
prepare_cost_data_short <- function(costs_overview)
{
  # reduce table size
  costs_overview %>%
    dplyr::select(-c(
      pic_number,
      partner_name,
      author_name,
      author_email,
      contact_name,
      contact_email,
      Participant
    )) %>%
    dplyr::select(-Reimbursement_rate, Reimbursement_rate) %>%
    dplyr::select(-Total_funded_cost, Total_funded_cost) %>%
    kwb.utils::moveColumnsToFront(c(
      "filename", "partner_id", "partner_name_short", "partner_type", "country"
    ))
}

# read_partner_info ------------------------------------------------------------
read_partner_info <- function(columns = NULL)
{
  path <- "proposals/h2020_covid/30_Partners/DWH_Partners-LOI-EAB_List.xlsx"

  path_partners <- kwb.nextcloud::download_files(paths = path)

  result <- openxlsx::read.xlsx(
    xlsxFile = path_partners,
    sheet = "Partners-PIC-Main contact"
  )

  if (! is.null(columns)) {
    result <- kwb.utils::selectColumns(result, columns)
  }

  structure(result, path_partners = path_partners)
}

# download_partner_budget_files ------------------------------------------------
download_partner_budget_files <- function()
{
  file_info <- kwb.nextcloud::list_files(
    "proposals/h2020_covid/60_Budget/10_Filled_out_forms",
    pattern = "DWH_partner-budget_\\d\\d",
    recursive = TRUE,
    full_info = TRUE
  )

  kwb.nextcloud::download_files(file_info$href)
}
