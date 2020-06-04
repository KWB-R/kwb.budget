library(kwb.budget)

# Create Budget Files per Partner ----------------------------------------------
if (FALSE)
{
  # 1) Get partner metadata
  path_partnerlist <- "proposals/h2020_covid/30_Partners/DWH_Partners-LOI-EAB_List.xlsx"
  path_partners <- kwb.nextcloud::download_files(paths = path_partnerlist)

  partner_infos <- openxlsx::read.xlsx(
    xlsxFile = path_partners,
    sheet = "Partners-PIC-Main contact"
  )

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

  tdir_root <-  kwb.nextcloud:::create_download_dir("nextcloud_")
  tdir_forms <- file.path(tdir_root, "10_filled_out_forms")
  tdir_summary <- file.path(tdir_root, "20_Summary_Files")

  dir.create(tdir_forms)
  dir.create(tdir_summary)

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

      message(sprintf("The following files were updated:\n\n%s", paste(file_comparsion$msg[which(file_updated)],collapse = "\n")))

      ### 1) write functions and add code for analysing (code below)

      # Provide the full paths by prepending the root path
      full_paths <- file.path(
        kwb.utils::getAttribute(file_info_latest, "root"), file_info_latest$file
      )

      # Download the corresponding files to a temp folder below ~/../Downloads
      system.time(
        downloaded_files <- kwb.nextcloud:::download_files(
          paths =  full_paths,
          target_dir = tdir_forms
        )
      )

      ## Filter out only budget XLSX files
      budget_files <- dir(
        dirname(downloaded_files[1]), "DWH_partner-budget_[0-9][0-9].*xlsx$",
        full.names = TRUE
      )

      # get costs data from input files
      costs_list <- setNames(
        object = kwb.budget::read_partners_budget_from_excel(
          budget_files,
          number_of_work_packages = 6,
          run_parallel = FALSE # false = slower but better for debugging (more debug messages)
        ),
        basename(budget_files)
      )

      # check if errors
      has_error <- sapply(costs_list, inherits, "try-error")
      print(has_error)

      # select participants without error
      costs_list <- costs_list[! has_error]

      # transform in dataframe
      costs <- kwb.utils::rbindAll(costs_list) %>%
        dplyr::select(-.data$Country)

      # table with direct costs by WP
      costs_by_wp <- kwb.budget::get_costs_by_work_package(costs_list)
      head(costs_by_wp)

      # add reimbursement rate
      costs_by_wp <- merge(
        costs_by_wp,
        costs[, c("partner_short_name", "Reimbursement_rate")],
        by.x = "partner",
        by.y = "partner_short_name"
      )

      # add indirect and total costs
      costs_by_wp <- costs_by_wp %>%
        dplyr::mutate(
          Reimbursement_rate = 0.01 * as.numeric(sub("%", "", .data$Reimbursement_rate)),
          Direct_cost = .data$cost.personnel + .data$cost.equipment + .data$cost.consumables + .data$cost.subcontracting,
          Indirect_cost = 0.25 * (.data$Direct_cost - .data$cost.subcontracting),
          Total_cost = .data$Direct_cost + .data$Indirect_cost,
          Total_funded_cost = .data$Reimbursement_rate * .data$Total_cost
        )

      #  used in DWC proposal
      #   # load partner info
      #   file_partner_info <- file.path(
      #     file.path(download_dir, "20_Summary_Files"),
      #     "Partner_country_type.csv"
      #   )
      #   partner_info <- read.csv2(file_partner_info)

      # Get partner metadata (for DWH proposal)

      path_partners <- kwb.nextcloud:::download_files(
        paths = "proposals/h2020_covid/30_Partners/DWH_Partners-LOI-EAB_List.xlsx"
      )

      partner_info <- openxlsx::read.xlsx(
        xlsxFile = path_partners,
        sheet = "Partners-PIC-Main contact"
      )

      # check if names are the same in the two files before merging
      check <- costs$partner_short_name %in% partner_info$partner_name_short
      costs$partner_short_name[! check]

      # create merge costs and costs_by_wp
      costs_data <- merge(costs, partner_info, by = "partner_id")
      head(costs_data)

      ### -> this merge is not possible because partners changed "short_name in "budget" excel file
      costs_data_by_wp <- merge(
        costs_by_wp, partner_info,
        by.x = "partner",
        by.y = "Partner_short_name"
      )

      head(costs_data_by_wp)

      # prepare simplified table with costs
      costs_data_short <- prepare_cost_data_short(costs_data)
      costs_data_short %>%
        dplyr::select(.data$partner_name_short, .data$Total_funded_cost) %>%
        dplyr::arrange(dplyr::desc(.data$Total_funded_cost))

      # prepare table with person month for each wp
      pm_data_by_wp <- costs_data_by_wp %>%
        dplyr::select(partner, wp, person_months.personnel) %>%
        tidyr::spread(wp, person_months.personnel)

      # merge with simplified table withz costs
      costs_data_short <- merge(
        costs_data_short, pm_data_by_wp,
        by.x = "partner_short_name",
        by.y = "partner"
      ) %>%
        select(-Total_funded_cost, Total_funded_cost)

      # prepare table with costs by company type
      costs_data_by_type <- prepare_cost_data_by_type(costs_data_short)
      costs_data_by_type

      ### save outputs
      version_num <- 7

      folder_out <- paste0(folder_bugdet, "20_Summary_Files")
      file_out <- file.path(folder_out, paste0("V", version_num, "_DWC_Costs"))

      write.csv2(costs_data, paste0(file_out, ".csv"))
      save(costs_data, file = paste0(file_out, ".rdata"))

      write.csv2(costs_data_by_wp, paste0(file_out, "_by_wp.csv"))
      save(costs_data_by_wp, file = paste0(file_out, "_by_wp.rdata"))

      write.csv2(costs_data_short, paste0(file_out, "_short.csv"))
      write.csv2(costs_data_by_type, paste0(file_out, "_by_type.csv"))

      ## 2) if successfull -> upload new file-info.csv

      #fs::dir_create(dirname(path_local_file_info))
      #readr::write_csv(x = file_info_latest, path = path_local_file_info)
      #kwb.nextcloud::upload_file(file = path_local_file_info,
      #                           target_path = "proposals/h2020_covid/60_Budget/20_Summary_Files")
    } else {
      message("Going to sleep, because I have nothing to do! (budget files on the cloud have not changed since last execution!)")
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

# prepare_cost_data_short ----------------------------------------------------
prepare_cost_data_short <- function(costs_data)
{
  # reduce table size
  costs_data <- costs_data %>%
    dplyr::select(-c(
      pic_number,
      partner_name,
      author_name,
      author_email,
      contact_name,
      contact_email,
      reimbursement_rate,
      Participant
    )) %>%
    dplyr::select(-Reimbursement_rate, Reimbursement_rate) %>%
    dplyr::select(-Total_funded_cost, Total_funded_cost)

  costs_data
}
