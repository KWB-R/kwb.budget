
# Create Budget Files per Partner ----------------------------------------------
if (FALSE)
{

  # 1) Get partner metadata

  path_partners <- kwb.nextcloud:::download_files(
    "proposals/h2020_covid/30_Partners/DWH_Partners-LOI-EAB_List.xlsx")

  partner_infos <- openxlsx::read.xlsx(xlsxFile = path_partners,
                                       sheet = "Partners-PIC-Main contact")




  # 3) Get budget template

  path_budget_template <- kwb.nextcloud:::download_files(
    "proposals/h2020_covid/60_Budget/DWH_partner-budget_template.xlsx",
     target_dir = dirname(path_partners))

  # 3) Create one file per partner with partner metadata

output_dir <- kwb.budget::create_partners_budget_files(path_partners,
                                           path_budget_template,
                                           target_dir = file.path(dirname(path_partners),
                                                                  "10_Filled_out_forms"),
                                           set_values = TRUE)

  # 4) Open folder locally in Windows Explorer
 kwb.utils::hsOpenWindowsExplorer(output_dir)

 # 5) Manually modify EXCEL files

 # - Add more advanced rules (e.g. WP leader get XX PM for XX)
 # - Add cell protection (otherwise R script will crash if someone deletes cells/rows) and
 # - Upload to files to folder  "proposals/h2020_covid/60_Budget/10_Filled_out_forms"
 # - Send email to partners with instructions (save under same name, ...)

}

# Download Budget Files from Nextcloud -----------------------------------------
if (FALSE)
{

  # 1) Download budget forms from Nextcloud

 #path <- "proposals/bmbf_digital/Previous-projects/Budget"
  path <- "proposals/h2020_covid/60_Budget/10_Filled_out_forms"


  # 2) Generate budget files metadata (in order to detect updates, required for automation)
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
  if (is_this_the_firs_time) {

  path_local_file_info <- file.path(dirname(path_partners),
                                    "10_Filled_out_forms/file-info.csv")

  fs::dir_create(dirname(path_local_file_info))

  readr::write_csv(x = file_info_latest, path = path_local_file_info)

  kwb.nextcloud::upload_file(file = path_local_file_info,
                             target_path = "proposals/h2020_covid/60_Budget/20_Summary_Files")

  } else {

    # Provide the full paths by prepending the root path
    full_paths <- file.path(
      kwb.utils::getAttribute(file_info_latest, "root"), file_info$file
    )

    tdir_root <-  kwb.nextcloud:::create_download_dir("nextcloud_")
    tdir_forms <- file.path(tdir_root, "10_filled_out_forms")
    tdir_summary <- file.path(tdir_root, "20_Summary_Files")

    dir.create(tdir_forms)
    dir.create(tdir_summary)

    # Download the corresponding files to a temp folder below ~/../Downloads
    system.time(
      downloaded_files <- kwb.nextcloud:::download_files(paths = full_paths,
                                                         target_dir = tdir_forms)
    )

   file_info_old <-  kwb.nextcloud::download_file(
    file = path_local_file_info,
    target_path = tdir_summary)

   ### test: open directory in explorer

   #kwb.utils::hsOpenWindowsExplorer(normalizePath(tdir_root))
  }


}

# Test reading files -----------------------------------------------------------
if (FALSE)
{
  # Define the path to an Excel file to read
  #file <- "C:/Users/hsonne/Documents/../Downloads/nextcloud_3d5c2c853fd2/DWC_Partner_Budget_Arctik_FINAL.xlsx"
  #file <- path.expand(downloaded_files[1])
  budget_filled_dir <- file.path(download_dir)

  # Try to read the whole file
  files <- dir(
    budget_filled_dir, "DWH_partner-budget_[0-9][0-9].*xlsx$",
    full.names = TRUE
  )

  read_partners_budget_from_excel(files, number_of_work_packages = 6)

  # Show available ranges named "range_..."
  grep_range <- function(x) grep("^range_", x, value = TRUE)
  region_names_1 <- grep_range(kwb.db::hsTables(file))
  region_names_2 <- grep_range(openxlsx::getNamedRegions(file))

  # Same ranges with both methods?
  identical(region_names_1, region_names_2)

  # Read cell region, method 1: using RODBC
  read_region_1 <- function(x) {
    #kwb.utils::removeEmptyColumns(
    kwb.db::hsGetTable(file, x, stringsAsFactors = FALSE)
    #)
  }

  # Read cell region, method 2: using openxlsx
  read_region_2 <- function(x) {
    openxlsx::read.xlsx(
      file, namedRegion = x, skipEmptyCols = FALSE, sep.names = " "
    )
  }

  # All region names
  (region_names <- region_names_1)

  regions_data_1 <- lapply(region_names, read_region_1)
  regions_data_2 <- lapply(region_names_2, read_region_2)

  diffobj::diffStr(regions_data_1, regions_data_2)
}

# DATA PREPARATION -------------------------------------------------------------
if (FALSE)
{

  # input costs files
  budget_filled_dir <- file.path(download_dir)

  # Try to read the whole file
  files <- dir(
    budget_filled_dir, "DWH_partner-budget_[0-9][0-9].*xlsx$",
    full.names = TRUE
  )

  # get costs data from input files
  costs_list <- read_partners_budget_from_excel(files, number_of_work_packages = 6)


  # check if errors
  has_error <- sapply(costs_list, inherits, "try-error")
  print(has_error)

  # select participants without error
  costs_list <- costs_list[! has_error]

  # transform in dataframe
  costs <- kwb.utils::rbindAll(costs_list) %>% dplyr::select(-.Country)

  # table with direct costs by WP
  costs_by_wp <- kwb.budget::get_costs_by_work_package(costs_list)
  head(costs_by_wp)


  # add reimbursement rate
  costs_by_wp <- merge(costs_by_wp,
                       costs[, c("partner_short_name", "Reimbursement_rate")],
                       by.x = "partner", by.y = "partner_short_name")

  # add indirect and total costs
  costs_by_wp <- costs_by_wp %>%
    dplyr::mutate(Reimbursement_rate = 0.01 * as.numeric(sub("%", "", .data$Reimbursement_rate)),
           Direct_cost = .data$cost.personnel + .data$cost.equipment + .data$cost.consumables + .data$cost.subcontracting,
           Indirect_cost = 0.25 * (.data$Direct_cost - .data$cost.subcontracting),
           Total_cost = .data$Direct_cost + .data$Indirect_cost,
           Total_funded_cost = .data$Reimbursement_rate * .data$Total_cost)


  # load partner info
  file_partner_info <- file.path(
    file.path(download_dir, "20_Summary_Files"),
    "Partner_country_type.csv"
  )
  partner_info <- read.csv2(file_partner_info)

  # check if names are the same in the two files before merging
  check <- costs$partner_short_name %in% partner_info$Partner_short_name
  costs$partner_short_name[!check]

  # create merge costs and costs_by_wp
  costs_data <- merge(costs, partner_info, by.x = "partner_short_name", by.y = "Partner_short_name")
  head(costs_data)

  costs_data_by_wp <- merge(costs_by_wp, partner_info, by.x = "partner", by.y = "Partner_short_name")
  head(costs_data_by_wp)

  # prepare simplified table with costs
  costs_data_short <- prepare_cost_data_short(costs_data)
  costs_data_short %>% select(partner_short_name, Total_funded_cost)

  # prepare table with person month for each wp
  pm_data_by_wp <- costs_data_by_wp %>%
    select(partner, wp, person_months.personnel) %>%
    tidyr::spread(wp, person_months.personnel)

  # merge with simplified table withz costs
  costs_data_short <- merge(costs_data_short, pm_data_by_wp,
                            by.x = "partner_short_name", by.y = "partner") %>%
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
}

# ANALYSIS ---------------------------------------------------------------------
if (FALSE) {

  # get total costs by WP
  costs_by_wp_summary <- costs_by_wp_data %>%
    group_by(wp) %>%
    summarise(Total_cost = sum(Total_cost),
              Total_funded_cost = sum(Total_funded_cost)) %>%
    as.data.frame()

  costs_by_wp_summary

  par(mfrow = c(2,2))

  plot(costs_by_wp_summary$wp,
       costs_by_wp_summary$Total_cost,
       pch = 16, las = 1,
       xlab = "WP",
       ylab = "",
       yaxt = "n",
       main = "",
       col = "blue",
       cex.main = 0.7)
  points(costs_by_wp_summary$wp, costs_by_wp_summary$Total_funded_cost, col = "green")

  grid()

  axis(2, las = 2, cex = 0.4)


  barplot(costs$Total_funded_cost, names.arg =  costs$partner_short_name,
          las = 2)
  grid()

  labels = paste0("Requested grant €:",
                  round(sum(costs_by_wp_summary$Total_funded_cost), 0),
                  "\n Max. grant €:",
                  round(sum(costs_by_wp_summary$Total_cost), 0))


  plot(1, xaxt = "n", yaxt = "n", xlab = "", ylab = "", type = "n", frame.plot = FALSE)
  text(1, 1, labels = labels)


  budget_merge_country <- budget_merge %>%
    group_by(Country.y) %>%
    summarise(Total_cost = sum(Total_cost),
              Total_funded_cost = sum(Total_funded_cost)) %>%
    as.data.frame()

  budget_merge_type <- budget_merge %>%
    group_by(Type) %>%
    summarise(Total_cost = sum(Total_cost),
              Total_funded_cost = sum(Total_funded_cost)) %>%
    as.data.frame()

  library(gridExtra)
  library(grid)

  grid.table(budget_merge_country)

  grid.table(budget_merge_type)

  #

  cost_matrices <- to_cost_matrices(costs_by_wp)

  print(budget)
}





# to_cost_matrices -------------------------------------------------------------
to_cost_matrices <- function(costs_by_wp)
{
  to_cost_matrix <- function(x, column) {
    input <- kwb.utils::selectColumns(x, c(names(x)[1:2], column))
    kwb.utils::countOrSum(input, by = names(input)[1:2], sum.up = names(input)[3])
  }

  cost_columns <- kwb.utils::toNamedList(names(costs_by_wp)[-(1:2)])

  lapply(cost_columns, to_cost_matrix, x = costs_by_wp)
}

# prepare_cost_data_short ----------------------------------------------------
prepare_cost_data_short <- function(costs_data){

  # reduce table size
  costs_data <- costs_data %>%
    select(- c(pic_number, pic_number, partner_name, author_name,
               author_email, contact_name, contact_email,
               reimbursement_rate, Participant)) %>%
    select(-Reimbursement_rate, Reimbursement_rate) %>%
    select(-Total_funded_cost, Total_funded_cost)

  costs_data

}
# prepare_cost_data_by_type --------------------------------------------------
prepare_cost_data_by_type <- function (costs_data){

  # create summary by participant type
  cost_data_by_type <- costs_data %>%
    group_by(Type) %>%
    summarize(Total_funded_cost = round(sum(Total_funded_cost), 2)) %>%
    mutate(Total_funded_cost_p = round(100 * Total_funded_cost / sum(Total_funded_cost), 2),
           Type = as.character(Type)) %>%
    as.data.frame()

  # show funded costs by type
  cost_data_by_type <- rbind(cost_data_by_type,
                             c("Total",
                               sum(cost_data_by_type$Total_funded_cost),
                               sum(cost_data_by_type$Total_funded_cost_p))
  ) %>%
    mutate(Total_funded_cost = as.numeric(Total_funded_cost),
           Total_funded_cost_p = as.numeric(Total_funded_cost_p))

  cost_data_by_type

}
