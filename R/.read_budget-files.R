# remotes::install_github("kwb-r/kwb.utils")
# remotes::install_github("kwb-r/kwb.db")
# remotes::install_github("kwb-r/kwb.nextcloud@dev")

library(dplyr)
library(kwb.utils)

# define folder for bugdet related files
#folder_bugdet <- "//servername/projekte$/PLANNING_PROJECTS/H2020_2018_Digital/Stage 2/60_Budget/"
#folder_bugdet <- "C:/Users/username/Desktop/H2020_2018_Digital/Stage 2/60_Budget/"
#folder_bugdet <- "//servername/projekte$/PLANNING_PROJECTS/H2020_2018_Digital/Stage 3 GA/50_Bugdet/"

# List and download nextcloud files --------------------------------------------
if (FALSE)
{
  path <- "proposals/bmbf_digital/Previous-projects/Budget"
  #path <- "proposals/h2020_covid/60_Budget"

  # List files recursively (only xlsx or csv files)
  file_info <- kwb.nextcloud::list_files(
    path = path,
    pattern = "(xlsx|csv)$",
    recursive = TRUE,
    full_info = TRUE
  )

  # Check the result
  View(file_info)

  # Provide the full paths by prepending the root path
  full_paths <- file.path(
    kwb.utils::getAttribute(file_info, "root"), file_info$file
  )

  # Download the corresponding files to a temp folder below ~/../Downloads
  system.time(
    downloaded_files <- kwb.nextcloud:::download_files(paths = full_paths)
  )

  download_dir <- dirname(dirname(downloaded_files[1]))
  kwb.utils::hsOpenWindowsExplorer(download_dir)
}

# Test reading files -----------------------------------------------------------
if (FALSE)
{
  # Define the path to an Excel file to read
  #file <- "C:/Users/hsonne/Documents/../Downloads/nextcloud_3d5c2c853fd2/DWC_Partner_Budget_Arctik_FINAL.xlsx"
  #file <- path.expand(downloaded_files[1])

  # Try to read the whole file
  files <- dir(
    "C:/Users/micha/Documents/../Downloads/nextcloud_38c41c3e466b/", "DWC_.*xlsx$",
    full.names = TRUE
  )

  parallel::
  for (file in files) {
    print(file)
    kwb.budget:::read_partner_budget_from_excel(file)
  }

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
  files <- dir(download_dir, "xlsx$",
               full.names = TRUE, all.files = FALSE)

  # get costs data from input files
  system.time(costs_list <- lapply(seq_along(files), function(i) {

    #i=1
    file <- files[i]
    message(sprintf("Reading '%s' (%d/%d)...", basename(file), i, length(files)))
    try(kwb.budget::read_partner_budget_from_excel(file))


  }))

  # parallel get costs data from input files
  ncores <- parallel::detectCores()
  parallel::makeCluster(ncores)
  system.time(costs_list <- lapply(seq_along(files), function(i) {

    #i=1
    file <- files[i]
    message(sprintf("Reading '%s' (%d/%d)...", basename(file), i, length(files)))
    try(kwb.budget::read_partner_budget_from_excel(file))


  }))



  # check if errors
  has_error <- sapply(costs_list, inherits, "try-error")
  print(has_error)

  # select participants without error
  costs_list <- costs_list[! has_error]

  # transform in dataframe
  costs <- rbindAll(costs_list) %>% select(-Country)

  # table with direct costs by WP
  costs_by_wp <- get_costs_by_work_package(costs_list)
  head(costs_by_wp)


  # add reimbursement rate
  costs_by_wp <- merge(costs_by_wp,
                       costs[, c("partner_short_name", "Reimbursement_rate")],
                       by.x = "partner", by.y = "partner_short_name")

  # add indirect and total costs
  costs_by_wp <- costs_by_wp %>%
    mutate(Reimbursement_rate = 0.01 * as.numeric(sub("%", "", costs_by_wp$Reimbursement_rate)),
           Direct_cost = cost.personnel + cost.equipment + cost.consumables + cost.subcontracting,
           Indirect_cost = 0.25 * (Direct_cost - cost.subcontracting),
           Total_cost = Direct_cost + Indirect_cost,
           Total_funded_cost = Reimbursement_rate * Total_cost)


  # load partner info
  file_partner_info <- file.path(
    paste0(folder_bugdet, "20_Summary_Files"),
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

# get_costs_by_work_package ----------------------------------------------------
get_costs_by_work_package <- function(costs)
{
  `%>%` <- magrittr::`%>%`

  collect_lines_with_work_package <- function(x, name) {
    result <- lapply(x, getAttribute, name)
    result <- lapply(result, append_zero_costs)
    result <- rbindAll(result)
    result$cost <- kwb.utils::defaultIfNA(result$cost, 0)
    result[! is.na(result$wp), ]
  }

  personnel <- collect_lines_with_work_package(costs, "personnel")
  equipment <- collect_lines_with_work_package(costs, "equipment")
  consumables <- collect_lines_with_work_package(costs, "consumables")
  subcontracting <- collect_lines_with_work_package(costs, "subcontracting")

  sum_by_work_package <- function(x) {
    x %>% dplyr::group_by(partner, wp) %>% dplyr::summarise(cost = sum(cost))
  }

  sum_pm_by_work_package <- function(x) {
    x %>% dplyr::group_by(partner, wp) %>% dplyr::summarise(person_months = sum(person_months, na.rm = TRUE))
  }

  kwb.utils::mergeAll(by = c("partner", "wp"), list(
    personnel = sum_pm_by_work_package(personnel),
    personnel = sum_by_work_package(personnel),
    equipment = sum_by_work_package(equipment),
    consumables = sum_by_work_package(consumables),
    subcontracting = sum_by_work_package(subcontracting)
  ))
}

# append_zero_costs ------------------------------------------------------------
append_zero_costs <- function(x)
{
  kwb.utils::safeRowBind(x, kwb.utils::noFactorDataFrame(
    partner = unique(x$partner), wp = 1:7, cost = 0
  ))
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
