# devtools::install_github("kwb-r/kwb.utils")
# devtools::install_github("kwb-r/kwb.db")
# kwb.package::updateKwbPackages("kwb.db")
# kwb.package::updateKwbPackages("kwb.utils")
library(dplyr)
library(kwb.utils)

# define folder for bugdet related files
#folder_bugdet <- "//servername/projekte$/PLANNING_PROJECTS/H2020_2018_Digital/Stage 2/60_Budget/"
#folder_bugdet <- "C:/Users/username/Desktop/H2020_2018_Digital/Stage 2/60_Budget/"
folder_bugdet <- "//servername/projekte$/PLANNING_PROJECTS/H2020_2018_Digital/Stage 3 GA/50_Bugdet/"

# DATA PREPARATION -------------------------------------------------------------
if (FALSE)
{
  # input costs files
  files <- dir(paste0(folder_bugdet, "10_Filled_out_forms"), "xlsx$", 
               full.names = TRUE, all.files = FALSE)
  
  # get costs data from input files
  system.time(costs_list <- lapply(seq_along(files), function(i) {
    
    #i=1
    file <- files[i]
    message(sprintf("Reading '%s' (%d/%d)...", basename(file), i, length(files)))
    try(read_partner_budget_from_excel(file))
    
    
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

# read_partner_budget_from_excel -----------------------------------------------
read_partner_budget_from_excel <- function(file, dbg = TRUE)
{
  #kwb.utils::assignArgumentDefaults(read_partner_budget_from_excel)
  
  #file <- files[1]
  
  ranges <- kwb.db:::getNamedExcelRanges(file)
  
  general <- rbind(ranges$range_partner, ranges$range_contact)
  
  general <- kwb.utils::toLookupTable(general$Key, general$Value)
  
  rownames(ranges$range_direct) <- ranges$range_direct$Key
  
  ranges$range_direct <- kwb.utils::removeColumns(
    ranges$range_direct, "Key", dbg = FALSE
  )
  
  budget <- kwb.utils::noFactorDataFrame(
    Participant	= general$partner_short_name,
    Country	= "",
    #Direct_personnel_costs = sum(ranges$range_personnel[["Cost (EUR)"]]),
    Direct_personnel_cost = ranges$range_direct["sum_personnel", "Cost (EUR)"],
    Direct_other_cost = sum(ranges$range_direct[2:4, "Cost (EUR)"]),
    Direct_subcontracting_cost = ranges$range_direct["sum_subcontracting", "Cost (EUR)"],
    Indirect_cost = ranges$range_indirect[["Cost (EUR)"]],
    Total_cost = ranges$range_total[1, "Cost (EUR)"],
    Reimbursement_rate = general$reimbursement_rate,
    Total_funded_cost = ranges$range_total[2, "Cost (EUR)"]
  )
  
  (personnel <- kwb.utils::renameAndSelect(ranges$range_personnel, list(
    "Estimated Person Months (PM) per Work Package" = "wp_name",
    "Cost (EUR)" = "cost",
    "Work (PM)" = "person_months"
  )))
  
  wp_acronyms <- substr(personnel$wp_name, 1, 3)
  
  stopifnot(identical(wp_acronyms, paste0("WP", 1:7)))
  
  personnel <- removeColumns(setColumns(personnel, wp = 1:7), "wp_name")
  
  (consumables <- kwb.utils::renameAndSelect(ranges$range_consumables, list(
    "Position" = "position",
    #"Item" = "item",
    "WP " = "wp",
    "Cost (EUR)" = "cost"
  )))
  
  (equipment <- kwb.utils::renameAndSelect(ranges$range_equipment, list(
    "Position" = "position",
    #"Description_Please specify type, may also comprise existing equi" = "item",
    "WP " = "wp",
    "(A/B)*C*D_Eligible Cost_(EUR)" = "cost",
    "(A)_Total Cost_(EUR)"= "total_cost",
    "(B)_Period of depreciation_(Months)" = "months_depreciation",
    "(C)_Period of use_(Months)" = "months_usage",
    "(D)_Usage in the project_(%)" = "percent_usage"
  )))
  
  (subcontracting <- kwb.utils::renameAndSelect(ranges$range_subcontracting, list(
    "Position" = "position",
    #"Task Subcontracted" = "item",
    "WP " = "wp",
    "Cost (EUR)" = "cost"
  )))
  
  bind_partner <- function(x) {
    cbind(kwb.utils::noFactorDataFrame(partner = general$partner_short_name), x)
  }
  
  structure(
    cbind(general, budget, stringsAsFactors = FALSE),
    personnel = bind_partner(personnel),
    consumables = bind_partner(consumables),
    equipment = bind_partner(equipment),
    subcontracting = bind_partner(subcontracting)
  )
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