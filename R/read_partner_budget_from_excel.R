#' Read Partner Budget From Excel File
#'
#' @param file full path to EXCEL file
#' @param n_work_packages number of work packages in EXCEL template
#' (default: 7, as used for DWC)
#' @param dbg debug message (default: TRUE)
#' @return list with imported EXCEL budget file data
#' @export
#' @importFrom kwb.utils noFactorDataFrame renameAndSelect removeColumns toLookupTable extractSubstring
#' @importFrom stringr str_extract
#'
read_partner_budget_from_excel <- function(
  file, n_work_packages = 7, dbg = TRUE
)
{
  #kwb.utils::assignPackageObjects("kwb.budget")
  #kwb.utils::assignArgumentDefaults(read_partner_budget_from_excel)
  #file <- files[1]

  #ranges <- kwb.db:::getNamedExcelRanges(file)

  ranges <- get_named_excel_ranges(file)

  general <- rbind(ranges$range_partner, ranges$range_contact)

  general <- kwb.utils::toLookupTable(general$Key, general$Value)

  rownames(ranges$range_direct) <- ranges$range_direct$Key

  ranges$range_direct <- kwb.utils::removeColumns(
    ranges$range_direct, "Key", dbg = FALSE
  )

  filename <- basename(file)

  budget <- kwb.utils::noFactorDataFrame(
    filename = filename,
    partner_id = as.numeric(stringr::str_extract(filename, "[0-9][0-9]")),
    #Participant	= kwb.utils::extractSubstring("_([^_]+)\\.xlsx$", filename, 1),
    Participant	= general$partner_short_name,
    #Country	= "",
    #Direct_personnel_costs = sum(ranges$range_personnel[["Cost (EUR)"]]),
    Direct_personnel_cost = ranges$range_direct["sum_personnel", "Cost.(EUR)"],
    Direct_other_cost = sum(ranges$range_direct[2:4, "Cost.(EUR)"]),
    Direct_subcontracting_cost = ranges$range_direct["sum_subcontracting", "Cost.(EUR)"],
    Indirect_cost = ranges$range_indirect[["Cost.(EUR)"]],
    Total_cost = ranges$range_total[1, "Cost.(EUR)"],
    Reimbursement_rate = as.numeric(general$reimbursement_rate),
    Total_funded_cost = ranges$range_total[2, "Cost.(EUR)"]
  )

  (personnel <- kwb.utils::renameAndSelect(ranges$range_personnel, list(
    "Estimated.Person.Months.(PM).per.Work.Package" = "wp_name",
    "Cost.(EUR)" = "cost",
    "Work.(PM)" = "person_months"
  )))

  wp_acronyms <- substr(personnel$wp_name, 1, 3)

  wp_id <- seq_len(n_work_packages)

  stopifnot(identical(wp_acronyms, paste0("WP", wp_id)))

  personnel <- kwb.utils::removeColumns(
    kwb.utils::setColumns(personnel, wp = wp_id, dbg = FALSE),
    columns = "wp_name"
  )

  (consumables <- kwb.utils::renameAndSelect(ranges$range_consumables, list(
    "Position" = "position",
    #"Item" = "item",
    "WP" = "wp",
    "Cost.(EUR)" = "cost"
  )))

  (equipment <- kwb.utils::renameAndSelect(ranges$range_equipment, list(
    "Position" = "position",
    #"Description_Please specify type, may also comprise existing equi" = "item",
    "WP" = "wp",
    "(A/B)*C*D.Eligible.Cost.(EUR)" = "cost",
    "(A).Total.Cost.(EUR)"= "total_cost",
    "(B).Period.of.depreciation.(Months)" = "months_depreciation",
    "(C).Period.of.use.(Months)" = "months_usage",
    "(D).Usage.in.the.project.(%)" = "percent_usage"
  )))

  (subcontracting <- kwb.utils::renameAndSelect(ranges$range_subcontracting, list(
    "Position" = "position",
    #"Task Subcontracted" = "item",
    "WP" = "wp",
    "Cost.(EUR)" = "cost"
  )))

  bind_partner <- function(x) {
    cbind(kwb.utils::noFactorDataFrame(partner = budget$partner_id), x)
  }

  result <- cbind(
    kwb.utils::removeColumns(general, c(
      "reimbursement_rate", "partner_short_name"
    )),
    budget,
    stringsAsFactors = FALSE
  )

  main_columns <- c("filename", "partner_id", "Participant", "partner_name")

  structure(
    kwb.utils::moveColumnsToFront(result, main_columns),
    personnel = bind_partner(personnel),
    consumables = bind_partner(consumables),
    equipment = bind_partner(equipment),
    subcontracting = bind_partner(subcontracting)
  )
}

#' Helper function: get named Excel ranges
#'
#' @param file full path to EXCEL file
#'
#' @return named Excel ranges
#' @export
#' @importFrom openxlsx getNamedRegions read.xlsx

get_named_excel_ranges <- function(file)
{
  region_names <- as.character(openxlsx::getNamedRegions(file))

  ranges <- lapply(stats::setNames(nm = region_names), function(name) {
    try(
      openxlsx::read.xlsx(file, namedRegion = name, sep.names = "."),
      silent = TRUE
    )
  })

  failed <- sapply(ranges, inherits, "try-error")

  if (any(failed)) warning(
    "The following named range(s) could not be read: ",
    kwb.utils::stringList(names(which(failed)))
  )

  ranges[! failed]
}
