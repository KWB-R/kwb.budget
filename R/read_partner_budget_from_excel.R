# read_partner_budget_from_excel -----------------------------------------------
read_partner_budget_from_excel <- function(file, dbg = TRUE)
{
  #kwb.utils::assignArgumentDefaults(read_partner_budget_from_excel)

  #file <- files[1]

  ranges <- kwb.db:::getNamedExcelRanges(file)

  ranges2 <- get_named_excel_ranges(file)

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

  personnel <- kwb.utils::removeColumns(
    kwb.utils::setColumns(personnel, wp = 1:7),
    columnsToRemove = "wp_name"
  )

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

# get_named_excel_ranges -------------------------------------------------------
get_named_excel_ranges <- function(file)
{
  region_names <- openxlsx::getNamedRegions(file)

  lapply(region_names, function(name) {
    try(openxlsx::read.xlsx(file, namedRegion = name))
  })
}
