# move_columns_right -----------------------------------------------------------
move_columns_right <- function(data, columns)
{
  kwb.utils::selectColumns(data, c(setdiff(names(data), columns), columns))
}
