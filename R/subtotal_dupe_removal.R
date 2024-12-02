#' Duplicate Subtotal Row Removal Function

#' @description Removes duplicate subtotal rows,
#' which may be created by totalling a variable with only one level.
#' In tables with many variables, some may have only one level within one section and many in other sections.

#' @param data Data frame or tibble to remove duplicate row labels from.
#' @param column Column containing duplicate row labels.
#' @param iterator Minimum number of rows meant to be between each section. Usually two.
#' @param skip Number of rows to skip removing rows from. Usually zero. Can be used to dodge NA values.
#' @param remove Label of subtotals to be removed. Usually "All".
#' @param lead_name Default name for lead column used to filter duplicates.

#' @details Adds a leading version of the requested column,
#' which places each observation in the same row as the next observation.
#' Usually the observation 2 rows on - determined by the iterator.
#' From here if both the original and leading column equal the value to be removed,
#' then the row is a duplicate subtotal and is removed.
#' Note: the last few rows will have NA values in the leading column, so they are covered separately.
#' Note: If you already have columns named Lead or Index, they receive a temporary suffix.

#' @return The data report without duplicate subtotal rows.
#' @export

#' @examples
#' library(dplyr)
#'
#' group_by(mtcars, cyl, vs) %>% summarise(sum(wt), .groups = "keep") %>%
#' subtotal_row(mtcars, "wt") %>%
#' subtotal_dupe_removal(2)

#' group_by(mtcars, cyl, vs, am) %>% summarise(mean(hp), .groups = "keep") %>%
#' subtotal_row(mtcars, "hp", "mean") %>%
#' subtotal_dupe_removal(3, skip = 1)

#' group_by(mtcars, cyl, vs, am) %>% summarise(mean(hp), .groups = "keep") %>%
#' subtotal_row(mtcars, "hp", "mean") %>%
#' subtotal_dupe_removal(3, skip = 1)

subtotal_dupe_removal <- function(data, column, iterator = 2, skip = 0,
                                  remove = "All", lead_name = "Lead_Column"){

  if(length(column) > 1){
    stop("This function only accepts one column")
  }
  if(nrow(data) <= iterator | nrow(data) <= skip){
    warning(ifelse(nrow(data) <= iterator,
                   "Your iterator is too big for this data report.",
                   "You're skipping through too many rows."))
  }
  i <- 1
  while(lead_name %in% names(data) & i < 1000){
    lead_name <- paste0(lead_name, letters[sample(1:length(letters), 1)])
    i <- i + 1
  }
  if(i == 1000){
    stop("Please designate an unused name in your data frame in the lead_name parameter
         and try again.")
  }

  lead_column <- select(data, column) %>% rename_all(function(x){x = lead_name})
  lead_data <- cbind(data, lead_column)
  lead_data <- mutate_at(lead_data, .vars = lead_name,
                         .funs = lead, n = iterator)
  lead_data_filter <- filter(lead_data,
                             !(row_number() > skip & across(lead_name) == remove & across(all_of(column)) == remove) &
                               !(row_number() > (nrow(data) - iterator) & across(all_of(column)) == remove))
  lead_data_filter <- select(lead_data_filter, -lead_name)
  lead_data_filter
}
