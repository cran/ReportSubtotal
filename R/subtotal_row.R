#' Subtotal Row Addition Function

#' @description Adds subtotal rows to a data report.

#' @param report A data report.
#' @param frame Data frame summarised by the data report.
#' @param vars Names of column(s) in the data frame aggregated in the data report.
#' @param aggregator Function to aggregate the data with.
#' @param exclude Vector of column indices determining which variables don't require subtotal rows.
#' @param agg_parameter Optional parameter for the aggregation function to use.
#' @param subtotal_label Label to be used for subtotal rows.

#' @details The dataset and report are factorized,
#' and a series of reports with the same variables are then generated,
#' but with some variables replaced by a subtotal label,
#' which effectively concentrates all levels of those variables
#' into one subtotal row for those variables.
#' The subtotal reports are all combined with the original report,
#' and the combined report is sorted, sorting the subtotal label to the top for all variables.

#' @return The data report with subtotal rows included.
#' @export

#' @examples
#' library(dplyr)
#'
#' group_by(iris, Species, Petal.Width) %>%
#' summarise(sum(Petal.Length), .groups = "keep") %>%
#' subtotal_row(iris, vars = "Petal.Length")
#'
#' group_by(iris, Species, Petal.Width) %>%
#' summarise(mean(Sepal.Width), .groups = "keep") %>%
#' subtotal_row(iris, vars = "Sepal.Width", aggregator = "mean")
#'
#' group_by(mtcars, cyl, gear, carb) %>%
#' summarise(median(wt), median(hp), .groups = "keep") %>%
#' subtotal_row(mtcars, vars = c("wt", "hp"), aggregator = "median")
#'
#' group_by(mtcars, cyl, gear, carb) %>%
#' summarise(Med_Weight = median(wt), Med_Hrspw = median(hp), .groups = "keep") %>%
#' subtotal_row(mtcars, vars = c("wt", "hp"), aggregator = "median", exclude = 1)
#'
#' group_by(mtcars, vs, am, drat, carb) %>%
#' summarise(min(mpg), min(disp), min(carb), .groups = "keep") %>%
#' subtotal_row(mtcars, vars = c("mpg", "disp", "carb"),
#' aggregator = "min", exclude = c(2, 4), subtotal_label = "Min_Cars_Total", agg_parameter = "na.rm")

subtotal_row <- function(report, frame, vars = "Population", aggregator = "sum", exclude = numeric(0),
                         agg_parameter = character(0), subtotal_label = "All"){

  include_vec <- list()
  final_label <- ncol(report) - length(vars)
  label_cols <- 1:final_label
  label_names <- names(report)[label_cols]


  exclude_length <- length(exclude)
  param_length <- length(agg_parameter)

  if (ncol(frame) == 1 | ncol(report) == 1){
    ifelse(ncol(frame) == 1,
           stop("\nYour dataset is too small!", call. = FALSE),
           stop("\nYour data report is too small!", call. = FALSE))
  }
  if (length(aggregator) > 1){
    stop("\nThis function doesn't accept multiple aggregators", call. = FALSE)
  }
  if (length(agg_parameter) > 1){
    stop("\nThis function only allows for one aggregation function parameter", call. = FALSE)
  }
  if (exclude_length > 0){
    if(min(exclude %in% label_cols) == 0 | length(unique(exclude)) < exclude_length){
      ifelse(min(exclude %in% label_cols) == 0,
             stop("\nSome excluded columns are not in the table", call. = FALSE),
             stop("\nSome excluded columns are repeated", call. = FALSE))
    }
  }
  if(min(c(label_names, vars) %in% names(frame)) == 0){
    stop("Some requested variables are not in your data frame.", call. = FALSE)
  }
  if (aggregator == "n"){
    warning("\nWarning: length() will replace n().")
    aggregator <- "length"
  }
  if(!("grouped_df" %in% class(report))){
    warning("\nWarning: Your report isn't grouped. Error handling may fail.")
  }
  if (max(vars %in% label_names) > 0){
    overlaps <- vars[vars %in% label_names]
    frame <- mutate_at(frame, .vars = overlaps,
                       .funs = c(Default_Suffix = function(x){x}))
    vars[vars %in% label_names] <- if(length(overlaps) == 1) {"Default_Suffix"} else {paste0(overlaps, "_Default_Suffix")}
    warning("\nWarning: your labels and variables are overlapping. The overlapping variables will receive a default suffix to fix this.\n")
  }
  if("grouped_df" %in% class(report) & length(label_names) != length(groups(report))){
    ifelse(length(label_names) < length(groups(report)),
           stop("\nYou're using more variables than your report can hold", call. = FALSE),
           warning("\nWarning: You may not be using all the variables in your report. Try using .groups = keep in your summarise() call."))
  }

  if (ncol(frame) == 1 | ncol(report) == 1){
    ifelse(ncol(frame) == 1,
           stop("\nYour dataset is too small!", call. = FALSE),
           stop("\nYour data report is too small!", call. = FALSE))
  }
  if (length(aggregator) > 1){
    stop("\nThis function doesn't accept multiple aggregators", call. = FALSE)
  }
  if (aggregator == "n"){
    warning("\nWarning: length() will replace n().")
    aggregator <- "length"
  }
  if(!("grouped_df" %in% class(report))){
    warning("\nWarning: Your report isn't grouped. Error handling may fail.")
  }
  if (exclude_length > 0){
    if(min(exclude %in% label_cols) == 0 | length(unique(exclude)) < exclude_length){
      ifelse(min(exclude %in% label_cols) == 0,
             stop("Some excluded columns are not in the table", call. = FALSE),
             stop("Some excluded columns are repeated", call. = FALSE))
    }
  }
  if (exclude_length == length(label_cols)){
    warning("\nWarning: Excluding all variables means adding nothing")
  }

  report <- ungroup(report)
  factor_report <- mutate_at(report, label_cols, function(x){if(is.factor(x)){x} else {factor(x)}})
  report_levels <- lapply(factor_report[,label_cols], function(x){unique(levels(x))})
  report_levels_subtotal <- lapply(report_levels, function(x){c(subtotal_label, x)})

  select_frame <- select(frame, all_of(c(names(report)[label_cols], vars)))

  for (l in label_cols){
    select_frame <- mutate_at(select_frame, l, function(x){
      if(is.factor(x)){x} else {factor(x, levels = report_levels[[l]])}})
  }


  for (i in label_cols){
    include_vec[[i]] <- i:final_label
  }

  if (exclude_length > 0){
    exclude_vec <- !(1:final_label %in% exclude)
    include_vec <- include_vec[exclude_vec]
  }


  for (j in 1:length(include_vec)){
    mutate_frame <- mutate_at(select_frame, include_vec[[j]], function(x){x = factor(subtotal_label)})
    if (length(agg_parameter) == 0){
      mutate_report <- group_by_at(mutate_frame, label_cols) %>% summarise_at(vars, get(aggregator))
    }
    else if (agg_parameter == "na.rm"){
      mutate_report <- group_by_at(mutate_frame, label_cols) %>% summarise_at(vars, get(aggregator), na.rm = TRUE)
    }
    else {
      warning("\nWarning: This aggregation parameter is currently not supported and will not be used.\n")
      mutate_report <- group_by_at(mutate_frame, label_cols) %>% summarise_at(vars, get(aggregator))
    }
    mutate_report <- ungroup(mutate_report)
    names(mutate_report) <- names(report)
    report <- rbind(report, mutate_report)
  }

  report <- mutate_at(report, label_cols, as.character)
  for (k in label_cols){
    report <- mutate_at(report, k, factor, report_levels_subtotal[[k]])
  }
  if (TRUE %in% is.na(select(report, -all_of(label_cols)))){
    warning("NA values detected in output. Setting agg_parameter to \"na.rm\" may resolve this.")
  }

  arrange_at(report, label_cols)
}
