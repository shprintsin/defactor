#' Add Labeled Column to Factored Variable
#'
#' This function adds a labeled column to a data frame based on a factored variable with associated labels.
#' It allows for customization of the new column's name and suffix.
#'
#' @param df A data frame containing the factored variable.
#' @param col A string specifying the name of the factored variable column in \code{df}.
#' @param bycol A string specifying the name for the new labeled column. Defaults to \code{NULL}, in which case the name will be constructed using \code{col} and \code{suffix}.
#' @param suffix A string specifying the suffix to be added to \code{col} to create the name for the new labeled column. Defaults to \code{"_label"}.
#' @return A data frame containing the original data with an additional labeled column.
#' @details The function retrieves the `labels` attribute of a factor variable and uses it to create a new column with descriptive labels. The new column name is either specified by the \code{bycol} parameter or dynamically generated using the \code{suffix} parameter.
#' @examples
#' # Create a sample data frame with a factor column
#' df <- data.frame(
#'   category = factor(c(1, 2, 1, 3)),
#'   value = c(10, 20, 15, 25)
#' )
#'
#' # Assign labels to the factor column
#' attr(df$category, "labels") <- c('A' = 1, 'B' = 2, 'C' = 3)
#'
#' # Use the function with the default suffix
#' df_with_labels <- getLabel(df, 'category')
#'
#' # Use the function with a custom suffix
#' df_with_custom_suffix <- getLabel(df, 'category', suffix = "_desc")
#'
#' print(df_with_labels)
#' print(df_with_custom_suffix)
#' @export
#' @title Get Label for a Column
#' @description Retrieves the labels of a specified column in a data frame.
#' @param df A data frame containing the column.
#' @param col The column name for which labels are to be retrieved.
#' @param suffix A string to append to the label column name (default: "_labels").
#' @param limit The maximum number of labels to return (default: 0, which means all labels).
#' @return A data frame with the column values and their corresponding labels.
#' @examples
#' getLabel(df, "V1", limit = 3)
library(haven)
df=read_dta("c:/cde/relig/WV4_Data_Bosnia_and_Herzegovina_Stata_v20201117.dta")
df=head(df[2:10],25)
df=data.frame(df)

getLabel <- function(df, col, suffix = "_labels", limit = 0) {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  if (!col %in% names(df)) {
    stop("Error: The specified column does not exist in the data frame.")
  }

  values <- attr(df[[col]], "labels")
  if (is.null(values) || length(values) == 0) {
    return(data.frame(value = NA, label = "No Labels Found", stringsAsFactors = FALSE))
  }
  else {
    lookup_table <- data.frame(value = array(values), label = names(values), stringsAsFactors = FALSE)
  }



  if (limit > 0) {
    result <- head(lookup_table, limit)
  } else {
    result <- lookup_table
  }

  colname <- paste0(col, suffix)
  names(result) <- c(col, colname)

  omitted <- lookup_table$value[!lookup_table[["value"]] %in% result[["value"]]]
#  print(paste0('Omitted ', length(omitted), ' variables.'))

  return(result)
}

#' @title Set Label for a Column
#' @description Merges labels into the original data frame for a specified column.
#' @param df A data frame.
#' @param col The column name for which labels are to be set.
#' @param suffix A string to append to the label column name (default: "_label").
#' @return A data frame with the labels merged into the original data frame.
#' @examples
#' setLabel(df, "V5", suffix = "_label")
setLabel <- function(df, col, suffix = "_label") {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  if (!col %in% names(df)) {
    stop("Error: The specified column does not exist in the data frame.")
  }

  lookup_table <- getLabel(df, col, suffix)

  if (length(lookup_table[["value"!=NA]])!=0){
  result <- merge(df, lookup_table, by = col, all.x = TRUE)
  }  else {    print(paste0(col,': no labels Found'))
    return(
      df

    )
  }
  return(result)
}


#' @title Get Details for All Columns
#' @description Retrieves labels and details for all columns in the data frame.
#' @param df A data frame.
#' @param limit The maximum number of labels per column (default: 0, which means all labels).
#' @param rep A logical value indicating whether to allow repetitions (default: FALSE).
#' @return A data frame with variable names, labels, category values, and category labels.
#' @examples
#' colInfo(df, limit = 1)
colInfo <- function(df, limit = 0, rep = FALSE) {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  all_vars <- data.frame()

  for (col in colnames(df)) {
    varlabel <- attr(df[[col]], 'label')
    if (length(varlabel)==0){
      varlabel=""
    }
    lookup_table <- getLabel(df, col, limit = limit)
    res <- cbind(var = col, var_label = varlabel, lookup_table)

    if (!rep) {
      res$var_label[-1] <- ""
    }

    names(res) <- c("var", "variable_label", "category_value", "category_label")
    all_vars <- rbind(all_vars, res)
  }
  return(all_vars)
}

#' @title Get Variable Labels
#' @description Retrieves all variable names and their labels from the data frame.
#' @param df A data frame.
#' @return A data frame with variable names and their corresponding labels.
#' @examples
#' listVars(df)
listVars <- function(df) {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  labels <- c()
  names <- c()

  for (col in colnames(df)) {
    label <- attr(df[[col]], "label")
    labels <- c(labels, ifelse(length(label) == 0, "", label))
    names <- c(names, col)
  }

  result <- data.frame(var = names, labels = labels)
  return(result)
}

#' @title Set Labels for All Columns by Pattern
#' @description Sets labels for all columns in a data frame that match a specified regex pattern.
#' @param df A data frame.
#' @param pattern A regex pattern to match column names (default: ".*" for all columns).
#' @return A data frame with labels set for the matching columns.
#' @examples
#' setLblPat(df, pattern = "^V")
setLblPat <- function(df, pattern = ".*") {
  matching_columns <- grep(pattern, colnames(df), value = TRUE)

  if (length(matching_columns) == 0) {
    stop("Error: No columns match the provided pattern.")
  }

  result <- df
  result <- setlstLbl(df,c(matching_columns))




  return(result)
}

# Example Usage

#' @title Set Labels for All Columns by Selected Names
#' @description Sets labels for all columns in a data frame that match a specified list of column names.
#' @param df A data frame.
#' @param columns A character vector of column names for which labels should be set.
#' @return A data frame with labels set for the specified columns.
#' @examples
#' setlstLbl(df, columns = c("V1", "V2"))
setlstLbl <- function(df, columns = c()) {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  if (length(columns) == 0) {
    stop("Error: No column names provided.")
  }

  invalid_columns <- columns[!columns %in% colnames(df)]
  if (length(invalid_columns) > 0) {
    stop(paste("Error: The following columns do not exist in the data frame:", paste(invalid_columns, collapse = ", ")))
  }

  result <- df
  for (col in colnames(df)) {
    result <- setLabel(result, col)
  }

  return(result)
}



### example

# # Load the required package
# library(data.table)
#
# # Create the data table
# dt <- data.table(
#   C_COW_ALPHA = c("BOS", "BOS", "BOS", "BOS", "BOS"),
#   COW = c(346, 346, 346, 346, 346),
#   B_COUNTRY_ALPHA = c("BIH", "BIH", "BIH", "BIH", "BIH"),
#   V2 = c(70, 70, 70, 70, 70),
#   V2A = c(70, 70, 70, 70, 70),
#   V3 = c(1, 2, 3, 4, 5),
#   V4 = c(1, 1, 1, 1, 1),
#   V5 = c(1, 1, 1, 1, 1),
#   V6 = c(1, 2, 2, 1, 2)
# )
#
# # Add column labels as attributes
# attr(dt$COW, "label") <- "colum1"
# attr(dt$V2, "label") <- "colum2"
# attr(dt$V2A, "label") <- "colum3"
# attr(dt$V4, "label") <- "colum4"
# attr(dt$V5, "label") <- "colum5"
# attr(dt$V6, "label") <- "colum6"
#
# # Add value labels as attributes
# attr(dt$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
# attr(dt$V2, "labels") <- c("Bosnia Herzegovina" = 70)
# attr(dt$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
# attr(dt$V4, "labels") <- c("Very important" = 1)
# attr(dt$V5, "labels") <- c("Very important" = 1)
# attr(dt$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
#
#
#
# getLabel(df, "V3", limit = 3)
# setLabel(df, "V2", suffix = "_label")
# colInfo(df, limit = 2)
# listVars(df)
# #setLblPat(df, pattern = "V*")
# setlstLbl(df, columns = c('V5','V6'))
# setLblPat(df, pattern = "V")
#
# structure(df)
