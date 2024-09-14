#' @title Get Label for a Column
#' @name getLbl
#' @description Retrieves the labels of a specified column in a data frame.
#' @param df A data frame containing the column.
#' @param col The column name for which labels are to be retrieved.
#' @param suffix A string to append to the label column name (default: "_labels").
#' @param limit The maximum number of labels to return (default: 0, which means all labels).
#' @return A data frame with the column values and their corresponding labels.
#' @details The function retrieves the `labels` attribute of a factor variable and uses it to create a new column with descriptive labels. The new column name is either specified by the `bycol` parameter or dynamically generated using the `suffix` parameter.
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
#' df_with_labels <- getLbl(df, 'category')
#'
#' # Use the function with a custom suffix
#' df_with_custom_suffix <- getLbl(df, 'category', suffix = "_desc")
#'
#' print(df_with_labels)
#' print(df_with_custom_suffix)
#' @export
getLbl <- function(df, col, suffix = "_labels", limit = 0) {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  if (!col %in% names(df)) {
    stop("Error: The specified column does not exist in the data frame.")
  }

  values <- attr(df[[col]], "labels")
  if (is.null(values) || length(values) == 0) {
    return(data.frame(value = NA, label = "No Labels Found", stringsAsFactors = FALSE))
  } else {
    lookup_table <- data.frame(value = array(values), label = names(values), stringsAsFactors = FALSE)
  }

  if (limit > 0) {
    result <- head(lookup_table, limit)
  } else {
    result <- lookup_table
  }

  colname <- paste0(col, suffix)
  names(result) <- c(col, colname)

  return(result)
}

#' @title Set Label for a Column
#' @name setLbl
#' @description Merges labels into the original data frame for a specified column.
#' @param df A data frame.
#' @param col The column name for which labels are to be set.
#' @param suffix A string to append to the label column name (default: "_label").
#' @return A data frame with the labels merged into the original data frame.
#' @examples
#' setLbl(df, "V5", suffix = "_label")
#' @export
setLbl <- function(df, col, suffix = "_label") {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  if (!col %in% names(df)) {
    stop("Error: The specified column does not exist in the data frame.")
  }

  lookup_table <- getLbl(df, col, suffix)

  if (length(lookup_table[["value"]] != NA) != 0) {
    result <- merge(df, lookup_table, by = col, all.x = TRUE)
  } else {
    print(paste0(col, ': no labels Found'))
    return(df)
  }
  return(result)
}

#' @title Get Details for All Columns
#' @name colInfo
#' @description Retrieves labels and details for all columns in the data frame.
#' @param df A data frame.
#' @param limit The maximum number of labels per column (default: 0, which means all labels).
#' @param rep A logical value indicating whether to allow repetitions (default: FALSE).
#' @return A data frame with variable names, labels, category values, and category labels.
#' @examples
#' colInfo(df, limit = 1)
#' @export
colInfo <- function(df, limit = 0, rep = FALSE) {
  if (!exists("df") || !is.data.frame(df)) {
    stop("Error: The input 'df' is not defined or is not a data frame.")
  }

  all_vars <- data.frame()

  for (col in colnames(df)) {
    varlabel <- attr(df[[col]], 'label')
    if (length(varlabel) == 0) {
      varlabel <- ""
    }
    lookup_table <- getLbl(df, col, limit = limit)
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
#' @name listVars
#' @description Retrieves all variable names and their labels from the data frame.
#' @param df A data frame.
#' @return A data frame with variable names and their corresponding labels.
#' @examples
#' listVars(df)
#' @export
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
#' @name setLblPat
#' @description Sets labels for all columns in a data frame that match a specified regex pattern.
#' @param df A data frame.
#' @param pattern A regex pattern to match column names (default: ".*" for all columns).
#' @return A data frame with labels set for the matching columns.
#' @examples
#' setLblPat(df, pattern = "^V")
#' @export
setLblPat <- function(df, pattern = ".*") {
  matching_columns <- grep(pattern, colnames(df), value = TRUE)

  if (length(matching_columns) == 0) {
    stop("Error: No columns match the provided pattern.")
  }

  result <- df
  result <- setlstLbl(df, matching_columns)

  return(result)
}

#' @title Set Labels for All Columns by Selected Names
#' @name setlstLbl
#' @description Sets labels for all columns in a data frame that match a specified list of column names.
#' @param df A data frame.
#' @param columns A character vector of column names for which labels should be set.
#' @return A data frame with labels set for the specified columns.
#' @examples
#' setlstLbl(df, columns = c("V1", "V2"))
#' @export
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
  for (col in columns) {
    result <- setLbl(result, col)
  }

  return(result)
}
