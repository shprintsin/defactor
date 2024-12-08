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
#' # Load the package
#' library(defactor)
#'
#' #Load the required package
#' library(data.table)
#' # Create the data table
#' df <- data.table(
#'   C_COW_ALPHA = c("BOS", "BOS", "BOS", "BOS", "BOS"),
#'   COW = c(346, 346, 346, 346, 346),
#'   B_COUNTRY_ALPHA = c("BIH", "BIH", "BIH", "BIH", "BIH"),
#'   V2 = c(70, 70, 70, 70, 70),
#'   V2A = c(70, 70, 70, 70, 70),
#'   V3 = c(1, 2, 3, 4, 5),
#'   V4 = c(1, 1, 1, 1, 1),
#'   V5 = c(1, 1, 1, 1, 1),
#'   V6 = c(1, 2, 2, 1, 2)
#' )
#'
#' # Add column labels as attributes
#' attr(df$COW, "label") <- "colum1"
#' attr(df$V2, "label") <- "colum2"
#' attr(df$V2A, "label") <- "colum3"
#' attr(df$V4, "label") <- "colum4"
#' attr(df$V5, "label") <- "colum5"
#' attr(df$V6, "label") <- "colum6"
#'
#' # Add value labels as attributes
#' attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
#' attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
#' attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
#' attr(df$V4, "labels") <- c("Very important" = 1)
#' attr(df$V5, "labels") <- c("Very important" = 1)
#' attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
#' attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)

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
    result <- utils::head(lookup_table, limit)
  } else {
    result <- lookup_table
  }

  colname <- paste0(col, suffix)
  names(result) <- c(col, colname)

  return(result)
}


