#' @title Set Label for a Column
#' @name setLbl
#' @description Merges labels into the original data frame for a specified column.
#' @param df A data frame.
#' @param col The column name for which labels are to be set.
#' @param suffix A string to append to the label column name (default: "_label").
#' @return A data frame with the labels merged into the original data frame.
#' @examples
#' library(defactor)
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

