#' @title Get Variable Labels
#' @name listVars
#' @description Retrieves all variable names and their labels from the data frame.
#' @param df A data frame.
#' @return A data frame with variable names and their corresponding labels.
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

