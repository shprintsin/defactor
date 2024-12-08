# defactor R package

**defactor** is an R package that provides a set of tools for managing, annotating, and retrieving variable labels in data frames. The package allows for dynamic setting and extraction of labels based on patterns, column selections, or custom specifications, making it particularly useful for data cleaning, analysis, and preparation tasks where labeled data is critical for interpretation and reporting.

## Key Features

- **Dynamic Label Management**: Easily retrieve, set, and manage variable labels in your data frames.
- **Pattern-Based Labeling**: Set labels for columns matching specific regex patterns.
- **Custom Column Selection**: Set labels for a user-defined list of column names.
- **Data Frame Summarization**: Retrieve detailed summaries of variable names, labels, and their corresponding category values.

## Installation

To install the **defactor** package from GitHub, use the following command:

```r
# Install the devtools package if you haven't already
install.packages("devtools")

# Install defactor from GitHub
devtools::install_github("shprintsin/defactor")
```

## Usage

Load the package and use its functions to manage labels in your data frames:

```r
# Load the package
library(defactor)

example
# Load the required package
library(data.table)

# Create the data table
dt <- data.table(
  C_COW_ALPHA = c("BOS", "BOS", "BOS", "BOS", "BOS"),
  COW = c(346, 346, 346, 346, 346),
  B_COUNTRY_ALPHA = c("BIH", "BIH", "BIH", "BIH", "BIH"),
  V2 = c(70, 70, 70, 70, 70),
  V2A = c(70, 70, 70, 70, 70),
  V3 = c(1, 2, 3, 4, 5),
  V4 = c(1, 1, 1, 1, 1),
  V5 = c(1, 1, 1, 1, 1),
  V6 = c(1, 2, 2, 1, 2)
)

# Add column labels as attributes
attr(dt$COW, "label") <- "colum1"
attr(dt$V2, "label") <- "colum2"
attr(dt$V2A, "label") <- "colum3"
attr(dt$V4, "label") <- "colum4"
attr(dt$V5, "label") <- "colum5"
attr(dt$V6, "label") <- "colum6"

# Add value labels as attributes
attr(dt$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(dt$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(dt$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(dt$V4, "labels") <- c("Very important" = 1)
attr(dt$V5, "labels") <- c("Very important" = 1)
attr(dt$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)



getLabel(df, "V3", limit = 3)
setLabel(df, "V2", suffix = "_label")
colInfo(df, limit = 2)
listVars(df)
#setLblPat(df, pattern = "V*")
setlstLbl(df, columns = c('V5','V6'))
setLblPat(df, pattern = "V")

structure(df)


```

## Functions

- `getLbl()`: Retrieves the labels of a specified column in a data frame.
- `setLbl()`: Merges labels into the original data frame for a specified column.
- `colInfo()`: Retrieves labels and details for all columns in the data frame.
- `listVars()`: Lists all variable names and their labels from the data frame.
- `setLblPat()`: Sets labels for all columns in a data frame that match a specified regex pattern.
- `setlstLbl()`: Sets labels for all columns in a data frame that match a specified list of column names.

## Contributing

Contributions are welcome! If you have suggestions for improvements or new features, please feel free to create an issue or submit a pull request.

## License

This package is licensed under the MIT License. See the [LICENSE](LICENSE) file for more details.

## Contact

Maintainer: Shneior Shprintsin  
Email: shnyor360@gmail.com
