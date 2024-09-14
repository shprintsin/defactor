# defactor package

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

# Example data frame with factors
df <- data.frame(
  category = factor(c(1, 2, 1, 3)),
  value = c(10, 20, 15, 25)
)

# Assign labels to the factor column
attr(df$category, "labels") <- c('A' = 1, 'B' = 2, 'C' = 3)

# Retrieve labels for a specific column
getLbl(df, "category")

# Set labels for all columns matching a pattern
setLblPat(df, pattern = "^V")

# Retrieve details for all columns
colInfo(df)
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
