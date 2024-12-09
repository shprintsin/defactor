pkgname <- "defactor"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('defactor')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("colInfo")
### * colInfo

flush(stderr()); flush(stdout())

### Name: colInfo
### Title: Get Details for All Columns
### Aliases: colInfo

### ** Examples

library(defactor)
library(data.table)
# Create the data table
df <- data.table(
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
attr(df$COW, "label") <- "colum1"
attr(df$V2, "label") <- "colum2"
attr(df$V2A, "label") <- "colum3"
attr(df$V4, "label") <- "colum4"
attr(df$V5, "label") <- "colum5"
attr(df$V6, "label") <- "colum6"

# Add value labels as attributes
attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V4, "labels") <- c("Very important" = 1)
attr(df$V5, "labels") <- c("Very important" = 1)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
colInfo(df, limit = 1)



cleanEx()
nameEx("getLbl")
### * getLbl

flush(stderr()); flush(stdout())

### Name: getLbl
### Title: Get Label for a Column
### Aliases: getLbl

### ** Examples

# Create a sample data frame with a factor column
# Load the package
library(defactor)

#Load the required package
library(data.table)
# Create the data table
df <- data.table(
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
attr(df$COW, "label") <- "colum1"
attr(df$V2, "label") <- "colum2"
attr(df$V2A, "label") <- "colum3"
attr(df$V4, "label") <- "colum4"
attr(df$V5, "label") <- "colum5"
attr(df$V6, "label") <- "colum6"

# Add value labels as attributes
attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V4, "labels") <- c("Very important" = 1)
attr(df$V5, "labels") <- c("Very important" = 1)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)



cleanEx()
nameEx("listVars")
### * listVars

flush(stderr()); flush(stdout())

### Name: listVars
### Title: Get Variable Labels
### Aliases: listVars

### ** Examples

library(defactor)
library(data.table)
# Create the data table
df <- data.table(
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
attr(df$COW, "label") <- "colum1"
attr(df$V2, "label") <- "colum2"
attr(df$V2A, "label") <- "colum3"
attr(df$V4, "label") <- "colum4"
attr(df$V5, "label") <- "colum5"
attr(df$V6, "label") <- "colum6"

# Add value labels as attributes
attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V4, "labels") <- c("Very important" = 1)
attr(df$V5, "labels") <- c("Very important" = 1)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
listVars(df)



cleanEx()
nameEx("setLbl")
### * setLbl

flush(stderr()); flush(stdout())

### Name: setLbl
### Title: Set Label for a Column
### Aliases: setLbl

### ** Examples

library(defactor)
library(data.table)
# Create the data table
df <- data.table(
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
attr(df$COW, "label") <- "colum1"
attr(df$V2, "label") <- "colum2"
attr(df$V2A, "label") <- "colum3"
attr(df$V4, "label") <- "colum4"
attr(df$V5, "label") <- "colum5"
attr(df$V6, "label") <- "colum6"

# Add value labels as attributes
attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V4, "labels") <- c("Very important" = 1)
attr(df$V5, "labels") <- c("Very important" = 1)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
setLbl(df, "V5", suffix = "_label")



cleanEx()
nameEx("setLblPat")
### * setLblPat

flush(stderr()); flush(stdout())

### Name: setLblPat
### Title: Set Labels for All Columns by Pattern
### Aliases: setLblPat

### ** Examples

library(defactor)
library(data.table)
# Create the data table
df <- data.table(
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
attr(df$COW, "label") <- "colum1"
attr(df$V2, "label") <- "colum2"
attr(df$V2A, "label") <- "colum3"
attr(df$V4, "label") <- "colum4"
attr(df$V5, "label") <- "colum5"
attr(df$V6, "label") <- "colum6"

# Add value labels as attributes
attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V4, "labels") <- c("Very important" = 1)
attr(df$V5, "labels") <- c("Very important" = 1)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
setLblPat(df, pattern = "^V5")



cleanEx()
nameEx("setlstLbl")
### * setlstLbl

flush(stderr()); flush(stdout())

### Name: setlstLbl
### Title: Set Labels for All Columns by Selected Names
### Aliases: setlstLbl

### ** Examples

library(defactor)
library(data.table)
# Create the data table
df <- data.table(
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
attr(df$COW, "label") <- "colum1"
attr(df$V2, "label") <- "colum2"
attr(df$V2A, "label") <- "colum3"
attr(df$V4, "label") <- "colum4"
attr(df$V5, "label") <- "colum5"
attr(df$V6, "label") <- "colum6"

# Add value labels as attributes
attr(df$COW, "labels") <- c("Bosnia and Herzegovina" = 346)
attr(df$V2, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V2A, "labels") <- c("Bosnia Herzegovina" = 70)
attr(df$V4, "labels") <- c("Very important" = 1)
attr(df$V5, "labels") <- c("Very important" = 1)
attr(df$V6, "labels") <- c("Very important" = 1, "Rather important" = 2)
setlstLbl(df, columns = c("V2", "V4"))



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
