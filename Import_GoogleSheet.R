# This script shows how to import Google Sheets into R



#### Imporiting from published Google Sheets ####

# The code is from the article below:
# http://blog.revolutionanalytics.com/2014/06/reading-data-from-the-new-version-of-google-spreadsheets.html

# To import Google Sheet into R from the web, we need the 3 steps below:
# 1. Publish the google sheet (in the "File" menu) and obtain the URL.
# 2. Feed the URL to function "readGoogleSheet"(defined below) to obtain a list of data frames where each 
     # data frame contains a tab in the original google sheet.
# 3. Feed the list to function "cleanGoogleTable" (defined below) and specify the number or name of the tab you
     # want to get. A data frame for the specified tab is returned.


library(XML)

readGoogleSheet <- function(url, na.string="", header=TRUE){
  stopifnot(require(XML))
  # Suppress warnings because Google docs seems to have incomplete final line
  suppressWarnings({
    doc <- paste(readLines(url), collapse=" ")
  })
  if(nchar(doc) == 0) stop("No content found")
  htmlTable <- gsub("^.*?(<table.*</table).*$", "\\1>", doc)
  ret <- readHTMLTable(htmlTable, header=header, stringsAsFactors=FALSE, as.data.frame=TRUE)
  lapply(ret, function(x){ x[ x == na.string] <- NA; x})
}

cleanGoogleTable <- function(dat, table=1, skip=0, ncols=NA, nrows=-1, header=TRUE, dropFirstCol=NA){
  if(!is.data.frame(dat)){
    dat <- dat[[table]]
  }
  if(is.na(dropFirstCol)) {
    firstCol <- na.omit(dat[[1]])
    if(all(firstCol == ".") || all(firstCol== as.character(seq_along(firstCol)))) {
      dat <- dat[, -1]
    }
  } else if(dropFirstCol) {
    dat <- dat[, -1]
  }
  if(skip > 0){
    dat <- dat[-seq_len(skip), ]
  }
  if(nrow(dat) == 1) return(dat)
  if(nrow(dat) >= 2){
    if(all(is.na(dat[2, ]))) dat <- dat[-2, ]
  }
  if(header && nrow(dat) > 1){
    header <- as.character(dat[1, ])
    names(dat) <- header
    dat <- dat[-1, ]
  }
  # Keep only desired columns
  if(!is.na(ncols)){
    ncols <- min(ncols, ncol(dat))
    dat <- dat[, seq_len(ncols)]
  }
  # Keep only desired rows
  if(nrows > 0){
    nrows <- min(nrows, nrow(dat))
    dat <- dat[seq_len(nrows), ]
  }
  # Rename rows
  rownames(dat) <- seq_len(nrow(dat))
  dat
}


# Try the example sheet published by the author. 
gdoc <- "https://docs.google.com/spreadsheets/d/1MQ50_tn76GqQAOpFigcHms4zFqkoM_JS4sOittv_vgA/pubhtml"
elem <- readGoogleSheet(gdoc)
m1 <- cleanGoogleTable(elem, table=1) # import the 1st tab
m2 <- cleanGoogleTable(elem, table=2) # import the 2nd tab



#### Downloading as xlsx and importing ####

# When downloading the google sheet (right-click the selected files to see the menu), the sheets will be 
# converted to xlsx files, which we can import to R using existing functions.

library(gdata)
library(xlsx) # An alternative is xlsx packages. 
# The xlsx need and Java API, which can be downloaded at http://poi.apache.org/download.html
# If you you still cannot use xlsx after installing the API, try rebooting your computer. 


file_path <- "E:\\Dropbox (Personal)\\Proj-PenSim\\CCR_Data\\"

# Example

# read.xls from gdata does not work, I am not sure why.
# read.xls(paste0(file_path, "PensionActuarialLiabilities.xlsx"), sheet = 1, skip = 2) 


# The read.xlsx2 from xlsx package works fine. (read.xlsx2 is much faster than read.xlsx)
# Note that the third rows in all files contain usable variable names. Bravo, JP!
CRR_AL <- read.xlsx2(paste0(file_path, "PensionActuarialLiabilities.xlsx"), sheetIndex = 1, startRow = 3)
CRR_Cost <- read.xlsx2(paste0(file_path, "PensionActuarialCosts.xlsx"), sheetIndex = 1, startRow = 3)












