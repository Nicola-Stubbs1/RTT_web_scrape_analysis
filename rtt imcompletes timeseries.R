
# Import libraries
library(readxl)
library(openxlsx)
library(tidyverse)
library(rvest)
library(plotly)

# Aim - to download the RTT WLMDS Management information file on NHSE's Statistics webpage 

# Webpage url - to search
# wlmds_url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/"
# can make this dynamic - by creating finacial year function - then calculating the different years (-1 year, iteratively)
year_list <- c("2025-26/","2024-25/","2023-24/")
rtt_url <-"https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-"

url_list <- list()
for (year in year_list){
  
  full_rtt_url <- paste0(rtt_url,year)
  url_list[[as.character(year)]] <- full_rtt_url
  
}

# Folder path including file name for the Excel file to be downloaded
# * try and make this file name adaptable - current date??
folder <- "Files/"
file_pattern <- "Incomplete-Provider"
#folder_path <- paste0(folder,Sys.Date(),"-rtt-file.xlsx")

# Adding this section to change text later on if changes to url text (This feeds in to the matching links section)
#start_text_rtt_url <- "https://www.england.nhs.uk/statistics/"
#end_text_rtt_url <- ".xlsx"

# create variable to later add the file URL
rtt_xlsx_url <- NULL 

file_pattern <- "Incomplete-Provider"
# read the (HTML) content of the webpage
read_page_content <- list()
for (item in url_list){
  
read_page_content <- read_html(url_list[item])

#--------- AI help with CSS & HTML -------------
# Find all hyperlink elements (<a> tags)
links <- read_page_content %>% html_elements("a") 

# Extract the 'href' attribute (the URL) from each link
hrefs <- links %>% html_attr("href")

# Remove potential NA values - links without an href attribute
hrefs <- hrefs[!is.na(hrefs)]
# number of links found
print(paste("Found", length(hrefs), "total links."))
print(hrefs)

# Filter the links - Must start with "https://www.england.nhs.uk/statistics/" 
# &  end with ".xlsx"
matching_links <- hrefs[
  str_detect(hrefs, file_pattern)
]
}
# --- Handle the results ---
num_found <- length(matching_links)

#---------------------------------------------------

for (item in matching_links){
  
file_name <- basename(item)
file_path <- file.path(folder, file_name)

# --- Download the file if a URL was found ---
if (!is.null(matching_links)) {
  # Use mode="wb" for binary files like Excel
  download.file(url = matching_links, destfile = file_path, mode = "wb")
} else {
  print("Error - no url link to download")
}
}

