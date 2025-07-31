# Import libraries
library(rvest)
library(httr)
library(stringr)
library(ggbump)
library(tidyverse)
library(plotly)
library(readxl)

# Ensure the folder exists
if (!dir.exists("Files")) dir.create("Files")
# Removes files in folder
folder <- "Files"
files_to_delete <- list.files(folder, full.names = TRUE)
if(length(files_to_delete) > 0) {
  file.remove(files_to_delete)
}
# List of years - to be imported
year_list <- c("2025-26/","2024-25/")
# The main RTT page URL
rtt_url <-"https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-"

# Build the URLs for each year page
url_list <- paste0(rtt_url, year_list)
# Pattern to find in URL
file_pattern <- "Incomplete-Provider"
download_folder <- "Files"

# Sets up the all_file_url variable
all_file_urls <- c()

# Reads the url, checks for html href section, then detects the pattern to find then store the url for that link
for (page_url in url_list) {
  cat("Reading:", page_url, "\n")
  page <- tryCatch(read_html(page_url), error = function(e) NULL)
  if (is.null(page)) next
  
# Find all links and filter to those that match the pattern and end with .xlsx
  links <- page %>% html_elements("a") %>% html_attr("href")
  links <- links[!is.na(links)]
  matching_links <- links[str_detect(links, file_pattern) & str_detect(links, "\\.xlsx$")]
  
  if (length(matching_links) > 0) {
    all_file_urls <- c(all_file_urls, matching_links)
  }
}

cat("Total files to download:", length(all_file_urls), "\n")
print(all_file_urls)

# Download each file in the url list, preserving original file name
for (file_url in unique(all_file_urls)) {
  file_name <- basename(file_url)
  dest_path <- file.path(download_folder, file_name)
  cat("Downloading:", file_url, "to", dest_path, "\n")
  tryCatch({
    GET(file_url, write_disk(dest_path, overwrite = TRUE), timeout(60))
  }, error = function(e) {
    cat("Failed to download", file_url, "\n")
  })
}

# Importing the files downloaded
folder <- "Files"
sheet <- "Provider"
skip_rows <- 13
file_list <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

# sets up the dataframe for the data
all_data <- data.frame()

# imports each excel file downloaded
for (file in file_list) {
  cat("Processing:", file, "\n")
  tmp <- tryCatch(
    read_excel(file, sheet = sheet, skip = skip_rows),
    error = function(e) {
      cat("  Could not read file:", basename(file), ":", e$message, "\n")
      return(NULL)
    }
  )
  if (!is.null(tmp)) {
    # Extract date string after "Incomplete-Provider-"
    fname <- basename(file)
    # creates a date string from file name - to use for date column
    date_str <- str_extract(fname, "(?<=Incomplete-Provider-).{5}")
    # temporary df
    tmp <- tmp %>%
      filter(`Treatment Function Code` == "C_999") %>%
      select(`Provider Code`, `Provider Name`, `% within 18 weeks`) %>%
      mutate(file_date = date_str)
    # combines all data in to final dataframe
    all_data <- bind_rows(all_data, tmp)
    # removes temporary df - to free up memory
    rm(tmp)
  }
}
wb <- openxlsx::createWorkbook()
openxlsx::addWorksheet(wb,"new")
openxlsx::writeData(wb,"new",all_data,startCol = 1,startRow = 2)
openxlsx::saveWorkbook(wb,"new1.xlsx")
# transforms all_data df - to include a month column, selecting and arraging then cols
all_data <- all_data %>%
  mutate( month_abbr = str_sub(file_date, 1, 3),       # First 3 chars
    year_short = str_sub(file_date, 4, 5),       # Last 2 chars
    # Convert abbreviated month to full month name
    month_full = month.name[match(month_abbr, month.abb)],
    # Convert 2-digit year to 4-digit year (assumes 2000+)
    year_full = paste0("20", year_short),
    first_of_month = dmy(paste("01", month_full, year_full))) %>%
  select(first_of_month,`Provider Code`,`Provider Name`,`% within 18 weeks`) %>%
  arrange (first_of_month)

# grouping and ranking the 18 week performance
all_data <- all_data %>%
  mutate (`Provider Name` = str_to_title(`Provider Name`),
          `Provider Name` = str_replace(`Provider Name`,"Nhs", "NHS")) %>%
  group_by(first_of_month) %>%
  mutate(rank = dense_rank(desc(`% within 18 weeks`))) %>%
  ungroup()

all_data_max_min <- all_data %>%
  group_by(`Provider Name`) %>%
  mutate(max_rank = max(rank),
           min_rank = min(rank)) %>%
  ungroup()

ggplot(all_data, aes(x =first_of_month, y = rank, color = `Provider Name`)) +
  geom_bump() +
  scale_y_reverse(breaks = c(150,120, 100,70,60,50,40,30,20,10,1), expand = c(0.02,0))+
  theme(legend.position = "none")
ggplotly()




