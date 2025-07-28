
# Import libraries
library(readxl)
library(openxlsx)
library(tidyverse)
library(rvest)
library(plotly)

# Aim - to download the RTT WLMDS Management information file on NHSE's Statistics webpage 

# Webpage url - to search
rtt_url <- "https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/wlmds/" 

# Folder path including file name for the Excel file to be downloaded
# * try and make this file name adaptable - current date??
folder <- "Files/"
folder_path <- paste0(folder,Sys.Date(),"-rtt-file.xlsx")

# Adding this section to change text later on if changes to url text (This feeds in to the matching links section)
start_text_rtt_url <- "https://www.england.nhs.uk/statistics/"
end_text_rtt_url <- ".xlsx"

# create variable to later add the file URL
rtt_xlsx_url <- NULL 

# read the (HTML) content of the webpage
read_page_content <- read_html(rtt_url)

#--------- AI help with CSS & HTML -------------
# Find all hyperlink elements (<a> tags)
links <- read_page_content %>% html_elements("a") 
  
# Extract the 'href' attribute (the URL) from each link
hrefs <- links %>% html_attr("href")
  
# Remove potential NA values - links without an href attribute
hrefs <- hrefs[!is.na(hrefs)]
# number of links found
print(paste("Found", length(hrefs), "total links."))
  
# Filter the links - Must start with "https://www.england.nhs.uk/statistics/" 
# &  end with ".xlsx"
matching_links <- hrefs[
  startsWith(hrefs, start_text_rtt_url) & 
    str_ends(tolower(hrefs), end_text_rtt_url)
  ]
  
  # --- Handle the results ---
num_found <- length(matching_links)
  
#---------------------------------------------------

# Counts the matching links then if else statements manage how to handle the link
# == 1 - use the link, more than one match, use the first match (may need to change this or make the start & end text more dynamic)
# else print message - no links
if (num_found == 1) {
    found_xlsx_url <- matching_links[1]
    print(paste("Found one matching .xlsx link:", found_xlsx_url))
  } else if (num_found > 1){
    # Found multiple matching links
    print(paste("Warning: Found multiple (", num_found, ") matching .xlsx links:"))
    for(i in 1:num_found) {
      print(paste("- ", matching_links[i]))
    }
    # Uses the first match
    found_xlsx_url <- matching_links[1] 
  } else {
  print("No .xlsx links starting with 'https://' found on the page.")
  }

# --- Download the file if a URL was found ---
if (!is.null(found_xlsx_url)) {
  # Use mode="wb" for binary files like Excel
  download.file(url = found_xlsx_url, destfile = folder_path, mode = "wb")
} else {
  print("Error - no url link to download")
}

# Gets sheet names of xlsx file
# sheet_names <- getSheetNames(folder_path)

# Set up for loop to import files

########################################
#    TO ADD EXCEL IMPORT 
# Set up sheet, rows to skip & set out column types
sheet_to_read <- 1
rows_to_skip <- 13
column_types <- c("date", rep("numeric", 20))

# Read in sheet 1 from RTT download
National_Time_Series <- readxl::read_excel(folder_path ,
                                           sheet = sheet_to_read,
                                           col_types = column_types,
                                           skip = rows_to_skip)


# Name changes in future?? could create a list of col names starts with ('text') then split out [1],[2],[3] then rename
# Rename columns 
National_Time_Series <- National_Time_Series %>%
  rename("Weekending" = ends_with('1'),
         "Total Clock Starts" = ends_with('16'),
         "Total Completed pathways" = ends_with('19'))

 # split data into open pathways - total
openpathways_totalwl <- National_Time_Series %>% 
  select (1,4) 
# To calculate the previous number- difference
?lag()
?writeLines()
gitcreds::gitcreds_set()                                                                                                                                
read in save in  df 
rm (df) # to save memory in r studio
gitcreds::gitcreds_set()
# Open pathways - Total wl - line plot
ggplot(openpathways_totalwl,
aes(x= Weekending, y =`Total Waiting List`))+
  geom_line() +
  ggtitle('Open Pathways - Total Waiting List Time Series')+
  theme_bw()
# Add interactive data points using plotly
  ggplotly()

openpathways_timebands <- National_Time_Series %>% 
  select (1,4:13)

timebands_order <- colnames(National_Time_Series)
timebands_order <- timebands_order[5:13]

openpathways_long <- openpathways_timebands %>% select (-2) %>% 
pivot_longer(cols = -1, names_to = "time_band", values_to = "pathways")

factor(openpathways_long$time_band, levels = timebands_order)
library(gganimate)
ggplot(openpathways_long,aes(x= time_band, y = pathways))+
  geom_col()+
  theme_bw()
#+ 
 # transition_time(Weekending) +
  #labs(title = "Week: {Weekending}")+
  #view_follow(fixed_y = FALSE)

                         
# split into clock starts and stops  
clock_starts_stops <- National_Time_Series %>% select(16, 19:21) 

  ggplot (openpathways_long, aes(x = Weekending, y =pathways, colour = time_band)) +
    geom_point(position = 'jitter') 
  ggplotly()

openpathways_long <-
  filter(openpathways_long, time_band != 'Up to 18 weeks',
         time_band !='Unknown Clock Start Date')
ggplot (openpathways_long, aes(x = Weekending, y =pathways, colour = time_band)) +
          geom_line() 

  facet_wrap(~time_band)
ggplotly()
