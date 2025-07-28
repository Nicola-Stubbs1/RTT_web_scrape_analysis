library(rvest)
library(httr)
library(stringr)
library(ggbump)
library(tidyverse)
library(plotly)
library(readxl)

# Ensure the folder exists
if (!dir.exists("Files")) dir.create("Files")
#removes files in folder
folder <- "Files"
files_to_delete <- list.files(folder, full.names = TRUE)
if(length(files_to_delete) > 0) {
  file.remove(files_to_delete)
}

year_list <- c("2025-26/","2024-25/")
rtt_url <-"https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-"

# Build URLs
url_list <- paste0(rtt_url, year_list)
("Files")
file_pattern <- "Incomplete-Provider"
download_folder <- "Files"

# Store all found file URLs
all_file_urls <- c()

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

# Download each file, preserving original file name
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

folder <- "Files"
sheet <- "Provider"
skip_rows <- 13
file_list <- list.files(folder, pattern = "\\.xlsx$", full.names = TRUE)

all_data <- data.frame()

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
    date_str <- str_extract(fname, "(?<=Incomplete-Provider-).{5}")
    tmp <- tmp %>%
      filter(`Treatment Function Code` == "C_999") %>%
      select(`Provider Code`, `Provider Name`, `% within 18 weeks`) %>%
      mutate(file_date = date_str)
    all_data <- bind_rows(all_data, tmp)
    rm(tmp)
    gc()
  }
}

all_data <- all_data %>%
  mutate( month_abbr = str_sub(file_date, 1, 3),       # First 3 chars
    year_short = str_sub(file_date, 4, 5),       # Last 2 chars
    # Convert abbreviated month to full month name
    month_full = month.name[match(month_abbr, month.abb)],
    # Convert 2-digit year to 4-digit year (assumes 2000+)
    year_full = paste0("20", year_short),
    first_of_month = dmy(paste("01", month_full, year_full))
  ) %>%
  select(first_of_month,`Provider Code`,`Provider Name`,`% within 18 weeks`) %>%
  arrange (first_of_month)

all_data <- all_data %>%
  group_by(first_of_month) %>%
  mutate(
    rank = dense_rank(desc(`% within 18 weeks`))
  ) %>%
  ungroup()

ggplot(all_data, aes(x =first_of_month, y = rank, color = `Provider Code`)) +
  geom_bump() +
  scale_y_reverse(breaks = c(150,120, 100,70,60,50,40,30,20,10,1), expand = c(0.02,0))+
  theme(legend.position = "none")
ggplotly()

#################



library(dplyr)
library(ggplot2)
library(ggbump)
library(plotly)

# 1. Find the latest date in your data
latest_date <- max(all_data$first_of_month, na.rm = TRUE)

# 2. Create a data frame for labels at the latest date for each provider
label_data <- all_data %>%
  filter(first_of_month == latest_date)

# 3. Bump plot with provider names at the rightmost point
p <- ggplot(all_data, aes(x = first_of_month, y = rank, color = `Provider Code`)) +
  geom_bump() +
  # Add provider label at the latest date
  geom_text(
    data = label_data,
    aes(label = `Provider Code`),
    hjust = 0, # adjust as needed (0 = left, 1 = right)
    nudge_x = 10, # nudge to the right to avoid overlap with the point; adjust as needed
    size = 2.5   # adjust size to your preference
  ) +
  scale_y_reverse(breaks = c(150,100,80,70,60,50,40,30,20,10,1), expand = c(0.02,0)) +
  theme(legend.position = "none")

ggplotly()



###############



max_date<- max(first_of_month)
selected <- all_data %>% filter(first_of_month == max_date) %>%
  arrange(rank) %>% select(`Provider Code`) %>% head(10)

library(ggplot2)
library(ggbump)
library(plotly)


current_year <- year(max(all_data$first_of_month, na.rm = TRUE))
latest_date <- max(all_data$first_of_month, na.rm = TRUE)

top_providers <- all_data %>%
  filter(year(first_of_month) == current_year & first_of_month == latest_date) %>%
  arrange(rank) %>%
  slice_head(n = 10) %>%  # Change n to how many top providers you want
  pull(`Provider Code`)

all_data <- all_data %>%
  mutate(
    highlight = ifelse(`Provider Code` %in% top_providers, `Provider Code`, "Other")
  )
# Choose a color palette for top providers
n_top <- length(top_providers)
my_palette <- c(RColorBrewer::brewer.pal(min(8, n_top), "Set1"), 
                rep("black", n_top-8))
names(my_palette) <- top_providers
my_palette <- c(my_palette, Other = "grey80")

p <- ggplot(all_data, aes(x = first_of_month, y = rank, group = `Provider Code`)) +
  geom_bump(aes(color = highlight), size = 1) +
  scale_color_manual(values = my_palette) +
  scale_y_reverse(breaks = c(150,100,80,70,60,50,40,30,20,10,1), expand = c(0.02,0)) +
  theme_minimal() +
  theme(legend.position = "none") # Remove legend

# Add provider labels at the last date only for top providers
label_data <- all_data %>%
  filter(first_of_month == latest_date & highlight != "Other")

p <- p +
geom_text(
  data = label_data,
  aes(label = `Provider Code`),
  hjust = -0.1,
  size = 0.002,  # Make text smaller
  color = "black"
)
# Optionally, expand the x-axis so labels don't get cut off
p <- p + 
  scale_x_date(expand = expansion(mult = c(0, 0.15)))

# Convert to plotly
#ggplotly(p)
ggsave("plot.png")


library(ggplot2)
library(ggbump)
library(dplyr)
# library(MetBrewer) # Uncomment if you have met.brewer

# Assume 'all_data' is your dataframe and 'selected' is your vector of top providers

latest_date <- max(all_data$first_of_month, na.rm = TRUE)

# Set up labels for only the latest date
label_data <- all_data %>%
  filter(first_of_month == latest_date & `Provider Code` %in% selected)

plot <- all_data %>%
  ggplot(aes(x = first_of_month, y = rank_18weeks, group = `Provider Code`)) +
  geom_bump(linewidth = 0.6, color = "gray90") +
  geom_bump(
    aes(color = `Provider Code`), linewidth = 0.8,
    data = ~. |> filter(`Provider Code` %in% selected)
  ) +
  geom_point(color = "white", size = 4) +
  geom_point(color = "gray90", size = 2) +
  geom_point(
    aes(color = `Provider Code`), size = 2,
    data = ~. |> filter(`Provider Code` %in% selected)
  ) +
  geom_text(
    data = label_data,
    aes(label = `Provider Code`), 
    size = 0.5, # smaller size
    color = "black", family = "Roboto Condensed"
  ) +
  # Uncomment if you have met.brewer, or provide your own palette
  # scale_color_manual(values = met.brewer("Juarez")) + 
  scale_color_brewer(palette = "Set1") + # fallback if you don't have met.brewer
  scale_x_date(
    expand = c(0.01,0)
  ) +
  scale_y_reverse(
    breaks = c(25,20,15,10,5,1), expand = c(0.02,0),
    labels = scales::number_format(suffix = ".")
  ) +
  labs(
    x = NULL, y = NULL,
    title = toupper("RTT PERFORMANCE"),
    subtitle = "18 Weeks % performance - Ranked",
    caption = "Data source: NHS Statistics"
  ) +
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    plot.title.position = "plot",
    plot.title = element_text(size = 14, hjust = .5),
    plot.subtitle = element_text(size = 10, hjust = .5),
    plot.caption = element_text(size = 8)
  )

# Save the plot
ggsave("rtt-bump-plot-highlight.png", plot = plot, width = 4, height = 8, dpi = 320, bg = "white")


# Find latest date
latest_date <- max(all_data$first_of_month, na.rm = TRUE)

# Get top providers for the latest date
top_providers_tbl <- all_data %>%
  filter(first_of_month == latest_date) %>%
  arrange(rank_18weeks) %>%
  slice_head(n = 10) # adjust n for the number of top providers




