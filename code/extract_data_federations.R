
# install.packages("rvest")
# install.packages("writexl")

library(rvest)
library(writexl)
library(dplyr)

# Read HTML page
setwd("/Users/dsesia/Desktop/case_study_CIO")
page <- read_html("data/federations.html", encoding = "utf-8")

# Find federations blocks
fed_nodes <- html_nodes(page, "li.federation")

# Function to extract data from each federation block
extract_federation <- function(node) {
  
  h3_text <- html_text(html_node(node, "h3"), trim = TRUE)
  code <- sub(" .*", "", h3_text)
  country <- sub("^\\S+\\s+", "", h3_text)
  
  data <- list(Code = code, Country = country)
  
  rows <- html_nodes(node, "table tr")
  for (row in rows) {
    label <- html_text(html_node(row, "td.label"), trim = TRUE)
    value <- html_text(html_node(row, "td.value"), trim = TRUE)
    
    if (!is.na(label) && label != "") {
      data[[label]] <- value
    }
  }
  return(as.data.frame(data, stringsAsFactors = FALSE))
}

list_df <- lapply(fed_nodes, extract_federation)
df_final <- dplyr::bind_rows(list_df)

# Export data
write_xlsx(df_final, "data/federations.xlsx")
