library(stringr)
library(udpipe)
library(tidytext)
library(rvest)
library(tidyverse)

zippath <- "C:/Users/momo_/OneDrive/Documents/GitHub/finalproject/"

#Text Analysis

url <- "https://www.chicago.gov/city/en/depts/fss/provdrs/emerg/svcs/PITcount.html"

#directory to save my links
output_dir <- paste0(zippath, "HomelessReports")
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

#Then i need to scarpe the page using url
page <- read_html(url)

#Extract all links
links <- page %>%
  html_elements("a") %>%
  html_attr("href")

#Extract the text associated with links
titles <- page %>%
  html_elements("a") %>%
  html_text()

#Filter for links that contain "Homeless Point-In-Time Count and Survey Report" because some were filled with images and unable to be converted to text
filtered_links <- links[grepl("Homeless Point-In-Time Count and Survey Report", titles, ignore.case = TRUE)]
filtered_titles <- titles[grepl("Homeless Point-In-Time Count and Survey Report", titles, ignore.case = TRUE)]

#Ensure full URLs for relative links
base_url <- "https://www.chicago.gov"
full_links <- ifelse(grepl("^http", filtered_links), 
                     filtered_links, 
                     paste0(base_url, filtered_links))

# Function to extract text content from PDF files
pdf_to_text <- function(pdf_url, output_path) {
  temp_file <- tempfile(fileext = ".pdf")
  download.file(pdf_url, temp_file, mode = "wb")
  
  # Use pdftools to extract text
  library(pdftools)
  text_content <- pdf_text(temp_file)
  
  # Write to .txt file
  writeLines(text_content, con = output_path)
  
  unlink(temp_file)  # Remove temporary PDF file
}

# Download and save as .txt files
for (i in seq_along(full_links)) {
  # Create a safe file name
  file_name <- str_replace_all(filtered_titles[i], "[^a-zA-Z0-9]", "_")
  file_path <- file.path(output_dir, paste0(file_name, ".txt"))
  
  # Download and convert to text
  tryCatch({
    pdf_to_text(full_links[i], file_path)
    cat("Saved:", filtered_titles[i], "as .txt\n")
  }, error = function(e) {
    cat("Failed to process:", filtered_titles[i], "\n")
  })
}

articles <- list.files(output_dir, pattern = "_Homeless_Point_In_Time_Count_and_Survey_Report.txt")

#Loads text from all of the articles
article_text <- list()

for(i in 1:length(articles)) {
  article_text[[i]] <- read_delim(paste0(output_dir, "/", articles[i]),
                                  col_names = F, delim = ',,,,', col_types = cols())
}


article_text <- do.call(rbind, article_text) %>%
  as.data.frame()

chicago_udpipe <- udpipe(article_text$X1, "english") %>%
  filter(! upos %in% c("PUNCT", "SYM"))

#Removes stop words
chicago_udpipe <- chicago_udpipe %>%
  anti_join(stop_words, by = c("token" = "word"))

#Loads sentiment data
sentiment_nrc   <- get_sentiments("nrc") %>% rename(nrc = sentiment) %>% group_by(word) %>% slice(1) %>% ungroup() 
sentiment_afinn <- get_sentiments("afinn") %>% rename(afinn = value)
sentiment_bing  <- get_sentiments("bing") %>% rename(bing = sentiment)

#Merges udpipe with sentiment data
chicago_udpipe <- chicago_udpipe %>%
  left_join(sentiment_nrc, by = c('lemma' = 'word')) %>%
  left_join(sentiment_afinn, by = c('lemma' = 'word')) %>%
  left_join(sentiment_bing, by = c('lemma' = 'word'))

#overall sentiment
agg_sentiment_nrc <- chicago_udpipe %>%
  filter(!is.na(nrc)) |>
  group_by(nrc) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(category = 'Overall') 

agg_sentiment_afinn <- chicago_udpipe %>%
  filter(!is.na(afinn)) |>
  group_by(afinn) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(category = 'Overall')

agg_sentiment_bing <- chicago_udpipe %>%
  filter(!is.na(bing)) |>
  group_by(bing) %>%
  summarise(count = n()) %>%
  arrange(desc(count)) %>%
  mutate(category = 'Overall')

nrc_plot <- ggplot() +
  geom_bar(data = agg_sentiment_nrc,
           aes(x = nrc, y = count, fill = category),
           stat = 'identity', position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust= 0.3, size = 9),
        plot.caption = element_text(hjust = 0)) +
  labs(x = element_text('NRC Sentiment'),
       y = element_text('Proportion'),
       title = 'Proportion of NRC Sentiment in Chicago Homelessness Articles',
       caption = 'Note: Only words with sentiment scores are included.',
       fill = NULL)
nrc_plot
ggsave(zippath, "nrc_plot.png", plot = nrc_plot, width = 6, height = 4, dpi = 300)


afinn_plot <- ggplot() +
  geom_bar(data = agg_sentiment_afinn,
           aes(x = afinn, y = count, fill = category),
           stat = 'identity', position = 'dodge') +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x = element_text('Afinn Sentiment'),
       y = element_text('Proportion'),
       title = 'Proportion of Afinn Sentiment in Chicago Homelessness Articles',
       caption = 'Note: Only words with sentiment scores are included.',
       fill = NULL) +
  scale_x_continuous(breaks = -5:5)
afinn_plot

bing_plot <- ggplot() +
  geom_bar(data = agg_sentiment_bing,
           aes(x = bing, y = count, fill = category),
           stat = 'identity', position = 'dodge') +
  theme(plot.caption = element_text(hjust = 0)) +
  labs(x = element_text('Bing Sentiment'),
       y = element_text('Proportion'),
       title = 'Proportion of Bing Sentiment in Chicago Homelessness Articles',
       caption = 'Note: Only words with sentiment scores are included.',
       fill = NULL)
bing_plot