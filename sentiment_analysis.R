suppressPackageStartupMessages({
  library(sentimentr)
  library(tidytext)
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(argparse)
  library(ggpubr)
})

load_data <- function(filename) {
  data <- read.csv(filename, stringsAsFactors = FALSE)
  # Removes HTML markers
  data$content <- gsub("<[^>]+>", "", data$content)
  # Makes create_at column a date time
  data$created_at <- parse_date_time(data$created_at, c("%Y-%m-%d %H:%M:%S",
                                                        "%Y-%m-%dT%H:%M:%S"))
  # Makes sure language is English
  data$language <- "en"
  # Makes id class into a character
  data$id <- as.character(data$id)
  return(data) #nolint
}

word_analysis<-function(toot_data, emotion) {

    return()
}

sentiment_analysis<-function(toot_data) {

    return()

}

main <- function(args) {

}


if(sys.nframe() == 0) {

  # main program, called via Rscript
  parser = ArgumentParser(
                    prog="Sentiment Analysis",
                    description="Analyse toots for word and sentence sentiments"
                    )
  parser$add_argument("filename",
                    help="the file to read the toots from")
  parser$add_argument("--emotion",
                      default="anger",
                      help="which emotion to search for")
  parser$add_argument('-v', '--verbose',
                    action='store_true',
                    help="Print progress")
  parser$add_argument('-p', '--plot',
                    help="Plot something. Give the filename")
  
  args = parser$parse_args()  
  main(args)
}
