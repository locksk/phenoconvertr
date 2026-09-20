## code to prepare `download_data` dataset goes here

usethis::use_data(download_data, overwrite = TRUE)

download_data <- function() {
  #install.packages("rvest")
  library(rvest)
  library(dplyr)
  library(stringr)
  library(tidyr)
  # URL of the website
  url <- "https://www.fda.gov/drugs/drug-interactions-labeling/healthcare-professionals-fdas-examples-drugs-interact-cyp-enzymes-and-transporter-systems"

  # Read the HTML code of the page
  html_code <- rvest::read_html(url)

  # Use the html_nodes function to extract the table
  table_html <- html_code %>% html_nodes("table") %>% .[[1]]

  # Use the html_table function to convert the table
  # HTML code into a data frame
  table_df <- table_html %>% html_table()

  for (i in 2:ncol(table_df)) {
    table_df[[i]] <- str_squish(table_df[[i]])
  }

  # split by inhibitors and inducers
  df <- dplyr::select(table_df, c(1:7))

  # change col names
  colnames(df) <- c("substance", "strong inhibitor", "moderate inhibitor", "weak inhibitor", "strong inducer", "moderate inducer", "weak inducer")

  cols <- colnames(df[-1])

  # clean data
  for (col in cols) {
    df <- separate_rows(df, col, sep = ";")
    df <- separate_rows(df, col, sep = ",")
  }



  # Define the constant phrase to be removed
  phrases_to_remove <- c("inhibitor5", "inhibitor",  "strong", "moderate", "weak", "inducer b", "inducer", "CYP",  "   ", "  ",  " ")


  for (phrase in phrases_to_remove) {
    df$`strong inhibitor` <- gsub(phrase, "", df$`strong inhibitor`)
    df$`moderate inhibitor` <- gsub(phrase, "", df$`moderate inhibitor`)
    df$`weak inhibitor` <- gsub(phrase, "", df$`weak inhibitor`)
    df$`strong inducer` <- gsub(phrase, "", df$`strong inducer`)
    df$`moderate inducer` <- gsub(phrase, "", df$`moderate inducer`)
    df$`weak inducer` <- gsub(phrase, "", df$`weak inducer`)
  }

  df$substance <- gsub('[[:digit:]]+', '', df$substance)
  df$substance <- gsub(',', '', df$substance)
  df$substance[df$substance == "tobacco (smoking)"] <- "cigarettes"

  # final clean - inhibit
  df <- df %>% mutate_all(na_if,"")

  for (i in 2:4) {
    df[[i]] <- sub("^", "CYP", df[[i]])
    df[[i]] <- sub(" ", "", df[[i]])
    #  df[[i]] <- sub("$", " inhibitor", df[[i]])
    df[[i]] <- str_squish(df[[i]])

  }

  for (i in 5:7) {
    df[[i]] <- sub("^", "CYP", df[[i]])
    df[[i]] <- sub(" ", "", df[[i]])
    #  df[[i]] <- sub("$", " inducer", df[[i]])
    df[[i]] <- str_squish(df[[i]])
  }



  for (i in 2:ncol(df)) {
    col <- colnames(df[i]) # colnames  = weak/moderate/strong inhibitor/inducer
    x <- df[,c(1, i)] %>% distinct()
    df_wide <- pivot_wider(x,
                           id_cols = substance,
                           names_from = col,
                           values_from = col,
                           values_fn = length,
                           names_sep = "_")
    df_wide <- dplyr::select(df_wide, -c("NA"))
    for (j in 2:ncol(df_wide)){
      df_wide[[j]][df_wide[[j]] >= 1] <- paste(col)
    }
    assign(paste0('df_wide_', colnames(df[i])), df_wide)
  }



  ref <- data.frame(substance = df$substance,
                    CYP1A2 = NA,
                    CYP2B6 = NA,
                    CYP2C19 = NA,
                    CYP2C8 = NA,
                    CYP2C9 = NA,
                    CYP2D6 = NA,
                    CYP3A4 = NA)

  ref <- distinct(ref)



  main <- merge(ref, `df_wide_strong inhibitor`, all = TRUE)
  main1 <- merge(main, `df_wide_moderate inhibitor`, all = TRUE)
  main2 <- merge(main1, `df_wide_weak inhibitor`, all = TRUE)
  main3 <- merge(main2, `df_wide_strong inducer`, all = TRUE)
  main4 <- merge(main3, `df_wide_moderate inducer`, all = TRUE)
  main5 <- merge(main4, `df_wide_weak inducer`, all = TRUE)

  main5[complete.cases(main5[ , 2:7]),]

  main <- subset(main5, rowSums(is.na(main5[,2:7])) < 6)

  # remove extra enzymes for now
  main <- main[,c(1,6,2,5,8,3)]
  main$CYP3A5 <- main$CYP3A4 # the flockhart table treats CYP3A4 and CYP3A5 the same in terms of substrates and inducers/inhibitors
  return(main)

  # references
  # https://pubmed.ncbi.nlm.nih.gov/31616047/
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8015939/
  # https://www.ncbi.nlm.nih.gov/pmc/articles/PMC8416898/
  # flockhart table: https://drug-interactions.medicine.iu.edu/

}
