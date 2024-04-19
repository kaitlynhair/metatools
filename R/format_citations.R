
#' Format DOIs in a DataFrame
#'
#' This function formats Digital Object Identifiers (DOIs) in a DataFrame by
#' converting them to uppercase and standardizing their representation. It
#' performs various replacements to ensure consistent formatting for DOIs.
#'
#' @import dplyr
#'
#' @param df A DataFrame containing a column named 'doi' with DOI strings.
#'
#' @return A DataFrame with the 'doi' column formatted for consistency.
#' @export
format_doi <- function(df){

  df$doi <- tolower(df$doi)
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%28", "(", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("%29", ")", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://dx.doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("https://dx.doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("http://doi.org/", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi: ", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi:", "", x)))
  df["doi"] <- as.data.frame(sapply(df["doi"], function(x) gsub("doi", "", x)))
  return(df)
}

#' Format Columns for SOLES Search Data
#'
#' This function processes a dataframe to retain relevant columns required for SOLES and standardizes the case of
#' specific columns to ensure consistent formatting.
#'
#' @import dplyr
#'
#' @param df A dataframe to be formatted for SOLES search data.
#'
#' @return A formatted dataframe containing the required SOLES columns with standardized case.
#'
format_cols <- function(df){

  # cols required for soles
  x <- c("record_id", "accession", "author", "year", "journal", "doi", "title",
         "pages", "volume", "abstract", "isbn", "keywords",
         "secondarytitle", "url", "date", "issn", "pmid", "ptype",
         "source", "author_country", "number", "author_affiliation")

  title_case_cols <- c("author","journal", "secondarytitle", "author_country", "author_affiliation")

  sentence_case_cols <- c("title",
                          "abstract")

  lower_case_cols <- c("record_id", "doi",  "keywords", "ptype",
                       "source")

  df[x[!(x %in% colnames(df))]] = NA
  df <- df %>%
    select(all_of(x)) %>%
    mutate(across(all_of(sentence_case_cols), ~stringr::str_to_sentence(.))) %>%
    mutate(across(all_of(lower_case_cols), ~stringr::str_to_lower(.))) %>%
    mutate(across(all_of(title_case_cols), ~stringr::str_to_title(.))) %>%
    mutate_at(vars(x), ~ gsub(";", "; ", .))

  df$pages <- lapply(df$pages, function(x) gsub("--", "-", x))
  df$date  <-  format(Sys.Date(), "%d%m%y")

  cols_to_modify <-  c('author', 'title', 'year', 'journal', 'doi', 'number', 'pages', 'volume', 'isbn', 'issn')
  df['abstract'] <- lapply(df['abstract'], function(x) gsub("[Aa]bstract", "", x))
  df[cols_to_modify] <- lapply(df[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  df['abstract'] <- lapply(df['abstract'], function(x) trimws(x))
  return(df)
}
