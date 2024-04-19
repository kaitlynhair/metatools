#' Extract concepts, funder, citation count, institution and open access data from OpenAlex.
#'
#' This function retrieves meta-data.
#'
#' @param citations dataframe containing a doi column
#'
#' @import openalexR
#' @return dataframe with open alex metadata for each DOI
#' @export

get_openalex_metadata <- function(citations){
  
   # Use the doi's with no discipline (which should also have no funder data) to search OpenAlex
  res <- NULL
  
  # Create a dataframe with data from openAlex ----
  for(i in 1:length(citations_missing_data$doi)){
    suppressWarnings({
      
      try(new <- openalexR::oa_fetch(
        identifier = NULL,
        entity = "works",
        doi = citations_missing_data$doi[i]),silent=TRUE)
    })
    if(is.data.frame(new)){
      res <- plyr::rbind.fill(res, new)
    }
  }

    return(res)
}


#' Fill in missing abstracts via crossref
#'
#' This function pulls in missing abstracts
#'
#' @param citations citations you want to find abstracts for
#' @return citations dataframe with abstracts (where feasible)
#' @export
#' @import rcrossref
get_missing_abstracts <- function(citations){
  
  # get studies with no abstract
  new_unique_no_abstract <- citations %>%
    filter(is.na(abstract) | abstract=="")
  
  print(paste0(length(new_unique_no_abstract$uid), " papers with no abstract"))
  
  # get vector of DOIs
  dois <- new_unique_no_abstract$doi
  
  # get abstracts from crossref
  abstract_result <- lapply(dois, function(z) tryCatch(rcrossref::cr_abstract(z), error = function(e) e))
  
  # format abstract dataframe
  abstracts_df <- do.call(rbind, abstract_result)
  abstracts_df <- as.data.frame(abstracts_df)
  abstracts_df <- cbind(abstracts_df, dois)
  abstracts_df <- as.data.frame(abstracts_df)
  
  abstracts_df <- abstracts_df %>%
    mutate(abstract = ifelse(call == "NULL", NA, paste0(call))) %>%
    mutate(abstract = ifelse(call == "nchar(hh)", NA, paste0(abstract))) %>%
    mutate(doi = as.character(dois)) %>%
    dplyr::select(abstract, doi)
  
  # bind new abstracts to existing df
  new_unique_no_abstract <- new_unique_no_abstract %>%
    dplyr::select(-abstract)
  
  new_unique_no_abstract <- left_join(abstracts_df,
                                      new_unique_no_abstract,
                                      by="doi")
  new_unique_no_abstract <- unique(new_unique_no_abstract)
  
  citations <- citations %>%
    filter(!uid %in% new_unique_no_abstract$uid)
  
  citations <- rbind(citations, new_unique_no_abstract)
  
  # make all NA real NAs
  citations[citations == "NA" ] <- NA
  citations[citations == "" ] <- NA
  
  still_no_abstract <- citations %>%
    filter(is.na(abstract) | abstract=="")
  
  print(paste0(length(still_no_abstract$uid), " papers still with no abstract"))
  
  return(citations)
}
