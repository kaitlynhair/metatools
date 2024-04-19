#' Write reference data to a CSV file formatted for SYRF
#' 
#' This function takes a data frame of reference information and writes it to a CSV file
#' formatted specifically for the Systematic Review Facility (SYRF).
#' 
#' @param refs A data frame containing reference information.
#' @param filename The name of the file to write the data to.
#' 
#' @return This function does not return anything explicitly. It writes the data frame
#' to a CSV file specified by `filename`.
#' 
#' @details The function renames columns, adds additional columns required by SYRF,
#' handles missing URL columns, selects desired columns, and then writes the resulting
#' data frame to a CSV file with specified filename.
#' 
#' @export
#' @import dplyr
write_for_syrf <- function(refs, filename){
  
  cols_to_modify <-  c('title', 'year', 'author', 'journal', 'abstract', 'doi', 'number', 'pages', 'volume', 'isbn', 'record_id', 'label')
  refs[cols_to_modify] <- lapply(refs[cols_to_modify], function(x) gsub("\\r\\n|\\r|\\n", "", x))
  
  
  refs <- refs %>% 
    rename(Authors = author, Title = title, 
           Url = url, Abstract = abstract, Year = year, DOI = doi, 
           PublicationName = journal) %>% mutate(AuthorAddress = "",
                                                 AlternateName = "", 
                                                 ReferenceType = "",
                                                 CustomId = record_id, 
                                                 Keywords = keywords,
                                                 PdfRelativePath = "") %>%
    select(Title, Authors, PublicationName, AlternateName, PdfRelativePath, Abstract, Url, AuthorAddress, Year, DOI, ReferenceType,
           Keywords, CustomId)
  
  write.csv(refs, filename,  row.names = F, quote = TRUE, na = "")
}