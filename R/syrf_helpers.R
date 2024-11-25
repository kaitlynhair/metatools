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


#' Download Full Texts for SYRF Search Results
#'
#' This function downloads the full text of articles from various sources 
#' (Unpaywall, CrossRef, Elsevier, Wiley) based on a list of citations from a 
#' SYRF search. It identifies available full texts, attempts downloads, 
#' and removes invalid or empty files.
#'
#' @param syrf_search A dataframe containing SYRF search citations, which must 
#' include a `CustomId` column (unique identifiers for articles) and a `Doi` 
#' column (digital object identifiers).
#' @param path A string specifying the path to the folder where downloaded PDFs 
#' or other full-text formats will be stored.
#' @param email Email address for CrossRef API access
#' @import dplyr
#' @importFrom httr GET add_headers write_disk
#' @importFrom tidyr unnest
#' @importFrom tools file_path_sans_ext
#' @importFrom jsonlite fromJSON
#' @importFrom roadoi oadoi_fetch
#' @importFrom rcrossref cr_works
#' @export
#' @examples
#' \dontrun{
#' syrf_search <- data.frame(
#'   CustomId = c("ID1", "ID2"),
#'   doi = c("10.1000/xyz123", "10.1001/abc456")
#' )
#' get_ft_for_syrf(path = "full_texts", syrf_search = syrf_search, email="")
#' }

get_ft_for_syrf <- function(path, syrf_search, email){
  
  
  if ("Doi" %in% names(syrf_search)) {
    names(syrf_search)[names(syrf_search) == "Doi"] <- "doi"
  }
  
  syrf_search$doi <- tolower(syrf_search$doi)
  
  # Unpawywall --------
  # try unpaywall using dois
  message("trying Unpaywall...")
  
  try(upw_res <- suppressWarnings(suppressMessages(roadoi::oadoi_fetch(dois = syrf_search$doi, email=email))),silent=TRUE)
  
  if(exists("upw_res")){
    
    upw_res <- upw_res %>%
      select(best_oa_location, doi) %>%
      tidyr::unnest(cols = c(best_oa_location))
    
    # link back to get CustomId
    upw_res <- left_join(upw_res, syrf_search, by="doi")
    
    upw_res <- upw_res %>%
      mutate(pdf = paste0(path, "/", CustomId, ".pdf"))
    
    
  } else {
    upw_res <- NULL}
  
  message(paste(nrow(upw_res), "full texts found via Unpaywall! Attempting download..."))
  
  urls <- upw_res$url_for_pdf
  urls <- na.omit(urls)
  dest <- upw_res$pdf
  
  # Download texts from Unpaywall
  for (i in 1:length(urls)) {
    tryCatch(
      {
        download.file(urls[i], dest[i])
        
        # download.file(urls[i], destfile = paste0(getwd(),"/", dest[i]))
        cat("Downloaded:", urls[i], "\n")
      },
      error = function(e) {
        cat("Error occurred while downloading:", urls[i], "\n")
        
      },
      warning = function(w) {
        cat("Warning occurred while downloading:", urls[i], "\n")
        
      }
    )
  }
  
  # List all files in the folder
  files <- list.files(path, full.names = TRUE)
  
  # Check file sizes and remove files with size 0 bytes
  for (file in files) {
    if (file.info(file)$size == 0) {
      file.remove(file)
      cat("Removed 0 file:", file, "\n")
    }
  }
  
  
  # CrossRef -------
  message("trying CrossRef....")
  pdfs_found_now <- get_CustomIds_with_ft(path)
  
  # correction to make lower - DOI not always in same case!
  still_missing <- syrf_search %>% filter(!(tolower(CustomId) %in% tolower(pdfs_found_now)))
  
  try(cr_res <- suppressWarnings(suppressMessages(rcrossref::cr_works(dois = still_missing$doi))), silent=TRUE)
  
  if(exists("cr_res")){
    df <- cr_res$data
    df <- left_join(df, syrf_search, by="doi")
    df <- df %>% select(doi, CustomId, link) %>% tidyr::unnest(cols = c(link))
    df <- df %>%
      mutate(name = CustomId)
    
    df <- df %>%
      mutate(ft_path=ifelse(grepl('pdf',content.type), paste0(path, "/", name, ".pdf"), as.character(name))) %>%
      mutate(ft_path=ifelse(grepl('text',content.type), paste0(path, "/", name, ".txt"), as.character(ft_path))) %>%
      mutate(ft_path=ifelse(grepl('xml',content.type), paste0(path, "/", name, ".xml"), as.character(ft_path))) %>%
      filter(!content.type == "unspecified")
    
  } else {df <- NULL}
  message(paste(length(unique(df$CustomId))), " full texts found via CrossRef! Attempting download....")
  
  cr_urls <- df$URL
  cr_dest <- df$ft_path
  
  # Download texts from CrossRef
  for (i in 1:length(cr_urls)) {
    tryCatch(
      {
        download.file(cr_urls[i], cr_dest[i])
        #download.file(cr_urls[i], destfile = paste0(getwd(),"/", cr_dest[i]))
        cat("Downloaded:", cr_urls[i], "\n")
      },
      error = function(e) {
        cat("Error occurred while downloading:", cr_urls[i], "\n")
        
      },
      warning = function(w) {
        cat("Warning occurred while downloading:", cr_urls[i], "\n")
        
      }
    )
    
    # Introduce a 2 second delay between requests
    Sys.sleep(2) 
  }
  
  # List all files in the folder
  files <- list.files(path, full.names = TRUE)
  
  # Check file sizes and remove files with size 0 bytes
  for (file in files) {
    if (file.info(file)$size == 0) {
      file.remove(file)
      cat("Removed 0 file:", file, "\n")
    }
  }
  
  
  # Elsevier --------
  message("trying Elsevier....")
  pdfs_found_now <- get_CustomIds_with_ft(path)
  
  still_syrf_search <- syrf_search %>% filter(!(tolower(CustomId) %in% tolower(pdfs_found_now)))
  
  for(i in 1:length(still_syrf_search$CustomId)){
    
    try(suppressWarnings(suppressMessages(
      elsevier_ft(still_syrf_search[i,"doi"], still_syrf_search[i,"CustomId"],  token = Sys.getenv("Elsevier_API"), path=path))), silent=TRUE)
  }
  
  pdfs_found_now <- get_CustomIds_with_ft(path)
  
  # check if any new pdfs found in sample
  found <- still_syrf_search %>%
    filter(CustomId %in% pdfs_found_now)
  
  message(paste0("Found ", length(found$CustomId), " more full texts via Elsevier!"))
  
  # List all files in the folder
  files <- list.files(path, full.names = TRUE)
  
  # Check file sizes and remove files with size 0 bytes
  for (file in files) {
    if (file.info(file)$size == 0) {
      file.remove(file)
      cat("Removed 0 file:", file, "\n")
    }
  }
  
  
  # Wiley --------
  message("trying Wiley...")
  pdfs_found_now <- get_CustomIds_with_ft(path)
  
  still_missing <- syrf_search %>% filter(!(tolower(CustomId) %in% tolower(pdfs_found_now)))
  
  still_missing$doi <- gsub('\\s+', '', still_missing$doi)
  
  for(i in 1:length(still_missing$CustomId)){
    
    try(suppressWarnings(suppressMessages(
      wiley_ft(still_missing[i,"doi"], still_missing[i,"CustomId"],  token = Sys.getenv("WILEY_API"), path=path))), silent=TRUE)
  }
  
  pdfs_found_now <- get_CustomIds_with_ft(path)
  
  # check if any new pdfs found in sample
  found <- still_missing %>%
    filter(CustomId %in% pdfs_found_now)
  
  message(paste0("Found ", length(found$CustomId), " more full texts via Wiley!"))
  
}

#' Get Custom IDs of Full Text Files
#'
#' This function retrieves a list of file names (or their base names, without extensions) 
#' from a specified directory. It is typically used to identify files related to full-text 
#' articles in a given directory.
#'
#' @param path Character string specifying the path to the directory containing files.
#' @param remove_ext Logical value indicating whether to remove file extensions from the 
#'        retrieved file names. Defaults to `TRUE`.
#' 
#' @return A character vector of file names. If `remove_ext = TRUE`, file extensions 
#'         are removed.
#' @examples
#' # Get base names of files in a directory
#' get_CustomIds_with_ft("path/to/directory", remove_ext = TRUE)
#' 
#' # Get full file names including extensions
#' get_CustomIds_with_ft("path/to/directory", remove_ext = FALSE)
#' 
get_CustomIds_with_ft <- function(path, remove_ext=TRUE){
  
  pdfs_found_now <- list.files(path = path)
  
  if(remove_ext==TRUE){
    
    pdfs_found_now <- tools::file_path_sans_ext(pdfs_found_now)
    
  } else {
    
    pdfs_found_now <- pdfs_found_now 
  }
  
}

#' Wiley full text retrieval
#'
#'  This function retrieves full texts from Wiley journals
#' @param doi digital object identifier
#' @param uid unique identifier for paper
#' @param token API token
#' @param path where full texts should be stored

wiley_ft <- function(doi, uid, token, path){
  
  doi <- gsub("\\/", "%2F", doi)
  uid  <- gsub("\\/", "%2F", uid)
  
  res <- httr::GET(paste0(url = "https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi),
                   httr::add_headers(`Wiley-TDM-Client-Token` = token))
  
  if(httr::status_code(res) == 200){
    
    res <- httr::GET(paste0(url = "https://api.wiley.com/onlinelibrary/tdm/v1/articles/", doi),
                     httr::add_headers(`Wiley-TDM-Client-Token` = token),
                     httr::write_disk(paste0(path, "/", uid, ".pdf"), overwrite=TRUE))
  } else{
    
    message("There is no Wiley API access to this doi")
  }
}


#' Elsevier full text retrieval
#'
#' This function retrieves full texts from Elsevier journals
#' @param doi digital object identifier
#' @param uid unique identifier for paper
#' @param token API token
#' @param path where full texts should be stored
elsevier_ft <- function(doi, uid, token, path){
  doi <- gsub("\\/", "%2F", doi)
  uid  <- gsub("\\/", "%2F", uid)
  
  res <- httr::GET(paste0(url = "https://api.elsevier.com/content/article/doi/", doi),
                   httr::add_headers(`X-ELS-APIKey` = token))
  
  if(httr::status_code(res) == 200){
    
    res <- httr::GET(paste0(url = "https://api.elsevier.com/content/article/doi/", doi),
                     httr::add_headers(`X-ELS-APIKey` = token),
                     httr::write_disk(paste0(path, "/", uid, ".json"), overwrite=TRUE))
    
    file <- jsonlite::fromJSON(paste0(path, "/", uid, ".json"))
    file_txt <- file[["full-text-retrieval-response"]][["originalText"]]
    
    if(!is.null(file_txt)){
      
      write.table(file_txt, paste0(path, "/", uid, ".txt"), row.names = FALSE)
    }
  } else{
    
    message("Elsiever API cannot access this doi")
  }
  
}
