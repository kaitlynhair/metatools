#' Read reference data from an XML file
#'
#' This function reads reference information from an XML file and returns a data frame
#' containing the extracted data.
#'
#' @param path The path to the XML file.
#' @param source The XML source, not used currently.
#'
#' @return A data frame containing the extracted reference information.
#'
#' @details This function uses XPath queries to extract specific fields from each record
#' in the XML file, such as author names, publication year, journal title, DOI, etc.
#' It then creates a data frame with these fields and returns it.
#'
#' @import XML
#' @import xml2
#' @import dplyr

#' @export
read_xml <- function(path, source){

  newdat <- XML::xmlParse(path)
  x <- XML::getNodeSet(newdat, "//record")
  xpath2 <- function(x, ...) {
    y <- XML::xpathSApply(x, ...)
    y <- gsub(",", "", y)
    ifelse(length(y) == 0, NA, paste(y, collapse = "; "))
  }
  newdat <- data.frame(author = sapply(x, xpath2, ".//author", xmlValue),
                       year = sapply(x, xpath2, ".//dates/year", xmlValue),
                       journal = sapply(x, xpath2, ".//periodical/full-title",  xmlValue),
                       doi = sapply(x, xpath2, ".//electronic-resource-num", xmlValue),
                       title = sapply(x, xpath2, ".//titles/title",  xmlValue),
                       pages = sapply(x, xpath2, ".//pages", xmlValue),
                       volume = sapply(x, xpath2, ".//volume", xmlValue),
                       number = sapply(x, xpath2, ".//number",  xmlValue),
                       abstract = sapply(x, xpath2, ".//abstract",  xmlValue),
                       keywords = sapply(x, xpath2, ".//keywords/keyword",  xmlValue),
                       record_id = sapply(x, xpath2, ".//rec-number", xmlValue),
                       isbn = sapply(x, xpath2, ".//isbn", xmlValue),
                       secondary_title = sapply(x, xpath2, ".//titles/secondary-title", xmlValue),
                       pmid = sapply(x, xpath2, ".//custom2", xmlValue),
                       label = sapply(x, xpath2, ".//label", xmlValue),
                       database = sapply(x, xpath2, ".//remote-database-name", xmlValue),
                       accession = sapply(x, xpath2, ".//accession-num", xmlValue),
                       url = sapply(x, xpath2, ".//urls/web-urls", xmlValue))


}
