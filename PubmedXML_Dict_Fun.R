#this function takes in a XML object resulted from entrez_fetch() and returns a data frame with variables of pmid, authors, year, articletitle, meshHeadings.

PubmedXML_Dict <- function(XMLdata) {
  library(XML)
  #XMLdata <- xmlParse(theFile) #if file is a .XML
  records <- getNodeSet(XMLdata, "//PubmedArticle")
  pmid <- xpathSApply(XMLdata,"//MedlineCitation/PMID", xmlValue)
  
  authLast <- lapply(records, xpathSApply, ".//Author/LastName", xmlValue)
  authLast[sapply(authLast, is.list)] <- NA
  authInit <- lapply(records, xpathSApply, ".//Author/Initials", xmlValue)
  authInit[sapply(authInit, is.list)] <- NA
  authors <- mapply(paste, authLast, authInit, collapse = "|")
  
  year <- lapply(records, xpathSApply, ".//PubDate/Year", xmlValue) 
  year[sapply(year, is.list)] <- NA
  year <- unlist(year)

  articletitle <- xpathSApply(XMLdata,"//ArticleTitle", xmlValue)

  
  meshHeadings <- lapply(records, xpathSApply, ".//DescriptorName", xmlValue)
  meshHeadings[sapply(meshHeadings, is.list)] <- NA
  meshHeadings <- sapply(meshHeadings, paste, collapse = "|")
  
  
  DF <- data.frame(pmid, authors, year, articletitle, meshHeadings, stringsAsFactors = FALSE)
  return(DF)
}

#reference https://github.com/christopherBelter/pubmedXML/blob/master/pubmedXML.R
