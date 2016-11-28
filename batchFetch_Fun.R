#this function takes in an object from result of entrez_search() which contains a web_history
#this function only uses on large records request from NCBI, since NCBI only allows 10,000 records per efetch
#the final output of this function is a data frame which contains extracted information from large XML records
#if you need to change the variables in the data frame please edit the function of PubmedXML_Dict()
batchFetch <- function(entrez_search){
  
  # Initiate restart at 0
  RetStart <- 0
  # Keep retmax at 10000
  RetMax <- 10000
  # Calculate how many itterations will be needed
  Runs <- (entrez_search$count %/% 10000) + 1
  # Create empty object to store data frame
  dt_extract <- NULL
  
  
  for (i in 1:Runs){
    fetch_xml_temp <- entrez_fetch(db = "pubmed", 
                                   web_history = entrez_search$web_history, 
                                   rettype = "xml", 
                                   retstart = RetStart,
                                   retmax = RetMax,
                                   parsed = TRUE)
    
    dt_Obesity_temp <- PubmedXML_Dict(fetch_xml_temp)
    
    dt_extract <- rbind(dt_extract,dt_Obesity_temp)  
    
    RetStart <- 0 + 10000*i
    
  }
  return(dt_extract)
  
}