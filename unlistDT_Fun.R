unlistDT <- function(dataframe){
  #input should be the name of the dataframe
  #this function only takes data with str as below
  #'data.frame':	# obs. of  5 variables:
  #$ pmid        : chr  
  #$ authors     : chr  
  #$ year        : chr  
  #$ articletitle: chr  
  #$ meshHeadings: chr  
  
  #load data.table
  library(data.table)
  
  #set the key (unique identifier)
  d.dt <- data.table(dataframe, key="pmid")
  
  #repeat the key and unlist the column's value
  
  newdt <- d.dt[, c(list(authors = unlist(strsplit(authors, ","))), list(mesh = unlist(strsplit(meshHeadings, ",")))), by=pmid]

  #create temporary dataframe for merging purposes 
  mergeTemp <- dataframe[,c("pmid", "articletitle","year")]
  
  
  finalDT <- merge(newdt,mergeTemp, by="pmid", all.x = TRUE, all.y = TRUE)
  
  finalDT <- as.data.frame(finalDT)
  
  return(finalDT)
  
}


