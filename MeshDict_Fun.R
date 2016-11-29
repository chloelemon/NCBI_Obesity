#this function runs after ExtractDict
#takes in a dataframe structure as below:

#'data.frame':	# obs. of  4 variables:
#$ ID              : int
#$ DescriptorUI    : chr  
#$ meshHeadings    : chr  
#$ SemanticTypeName: chr  

#return a new unlisted data frame (aka. each cell contain single value)

MeshDict <- function(dataframe){
  
  #set the key (unique identifier)
  d.dt <- data.table(dataframe, key="ID")
  
  #repeat the key and unlist the column's value
  
  newdt <- d.dt[, c(list(DescriptorUI = unlist(strsplit(DescriptorUI, ","))),list(meshHeadings = unlist(strsplit(meshHeadings, ","))),list(SemanticTypeName = unlist(strsplit(SemanticTypeName, ",")))), by=ID]
  
  newdt <- as.data.frame(newdt)
  
  #remove ID field
  newdt[,"ID"] <- NULL
  
  #remove duplicate
  newdt <- unique(newdt)
  
  return(newdt)
  
}







