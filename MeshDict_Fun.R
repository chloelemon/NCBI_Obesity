#this function runs after ExtractDict
#takes in a dataframe structure as below:

#'data.frame':	# obs. of  2 variables:
#$ meshHeadings    : chr  
#$ SemanticTypeName: chr  

#return a new unlisted data frame (aka. each cell contain single value)

MeshDict <- function(dataframe){
  
  #set the key (unique identifier)
  d.dt <- data.table(dataframe, key="meshHeadings") #set key to meshHeadings first
  
  #repeat the meshHeadings for each element of each SemanticType
  newdt <- d.dt[, c(list(SemanticTypeName = unlist(strsplit(SemanticTypeName, "[|]")))), by=meshHeadings]
  #split by "|" but has to add [|] because it is a regex
  
  newdt <- as.data.frame(newdt)
  #remove duplicate so that there are no duplicate in the semantictypename cell
  newdt <- unique(newdt)
  
  #then do the same to meshHeadings
  newdt <- data.table(newdt, key="SemanticTypeName")
  newdt <- newdt[, c(list(meshHeadings = unlist(strsplit(meshHeadings, "[|]")))), by=SemanticTypeName]
  
  newdt <- as.data.frame(newdt)
  #remove duplicate
  newdt <- unique(newdt)
  
  return(newdt)
  
}







