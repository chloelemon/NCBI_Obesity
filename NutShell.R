#code in a nut shell
#obesity comorbidity analysis


#preparation

#clear environment if needed
rm(list = ls())

#set working directory
setwd('~/Documents/Dev_dataScienceProjects/BrownUniv/')


#install/reuqire libraries
if (!'pacman' %in% installed.packages()){
  install.packages('pacman')
}

pacman::p_load("ggplot2","dplyr", "rentrez","knitr","RCurl","plyr","tidyr","data.table")

#rentrez is the package in R provides an interface to the NCBI's EUtils API


#show the database summary information
entrez_db_summary("pubmed")


#Searchable fields for database 'pubmed'
searchField <- entrez_db_searchable("pubmed")

searchField$MAJR

query <- "obesity[MAJR] AND 2000/01/01:2012/12/31[PDAT]"

obesity_search <- entrez_search(db="pubmed",
                                query,
                                retmode = "xml",
                                use_history = TRUE,
                                retmax=60000)

kable(summary(obesity_search))

obesity_search$web_history


obesity_search$QueryTranslation

#fetch all records from the formulated search
source('./batchFetch_Fun.R')

#this returns a data frame with extracted information from all records
dt_sum <- batchFetch(obesity_search)
#function code will be shown at the end of this report

#save dt_sum in local so don't need to run every time
#write.csv(dt_sum, "obesity_SumTable.csv")
#dt_sum <- read.csv("obesity_SumTable.csv")
#dt_sum$X <- NULL

kable(dt_sum[1:3,])


source('./unlistDT_Fun.R')

#return a data frame of 5 variables with each cell contains only single value
dt_tidy <- unlistDT(dt_sum)
#write.csv(dt_tidy, "tidy_obesitySearch.csv")

#download XML file - 2015 MeSH descriptor
MeSHdescriptor <- XML::xmlParse("ftp://nlmpubs.nlm.nih.gov/online/mesh/2015/desc2015.xml")
#meshList <- xmlToList(MeSHdescriptor)


#extract value from xml
source("./ExtractDict_Fun.R")
dt_MeshDict <- ExtractDict(MeSHdescriptor)


#unlist variables (tidy)
source("./MeshDict_Fun.R")
tidy_Dict <- MeshDict(dt_MeshDict)

#reorder
tidy_Dict <- dplyr::arrange(tidy_Dict, SemanticTypeName)


#save the dataframe in case
dt_PubMed <- dt_tidy

#merge/VLOOKUP to map the semantictype to dt_PubMed
dt_Mapped <- merge(dt_PubMed, tidy_Dict, by="meshHeadings",all.x=TRUE)

#rearrange columns and rows
dt_Mapped <- dplyr::select(dt_Mapped, pmid,meshHeadings,SemanticTypeName,articletitle,authors,year)
dt_Mapped <- dplyr::arrange(dt_Mapped, SemanticTypeName)


#remove meshHeadings that are 'Obesity'
dt_filter <- dplyr::filter(dt_Mapped, meshHeadings != 'Obesity')
#select only 'Disease or Syndrome' as SemanticTypeName
dt_filter <- dplyr::filter(dt_filter, SemanticTypeName == 'Disease or Syndrome')
#check if neccessary
#str(dt_filter)
kable(dt_filter[1:5,])

search_year <- function(year, term){
  query <- paste(term, "AND (", year, "[PDAT])")
  entrez_search(db="pubmed", term=query, retmax=0)$count
}

Year <- 2000:2012
papers <- sapply(Year, search_year, term="Obesity[MeSH Major Topic]", USE.NAMES=FALSE)

dtPlot <- data.frame(Year, papers)
dtPlot$Year <- as.character(dtPlot$Year)

ggplot(data = dtPlot, aes(x=Year, y=papers,group=1)) +
  geom_line(colour="light blue", size=1.5) +
  geom_point(colour="black") +
  ggtitle("Trend for Publications on Obesity") +
  xlab("Year") +
  ylab("Number of Papers")


#group by meshHeadings and count number records for each MeSH
meSH_count <- setDT(dt_filter)[, .(count = uniqueN(pmid)), by = meshHeadings]
meSH_count <- as.data.frame(meSH_count)
#rarrange
meSH_count <- dplyr::arrange(meSH_count, -count) 
meSH_count <- dplyr::select(meSH_count, meshHeadings,Count = count)

kable(meSH_count[1:10,])


#subset the top 25 records
Top25 <- meSH_count[1:29,]
Top25 <- dplyr::filter(Top25, meshHeadings != "Life Style"& meshHeadings != "Health Behavior" & meshHeadings != "Feeding Behavior"&meshHeadings != "Quality of Life")


ggplot(Top25) +
  geom_bar(aes(x=reorder(meshHeadings, Count),y=Count),stat="identity",fill = "orange") +                               #theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Top Comorbidities for Obesity") +
  xlab("Disease or Syndrome") +
  ylab("Number of pmid") +
  coord_flip()#+


