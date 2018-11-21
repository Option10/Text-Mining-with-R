Extract_Data <- function(query,abstractSize){
  library(XML)
  library(easyPubMed)

  ############### PART 1: Information extraction ###############
  
  ## Dataset importation
  #---------------------
  # Option 1: 698 documents via une requete
  #----------
  if(query != ''){
    cat("Extract from query :",query)
    Ids <- get_pubmed_ids(query)
    papers <- fetch_pubmed_data(Ids)
  }
  # Option 2: 52349 documents via importation du fichier xml.
  #----------
  if (query == ''){
    # papers <- xmlParse(file = "/home/francois/Documents/Projet_Text_mining/pubmed18n0924.xml")
    papers <- xmlParse(file = "pubmed18n0924.xml")
  }
  ## Information Extraction from dataset ("papers")
  #------------------------------------------------
  xmltop = xmlRoot(papers) # top node of "papers" xml structure
  Article_Num <- xmlSize(xmltop) # number of nodes (Articles) "in papers"
  # xmlSApply(xmltop[[1]], xmlName) # shows names of child nodes
  
  ID <- vector()
  Abstract <- vector()
  Title <- vector()
  Date <- vector()
  Author_lastname <- vector()
  Author_forename <- vector()
  Author <- vector()
  
  ptm <- proc.time()
  # info extraction
  for (i in 1:Article_Num) {
    ID[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["PMID"]])
    Abstract[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["Abstract"]])
    Title[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]])
    Date[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
    Author_lastname[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["LastName"]])
    Author_forename[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][["Author"]][["ForeName"]])
    Author[i] <- paste(Author_lastname[i],Author_forename[i])
  }
  proc.time() - ptm
  rm(papers)
  
  # create dataframe
  df <- data.frame(ID, Abstract, Title, Date, Author)
  rm(ID, Abstract, Title, Date, Author, Author_forename, Author_lastname)
  
  df <- df[complete.cases(df[ , 2]),]
  df <- df[nchar(as.character(df[ , 2]))<abstractSize[2] & nchar(as.character(df[ , 2]))>abstractSize[1],]

  # export dataframe
  saveRDS(df, file = "Dataframe", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)

  return(df)  
}