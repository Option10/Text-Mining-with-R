Extract_Data <- function(query,abstractSize){
  loadPackage("XML","easyPubMed")
  
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
  Authors <- vector()
  ForeName <- vector()
  LastName <- vector()
  Keywordlist <- vector()
  Keywords <-vector()
  k <- 0L
  
  # info extraction
  ptm <- proc.time()
  cat("Please wait \n")
  cat("|-----------------------------| \n")
  for (i in 1:Article_Num){
    if (i/(Article_Num/30) >= k){
      cat("|")
      k = k +1
    }
    ID[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["PMID"]])
    Abstract[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["Abstract"]])
    Title[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleTitle"]])
    Date[i] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["ArticleDate"]])
    for (k in 1:5){
      ForeName[k] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][[k]][["ForeName"]])
      LastName[k] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["Article"]][["AuthorList"]][[k]][["LastName"]])
      Keywordlist[k] <- xmlValue(xmltop[[i]][["MedlineCitation"]][["KeywordList"]][[k]])
    }
    fullnames <- paste(ForeName, LastName)
    fullnames <- fullnames[fullnames!="NA NA"]
    Authors[i] <- paste(fullnames, collapse = '/')
    Keywords[i] <- paste(Keywordlist,collapse = '/')
    
  }
  cat("\n")
  proc.time() - ptm
  rm(papers)
  
  # create dataframe
  df <- data.frame(ID, Abstract, Title, Date, Authors, Keywords)
  rm(ID, Abstract, Title, Date, Authors, ForeName, LastName,fullnames,Keywords, Keywordlist)
  rm(xmltop, Article_Num, Book_count, Medline_count, i, k)
  
  # clean dataframe
  df <- df[complete.cases(df[ , 2]),]
  df <- df[nchar(as.character(df[ , 2]))<abstractSize[2] & nchar(as.character(df[ , 2]))>abstractSize[1],]

  # export dataframe
  saveRDS(df, file = "Dataframe", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)

  return(df)  
}