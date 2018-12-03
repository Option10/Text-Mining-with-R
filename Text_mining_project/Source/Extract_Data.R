Extract_Data <- function(query,abstractSize){
  
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
     papers <- xmlParse(file = "Data/database.xml")
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
  j <- 0L
  
  # info extraction
  ptm <- proc.time()
  cat("Please wait \n")
  cat("|-----------------------------| \n")
  for (i in 1:Article_Num){
    #if querry via PubMedQuerry, add code chunck at bottom of page.
    # and add if(startsWith(xmlSApply(xmltop[[i]], xmlName)[1],"MedlineCitation")){...}
    if (i/(Article_Num/30) >= j){
      cat("|")
      j = j + 1L
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
  print(proc.time() - ptm)
  rm(papers)
  
  # create dataframe
  df <- data.frame(ID, Abstract, Title, Date, Authors, Keywords)
  rm(ID, Abstract, Title, Date, Authors, ForeName, LastName,fullnames,Keywords, Keywordlist)
  rm(xmltop, Article_Num, Book_count, Medline_count, i, k)
  
  # clean dataframe
  df <- df[complete.cases(df[ , 2]),]
  df <- df[nchar(as.character(df[ , 2]))<abstractSize[2] & nchar(as.character(df[ , 2]))>abstractSize[1],]

  return(df)  
}

# if(startsWith(xmlSApply(xmltop[[i]], xmlName)[1],"BookDocument")){
#   Book_count=Book_count+1
#   ID[i] <- xmlValue(xmltop[[i]][["BookDocument"]][["PMID"]])
#   Abstract[i] <- xmlValue(xmltop[[i]][["BookDocument"]][["Abstract"]][["AbstractText"]])
#   Title[i] <- xmlValue(xmltop[[i]][["BookDocument"]][["ArticleTitle"]])
#   Date[i] <- xmlValue(xmltop[[i]][["BookDocument"]][["ContributionDate"]])
#   if(startsWith(xmlName(xmltop[[i]][["BookDocument"]][["AuthorList"]][[1]][[1]]),"CollectiveName")){
#     Authors[i]<-xmlValue(xmltop[[i]][["BookDocument"]][["AuthorList"]])
#   }else{
#     for (k in 1:5){
#       ForeName[k] <- xmlValue(xmltop[[i]][["BookDocument"]][["AuthorList"]][[k]][["ForeName"]])
#       LastName[k] <- xmlValue(xmltop[[i]][["BookDocument"]][["AuthorList"]][[k]][["LastName"]])
#       Keywordlist[k] <- xmlValue(xmltop[[i]][["BookDocument"]][["KeywordList"]][[k]])
#     }
#     fullnames <- paste(ForeName, LastName)
#     fullnames <- fullnames[fullnames!="NA NA"]
#     Authors[i] <- paste(fullnames, collapse = '/')
#     Keywords[i] <- paste(Keywordlist,collapse = '/')
#   }
#   
# }