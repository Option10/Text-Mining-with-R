library(shiny)
library(topicmodels)
library(dplyr)
library(data.table)
library(easyPubMed)

if("shiny" %in% rownames(installed.packages()) == FALSE) {install.packages("shiny")}
if("topicmodels" %in% rownames(installed.packages()) == FALSE) {install.packages("topicmodels")}
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}
if("data.table" %in% rownames(installed.packages()) == FALSE) {install.packages("data.table")}


df <- readRDS("/home/francois/Documents/Projet_Text_mining/Text-Mining-with-R/PubMed-app/Dataframe_1")
irlba <- readRDS("/home/francois/Documents/Projet_Text_mining/Text-Mining-with-R/PubMed-app/data/irlba")
ap_documents <- readRDS("data/ap_documents.rds")
ap_lda <- readRDS("data/ap_lda.rds")
ap_topics <- readRDS("data/ap_topics.rds")
ap_top_terms <- readRDS("data/ap_top_terms.rds")

## CSS
mycss <- "
#error{color: #F5A52A;
  font-size: 13px;
}
#loadmessage {
   position: relative;
   border-radius: 25px;
   margin-left: auto; 
   margin-right: auto;
   top: 0px;
   left: 0px;
   width: 50%;
   padding: 5px 0px 5px 0px;
   text-align: center;
   font-weight: bold;
   font-size: 100%;
   color: #000000;
   background-color: #F5F5F5;
   z-index: 105;
}
#pubmed_img{
   position: relative;
   top: -54px;
   left: 115px;
}
"

Strings <- data.frame("noPosQuery" = "Your positive querry isn't significant in any of our topics, try an other research"
                      , "noNegQuery" = "Sorry, we will not take into account the negative request"
                      , "insignificantNegQuery" = "Your negative querry isn't significant in any of our topics, try an other research or continue")


############################## User Interface ##############################
ui <- fluidPage(
  
  titlePanel("Search"),
  img(id="pubmed_img",src = "PubMed.png", height = 52, width = 150),
  tags$head(tags$style(HTML(mycss))),
  
  sidebarLayout(
    
    #side panel ---------------------
    sidebarPanel(width = 3,
      textInput("positive_query","Search:",value = "breast"),
      textInput("negative_query","negative querry:",value = ""),
      helpText("It is possible to add a negative querry in order to avoid certain topics"),
      selectInput("method", 
                  label = "Select your search method:",
                  choices = c("LSA", "LDA", "Pubmed query"),
                  selected = "LSA"),
      helpText("LSA: or latent semantic analysis is a method that bla bla"),
      helpText("LDA: or latent dirichlet allocation is a bla bla bla"),
      hr(),
      textOutput("error") # marche que sur LDa pour l'instant
    ),
    
    
    # Main panel --------------------
    mainPanel(
      textOutput("OK"),
      textOutput("show_tot_text"),
      hr(),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      tableOutput("table")
    )
  )
)

############################## Server logic ##############################
server <- function(input, output) {
  
  output$OK <- renderPrint({ 
    
    tot_text <- 0
    Result <- 0
#################  LSA
    if (input$method == "LSA") {
      
        
      # input$positive_query <- stemDocument(input$positive_query) # IF STEMMING
      flag1 <- match(input$positive_query, rownames(irlba$v))
      
      # input$negative_query <- stemDocument(input$negative_query) # IF STEMMING
      flag2 <- match(input$negative_query, rownames(irlba$v))
      
      # try(if(sum(is.na(flag)) > 0) stop("Query not found"))
      # try(if(input$negative_query != '' & sum(is.na(flag)) > 0) stop("Query not found"))
      
      output$error <- renderPrint({ 
        try(if(sum(is.na(flag1)) > 0) cat(as.character(Strings$noPosQuery)))
        try(if(input$negative_query != '' & sum(is.na(flag2)) > 0) cat(as.character(Strings$noNegQuery)))
        
      })
      
      
      # Compute the queries' coordinates in SVD matrix
      posIndex <- vector(length = length(input$positive_query))
      for (i in (1:length(input$positive_query))) {
        posIndex[i] <- match(input$positive_query[i], rownames(irlba$v))
      }
      eig_posQuerry <- irlba$v[posIndex,]
      
      if (length(input$negative_query)>1 | input$negative_query != ''){
        negIndex <- vector(length = length(input$negative_query))
        for (i in (1:length(input$negative_query))) {
          negIndex[i] <- match(input$negative_query[i], rownames(irlba$v))
        }
        eig_negQuerry <- irlba$v[negIndex,]
      }
      
      # This function computes the euclidean distance between the queries and each document
      euc.dist <- function(docs,querry){ 
        dimDocs <- dim(docs)
        squareSum <- 0
        euc.dist <- vector(length=dimDocs[1])
        for (i in (1:dimDocs[1])) {
          for (j in (1:dimDocs[2])){ # TODO: remove the loop and calculate with the whole vectors
            squareSum <- squareSum + (docs[i,j] - querry[j])^2
          }  
          euc.dist[i] <- squareSum ^ 0.5
          squareSum <- 0
        }
        return(euc.dist)
      }
      
      # Calculate distance, order and name the rows
      posdistMatrix <- matrix(nrow = length(input$positive_query),ncol=dim(irlba$u)[1])
      posdist <- rep(1,length = dim(irlba$u)[1])
      if (length(input$positive_query) > 1){
        for (i in (1:length(input$positive_query))) {
          posdistMatrix[i,] <- euc.dist(irlba$u, eig_posQuerry[i,])
          posdist <- posdist + posdistMatrix[i,]
        }
      }else{posdist <- euc.dist(irlba$u, eig_posQuerry)}
      
      if (input$negative_query[1] != ""){
        negdistMatrix <- matrix(1L,nrow = length(input$negative_query),ncol=dim(irlba$u)[1])
        negdist <- rep(1,length = dim(irlba$u)[1])
        if (length(input$negative_query) > 1){
          for (i in (1:length(input$negative_query))) {
            negdistMatrix[i,] <- euc.dist(irlba$u, eig_negQuerry[i,])
            negdist <- negdist + negdistMatrix[i,]
          }
        }else{negdist <- euc.dist(irlba$u, eig_negQuerry)}
        distMatrix <- 0.8*posdist - 0.2*negdist
      }else distMatrix <- posdist
      
      names(distMatrix) <- rownames(irlba$u)
      distMatrix <- distMatrix[order(distMatrix),drop=FALSE]
      
      Result <- names(distMatrix)
      Result <- gsub(pattern = 'text',replacement = '',x = Result)
      
      
      DT = data.table(
        Title = df[Result[1:10],3],
        abstract = df[Result[1:10],2],
        id = df[Result[1:10],1],
        date = df[Result[1:10],4],
        author = df[Result[1:10],5])
      
      
    } # end if LSA
    
    
#################  LDA
    if (input$method == "LDA") {
      
      ind_pos <- which(ap_top_terms$term==input$positive_query)
      topic_int_pos <- ap_top_terms$topic[ind_pos]
      
      ind_neg <- which(ap_top_terms$term[1:200]==input$negative_query)
      topic_int_neg <- ap_top_terms$topic[ind_neg]

      topic_int_tot <- setdiff(topic_int_pos,topic_int_neg)
      
      if (length(topic_int_tot)<1) {
        topic_int_tot <- topic_int_pos
      }
      
      output$error <- renderPrint({ 
        tryCatch(if(length(topic_int_pos)<1) cat(as.character(Strings$noPosQuery)))
        tryCatch(if(length(topic_int_neg)<1 & input$negative_query!="") cat(as.character(Strings$noNegQuery)))
        tryCatch(if(length(topic_int_tot)<1 & input$negative_query!="") cat(as.character(Strings$insignificantNegQuery)))

      })
      
      ind3 <- which(ap_documents$topic %in% c(topic_int_tot))

      dfr <- ap_documents[c(ind3),]
      dfr$beta1=0
      for (i in 1:nrow(dfr)) {
        for (j in 1:length(topic_int_tot)) {
          if (dfr$topic[i]==topic_int_tot[j]) {
            dfr$beta1[i]<-ap_top_terms$beta[ind_pos[j]]
          }
        }
      }
      
      dfr$gamma <- dfr$gamma/sum(dfr$gamma[1:length(dfr$gamma)])          
      dfr$pond1 <- 0
      dfr$pond1 <- dfr$gamma+dfr$beta1
      
      result <- dfr[order(-dfr$pond1),]
      
      top_text <- select(head(result,200),"document")
      
      top_text_number <- 0
      right_text <- NA
      wrong_text <- NA
      tot_text <- NA
      perfect_match <- 0
      
      for (j in 1:200) {
        top_text_number[j] <- as.numeric(as.character(gsub("text",'',top_text[j,1])))
      }
      
      Abstract <- as.character(df$Abstract)
      
      for (k in top_text_number){
        right_text[match(k,top_text_number)]<-grepl(input$positive_query, Abstract[k])
        wrong_text[match(k,top_text_number)]<-grepl(input$negative_query, Abstract[k])
      }
      
      tot_text <- setdiff(top_text_number[right_text],top_text_number[wrong_text])
      
      DT = data.table(
        Title = df[tot_text[1:10],3],
        abstract = df[tot_text[1:10],2],
        id = df[tot_text[1:10],1],
        date = df[tot_text[1:10],4],
        author = df[tot_text[1:10],5])
      
      
      
    } # end if LDA
    
    
#################  Pubmed query 
    if (input$method == "Pubmed query") {
      
      Ids <- get_pubmed_ids(input$positive_query)
      papers <- fetch_pubmed_data(Ids)
      
      xmltop = xmlRoot(papers) 
      Article_Num <- xmlSize(xmltop) 

      ID <- vector()
      Abstract <- vector()
      Title <- vector()
      Date <- vector()
      Author_lastname <- vector()
      Author_forename <- vector()
      Author <- vector()
      
      ptm <- proc.time()
      
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
      
      DT = data.table(
        Title = Title[1:10],
        abstract = Abstract[1:10],
        id = ID[1:10],
        date = Date[1:10],
        author = Author[1:10])
      
      rm(ID, Abstract, Title, Date, Author, Author_forename, Author_lastname)
      
    } # end Pubmed query
    
    
##### Print output: 
    output$show_tot_text <- renderText({ paste("We found:",max(length(tot_text),length(Result)),"results.") })
    output$table <- renderTable(DT)
    
  }) # end Render OK
  
}

# Run the app --------------------
shinyApp(ui = ui, server = server)





