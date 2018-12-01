setwd("~/Text-Mining-with-R/Text_mining_project")

loadPackage <- dget("Source/loadPackage.R")
loadPackage("shiny","topicmodels","dplyr","data.table","easyPubMed","XML","quanteda","tm","foreach","doParallel")


df <- readRDS("Data/Dataframe") # row data
irlba <- readRDS("Data/irlba") # SVD matrix (for LSA)

ap_documents <- readRDS("Data/LDAdoc") # for LDA
ap_top_terms <- readRDS("Data/LDAtop_terms")
# ap_lda <- readRDS("Data/LDAtop_terms")
# ap_topics <- readRDS("Data/ap_topics.rds")

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
      textInput("negative_query","negative query:",value = ""),
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
      
      query_system <- dget("Source/LSA_query_system.R")
      
      if (sum(which(rownames(irlba$v) == "hepatic")) > 0){
        stemming <- FALSE
      }else{stemming <- TRUE}
      
      Result <- query_system(irlba,input$positive_query,input$negative_query,df$Abstract,stemming)
      # cat(input$positive_query,input$negative_query)
      DT = data.table(
        Title = df$Title[Result[1:10]],
        abstract = df$Abstract[Result[1:10]],
        id = df$ID[Result[1:10]],
        date = df$Date[Result[1:10]],
        author = df$Author[Result[1:10]])
      
      
    } # end if LSA
    
    
#################  LDA
    if (input$method == "LDA") {
      
      query_system <- dget("Source/LDA_query_system.R")
      # TODO 
      
      
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
      
      query_system <- dget("Source/Extract_Data.R")
      
      abstractSize <- c(100,3000) # min and max caracter in abstracts analysed
      
      df <- query_system(input$positive_query,abstractSize)
      
      DT = data.table(
        Title = df$Title[1:10],
        abstract = df$Abstract[1:10],
        id = df$ID[1:10],
        date = df$Date[1:10],
        author = df$Author[1:10])
      
    } # end Pubmed query
    
    
##### Print output: 
    output$show_tot_text <- renderText({ paste("We found:",max(length(tot_text),length(Result)),"results.") })
    output$table <- renderTable(DT)
    
  }) # end Render OK
  
}

# Run the app --------------------
shinyApp(ui = ui, server = server)





