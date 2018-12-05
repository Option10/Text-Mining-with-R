# setwd("~/Text-Mining-with-R/Text_mining_project")

library(shiny)
library(topicmodels)
library(dplyr)
library(data.table)
library(easyPubMed)
library(XML)
library(quanteda)
library(tm)
library(foreach)
library(doParallel)


df <- readRDS("Data/Dataframe") # row data
irlba <- readRDS("Data/irlba") # SVD matrix (for LSA)

ap_documents <- readRDS("Data/LDAdoc") # for LDA
ap_top_terms <- readRDS("Data/LDAtop_terms")
# ap_lda <- readRDS("Data/LDAtop_terms")
# ap_topics <- readRDS("Data/ap_topics.rds")

## CSS
mycss <- "
#pubmed_img{
   position: relative;
   top: -54px;
   left: 115px;
}
#error{
  color: #F5A52A;
  font-size: 18px;
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
"

Strings <- data.frame(  "noPosQuery" = "Your positive querry isn't significant in any of our topics, try an other research"
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
      textInput("positive_query","Search:",value = "protein"),
      hr(),
      textInput("negative_query","Negative query:",value = ""),
      helpText("It is possible to add a negative querry in order to avoid certain topics."),
      br(),
      div(style="display: inline-block;vertical-align:top; width: 250;",selectInput("method", 
                  label = "Select your search method:",
                  choices = c("LSA", "LDA", "Pubmed query"),
                  selected = "LDA")),
      div(style="display: inline-block;vertical-align:top; width: 50px;",HTML("<br>")),
      div(style="display: inline-block;vertical-align:top; width: 150;",selectInput("max_Results", 
                  label = "Displayed results:",
                  choices = c("10", "25", "50"),
                  selected = "10")),
      helpText("LSA: latent semantic analysis"),
      helpText("LDA: latent dirichlet allocation"),
      br(),
      helpText(em("for more information visit www.github-lien.com"),align = "center"),
      hr(),
      textOutput("error") # marche que sur LDa pour l'instant
    ),
    
    
    # Main panel --------------------
    mainPanel(
      textOutput("environment"),
      textOutput("show_Results_num"),
      hr(),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      tableOutput("table")
    )
  )
)

############################## Server logic ##############################
server <- function(input, output) {
  
  output$environment <- renderPrint({ 
  
    Result <- 0
    
#################  LSA
    if (input$method == "LSA") {
      query_system <- dget("Source/LSA_query_system.R")
      
      if (sum(which(rownames(irlba$v) == "hepatic")) > 0){
        stemming <- FALSE
      }else{stemming <- TRUE}
      
      query_output <- query_system(irlba,input$positive_query,input$negative_query,df$Abstract,stemming)
      Result <- query_output$res
      error <- query_output$err
      output$error <- renderText({error})
      
      # cat(input$positive_query,input$negative_query)
      
      
      DT = data.table(
        Title = df$Title[Result[1:input$max_Results]],
        Abstract = df$Abstract[Result[1:input$max_Results]],
        ID = df$ID[Result[1:input$max_Results]],
        Date = df$Date[Result[1:input$max_Results]],
        Authors = gsub("/", ", ", df$Authors[Result[1:input$max_Results]]))

    } # end if LSA
    
    
#################  LDA
    if (input$method == "LDA") {
      
      query_system <- dget("Source/LDA_query_system.R")
      
      query_output <- query_system(input$positive_query,input$negative_query,ap_top_terms,ap_documents,df$Abstract,Strings)
      Result <- query_output$res
      error <- query_output$err
      output$error <- renderText({error})
      
      DT = data.table(
        Title = df$Title[Result[1:input$max_Results]],
        Abstract = df$Abstract[Result[1:input$max_Results]],
        ID = df$ID[Result[1:input$max_Results]],
        Date = df$Date[Result[1:input$max_Results]],
        Authors = gsub("/", ", ", df$Authors[Result[1:input$max_Results]]))
      
    } # end if LDA
    
    
#################  Pubmed query 
    if (input$method == "Pubmed query") {
      
      query_system <- dget("Source/Extract_Data.R")
      
      abstractSize <- c(100,3000) # min and max caracter in abstracts analysed
      
      df <- query_system(input$positive_query,abstractSize)
      
      DT = data.table(
        Title = df$Title[1:input$max_Results],
        Abstract = df$Abstract[1:input$max_Results],
        ID = df$ID[1:input$max_Results],
        Date = df$Date[1:input$max_Results],
        Authors = gsub("/", ", ",df$Author[1:input$max_Results]))
      
    } # end Pubmed query
    # as.Date(df$Date[100:110], "%Y")
    
##### Print output: 
    output$show_Results_num <- renderText({ paste("We found:",length(Result),"results, showing", input$max_Results) })
    output$table <- renderTable(DT)
    
  }) # end Render OK
  
}

# Run the app --------------------
shinyApp(ui = ui, server = server)



