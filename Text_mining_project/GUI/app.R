setwd("/home/francois/Documents/Projet_Text_mining/Text-Mining-with-R/Text_mining_project")

loadPackage <- dget("Source/loadPackage.R")
loadPackage("shiny","topicmodels","dplyr","data.table","easyPubMed","XML","quanteda","tm","foreach","doParallel","DT")


df <- readRDS("Data/Dataframe") # row data
irlba <- readRDS("Data/irlba") # SVD matrix (for LSA)

ap_documents <- readRDS("Data/LDAdoc") # for LDA
ap_top_terms <- readRDS("Data/LDAtop_terms")
# ap_lda <- readRDS("Data/LDAtop_terms")
# ap_topics <- readRDS("Data/ap_topics.rds")


Strings <- data.frame(  "noPosQuery" = "Your positive querry isn't significant in any of our topics, try an other research"
                      , "noNegQuery" = "Sorry, we will not take into account the negative request"
                      , "insignificantNegQuery" = "Your negative querry isn't significant in any of our topics, try an other research or continue")


############################## User Interface ##############################
ui <- fluidPage(id = "fluidpage", theme = "stylesheet.css",
  hr(id="header"),
  div(style="display:inline-block; position: relative; left: 90px",titlePanel("Search")),
  div(style="display:inline-block; position: relative; top: -8px; left: 95px",img(id="pubmed_img",src = "PubMed.png", height = 52, width = 150)),
  sidebarLayout(
    
    #side panel ---------------------
    sidebarPanel(id = "sidebarpanel",width = 3,
      textInput("positive_query","Positive query:",value = "protein"),
      textInput("negative_query","Negative query:",value = ""),
      helpText("The negative query allows you to avoid certain topics."),
      br(),
      selectInput("method", 
                  label = "Select your search method:",
                  choices = c("Latent Semantic Analysis", "Latent Dirichlet Allocation"),
                  selected = "Latent Semantic Analysis",
                  width = 250),
      hr(),
      helpText(em("for more information visit https://github.com/Option10/Text-Mining-with-R"),align = "center"),
      textOutput("error") # marche que sur LDa pour l'instant
    ),
    
    
    # Main panel --------------------
    mainPanel(id = "mainpage",
      textOutput("environment"),
      conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                       tags$div("Loading...",id="loadmessage")),
      DTOutput("table")
    )
  )
)

############################## Server logic ##############################
server <- function(input, output) {
  
  output$environment <- renderPrint({ 
  
    Result <- 0

#################  LSA
    if (input$method == "Latent Semantic Analysis") {
      query_system <- dget("Source/LSA_query_system.R")
      
      if (sum(which(rownames(irlba$v) == "hepatic")) > 0){
        stemming <- FALSE
      }else{stemming <- TRUE}
      
      query_output <- query_system(irlba,input$positive_query,input$negative_query,df$Abstract,stemming)
      Result <- query_output$res
      error <- query_output$err
      output$error <- renderText({error})
      
    } # end if LSA
    
#################  LDA
    if (input$method == "Latent Dirichlet Allocation") {
      
      query_system <- dget("Source/LDA_query_system.R")
      
      query_output <- query_system(input$positive_query,input$negative_query,ap_top_terms,ap_documents,df$Abstract,Strings)
      Result <- query_output$res
      error <- query_output$err
      output$error <- renderText({error})
      
    } # end if LDA
    
##### output Table: 
    
    # create dataframe
    DT = data.table(
      Title = df$Title[Result],
      Abstract = df$Abstract[Result],
      ID = paste("<a href='https://www.ncbi.nlm.nih.gov/pubmed/?term=",df$ID[Result],"'>",df$ID[Result],"</a>",sep = ""),
      Date = as.Date(as.character(df$Date[Result]),format = "%Y"),
      Authors = gsub("/", ", ", df$Authors[Result]))
    
    # create dataframe output with DT::datatable and renderDT 
    DT<- datatable(cbind(' ' = '&oplus;', DT), escape = FALSE,
                   options = list(
                     pageLength = 25,
                     columnDefs = list(
                       list(visible = FALSE, targets = c(3,4)),
                       list(width = '65px', targets = 5),
                       list(orderable = FALSE, className = 'details-control', targets = 1),
                       list(orderable = TRUE, className = 'details-control', targets = 0)),
                     searchHighlight = TRUE),
                   callback = JS("
  table.column(1).nodes().to$().css({cursor: 'pointer'});
  var format = function(d) {
    return '<div style=\"background-color:#eee; padding: .5em;\"> Abstract: ' +
            d[3] + '</div>' +
           '<div style=\"background-color:#eee; padding: .5em;\"> ID: ' + d[4] + '</div>';
  };
  table.on('click', 'td.details-control', function() {
    var td = $(this), row = table.row(td.closest('tr'));
    if (row.child.isShown()) {
      row.child.hide();
      td.html('&oplus;');
    } else {
      row.child(format(row.data())).show();
      td.html('&CircleMinus;');
    }
  });"))
    
    # render output
    output$table <- renderDT(DT)
    
  }) # end Render environement
  
}

# Run the app --------------------
shinyApp(ui = ui, server = server)



