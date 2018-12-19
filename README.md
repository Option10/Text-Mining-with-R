# Text mining app with LSA, LDA and pubmed query on the PubMed abstracts database  

## Introduction

This work is the result of a four-month period of work for academic purpose. The objectif of our project is to build a tool able to find documents of interest regarding a postive and a negative query. Powerpoints presentations are available on the "Presentation" forlder with the bibliography. All the objectives are explained in those slides.

Finally, we manage to make an application on shinyapps able to treat and sort revelant documents regarding user's query's.

## How to run the app

- Make sure you have R installed  

- The first line of the Source/main.R and the GUI/app.R is to set the working directory (path to Text_mining_project)  

- TIP: to get the path to the current working directory, type getwd() in your console
  
  example: setwd("C:/Users/Brieuc/Documents/Text_mining_project")

- The packages are installed automaticly  

- Choose the parameters you want in the Source/main.R  

- Check if you have the database.xml in the Data directory  

- Run main.R (it should fill the Data dir with some cool files)
  
- Run app.R and start querying  



The ShinyApps can be hosted on a server so the final user can access it without having R installed

## Code packages : 

Main.R : Core of the code. Here the user can chose if he wants to make a new calculus for LDA, LSA or the tokenization, change the hyperparamter k and use new dataset. It's in this part of the code that query's are implemented. It will then uses the outputs of the functions below to run the application.

Extract_Data.R : This code takes all documents from the PubMed database, preprocess them to make the abstracts readabel for the app and identifying every abstract with its ID. Finally, we create a dataframe will all those documents.

LDA.R : The code use the dataframe of tokens to implement the LDA algorithm and calculate the gamma and beta values.

LDA_query_system.R : Here the code uses the output of LDA.R to implement the query system. Query's are taken from the main.R

LSA.R and LSA_query_system.R : Works exactly the same as for the LDA algorithm.

Tokenization.R : Function to transform words from documents into tokens (i.e. every word becomes a row in the dataframe). It will also remove all stopwords.

Load.packages.R : Function to download all packages needed in the functions above.

## Q&A 

- How to run R  
start the RGui or any other interface for R (Rstudio works well)   
In R console: source("script.R") will run the script  
In R script: ctrl + enter will run selected lines  
Linux: open a terminal, write R and it will start a session   
the Rscript file.R command directly run the script in the terminal  
- The app is not starting (but the code runs well)  
open the code as a script and run it  
it won't work with source("GUI/app.R")  
