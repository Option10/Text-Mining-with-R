# Text mining app with LSA, LDA and pubmed query on the PubMed abstracts database  

## Introduction

This work is the result of a four-month period of work for academic purpose. The objectif of our project is to build a tool able to find documents of interest regarding a postive and a negative query. Powerpoints presentations are available on the "Presentation" forlder with the bibliography. All the objectives are explained in those slides and the report for this work is also available.

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

## Conclusion 

This project is a result of a four-month period of work, and we are aware of its limits. 

The user interface is very intuitive, and the user can select the algorithm he wants for its research. 
Both LSA and LDA work pretty well, they both have their pros and cons.

LSA will be preferentially used when the user knows what kind of documents he wants. The user can also put several words in his query’s, something LDA can’t do.

On the other hand, LDA will be very useful when users want a more prospective approach.
Furthermore, we have observed that LSA algorithm deals more with the frequencies of words and select documents with a very high frequency for the researched word. LDA in contrast will select documents dealing mainly with all the “concepts” behind the query. If the positive query is “cancer”, LDA will select documents dealing with cancer but also tumor, metastasis etc.

Finally, one thing we can discuss is the weighting of the beta and gamma values for the LDA and the Euclidean distance calculations in LSA.
Indeed, the weighting we used is a basic normalization of gamma values and a sum between those new gammas with beta values, but others weighting methods may be used. 

For LSA, we’ve chose to measure distance between our query’s and document using an Euclidean distance. We justify this choice because of its simplicity, but other methods may be used.

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
