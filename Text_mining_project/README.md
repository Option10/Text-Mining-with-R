# Text mining app with LSA, LDA and pubmed query on the PubMed abstracts database  


## How to run the app


- Make sure you have R installed  

- The first line of the Source/main.R and the GUI/app.R is to set the working directory (path to Text_mining_project must be set)  

- TIP: to get the path to the current working directory, type getwd() in the console
  
example: setwd("C:/Users/Brieuc/Documents/Text_mining_project")
- The packages are installed automaticly  

- Choose the parameters you want in the Source/main.R  

- Check if you have the database.xml in the Data directory  
- Run main.R (it should fill the Data dir with some cool files)
  
- Run app.R and start querying  



The ShinyApps can be hosted on a server so the final user can access it without having R installed


## Q&A 
- How to run R  
start the RGui or any other interface for R (Rstudio works well)   
Linux: open a terminal, write R and it will start a session   the Rscript file.R command is also useful
- The app is not starting (but the code runs well)  
open the code as a script and run it  
it won't work with source("GUI/app.R")  