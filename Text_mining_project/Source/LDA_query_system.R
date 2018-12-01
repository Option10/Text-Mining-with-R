function(posQuery_String,negQuery_String,LDAtop_terms,LDAdoc,Abstract){
  
  beta1 <-0
  
  ap_top_terms <- LDAtop_terms
  ap_documents <- LDAdoc
  
  positive_querry <- posQuery_String
  negative_querry <- "kidney" #negQuery_String
  
  ind_pos <- which(ap_top_terms$term == positive_querry)  
  topic_int_pos <- ap_top_terms$topic[ind_pos]  
  
  if (length(topic_int_pos)<1) {
    print("Your positive query isn't significant in any of our topics, try an other research")
  }
  
  ind_neg <- which(ap_top_terms$term == negative_querry)
  topic_int_neg <- ap_top_terms$topic[ind_neg]
  
  if (length(topic_int_neg)<1) {
    print("Your negative query isn't significant in any of our topics, try an other research or continue" )
  }
  
  topic_int_tot <- setdiff(topic_int_pos,topic_int_neg)
  
  if (length(topic_int_tot)<1) {
    print("Sorry, we will not take into account the negative request")
    topic_int_tot <- topic_int_pos
  }
  
  ind3 <- which(ap_documents$topic %in% c(topic_int_tot))
  
  df <- ap_documents[c(ind3),]
  df$beta1=0
  
  for (i in 1:nrow(df)){
    for (j in 1:length(topic_int_tot)) {
      if (df$topic[i] == topic_int_tot[j]) {
        df$beta1[i]=ap_top_terms$beta[ind_pos[j]]
      }
    }
  }
  
  df$gamma=df$gamma/sum(df$gamma[1:length(df$gamma)])
  df$pond1=0
  df$pond1=df$gamma+df$beta1
  
  result <- df[order(-df$pond1),]
  head(result)
  
  top_text <- select(head(result,200),"document")
  top_text
  top_text_number <- 0
  right_text <- NA
  wrong_text <- NA
  tot_text <- NA
  perfect_match <- 0
  
  for (j in 1:200) {
    top_text_number[j] <- as.numeric(as.character(gsub("text",'',top_text[j,1])))
  }
  
  for (k in top_text_number){
    right_text[match(k,top_text_number)]<-grepl(positive_querry, Abstract[k])
    wrong_text[match(k,top_text_number)]<-grepl(negative_querry, Abstract[k])
  }
  
  tot_text <- setdiff(top_text_number[right_text],top_text_number[wrong_text])
  
  length(top_text_number[right_text])
  length(top_text_number[wrong_text])
  length(tot_text)
  print(head(tot_text))
  print(Abstract[head(tot_text,10)])
}