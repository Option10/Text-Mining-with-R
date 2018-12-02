runLDA <- function(k,tokensDF){
loadPackage("dplyr","quanteda","stringr","tidytext","topicmodels","tictoc","ggplot2")

tic("LDA")
#--------------------------------------------------------------------------
cat("LDA \n")
ap_lda <- LDA(tokensDF, k, control = list(seed = 1234))

ap_topics <- tidy(ap_lda, matrix = "beta")

cat("top terms")
ap_top_terms <- ap_topics %>%
  group_by(topic) %>%
  top_n(dim(tokensDF)[2], beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

ap_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()

ap_documents <- tidy(ap_lda, matrix = "gamma")

saveRDS(ap_top_terms, file = "Data/LDAtop_terms", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)
saveRDS(ap_documents, file = "Data/LDAdoc", ascii = FALSE, version = NULL,
          compress = TRUE, refhook = NULL)

toc()
}
