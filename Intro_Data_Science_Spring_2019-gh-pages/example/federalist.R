# in this script we will recreate one of the most famous examples of 
# using Naive Bayes, to identify the author of several disputed papers
# this was done in the 1960s

install.packages("corpus")
install.packages("tidyverse")
install.packages("stringr")


library(corpus)

federalist$text[1]


#install/load packages


library("tidyverse")
library("stringr")


federalist$author

# authorship of 15 of these papers disputed by hamilton and madison

missing_author <- is.na(federalist$author)


federalist_test_set <- federalist[missing_author,]$text
federalist_test_set

federalist_test_set <- federalist %>% filter(is.na(author)) %>% select(text) #%>% head()
federalist_test_set$text

# remove the papers written by john jay, not considered for disputed papers

federalist_training_set <- federalist %>% anti_join(federalist_test_set) %>% 
  filter(author != "Jay") %>%
  select(text,author) #%>%
  #head() 


# want to train a classifier on this data
# want to split this up into the words in each article,
# which article it is, and the author

# not easy to do before the tidytext package!

library(tidytext)

lines <- federalist_training_set %>% str_split(text,pattern = "\n") %>% head()


unnest_tokens(word,federalist_training_set$text[1])

paper_text <- str_split(federalist_training_set$text[1],pattern = "\n")

paper_text
unnest_tokens(word,text)

typeof(text[1])


text <- c(unlist(paper_text[1:4]))

text_df <- tibble(line = 1:4, text = text[1:4])

text_df %>% unnest_tokens(word,text)


federalist_training_set %>% head()


fed_training <- federalist_training_set %>% 
  mutate(article= row_number()) %>%
  unnest_tokens(word,text) #%>%
  


#%>% group_by(author) %>% count(word,sort=TRUE)

fed_training %>% head()
fed_training %>% tail()
federalist$author

fed_training %>% group_by(author) %>% count(word,sort=TRUE)
# these words are always the most common words in english text. often makes sense to remove them
training_vocab <- fed_training %>% group_by(author) %>% count(word, sort=TRUE) %>%
  mutate(author_count = n) %>% select(-n)

# could also use rename for the previous step

# naive bayes uses the probability of the word in one example vs the other.
# so we need to know the proportion of times hamilton or madison used a word

# total count of these words
word_count <- fed_training %>% count(word,sort = TRUE) #%>% head()

training_vocab <- training_vocab %>% full_join(word_count) %>% 
  mutate(prob_author = author_count/n, prob_not = 1- prob_author) %>% 
  distinct(word,.keep_all=TRUE) #%>%
  #dim()

final_training <- training_vocab %>%
  mutate(prob_Hamilton = if_else(author=="Hamilton",prob_author,prob_not),
         count_Hamilton = if_else(author=="Hamilton",author_count,n-author_count)) %>%
  ungroup() %>%
  mutate(prob_Madison = 1 - prob_Hamilton,
         count_Madison = n - count_Hamilton) %>%
  select(word,prob_Hamilton,prob_Madison,count_Hamilton,count_Madison)


final_training %>% head()
# no na's up to here anyway, they may appear later
# want to do something similar for the test 

fed_test <- federalist_test_set %>%
  mutate(document = row_number()) %>%
  unnest_tokens(word,text)

fed_test %>% head()


test_vocab <- fed_test %>% group_by(document) %>% count(word,sort = TRUE) %>% 
  rename(word_counts = n) #%>% head()




i = 2 # which of the 15 documents with missing authors
doc_vocab <- federalist_test_set %>%
  mutate(article = row_number()) %>%
  unnest_tokens(word,text) %>%
  group_by(article,word) %>% count(word,sort = TRUE) %>% #head()
  filter(article == i)
doc_vocab %>% head()


doc_test <-final_training %>% left_join(doc_vocab, by="word") %>% select(-article)


prior <- 0.5
rule <- sum(log(doc_test$prob_Hamilton), na.rm = TRUE) + 
  sum(log(doc_test$n)) + 
  log(prior) > 
  sum(log(doc_test$prob_Madison), na.rm = TRUE) + 
  sum(log(doc_test$n)) +
  log(1 - prior)

rule
# words with zero probability mess it up. need to add one event to fix that

final_training %>% head()
final_training %>% filter(count_Madison==0)

# set words with no counts to be 1 to avoid this error. only makes a small difference.
modified_training <- final_training %>%
  mutate(count_Hamilton = ifelse(count_Hamilton==0,1,count_Hamilton),
         count_Madison = ifelse(count_Madison==0,1,count_Madison),
         prob_Hamilton = count_Hamilton/sum(count_Hamilton),
         prob_Madison = count_Madison/sum(count_Madison))
modified_training %>% filter(count_Madison ==0 )



doc_test <-modified_training %>% left_join(doc_vocab, by="word") %>% 
  select(-article) #%>% replace_na(list(n=0)) %>%
  # mutate(n = n+1)



doc_test$n
prior <- 0.5 # prior probability of being written by Hamilton

doc_test = doc_test %>%
  mutate(log_prob_Ham = n* log(prob_Hamilton),
         log_prob_Madison = n*log(prob_Madison))

rule <- sum(doc_test$log_prob_Ham,na.rm = T) + 
  log(prior) > 
  sum(doc_test$log_prob_Madison,na.rm = T) +
  log(1 - prior)

rule
ifelse(rule, "Hamilton", "Madison")
# classifies the first as Madison, do it for all of them

####
doc_test = doc_test %>%
  mutate(log_prob_Ham = n* log(prob_Hamilton),
         log_prob_Madison = n*log(prob_Madison))



####


for(i in c(1:15)){
  doc_vocab <- federalist_test_set %>%
    mutate(article = row_number()) %>%
    unnest_tokens(word,text) %>%
    group_by(article,word) %>% count(word,sort = TRUE) %>% #head()
    filter(article == i)
  doc_test <-modified_training %>% left_join(doc_vocab, by="word") %>% 
    select(-article) 
  prior = 0.5
  doc_test = doc_test %>%
    mutate(log_prob_Ham = n* log(prob_Hamilton),
           log_prob_Madison = n*log(prob_Madison)) 
  
  rule <- sum(doc_test$log_prob_Ham,na.rm = T) + 
    log(prior) > 
    sum(doc_test$log_prob_Madison,na.rm = T) +
  log(1 - prior)
  
  print(ifelse(rule, "Hamilton", "Madison"))
  
}

# similar result as mosteller in 1960s


### extensions
# this is a simple implementation of naive bayes. we could remove common stop
# words. we could also only look at the top 100 words