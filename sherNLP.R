# Clears Environment variable
rm(list=ls())

# Just could not bring myself to delete : number 1
#library(SnowballC)
# Tokenization function 
# wordTokenize <- function(x){
#   x <- tolower(x)
#   #x <- gsub("<.*?>"," ",x)
#   x <- gsub("\n", " ",x)
#   x <- gsub("[^[:alnum:] ]", " ", x)
#   x <- wordStem(words = unlist(strsplit(x,split = " ")), language = "porter")
#   x <- toString(x)
#   x <- gsub(",", " ", x)
#   #cat(class(x))
#   return(x)
# }


#Library for Porter Stemming
library(SnowballC)

# Tokenization Function
wordTokenize <- function(x){
  x <- tolower(x)
  x <- gsub("\n", " ",x) # : removes newline
  x <- gsub("[^[:alnum:] ]", " ", x) # : removes non-alphanumerics
  x <- toString(x)
  return(x)
}

# Removes percentage of words that appears n percent.
remove_percentage <- function(n, doc){
  ul <- unlist(strsplit(doc,split = " "))
  unique_val <- unique(ul)
  t <- table(ul)
  emp <- c()
  newt <- t/length(ul)
  for (i in 1:length(newt)){
    if (newt[i] > n) {
      t[i] <- 0
    }
  }
  emp <- append(emp, which(t==0))
  zero_index <- c(names(emp))
  n_doc <- ul
  for (i in 1:length(zero_index)){
    n_doc <- replace(n_doc, n_doc == zero_index[i], " ")
  }
  n_doc <- wordStem(words = n_doc, language = "porter")
  #n_doc <- gsub(",","",toString(n_doc))
  n_doc <- toString(n_doc)
  return(n_doc)
}


library(readr)
library(stringr)
percentage <- 0.01
load_data <- unlist(strsplit(str_squish(gsub(",","",remove_percentage(percentage, wordTokenize(str_squish(read_file(".../sherlockdataNLP.txt")))))),split=" "))

# Couldn't bring myself to delete: number 2
# Chances are, I might need them some day like an iphone box
#load_data <- unlist(strsplit(str_squish(wordTokenize(str_squish(read_file(".../sherlockdataNLP.txt")))),split = " "))
#load_data <-  strsplit(str_squish(wordTokenize(read_file(".../sherlockdataNLP.txt"))),split = " ")

#Creates vocabulary
vocab <- unique(load_data)

#Needed library for sparse matrix initialization
library(Matrix)



# Sparse matrix initialization 
t_matrix <-sparseMatrix(i=integer(0), j=integer(0), dims=c(length(vocab), length(vocab)),x = numeric(0), index1 = TRUE)

# Row and colnames of sparse matrix == vocab 
colnames(t_matrix)<- rownames(t_matrix) <- vocab

# Computes the most likely probability using bigram (first-order Markov)
# Finds the indexes of the first word, adds one, finds indexes of the second word, find the most count of second word and computes bigram probability
# Example: P(x|project),(x is unknown). finds all the indexes of project, adds one, find all strings corresponding the add_one indexes, finds the most 
# frequent(x) and divide by the count of project
num <- 1
for(i in vocab){
  cat(num,"\n")
  count_i <- which(load_data %in% i) 
  add_one <- count_i + 1
  get_names <- c()
  for(elem in add_one){
    get_names <- append(get_names,values = load_data[elem])
  }
  most_freq <- table(get_names)[which.max(table(get_names))]
  j <- names(most_freq)
  t_matrix[i,j] <- most_freq/length(count_i) # entries in t_matrix are the most likely bigram probabilities
  num <- num + 1
}


# natural language generation function using markov bigram
# uses the t_matrix (bigram matrix) to find the next probable word
markov_generate <- function(initial_word, word_length){
  markov_words <- c(initial_word) 
  for (word in 1:word_length){
    markov_words[word+1] <-colnames(t_matrix)[which.max(t_matrix[markov_words[word],])]
  }
  
  return(gsub(",","",toString(markov_words)))
}

# Note that, most probable words will loop themselves. Markov is simple and not the best at generating semantically appropriate sentences.
# You can substitute "arthur" below with any word in the vocabulary
markov_generate("arthur",10)


