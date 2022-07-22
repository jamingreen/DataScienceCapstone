#' The function export is generate(n) where n is the number of words in the sentence it generate.
#' This function will generate word using the word that is most frequent with a probability of 0.2 where it will generate according to the frequency in the sample text
#' " done it in beach their of at i in isone was a siren rape and hear destroy ttt trys in"
library(stringr)



#' sen is the vector of words that needs to be matched
#' pt is the phrase table
#' n is the degree of the phrase table
match_phrase <- function(sen, pt, n)
{
  sen <- str_split(sen, " ")[[1]]
  len = length(sen)
  resPt <- pt
  
  for (i in 1:(n-1))
  {
    resPt <- resPt[resPt[,i] == sen[len-n+i+1],]
  }
  resPt
}

roll <- function()
{
  if (sample(0:1, size = 1, prob = c(0.2,0.8)) == 1)
  {
    return(TRUE)
  }
  else
  {
    return(FALSE)
  }
    
}

genFirst2 <- function()
{
  
  # generate first word
  pt1 <- readRDS("./ngramTable/ngram1_phrase_table.rds")
  
  word <- sample(pt1$word, size = 1, prob = pt1$n)
  sen <- word
  rm(list= "pt1")
  print(sen)
  
  # generate second word
  pt2 <- readRDS("./ngramTable/ngram2_phrase_table.rds")
  respt <- match_phrase(sen, pt2, 2)
  if (nrow(respt) == 0)
  {
    respt <- readRDS("./ngramTable/ngram1_phrase_table.rds")
    word <- sample(respt$word, size = 1, prob = respt$n)
  }
  else if(roll())
  {
    word <- respt$word2[1]
  }
  else
  {
    word <- sample(respt$word2, size = 1, prob = respt$n)
  }
  sen <- paste(sen, word)
  rm(list = c("respt", "pt2", "word"))
  print(sen)
  
  return(sen)
}

genNext <- function(sen, pt3)
{
  print("generate next")
  respt <- match_phrase(sen, pt3, 3)
  if (nrow(respt) == 0)
  {
    pt2 <- readRDS("./ngramTable/ngram2_phrase_table.rds")
    respt <- match_phrase(sen, pt2, 2)
    if (nrow(respt) == 0)
    {
      respt <- readRDS("./ngramTable/ngram1_phrase_table.rds")
      word <- sample(respt$word, size = 1, prob = respt$n)
      rm(list = "respt")
    }
    else if (roll())
    {
      word <- respt$word2[1]
    }
    else
    {
      word <- sample(respt$word2, size = 1, prob = respt$n)
      rm(list = "respt")
    }
    rm(list = "pt2")
  }
  else if (roll())
  {
    word <- respt$word3[1]
  }
  else
  {
    word <- sample(respt$word3, size = 1, prob = respt$n)
  }
  sen <- paste(sen, word)
  return(sen)
}






generate <- function(n)
{
  if (!is.na(as.numeric(n))| n<0 | n > 30 | !n%%1 == 0)
  {
    return("Number not valid. (need to be positive integer smaller than or equal to 30)")
  }
  if (n==1)
  {
    pt1 <- readRDS("./ngramTable/ngram1_phrase_table.rds")
    word <- sample(pt1$word, size = 1)
    return(word)
  }
  else if (n==2)
  {
    return(genFirst2())
  }
  else if(n > 2)
  {
    pt3 <- readRDS("./ngramTable/ngram3_phrase_table.rds")
    sen <- genFirst2()
    for (i in 1:(n-2))
    {
      sen <- genNext(sen, pt3)
      print(i+2)
      print(sen)
    }
    return(sen)
  }
}