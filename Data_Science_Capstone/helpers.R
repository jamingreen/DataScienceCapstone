library(tm)
library(stringr)
library(dplyr)
library(tidyr)

pt1 <- readRDS("ngramTable50/ngram1_phrase_table.rds")
pt2 <- readRDS("ngramTable50/ngram2_phrase_table.rds")
pt3 <- readRDS("ngramTable50/ngram3_phrase_table.rds")

predic <- function(sen, n, len)
{
  if (n == 1)
  {
    pt <- pt1
  }
  else if (n == 2)
  {
    pt <- pt2
  }
  else
  {
    pt <- pt3
  }
  
  if (n == 1)
  {
    return(pt[1:10,])
  }
  if (n == 2)
  {
    resPt <- pt[pt[,1] == sen[len],]
  }
  else
  {
    resPt <- pt
    for (i in 1:(n-1))
    {
      resPt <- resPt[resPt[,i] == sen[len-n+i+1],]
    }
  }
  if (nrow(resPt) ==0)
  {
    return(predic(sen, n-1, len))
  }
  else
  {
    return(resPt[1:10,] %>% na.omit())
  }
}

pred <- function(sen)
{
  sen <- sen %>% str_trim %>% removePunctuation() %>% tolower() %>% str_split(" ")
  sen <- sen[[1]]
  if (sen[1] == "")
  {
    return("")
  }
  len <- length(sen)
  n <- min(len+1, 3)
  res <- predic(sen, n, len)
  res[1,ncol(res)-1]
}

#' The function export is generate(n) where n is the number of words in the sentence it generate.
#' This function will generate word using the word that is most frequent with a probability of 0.2 where it will generate according to the frequency in the sample text



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
  
  word <- sample(pt1$word, size = 1, prob = pt1$prop)
  sen <- word

  # generate second word
  respt <- match_phrase(sen, pt2, 2)
  if (nrow(respt) == 0)
  {
    word <- sample(pt1$word, size = 1, prob = pt1$prop)
  }
  else if(roll())
  {
    word <- respt$word2[1]
  }
  else
  {
    word <- sample(respt$word2, size = 1, prob = respt$prop)
  }
  sen <- paste(sen, word)

  return(sen)
}

genNext <- function(sen, pt3)
{
  respt <- match_phrase(sen, pt3, 3)
  if (nrow(respt) == 0)
  {
    respt <- match_phrase(sen, pt2, 2)
    if (nrow(respt) == 0)
    {
      word <- sample(pt1$word, size = 1, prob = pt1$prop)
    }
    else if (roll())
    {
      word <- respt$word2[1]
    }
    else
    {
      word <- sample(respt$word2, size = 1, prob = respt$prop)
    }
  }
  else if (roll())
  {
    word <- respt$word3[1]
  }
  else
  {
    word <- sample(respt$word3, size = 1, prob = respt$prop)
  }
  sen <- paste(sen, word)
  return(sen)
}






generate <- function(n)
{
  n <- as.numeric(n)
  if (is.na(n))
  {
    return("Number not valid. (need to be positive integer smaller than or equal to 30)")
  }
  if (n<0 | n > 30 | !n%%1 == 0)
  {
    return("Number not valid. (need to be positive integer smaller than or equal to 30)")
  }
  if (n==1)
  {
    word <- sample(pt1$word, size = 1)
    return(word)
  }
  else if (n==2)
  {
    return(genFirst2())
  }
  else if(n > 2)
  {
    sen <- genFirst2()
    for (i in 1:(n-2))
    {
      sen <- genNext(sen, pt3)
    }
    return(sen)
  }
}