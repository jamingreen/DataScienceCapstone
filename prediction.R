library(tm)
library(stringr)

pred_1 <- function(pt1){
  pt1$word[1]
}

pred_2 <- function(sen, pt2) {
  resPt <- pt2[pt2$word1 == sen[1],2]
  if (nrow(resPt) > 0)
  {
    return(resPt[1])
  }
  else
  {
    return(pred_1())
  }
}

my_pred <- function(sen, pt1,pt2,pt3) {
  sen <- sen %>% removePunctuation() %>% tolower() %>% str_split(" ")
  sen <- sen[[1]]
  len <- length(sen)
  print(len)
  if (len == 0) {
    return(pred_1())
  }
  else if (len == 1) {
    return(pred_2(sen, pt2))
  }
  else if (len > 1) {
    resPt <- pt3[pt3$word1 == sen[len - 1] & pt3$word2 == sen[len],c(3,6) ]
    if (nrow(resPt)> 0) 
    {
      return(resPt[1,1])
    }
    else
    {
      return(pred_2(sen,pt2))
    }
  }
}

predic <- function(sen, n, len)
{
  filePath <- paste0("ngramTable/ngram",as.character(n),"_phrase_table.rds")
  pt <- readRDS(filePath)
  if (n == 1)
  {
    return(pt[1:10,1])
  }
  print(sen[(len-n+2):len])
  print(n)
  if (n == 2)
  {
    resPt <- pt[pt[,1] == sen[len],c(3,5)]
  }
  else
  {
    resPt <- pt[which(apply(pt[,1:(n-1)],1,function(x) all(x == sen[(len-n+2):len]))),c(1:(n+1), n+3)]
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

pred1 <- function(sen)
{
  start_time <- Sys.time()
  sen <- sen %>% removePunctuation() %>% tolower() %>% str_split(" ")
  sen <- sen[[1]]
  len <- length(sen)
  n <- min(len, 5)
  res <- predic(sen, n, len)
  print(Sys.time() - start_time)
  res
}

predic2 <- function(sen, n, len)
{
  print("predict2")
  filePath <- paste0("ngramTable/ngram",as.character(n),"_phrase_table.rds")
  pt <- readRDS(filePath)
  if (n == 1)
  {
    return(pt[1:10,1])
  }
  print(sen[(len-n+2):len])
  print(n)
  if (n == 2)
  {
    resPt <- pt[pt[,1] == sen[len],c(3,5)]
  }
  else
  {
    resPt <- pt
    for (i in 1:(n-1))
    {
      resPt <- resPt[resPt[,i] == sen[len-n+i],]
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

pred2 <- function(sen)
{
  start_time <- Sys.time()
  sen <- sen %>% removePunctuation() %>% tolower() %>% str_split(" ")
  sen <- sen[[1]]
  len <- length(sen)
  n <- min(len, 5)
  res <- predic2(sen, n, len)
  print(Sys.time() - start_time)
  res
}
