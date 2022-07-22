library(tm)
library(stringr)

predic4 <- function(sen, n, len)
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
    resPt <- pt
    for (i in 1:(n-1))
    {
      resPt <- resPt[resPt[,i] == sen[len-n+i+1],]
    }
  }
  if (nrow(resPt) ==0)
  {
    return(predic4(sen, n-1, len))
  }
  else
  {
    return(resPt[1:10,] %>% na.omit())
  }
}

pred4 <- function(sen)
{
  start_time <- Sys.time()
  sen <- sen %>% removePunctuation() %>% tolower() %>% str_split(" ")
  sen <- sen[[1]]
  len <- length(sen)
  n <- min(len, 3)
  res <- predic4(sen, n, len)
  print(Sys.time() - start_time)
  res
}