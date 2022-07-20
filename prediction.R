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
