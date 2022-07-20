library(stringr)
library(ngram)
processFile <- function(filePath,destFile, p) {
  con <- file(filePath, "r")
  lin <- readLines(con)
  for (i in 1:length(lin)) {
    if (rbinom(1,1,p) == 1) {
      write(lin[i], file = destFile, append = TRUE)
    }
  }
}

randomSelectLine <- function(filePath, destFile, n) {
  con <- file(filePath, "r")
  lin <- readLines(con)
  inn <- sample(length(lin), n, replace = FALSE)
  writeLines(lin[inn], destFile)
}

pred <- function() {
  rbind(
    df[grep("^during the bad", df$phrase), c(4,6)],
    df[grep("^during the sad", df$phrase), c(4,6)],
    df[grep("^during the hard", df$phrase), c(4,6)],
    df[grep("^during the worse", df$phrase), c(4,6)]
  )
}
