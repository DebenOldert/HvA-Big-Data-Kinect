suppressMessages(library(dplyr))
suppressMessages(library(stringr))
suppressMessages(library(crayon))

source("code/functions.R")

WD <- getwd()

CSV <- file.choose()

CSV <- read.csv(CSV)

# CONVERT
DATA <- NULL

for(i in 1:(nrow(CSV) / 3)) {
  x <- NULL
  y <- NULL
  z <- NULL

  if(i > 1) { i <- i * 3 - 2 }

  x <- CSV[i,]
  y <- CSV[i+1,]
  z <- CSV[i+2,]

  cnv_ <- merge(x, y, all = TRUE, suffixes = c(".x", ".y"), by = "Time")
  cnv_ <- merge(cnv_, z, all = TRUE, suffixes = c("", ".z"), by = "Time")

  if(i == 1) {
    DATA <- cnv_
  }
  else {
    DATA <- bind_rows(DATA, cnv_)
  }

}

DATA$state = NA

remove(x)
remove(y)
remove(z)
remove(cnv_)

Console.NewLine("DATA PREPERATION DONE")

source("code/state.R")

source("code/head.R")
#source("code/ankle.R")
#source("code/back.R")
