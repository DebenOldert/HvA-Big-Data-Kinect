suppressMessages(library(dplyr))
suppressMessages(library(stringr))

WD <- getwd()

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

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

DATA$state = "UNKNOWN"

remove(x)
remove(y)
remove(z)
remove(cnv_)

#Plot graph
plot(DATA$Head.y,
     type = "l",
     ylab = "Hoogte hoofd"
)

# CALCULATE PATIENT STATE ALGORITHM
STATE = c("WALK", "UP", "DOWN", "SIT")
size_ <- 5
differ_ <- 0.1
  #color <- rainbow(length(STATE))
color <- c("red", "blue", "blue", "green")

POINTS <- data.frame(
  index = integer(1)
)
POINTS["state"] <- 0

for(i in 1:size_){
  POINTS[LETTERS[i]] <- 0
}

for(i in size_:(nrow(DATA)-size_)){
  start <- DATA[i,]$Head.y
  end <- DATA[i+size_,]$Head.y

  state_ <- NULL

  if(((start - end) < differ_) && (start - end) > -differ_){
    if(start >= mean(DATA$Head.y)){ #WALK (4)
      state_ <- 1
    }
    else{ #SIT (4)
      state_ <- 4
    }
  }
  else if((start - end) < differ_) { #UP (2)
    state_ <- 2
  }
  else if((start - end) > differ_) { #UP (3)
    state_ <- 3
  }
  else {
    state_ <- 1
  }

  for(j in 0:size_-1){
    POINTS[i-j,LETTERS[j+1]] <- state_
    #POINTS[i,LETTERS[j+1]] <- state_
  }

  if(i>size_*2 && i<=((nrow(DATA) - size_)+1)){
    POINTS[i-size_,]$state <- (
      tbl_df(
        table(
          POINTS[i-size_,] %>%
            unlist(., use.names=FALSE)
          )
        ) %>%
        arrange(
          desc(n)
          )
      )[1,]$Var1
  }
  #Plot state point on graph
  points(i, DATA[i,]$Head.y, col = color[state_])

}






