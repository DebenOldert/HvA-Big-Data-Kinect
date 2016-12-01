# CALCULATE PATIENT STATE ALGORITHM
STATE = c("WALK", "UP", "DOWN", "SIT")
size_ <- 7
differ_ <- 0.1

POINTS <- data.frame(
  index = integer(1)
)
POINTS["state"] <- 0
POINTS["probability"] <- 0

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
  POINTS[i,]$index <- DATA[i,]$Time

  if(i>size_*2 && i<=((nrow(DATA) - size_))){
    tmp_ <- (
      tbl_df(
        table(
          POINTS[i-size_,] %>%
            unlist(., use.names=FALSE)
        )
      ) %>%
        arrange(
          desc(n)
        )
    )
    POINTS[i-size_,]$state <- tmp_[1,]$Var1
    POINTS[i-size_,]$probability <- ((1 / size_) * tmp_[1,]$n)
  }
  DATA[i,]$state = state_
  print(i)
}
print("STATE DONE")
remove(tmp_)
DATA <- DATA[complete.cases(DATA),]
rownames(DATA) <- 1:nrow(DATA)
