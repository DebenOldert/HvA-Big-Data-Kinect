# CALCULATE PATIENT STATE ALGORITHM
STATE = c("WALK", "UP", "DOWN", "SIT")
size_ <- 5
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
  }
  POINTS[i,]$index <- DATA[i,]$Time
  remove(j)

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
}
remove(tmp_)
remove(state_)
remove(i)
remove(start)
remove(end)

DATA <- DATA[complete.cases(DATA),]
rownames(DATA) <- 1:nrow(DATA)
DATA$index = as.integer(rownames(DATA))

Console.NewLine("STATE CALCULATION DONE")

# USE PATIENT ENVIRONMENT
source("code/patient.R")

patient$WALKING <- group(strtoi(rownames(DATA[DATA$state==1,])), 10)
patient$SITTING <- group(strtoi(rownames(DATA[DATA$state==4,])), 10)
patient$UP <- group(strtoi(rownames(DATA[DATA$state==2,])), 5)
patient$DOWN <- group(strtoi(rownames(DATA[DATA$state==3,])), 5)

patient$SITBASE <- mean(c(DATA[as.integer(unlist(patient$SITTING)),]$FootLeft.y, DATA[as.integer(unlist(patient$SITTING)),]$FootRight.y))

if(!consistent(as.integer(unlist(patient$WALKING)))){
  stop("Patient not consistently walking, (Maybe he/she fell). Anyway, we can't analyse this data", call. = FALSE)
}
patient$WALKING <- as.integer(unlist(patient$WALKING))

# CALCULATE STRAIGHT WALKING PATH
yPrediction <-lm(Head.y ~ I(index^2)+index, data=DATA[min(patient$WALKING):max(patient$WALKING),])
yPredicted <- as.vector(predict(yPrediction, data.frame(index=patient$WALKING)))

patient$WALKBASE <- mean(c(yPredicted[1], tail(yPredicted, n=1)))

patient$WALKERROR <- yPredicted - patient$WALKBASE

for(i in patient$WALKING[1]:(length(patient$WALKING) + patient$WALKING[1] - 1)){
  #DATA[i,]$Head.y <- DATA[i,]$Head.y - (yPredicted[i-patient$WALKING[1]+1] - patient$WALKBASE)
  #DATA[,grep(".y", colnames(DATA))]

  for(j in colnames(DATA[i,grep(".y", colnames(DATA))])){
    DATA[i,j] <- DATA[i,j] - patient$WALKERROR[i - patient$WALKING[1] + 1]
  }
}
remove(yPrediction)
remove(yPredicted)

plot(POINTS$probability, type = "l")

Console.NewLine("FILLING PATIENT CLASS DONE")

# http://stats.stackexchange.com/questions/30975/how-to-add-non-linear-trend-line-to-a-scatter-plot-in-r
# http://www.mathsisfun.com/geometry/parabola.html
