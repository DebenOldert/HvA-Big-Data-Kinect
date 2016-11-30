suppressMessages(library(dplyr))

WD <- getwd()

specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

CSV <- file.choose()

CSV <- read.csv(CSV)

# CONVERT
DATA <- NULL

for(i in 1:(nrow(CSV) / 3)) {
  #if(i < 215 || i > 240) next
  x <- NULL
  y <- NULL
  z <- NULL

  if(i > 1) { i <- i * 3 - 2 }

  x <- CSV[i,]
  y <- CSV[i+1,]
  z <- CSV[i+2,]
  #print(i)
  cnv_ <- merge(x, y, all = TRUE, suffixes = c(".x", ".y"), by = "Time")
  cnv_ <- merge(cnv_, z, all = TRUE, suffixes = c("", ".z"), by = "Time")

  if(i == 1) {
    #print("CREATE")
    DATA <- cnv_
  }
  else {
    #print("BIND")
    DATA <- bind_rows(DATA, cnv_)
  }

}

DATA$state = "UNKNOWN"

remove(x)
remove(y)
remove(z)
remove(cnv_)

headDF <- select(DATA, Time, Head.y)
headDF$IndexTime <- 1:nrow(headDF)
lowpoints <- subset(headDF, headDF$Head.y <= 0.0)
lowpoints <- diff(as.matrix(lowpoints))
lowpoints <- as.data.frame(lowpoints)

calcIndexTime <- max(lowpoints$IndexTime)

#GET SMALLEST VALUE OF HEAD HIGHT

# GRAPH IS ALWAYS => SITTING, GET_UP, WALKING, GET_DOWN, SITTING

min_ <- min(DATA$Head.y)
norm_ <- 0

sitting1 <- c()
get_up <- c()
walking <- c()
get_down <- c()
sitting2 <- c()

cur_state  <- ""
cur_count  <- 0
prev_state <- ""
prev_count <- 0
deep_state <- ""
deep_count <- 0

row_count <- 0

plot(DATA$Head.y,
     type = "l",
     ylab = "Hoogte hoofd"
)

for(i in 2:nrow(DATA)){
  offset <- DATA[i-1,]

  diff = offset$Head.y - DATA[i,]$Head.y

  color <- "black"

  if(diff < -0.013){
    DATA[i,]$state = "GET_UP"
    color <- "red"
  }
  else if(diff > 0.013){
    DATA[i,]$state = "GET_DOWN"
    color <- "red"
  }
  else if(DATA[i,]$Head.y - min_ > 0.15){
    DATA[i,]$state = "WALKING"
    color <- "blue"
  }
  else{
    DATA[i,]$state = "SITTING"
    color <- "green"
  }

  prox <- 8

  if(cur_state == DATA[i,]$state && i != nrow(DATA)){
    cur_count <- cur_count+1
    row_count <- row_count+1
    if(cur_count > prox){
      prev_state <- cur_state
      prev_count <- cur_count
      deep_count <- 0
      deep_state <- cur_state
    }
  }
  else if(DATA[i,]$state == deep_state){
    cur_state <- DATA[i,]$state
    #cur_count <- deep_count+1
  }
  else if(i == nrow(DATA)){
    sitting2 <- c(i-deep_count, i-1)
  }
  else{
    if(prev_count == row_count){
      cur_count <- cur_count+1
    }
    else if(deep_state == "WALKING" && row_count > 3){
      cur_count <- cur_count+2
    }
    else if(deep_state == ""){
      deep_state = DATA[i,]$state
    }
    else if(cur_count < prox){
      cur_count <- cur_count+1
    }
    else if(prev_count <= prox){
      deep_count <- deep_count + 1
      prev_state <- deep_state
      prev_count <- deep_count
      cur_state <- prev_state
      cur_count <- cur_count+1
    }
    else if (prev_count > prox ){
      deep_count <- cur_count + 1
      #deep_state <- prev_state
      vct <- c(i-deep_count, i-1)
      points(i-deep_count, 0, col = "black")
      if(deep_state == "SITTING" && i < nrow(DATA) / 2){
        sitting1 <- vct
      }
      else if(deep_state == "SITTING"){
        sitting2 <- vct
      }
      else if(deep_state == "GET_UP"){
        get_up <- vct
      }
      else if(deep_state == "WALKING"){
        walking <- c(walking, vct)
      }
      else if(deep_state == "GET_DOWN"){
        get_down <- vct
      }
      #print(deep_state)
      #print(vct)

      deep_count <- 0
      #deep_state <- ""
      #prev_count <- 0

      cur_count <- 1
      prev_count <- 1
      row_count <- 1
      cur_state <- DATA[i,]$state

    }
    cur_state <- DATA[i,]$state
    row_count <- 1
  }
  points(i, DATA[i,]$Head.y, col = color)
}


