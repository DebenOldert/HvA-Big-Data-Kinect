# FUNCTION FILE
#
# THIS FILE CONTAINS ALL THE FUNCTIONS THAT MIGTH BE USED GLOBALLY


# specify_decimal. Set how many decimals an number should have
#   x => numeric
#   k => integer
#
# Returns => numeric with {k} decimals
specify_decimal <- function(x, k) format(round(x, k), nsmall=k)

# consistent. Check if a vactor contains all values between the start and the end
#   a => vector containing integers
#
# Returns => bool, true contains all values/ false not
consistent <- function(a) suppressWarnings(all(a == min(a):max(a)))

# group. Makes groups of consistent integer vectors
#   a => vector containing integers
#   n => integer defining minimum group size
#
# Returns => list, containing consistent integer vectors
group <- function(a, n=1){
  if(consistent(a)) return (list(A=a))
  else{
    l_ <- list()
    cur_ <- 1
    for(i in a){
      if(is.null(l_$A)){ # Create first group
        l_$A <- c(i)
        next
      }

      if(max(l_[[cur_]])+1 == i) l_[[cur_]] <- c(l_[[cur_]], i) # Add to group
      else{ # Create new group
        cur_ <- cur_ + 1
        l_[[LETTERS[cur_]]] <- c(i)
      }
    }
    cur_ <- 1
    while(cur_ <= length(l_)){
      if(length(l_[[cur_]]) < n){
        l_[[cur_]] <- NULL
        cur_ <- 0
      }
      cur_ <- cur_ + 1
    }
    names(l_) <- LETTERS[1:length(l_)]
    remove(cur_)
    return(l_)
  }
}
