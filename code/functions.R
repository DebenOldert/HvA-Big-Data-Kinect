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
