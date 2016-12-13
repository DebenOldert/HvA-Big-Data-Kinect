# PATIENT CLASS

patient <- new.env()

  patient$WALKING <- NULL

  patient$SITTING <- NULL

  patient$UP <- NULL

  patient$DOWN <- NULL

  patient$WALKBASE <- NULL

  patient$SITBASE <- NULL

  patient$WALKERROR <- NULL


  patient$height <- function() abs(patient$SITBASE) + patient$WALKBASE
