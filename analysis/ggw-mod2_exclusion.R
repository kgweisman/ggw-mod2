# BROKEN!!

# examine exclusions by study

excludedCounts <- function(datasource) {
  
  # set datasource
  if(datasource == "study 1"){
    d <- d1
    d_raw <- d_raw_study1
  }
  if(datasource == "study 2"){
    d <- d2
    d_raw <- d_raw_study2
  }
  if(datasource == "study 3"){
    d <- d3
    d_raw <- d_raw_study3
  }
  if(datasource == "study 4"){
    d <- d4
    d_raw <- d_raw_study4
  }
  
  # get subids of successful participants
  d_subids <- levels(factor(as.character(d$subid)))
  
  # get subids of excluded participants
  d_excluded <- d_raw %>%
    filter(is.element(subid, d_subids) == FALSE) %>%
    select(condition, subid, finished, CATCH, yob)
  
  # count excluded participants
  d_excluded_n <- length(d_excluded$subid)
  
  if(datasource %in% c("study 1", "study 2", "study 4")) {
    # count participants who did not finish
    d_excluded_unfinished <- d_excluded %>%
      filter(is.na(CATCH) == T,
             finished != 1) %>%
      select(subid) %>%
      c()
    
    # count participants who finished, but failed catch trial
    d_excluded_CATCH <- d_excluded %>%
      filter(is.element(subid, d_excluded_unfinished$subid) == FALSE) %>%
      filter(CATCH != 1) %>%
      select(subid) %>%
      c()
  }
  
  if(datasource == "study 3") {
    # count participants who did not finish
    d_excluded_unfinished <- d_excluded %>%
      filter(is.na(CATCH..characterL) == T,
             is.na(CATCH..characterR) == T,
             finished != 1) %>%
      select(subid) %>%
      c()
    
    # count participants who finished, but failed catch trial
    d_excluded_CATCH <- d_excluded %>%
      filter(is.element(subid, d_excluded_unfinished$subid) == FALSE) %>%
      filter(CATCH..characterL != 5,
             CATCH..characterR != 5) %>%
      select(subid) %>%
      c()
  }
  
  # count participants who finished and passed catch trial, 
  # but did not provide year of birth
  d_excluded_yob <- d_excluded %>%
    filter(is.element(subid, d_excluded_unfinished$subid) == FALSE,
           is.element(subid, d_excluded_CATCH$subid) == FALSE) %>%
    filter(nchar(as.character(yob)) != 4) %>%
    select(subid) %>%
    c()
  
  # sum up excluded participants by category
  total <- sum(length(d_excluded_unfinished$subid),
               length(d_excluded_CATCH$subid),
               length(d_excluded_yob$subid))
  
  if(total == d_excluded_n) {
    stop("Error: 3 sources of exclusion do not add up to total.")
  } else {
    
    # return counts
    excluded_counts <- 
      data.frame("did_not_finish" = length(d_excluded_unfinished$subid),
                 "failed_catch_trial" = length(d_excluded_CATCH$subid),
                 "did_not_provide_yob" = length(d_excluded_yob$subid),
                 "TOTAL" = total)
    return(excluded_counts)
  }

}

excludedCounts("study 1")
