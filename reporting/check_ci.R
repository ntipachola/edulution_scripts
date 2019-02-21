# A little function to get n number of characters from the right of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Change centre_id to CI[].0 depending on group suffix
check_ci <- function(centreid, group){
	# list of CI centres
  ci_centres <- c("CIC","CIK","CIG")
  # Suffixes of groups which are in-centre
  centre_groups <- c("2a","2b","4a","4b")
  # get last 2 letters from group name
  group <- substrRight(group,2)
  # If centre_id is a ci_centre and the group is not an in-centre group - must be 0
  if (any(centreid == ci_centres)&!(any(group == centre_groups))){
    centreid <- paste(centreid,".O",sep = "")
  }
  return(centreid)
}