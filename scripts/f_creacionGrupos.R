f_creacionGrupos <- function(vec_length, n){
  
  # Calculate how many complete groups of size n can be formed
  group_count <- ceiling(vec_length / n)
  
  # Generate the group assignment, without handling the last group yet
  groups <- rep(1:(group_count - 1), each = n)
  remaining <- vec_length - length(groups)
  
  # If the last group is smaller than n, combine it with the penultimate group
  if (remaining < n) {
    groups <- c(groups, rep(group_count - 1, remaining))
  } else {
    # Otherwise, create a last group as usual
    groups <- c(groups, rep(group_count, remaining))
  }
  
  # Show the result
  groups
}

# f_creacionGrupos(12, 5)