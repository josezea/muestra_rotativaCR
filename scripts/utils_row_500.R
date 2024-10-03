#' Generate a Column for a Rotating Panel Block 5-0-0
#'
#' This function generates a column of a block in a rotating panel using the 5-0-0 scheme.
#'
#' @param n_periods Integer. The number of periods (e.g., quarters or months).
#' @param vctr_rep1stLetterCols Integer vector. A vector of repetitions of the first letter of each column.
#' @param str_blockInitLetter String. The initial letter of the block.
#' 
#' @return A character vector representing the block with initialized values and updated based on the logic provided.
#' 
#' @examples
#' # Example usage:
#' row_500(10, c(1, 2, 3, 4, 5), "A")
#' 
#' @export

utils_row_500 <- function(n_periods, vctr_rep1stLetterCols, str_blockInitLetter){
  initial_block <- rep(1, 5 - vctr_rep1stLetterCols + 1)
  
  if(length(initial_block) == 4) initial_block <- c(initial_block, 2)
  if(length(initial_block) == 3) initial_block <- c(initial_block, 2, 2)
  if(length(initial_block) == 2) initial_block <- c(initial_block, 2, 2, 2)
  if(length(initial_block) == 1) initial_block <- c(initial_block, 2, 2, 2, 2)
  
  vctr_block <- rep(NA_integer_, n_periods - length(initial_block))  
  vctr_block[1:5] <- initial_block
  
  for(i in 6:n_periods){
    if((vctr_block[i-1] == vctr_block[i-2]) & (vctr_block[i-2] == vctr_block[i-3]) &
       (vctr_block[i-3] == vctr_block[i-4]) & (vctr_block[i-4] == vctr_block[i-5])){
      vctr_block[i] <- vctr_block[i-1] + 1  
    }
    if((vctr_block[i-1] == (vctr_block[i-2] + 1)) & 
       ((vctr_block[i-2] + 1) == (vctr_block[i-3] + 1)) &
       ((vctr_block[i-3] + 1) ==  (vctr_block[i-4] + 1)) &
       ((vctr_block[i-4] + 1) == (vctr_block[i-5] + 1))){
      vctr_block[i] <- vctr_block[i-1]   
    }
    
    if((vctr_block[i-1] == vctr_block[i-2] ) &
       (vctr_block[i-2] == (vctr_block[i-3] + 1)) &
       ((vctr_block[i-3] + 1) ==  (vctr_block[i-4] + 1)) &
       ((vctr_block[i-4] + 1) == (vctr_block[i-5] + 1))){
      vctr_block[i] <- vctr_block[i-1]   
    }
    
    if((vctr_block[i-1] == vctr_block[i-2] ) &
       (vctr_block[i-2] == vctr_block[i-3]) &
       (vctr_block[i-3] ==  (vctr_block[i-4] + 1)) &
       ((vctr_block[i-4] + 1) == (vctr_block[i-5] + 1))){
      vctr_block[i] <- vctr_block[i-1]   
    }
    
    if((vctr_block[i-1] == vctr_block[i-2] ) &
       (vctr_block[i-2] == vctr_block[i-3]) &
       (vctr_block[i-3] ==  vctr_block[i-4]) &
       (vctr_block[i-4]== (vctr_block[i-5] + 1))){
      vctr_block[i] <- vctr_block[i-1]   
    }
  }
  vctr_block <- paste0(str_blockInitLetter, vctr_block)
  vctr_block
}
