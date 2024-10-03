#' Generate a 5-0-0 Rotative Panel for Monthly or Quarterly Periods
#'
#' This function generates a 5-0-0 rotative panel for either monthly or quarterly periods.
#'
#' @param n_periods Integer. The number of quarters or months.
#' @param name_period Character of length 1. It must take one of two possible values: "month" or "quarter". Default is "month".
#'
#' @return A data frame representing the 5-0-0 rotative panel. If `name_period` is "month", the data frame will have three sets of columns for each month in a quarter. If `name_period` is "quarter", the data frame will have one set of columns representing the quarters.
#'
#' @examples
#' # Example usage:
#' panel500(40, "month")
#' panel500(40, "quarter")
#'
#' @export

panel500 <- function(n_periods, name_period = "month"){
  if (!all(name_period %in% c("month", "quarter"))) {
    stop("Error: my_vector must take the possible values: 'month' or 'quarter'.")
  }
  vctr_rep1stLetterCols = 1:5
  if(!identical(sort(as.integer(unique(vctr_rep1stLetterCols))), 1:5)) stop("El vector inicia_primeraVisita  debe ser cualquier permutación del vector 1, 2, 3, 4, 5 en cualquier orden, por ejemplo: 3, 5, 1, 3, 2")
  
  
  block1 <- utils_minimal_block500(n_periods =  n_periods, vctr_rep1stLetterCols = vctr_rep1stLetterCols, 
                             str_blockInitLetter = "A")
  
  if(name_period == "month"){
    block2 <- utils_minimal_block500(n_periods =  n_periods, 
                               vctr_rep1stLetterCols = vctr_rep1stLetterCols, 
                               str_blockInitLetter = "F")
    block3 <- utils_minimal_block500(n_periods =  n_periods, 
                               vctr_rep1stLetterCols = vctr_rep1stLetterCols, 
                               str_blockInitLetter = "K")
    
    block <- as.data.frame(cbind(block1, block2, block3))
    row.names(block) <- paste0("quarter", 1:n_periods)
    colnames(block) <- c(rep("month1", 5), rep("month2", 5), rep("month3", 5))  
  }
  
  if(name_period == "quarter"){
    block <- block1
    row.names(block) <- paste0("quarter", 1:n_periods)
    colnames(block) <- c("col1", "col2", "col3", "col4", "col5")  
  }
  block
}

