#' Test Rotative Panel 5-0-0 for Contiguity and Overlap
#'
#' This function performs various tests to ensure that the rotative panel 5-0-0 meets specific properties:
#' - Two contiguous periods should have an 80% overlap.
#' - After two periods, there should be a 60% overlap.
#' - After three periods, there should be a 40% overlap.
#' - After four periods, there should be a 20% overlap.
#'
#' @param block Matrix. The block of the rotative panel to be tested.
#'
#' @return A list containing:
#' \item{Panel}{The input panel block.}
#' \item{Rows_CommonElements}{Matrix of common elements between rows.}
#' \item{commonRows}{Number of common elements between contiguous rows.}
#' \item{prop_commonRows}{Proportion of common elements between contiguous rows.}
#' \item{test_commonRows}{Percentage of rows with exactly 80% overlap between contiguous rows.}
#' \item{commonRowsEach2}{Number of common elements between rows with a gap of two periods.}
#' \item{prop_commonRowsEach2}{Proportion of common elements between rows with a gap of two periods.}
#' \item{test_commonRowsEach2}{Percentage of rows with exactly 60% overlap between rows with a gap of two periods.}
#' \item{commonRowsEach3}{Number of common elements between rows with a gap of three periods.}
#' \item{prop_commonRowsEach3}{Proportion of common elements between rows with a gap of three periods.}
#' \item{test_commonRowsEach3}{Percentage of rows with exactly 40% overlap between rows with a gap of three periods.}
#' \item{commonRowsEach4}{Number of common elements between rows with a gap of four periods.}
#' \item{prop_commonRowsEach4}{Proportion of common elements between rows with a gap of four periods.}
#' \item{test_commonRowsEach4}{Percentage of rows with exactly 20% overlap between rows with a gap of four periods.}
#' \item{globalTest}{The average of all the individual overlap tests.}
#'
#' @examples
#' # Example usage:
#' block <- matrix(c(1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 1, 2, 3, 4, 5), ncol = 5, byrow = TRUE)
#' test_contigous500(block)
#'
#' @export
 

utils_test_contigous500 <- function(block){
  num_Panels <- ncol(block) 
  common_elements_matrix <- utils_create_common_elements_matrix(block)
  
  contiguous <- NA
  for(i in 1:(nrow(common_elements_matrix) - 1)){
    contiguous[i] <- common_elements_matrix[i,i+1] 
    names(contiguous)[i] <- paste0(i, "-", i+1)
  }
  
  prop_contiguous <- contiguous / num_Panels
  indica_contiguous <- 100 * sum(prop_contiguous == 0.8) / length(contiguous)
  
  contiguousEach2 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 2)){
    contiguousEach2[i] <- common_elements_matrix[i,i+2] # typo Each4
    names(contiguousEach2)[i] <- paste0(i, "-", i + 2)
    
  }
  
  prop_contiguousEach2 <- contiguousEach2 / num_Panels
  indica_contiguousEach2 <- 100 * sum(prop_contiguousEach2 == 0.6) / length(prop_contiguousEach2)
  
  
  contiguousEach3 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 3)){
    contiguousEach3[i] <- common_elements_matrix[i,i+3] # typo Each4
    names(contiguousEach3)[i] <- paste0(i, "-", i + 3)
  }
  
  prop_contiguousEach3 <- contiguousEach3 / num_Panels
  indica_contiguousEach3 <- 100 * sum(prop_contiguousEach3 == 0.4) / length(contiguousEach3)
  
  
  contiguousEach4 <- NA
  for(i in 1:(nrow(common_elements_matrix) - 4)){
    contiguousEach4[i] <- common_elements_matrix[i,i+4] # typo Each4
    names(contiguousEach4)[i] <- paste0(i, "-", i + 4)
  }
  
  prop_contiguousEach4 <- contiguousEach4 / num_Panels
  indica_contiguousEach4 <- 100 * sum(prop_contiguousEach4 == 0.2) / length(contiguousEach4)
  
  
  outcome <- list(block, common_elements_matrix, 
                  contiguous, prop_contiguous, indica_contiguous,
                  contiguousEach2, prop_contiguousEach2, indica_contiguousEach2,
                  contiguousEach3, prop_contiguousEach3, indica_contiguousEach3,
                  contiguousEach4, prop_contiguousEach4, indica_contiguousEach4,
                  (indica_contiguous +  indica_contiguousEach2 + 
                     indica_contiguousEach3 + indica_contiguousEach4) / 4)
  
  
  
  names(outcome) <- c("Panel", "Rows_CommonElements", 
                      "commonRows", "prop_commonRows", "test_commonRows",
                      "commonRowsEach2", "prop_commonRowsEach2", "test_commonRowsEach2",
                      "commonRowsEach3", "prop_commonRowsEach3", "test_commonRowsEach3",
                      "commonRowsEach4", "prop_commonRowsEach4", "test_commonRowsEach4",
                      "globalTest")
  
  outcome  
  
}
