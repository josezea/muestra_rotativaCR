#' Generate a Minimal Block for a Rotating Panel 5-0-0
#'
#' This function generates a minimal block for a rotating panel using the 5-0-0 scheme.
#'
#' @param n_periods Integer. The number of periods (e.g., quarters or months). Must be at least 6.
#' @param vctr_rep1stLetterCols Integer vector. A vector of repetitions of the first letter of each column. Must be a permutation of the vector 1, 2, 3, 4, 5.
#' @param str_blockInitLetter String. The initial letter of the block. Must be a single letter from A to Z. Defaults to "A".
#' 
#' @return A matrix with 5 columns, each representing a row of the rotating panel block.
#' 
#' @examples
#' # Example usage:
#' minimal_block500(10, c(1, 2, 3, 4, 5), "B")
#' minimal_block500(12, c(3, 5, 1, 3, 2))
#' 
#' @export

utils_minimal_block500 <- function(n_periods, vctr_rep1stLetterCols, 
                             str_blockInitLetter = "A"){
  if(n_periods < 6) stop("Cómo mínimo debe considerar 6 n_periods")
  if(!(is.integer(vctr_rep1stLetterCols) | is.numeric(vctr_rep1stLetterCols))) stop("The vector vctr_rep1stLetterCols must be any permutation of the vector 1, 2, 3, 4, 5 in any order, for example: 3, 5, 1, 3, 2")
  if(!identical(sort(as.integer(unique(vctr_rep1stLetterCols))), 1:5)) stop("The vector vctr_rep1stLetterCols must be any permutation of the vector 1, 2, 3, 4, 5 in any order, for example: 3, 5, 1, 3, 2")
  if(!(toupper(str_blockInitLetter) %in% LETTERS) | !is.character(str_blockInitLetter)) stop ("Choose a letter from A to Z to initialize the block")
  str_blockInitLetter <- toupper(str_blockInitLetter)
  Letters <- rep(NA_character_, 5)
  Letters[1] <- str_blockInitLetter
  posinitial_letter <- which(LETTERS == str_blockInitLetter)
  Letters[2:5] <- LETTERS[(posinitial_letter + 1):(posinitial_letter + 4)] 
  out <- cbind(
    utils_row_500(n_periods, vctr_rep1stLetterCols[1], Letters[1]), 
    utils_row_500(n_periods, vctr_rep1stLetterCols[2], Letters[2]),
    utils_row_500(n_periods, vctr_rep1stLetterCols[3], Letters[3]),
    utils_row_500(n_periods, vctr_rep1stLetterCols[4], Letters[4]),
    utils_row_500(n_periods, vctr_rep1stLetterCols[5], Letters[5]))
  out
}
