#' Flag columns with missing values
#'
#' @param numberMissing a labelled vector reporting number of missing values for each column
#' @param threshold above this number of missing values warning is given. Defaults to 0.
flagMissing <- function(numberMissing, threshold = 0){
  
  # Get column names from labelled vector
  columnNames <- names(numberMissing)
  
  # Examine number of missing values for each column
  for(columnName in columnNames){
    
    # Throw warning when number of missing values exceed threshold
    if(numberMissing[columnName] > threshold){
      warning(numberMissing[columnName], " missing values found in ", columnName, " column.")
    }
  }
}