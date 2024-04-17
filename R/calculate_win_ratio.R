#' calculate_win_ratio
#'
#' @param wins Number of wins
#' @param losses Number of losses
#'
#' @return A float
#' @export
#'
#' @examples
calculate_win_ratio <- function(wins, losses) {
  if (wins + losses == 0) {
    return(NA)  # Avoid division by zero
  }

  win_ratio <- wins / (wins + losses)
  return(win_ratio)
}

