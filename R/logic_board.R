# Логика доски ====

#' Initialize Game Board
#'
#' Creates a standard 8x8 Russian Checkers board.
#' 1=White, 2=Black, 3=White King, 4=Black King.
#'
#' @return A matrix 8x8.
#' @export
init_board <- function() {
  board <- matrix(0, nrow = 8, ncol = 8)

  # Расстановка черных (строки 1-3)
  for (row in 1:3) {
    for (col in 1:8) {
      if ((row + col) %% 2 != 0) { # Темные клетки
        board[row, col] <- 2
      }
    }
  }

  # Расстановка белых (строки 6-8)
  for (row in 6:8) {
    for (col in 1:8) {
      if ((row + col) %% 2 != 0) {
        board[row, col] <- 1
      }
    }
  }

  return(board)
}
