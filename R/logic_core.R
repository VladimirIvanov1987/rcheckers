# Основная логика доски и хелперы ====

#' Initialize Game Board
#'
#' Creates a standard 8x8 Russian Checkers board.
#' 0=Empty, 1=White Man, 2=Black Man, 3=White King, 4=Black King.
#' Top of matrix (row 1) is Black side, Bottom (row 8) is White side.
#'
#' @return A matrix 8x8.
#' @export
init_board <- function() {
  board <- matrix(0, nrow = 8, ncol = 8)

  # Black starts at rows 1, 2, 3
  for (row in 1:3) {
    for (col in 1:8) {
      if ((row + col) %% 2 != 0) board[row, col] <- 2
    }
  }

  # White starts at rows 6, 7, 8
  for (row in 6:8) {
    for (col in 1:8) {
      if ((row + col) %% 2 != 0) board[row, col] <- 1
    }
  }
  return(board)
}

#' Check if coordinate is on board
#'
#' @param r Row index
#' @param c Column index
#' @return logical
#' @noRd
is_on_board <- function(r, c) {
  r >= 1 && r <= 8 && c >= 1 && c <= 8
}

#' Get piece owner
#'
#' @param piece Integer representation of piece
#' @return 0 for empty, 1 for White (1,3), 2 for Black (2,4)
#' @noRd
get_piece_owner <- function(piece) {
  if (piece == 0) return(0)
  if (piece == 1 || piece == 3) return(1) # White
  return(2) # Black
}

#' Get opponent color
#'
#' @param player 1 or 2
#' @return Integer (1 or 2)
#' @export
get_opponent <- function(player) {
  if (player == 1) return(2) else return(1)
}

# --- Манипуляции с состоянием (Append to R/logic_core.R) ----

#' Apply Move to Board
#'
#' Executes a move, removes captured pieces, and handles promotion.
#'
#' @param board Current matrix
#' @param move A list structure (from get_legal_moves) containing from, to, and captures
#'
#' @return Updated board matrix
#' @export
apply_move <- function(board, move) {
  # Координаты
  r_start <- move$from[1]
  c_start <- move$from[2]
  r_end <- move$to[1]
  c_end <- move$to[2]

  piece <- board[r_start, c_start]

  # 1. Снимаем фигуру со старого места
  board[r_start, c_start] <- 0

  # 2. Удаляем сбитые фигуры (если есть)
  if (!is.null(move$captures) && length(move$captures) > 0) {
    for (cap in move$captures) {
      board[cap[1], cap[2]] <- 0
    }
  }

  # 3. Проверка на превращение в Дамку (Promotion)
  # Если простая шашка достигла последней горизонтали
  is_white_man <- (piece == 1)
  is_black_man <- (piece == 2)

  if (is_white_man && r_end == 1) {
    piece <- 3 # White King
  } else if (is_black_man && r_end == 8) {
    piece <- 4 # Black King
  } else if (!is.null(move$is_king)) {
    if ((move$is_king==3 || move$is_king==4) && piece < 3) {
      # Шашка не остановилась на поле превращения, а продолжила взятие как дамка
      piece <- move$is_king
    }
  }

  # 4. Ставим фигуру на новое место
  board[r_end, c_end] <- piece

  return(board)
}

#' Check Game State
#'
#' Determines if the game is over based on the *next* player's available moves.
#' If the next player has no moves, they lose.
#'
#' @param board Current matrix
#' @param next_player The player who is about to move (1 or 2)
#'
#' @return Character: "active", "white_won", "black_won"
#' @export
check_game_state <- function(board, next_player) {
  # Проверяем, остались ли вообще фигуры у игрока
  # (Оптимизация: можно не искать ходы, если фигур нет)
  player_pieces <- which(get_piece_owner(board) == next_player)
  if (length(player_pieces) == 0) {
    return(ifelse(next_player == 1, "black_won", "white_won"))
  }

  # Проверяем наличие легальных ходов
  moves <- get_legal_moves(board, next_player)

  if (length(moves) == 0) {
    # У игрока нет ходов -> он проиграл
    return(ifelse(next_player == 1, "black_won", "white_won"))
  }

  return("active")
}

#' Vectorized Helper to get owner of all cells (Internal)
#' @noRd
get_piece_owner <- function(x) {
  # 0->0, 1->1, 2->2, 3->1, 4->2
  ifelse(x == 0, 0, ifelse(x == 1 | x == 3, 1, 2))
}

