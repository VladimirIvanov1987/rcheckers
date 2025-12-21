# Логика Искусственного Интеллекта ====

#' Get Best Move for AI
#'
#' Currently implements a simple strategy:
#' 1. Must capture if available.
#' 2. If multiple captures, picks the one removing most pieces.
#' 3. If quiet move, picks randomly (can be improved later).
#'
#' @param board Current board
#' @param player AI player color (usually 2)
#'
#' @return A move object (list) or NULL if no moves
#' @export
get_ai_move <- function(board, player) {
  moves <- get_legal_moves(board, player)

  if (length(moves) == 0) return(NULL)

  # Если ходов несколько, нужно выбрать лучший

  # Стратегия 1: Приоритет взятия (длина списка captures)
  capture_counts <- sapply(moves, function(m) length(m$captures))
  max_capture <- max(capture_counts)

  # Оставляем только ходы с максимальным взятием
  best_moves <- moves[capture_counts == max_capture]

  # Стратегия 2 (пока Random): Из оставшихся выбираем случайный
  # В будущем здесь можно подключить Minimax
  selected_index <- sample(length(best_moves), 1)

  return(best_moves[[selected_index]])
}
