#' Check for Draw Conditions
#'
#' Checks standard Russian Draughts draw rules based on game history and board state.
#'
#' @param game The game object containing board, turn, and counters
#' @return list(is_draw = logical, reason = character)
#' @export
check_draw_conditions <- function(game) {

  board <- game$board

  # 1. Троекратное повторение позиции
  # Мы создаем "хэш" позиции (расположение + чей ход)
  current_hash <- paste(c(as.vector(board), game$turn), collapse = "")
  # Считаем, сколько раз такой хэш уже встречался
  repetitions <- sum(unlist(game$position_history) == current_hash)

  if (repetitions >= 2) { # Если это уже 3-й раз (2 в истории + 1 сейчас)
    return(list(is_draw = TRUE, reason = "Threefold repetition"))
  }

  # --- Анализ материала ---
  w_men <- sum(board == 1)
  w_kings <- sum(board == 3)
  b_men <- sum(board == 2)
  b_kings <- sum(board == 4)

  w_total <- w_men + w_kings
  b_total <- b_men + b_kings

  # Счетчик "тихих" ходов (без взятия и движения пешек)
  # Этот счетчик ты должен обновлять в логике совершения хода!
  moves_cnt <- game$moves_quiet

  # 2. Правило 15 ходов (Дамки только)
  # Если игроки делают ходы только дамками 15 ходов подряд
  if (moves_cnt >= 15) {
    return(list(is_draw = TRUE, reason = "15 moves rule (Kings only)"))
  }

  # 3. Три дамки против одной (и другие соотношения)
  # упрощенный вариант:
  # Если у одной стороны 3+ фигуры (из них есть дамки), а у другой 1 дамка -> 15 ходов

  has_advantage_white <- (w_total >= 3 && w_kings > 0 && b_total == 1 && b_kings == 1)
  has_advantage_black <- (b_total >= 3 && b_kings > 0 && w_total == 1 && w_kings == 1)

  if (has_advantage_white || has_advantage_black) {
    # Здесь по правилам нужно проверять "Большую дорогу", но для начала хватит счетчика
    if (moves_cnt >= 15) {
      return(list(is_draw = TRUE, reason = "15 moves rule (3+ vs 1)"))
    }
  }

  # 4. Правила 30 и 60 ходов для эндшпилей
  # 4-5 фигур: 30 ходов, 6-7 фигур: 60 ходов
  total_pieces <- w_total + b_total

  limit <- 0
  if (total_pieces <= 3 && w_kings > 0 && b_kings > 0) limit <- 5
  else if (total_pieces >= 4 && total_pieces <= 5) limit <- 30
  else if (total_pieces >= 6 && total_pieces <= 7) limit <- 60

  if (limit > 0 && moves_cnt >= limit) {
    return(list(is_draw = TRUE, reason = paste(limit, "moves rule")))
  }

  return(list(is_draw = FALSE, reason = ""))
}
