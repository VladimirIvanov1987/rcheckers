#' Check for Draw Conditions
#'
#' Checks standard Russian Draughts draw rules based on game history and board state.
#'
#' @param game The game object containing board, turn, and counters
#' @param lang Language code for the output reason (e.g., "en", "ru")
#' @return list(is_draw = logical, reason = character)
#' @export
check_draw_conditions <- function(game, lang = "en") {
  board <- game$board

  # 1. Троекратное повторение позиции
  current_hash <- paste(c(as.vector(board), game$turn), collapse = "")
  repetitions <- sum(unlist(game$position_history) == current_hash)

  if (repetitions >= 2) {
    return(list(is_draw = TRUE, reason = tr("draw_repetition", lang)))
  }

  # --- Анализ материала ---
  w_men <- sum(board == 1)
  w_kings <- sum(board == 3)
  b_men <- sum(board == 2)
  b_kings <- sum(board == 4)

  w_total <- w_men + w_kings
  b_total <- b_men + b_kings

  moves_cnt <- game$moves_quiet

  # 2. Правило 15 ходов (Дамки только)
  if (moves_cnt >= 15) {
    return(list(is_draw = TRUE, reason = tr("draw_15_kings", lang)))
  }

  # 3. Три дамки против одной (и другие соотношения)
  has_advantage_white <- (w_total >= 3 && w_kings > 0 && b_total == 1 && b_kings == 1)
  has_advantage_black <- (b_total >= 3 && b_kings > 0 && w_total == 1 && w_kings == 1)

  if (has_advantage_white || has_advantage_black) {
    if (moves_cnt >= 15) {
      return(list(is_draw = TRUE, reason = tr("draw_15_adv", lang)))
    }
  }

  # 4. Правила 30 и 60 ходов для эндшпилей
  total_pieces <- w_total + b_total

  limit <- 0
  if (total_pieces <= 3 && w_kings > 0 && b_kings > 0) {
    limit <- 5
  } else if (total_pieces >= 4 && total_pieces <= 5) {
    limit <- 30
  } else if (total_pieces >= 6 && total_pieces <= 7) limit <- 60

  if (limit > 0 && moves_cnt >= limit) {
    # Формируем строку вида "30 ходов без взятия"
    reason_text <- paste(limit, tr("draw_moves_limit", lang))
    return(list(is_draw = TRUE, reason = reason_text))
  }

  return(list(is_draw = FALSE, reason = ""))
}
