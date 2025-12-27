#' Get Best Move for AI (Heuristic Version)
#'
#' Strategy:
#' 1. Mandatory Max Capture (Greedy).
#' 2. Heuristics for quiet moves:
#'    - Promote to King (+Score)
#'    - Control Center (+Score)
#'    - Keep Back Rank (+Score)
#'    - Random noise (to vary gameplay)
#'
#' @param board Current board
#' @param player AI player color (usually 2 - White in code logic usually moves "down" or depends on setup)
#' @export
get_ai_move <- function(board, player) {
  moves <- get_legal_moves(board, player)

  if (length(moves) == 0) return(NULL)

  # 1. Анализ взятий
  capture_counts <- sapply(moves, function(m) length(m$captures))
  max_capture <- max(capture_counts)

  # Если есть взятия, фильтруем только их (Жадная стратегия)
  if (max_capture > 0) {
    # Оставляем только те ходы, где рубим максимум шашек
    # (Можно ослабить это условие, если хочешь разрешить рубить меньше)
    candidates <- moves[capture_counts == max_capture]

    # Если вариантов взятия несколько, выбираем тот, который ведет в дамки
    # или просто случайный
    # Тут можно вызвать ту же эвристику, что ниже, но пока оставим sample
    return(candidates[[sample(length(candidates), 1)]])
  }

  # 2. Тихие ходы: Эвристическая оценка
  # Предположим, что AI (player 2) играет белыми и идет ВВЕРХ (к строке 8)
  # или черными и идет ВНИЗ (к строке 1).
  # Нужно знать направление. Обычно player 1 (белые) идут 1->8, player 2 (черные) 8->1.
  # Давай предположим для универсальности, что цель AI - противоположный край.

  # Определяем целевую строку для дамок
  target_row <- if (player == 1) 8 else 1
  home_row   <- if (player == 1) 1 else 8

  scores <- sapply(moves, function(m) {
    score <- 0
    r_to <- m$to[1]
    c_to <- m$to[2]
    r_from <- m$from[1]

    # Фактор 1: Идем в дамки
    if (r_to == target_row) {
      score <- score + 50
    }

    # Фактор 2: Контроль центра (строки 3-6, колонки 3-6)
    # Центр стратегически важен
    if (r_to >= 3 && r_to <= 6 && c_to >= 3 && c_to <= 6) {
      score <- score + 10
    }

    # Фактор 3: Защита тыла (не уводить шашки с первой линии без нужды)
    if (r_from == home_row) {
      score <- score - 15  # Штраф за вскрытие тыла
    }

    # Фактор 4: Случайный шум (чтобы игры были разными)
    # runif добавляет от 0 до 5 очков, что может перевесить слабые факторы
    score <- score + runif(1, 0, 5)

    return(score)
  })

  # Выбираем ход с максимальным баллом
  best_score <- max(scores)
  best_moves <- moves[scores == best_score] # Их может быть несколько с одинаковым макс. баллом

  # Если все равно несколько одинаково хороших, берем любой
  selected_index <- sample(length(best_moves), 1)

  return(best_moves[[selected_index]])
}
