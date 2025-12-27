# Логика генерации ходов (R/logic_moves.R) ====

#' Get All Legal Moves
#'
#' Entry point for move generation. Enforces mandatory capture.
#'
#' @param board Matrix 8x8
#' @param player 1 (White) or 2 (Black)
#'
#' @return List of moves.
#' @export
get_legal_moves <- function(board, player) {
  # 1. Ищем взятия
  capture_moves <- get_all_capture_moves(board, player)

  # 2. Правило обязательного битья: если есть взятия, возвращаем только их
  if (length(capture_moves) > 0) {
    return(capture_moves)
  }

  # 3. Иначе ищем тихие ходы
  return(get_all_quiet_moves(board, player))
}

# --- Тихие ходы (Quiet Moves) ---

#' @noRd
get_all_quiet_moves <- function(board, player) {
  moves <- list()
  forward <- if (player == 1) -1 else 1 # Белые вверх (-1), Черные вниз (+1)

  for (r in 1:8) {
    for (c in 1:8) {
      piece <- board[r, c]
      if (get_piece_owner(piece) != player) next

      is_king <- (piece == 3 || piece == 4)
      directions <- if (is_king) list(c(-1,-1), c(-1,1), c(1,-1), c(1,1)) else list(c(forward, -1), c(forward, 1))

      if (is_king) {
        # Логика дамки: скольжение
        for (dir in directions) {
          for (dist in 1:7) {
            nr <- r + dir[1] * dist
            nc <- c + dir[2] * dist
            if (!is_on_board(nr, nc)) break
            if (board[nr, nc] != 0) break # Уперлись

            moves[[length(moves) + 1]] <- list(from = c(r, c), to = c(nr, nc), captures = list())
          }
        }
      } else {
        # Логика простой: 1 шаг
        for (dir in directions) {
          nr <- r + dir[1]
          nc <- c + dir[2]
          if (is_on_board(nr, nc) && board[nr, nc] == 0) {
            moves[[length(moves) + 1]] <- list(from = c(r, c), to = c(nr, nc), captures = list())
          }
        }
      }
    }
  }
  return(moves)
}

# --- Взятия (Capture Moves - Recursive) ---

#' @noRd
get_all_capture_moves <- function(board, player) {
  all_captures <- list()

  for (r in 1:8) {
    for (c in 1:8) {
      piece <- board[r, c]
      if (get_piece_owner(piece) != player) next

      # Запускаем рекурсивный поиск для каждой фигуры
      # Передаем пустой список captured (сбитые в этой цепочке)
      chains <- find_capture_chains(board, r, c, piece, player, list())

      if (length(chains) > 0) {
        all_captures <- c(all_captures, chains)
      }
    }
  }

  return(all_captures)
}

#' Recursive finder for capture chains
#'
#' @param board Current board state (virtual)
#' @param r Current row
#' @param c Current col
#' @param piece Current piece type (may change on promotion)
#' @param player Player color
#' @param captured_pos List of vectors c(r,c) of pieces already captured in this sequence
#' @param start_pos Where the move started (to form the final object)
#'
#' @return List of valid moves
#' @noRd
# find_capture_chains <- function(board, r, c, piece, player, captured_pos,
#                                 start_pos = NULL) {
#
#   if (is.null(start_pos)) start_pos <- c(r, c)
#
#   is_king <- (piece == 3 || piece == 4)
#   opponent <- get_opponent(player)
#
#   moves_found <- list()
#   can_continue <- FALSE
#
#   directions <- list(c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))
#
#   for (dir in directions) {
#     dr <- dir[1]
#     dc <- dir[2]
#
#     # --- ЛОГИКА ДЛЯ ПРОСТОЙ ШАШКИ (Бьет во все стороны) ---
#     if (!is_king) {
#       # Враг
#       mr <- r + dr
#       mc <- c + dc
#
#       # Место приземления
#       nr <- r + 2 * dr
#       nc <- c + 2 * dc
#
#       if (is_on_board(nr, nc)) {
#         mid_piece <- board[mr, mc]
#         dest_piece <- board[nr, nc]
#
#         # Проверяем: в середине враг?
#         is_enemy <- (get_piece_owner(mid_piece) == opponent)
#
#         # Проверяем: не сбили ли мы его уже в этом ходе?
#         already_captured <- FALSE
#         for (cp in captured_pos) {
#           if (cp[1] == mr && cp[2] == mc) already_captured <- TRUE
#         }
#
#         # Проверяем: место приземления свободно? (Или это то место, откуда мы начали этот шаг, но это редкость)
#         is_dest_empty <- (dest_piece == 0) || (nr == start_pos[1] && nc == start_pos[2]) # Упрощение, обычно 0
#
#         if (is_enemy && !already_captured && dest_piece == 0) {
#           can_continue <- TRUE
#
#           # Логика превращения в дамку "на лету"
#           new_piece <- piece
#           # Белые достигают 1-й строки, Черные 8-й (инверсия координат в матрице может путать, проверим init_board)
#           # В init_board: Черные (2) сверху (1-3), Белые (1) снизу (6-8).
#           # Белые идут к 1, Черные к 8.
#           promotion <- FALSE
#           if (player == 1 && nr == 1) { new_piece <- 3; promotion <- TRUE }
#           if (player == 2 && nr == 8) { new_piece <- 4; promotion <- TRUE }
#
#           new_captured <- c(captured_pos, list(c(mr, mc)))
#
#           # Рекурсия
#           sub_moves <- find_capture_chains(board, nr, nc, new_piece, player, new_captured, start_pos)
#           moves_found <- c(moves_found, sub_moves)
#         }
#       }
#     }
#     # --- ЛОГИКА ДЛЯ ДАМКИ ---
#     else {
#       # --- ЛОГИКА ДЛЯ ДАМКИ (Flying King) ---
#       found_enemy <- FALSE
#       enemy_pos <- NULL
#
#       # Сканируем диагональ
#       for (dist in 1:7) {
#         mr <- r + dr * dist
#         mc <- c + dc * dist
#
#         if (!is_on_board(mr, mc)) break
#
#         p_here <- board[mr, mc]
#
#         # 1. Уперлись в свою фигуру -> дальше хода нет
#         if (get_piece_owner(p_here) == player) break
#
#         # 2. Нашли фигуру противника
#         if (get_piece_owner(p_here) == opponent) {
#           # Если уже нашли врага в этом направлении ранее - нельзя бить двоих подряд
#           if (found_enemy) break
#
#           # Проверка: не били ли мы эту шашку уже в этой цепочке?
#           already_captured <- FALSE
#           for (cp in captured_pos) {
#             if (cp[1] == mr && cp[2] == mc) already_captured <- TRUE
#           }
#           if (already_captured) break # Нельзя прыгать через одну шашку дважды
#
#           found_enemy <- TRUE
#           enemy_pos <- c(mr, mc)
#         }
#         # 3. Нашли пустую клетку
#         else if (p_here == 0) {
#           if (found_enemy) {
#             # Нашли врага, а теперь за ним пустая клетка (или несколько)
#             # Это возможная точка приземления.
#
#             can_continue <- TRUE
#             new_captured <- c(captured_pos, list(enemy_pos))
#
#             # Рекурсия: пробуем бить дальше с этой точки
#             sub_moves <- find_capture_chains(board, mr, mc, piece, player, new_captured, start_pos)
#
#             # Собираем ВСЕ варианты (и короткие остановки, и длинные продолжения)
#             moves_found <- c(moves_found, sub_moves)
#
#             # Важно: break НЕ делаем. Дамка может приземлиться на следующую пустую клетку тоже.
#           }
#         }
#       } # конец for dist
#     } # конец else (King logic)
#   } # конец for dir
#
#   # --- ФИНАЛЬНАЯ СБОРКА И ФИЛЬТРАЦИЯ (The Fix) ---
#
#   # 1. Если продолжений (moves_found) нет, но мы что-то сбили ->
#   #     это конечная точка
#   if (length(moves_found) == 0) {
#     if (length(captured_pos) > 0) {
#       return(list(list(
#         from = start_pos,
#         to = c(r, c),
#         captures = captured_pos
#       )))
#     } else {
#       return(list()) # Пустой список, ничего не нашли
#     }
#   }
#
#   # 2. ВАЖНЕЙШИЙ МОМЕНТ: Правило "Бить до конца"
#   # В moves_found могут быть:
#   # - Вариант А: Сбили 1 шашку, встали сразу за ней (тупик).
#   # - Вариант Б: Сбили 1 шашку, пролетели дальше, сбили 2-ю шашку.
#   # Мы должны вернуть ТОЛЬКО Вариант Б.
#
#   # Считаем длину цепочки взятий для каждого найденного варианта
#   lengths <- sapply(moves_found, function(m) length(m$captures))
#   max_len <- max(lengths)
#
#   # Оставляем только самые длинные цепочки
#   best_moves <- moves_found[lengths == max_len]
#
#   return(best_moves)
# }
find_capture_chains <- function(board, r, c, piece, player, captured_pos,
                                start_pos = NULL, path = NULL, promotion_info = 0) {

  if (is.null(start_pos)) start_pos <- c(r, c)
  if (is.null(path)) path <- list(c(r, c))

  is_king <- (piece == 3 || piece == 4)
  opponent <- get_opponent(player)

  moves_found <- list()
  can_continue <- FALSE

  directions <- list(c(-1, -1), c(-1, 1), c(1, -1), c(1, 1))

  for (dir in directions) {
    dr <- dir[1]
    dc <- dir[2]

    if (!is_king) {
      mr <- r + dr
      mc <- c + dc
      nr <- r + 2 * dr
      nc <- c + 2 * dc

      if (is_on_board(nr, nc)) {
        mid_piece <- board[mr, mc]
        dest_piece <- board[nr, nc]

        is_enemy <- (get_piece_owner(mid_piece) == opponent)
        already_captured <- any(sapply(captured_pos, function(cp) cp[1] == mr && cp[2] == mc))

        if (is_enemy && !already_captured && dest_piece == 0) {
          can_continue <- TRUE

          new_piece <- piece
          new_promotion_info <- promotion_info

          if (player == 1 && nr == 1 && promotion_info == 0) {
            new_piece <- 3
            new_promotion_info <- 3
          }
          if (player == 2 && nr == 8 && promotion_info == 0) {
            new_piece <- 4
            new_promotion_info <- 4
          }

          new_captured <- c(captured_pos, list(c(mr, mc)))
          new_path <- c(path, list(c(nr, nc)))

          sub_moves <- find_capture_chains(board, nr, nc, new_piece, player,
                                           new_captured, start_pos, new_path, new_promotion_info)
          moves_found <- c(moves_found, sub_moves)
        }
      }
    }
    else {
      found_enemy <- FALSE
      enemy_pos <- NULL

      for (dist in 1:7) {
        mr <- r + dr * dist
        mc <- c + dc * dist

        if (!is_on_board(mr, mc)) break

        p_here <- board[mr, mc]

        if (get_piece_owner(p_here) == player) break

        if (get_piece_owner(p_here) == opponent) {
          if (found_enemy) break

          already_captured <- any(sapply(captured_pos, function(cp) cp[1] == mr && cp[2] == mc))
          if (already_captured) break

          found_enemy <- TRUE
          enemy_pos <- c(mr, mc)
        }
        else if (p_here == 0) {
          if (found_enemy) {
            can_continue <- TRUE
            new_captured <- c(captured_pos, list(enemy_pos))
            new_path <- c(path, list(c(mr, mc)))

            sub_moves <- find_capture_chains(board, mr, mc, piece, player,
                                             new_captured, start_pos, new_path, promotion_info)
            moves_found <- c(moves_found, sub_moves)
          }
        }
      }
    }
  }

  if (length(moves_found) == 0) {
    if (length(captured_pos) > 0) {
      return(list(list(
        from = start_pos,
        to = c(r, c),
        captures = captured_pos,
        is_king = promotion_info,
        detail = path
      )))
    } else {
      return(list())
    }
  }

  lengths <- sapply(moves_found, function(m) length(m$captures))
  max_len <- max(lengths)
  best_moves <- moves_found[lengths == max_len]
  return(best_moves)
}
