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
      directions <- if (is_king) list(c(-1, -1), c(-1, 1), c(1, -1), c(1, 1)) else list(c(forward, -1), c(forward, 1))

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

          sub_moves <- find_capture_chains(
            board, nr, nc, new_piece, player,
            new_captured, start_pos, new_path, new_promotion_info
          )
          moves_found <- c(moves_found, sub_moves)
        }
      }
    } else {
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
        } else if (p_here == 0) {
          if (found_enemy) {
            can_continue <- TRUE
            new_captured <- c(captured_pos, list(enemy_pos))
            new_path <- c(path, list(c(mr, mc)))

            sub_moves <- find_capture_chains(
              board, mr, mc, piece, player,
              new_captured, start_pos, new_path, promotion_info
            )
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
