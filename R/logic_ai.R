# AI Logic (logic_ai.R) ====

#' Get AI Move
#'
#' Main entry point for AI move selection
#'
#' @param board Matrix 8x8
#' @param player 1 (White) or 2 (Black)
#' @param level AI difficulty: 1 (Beginner), 2 (Amateur), 3 (Advanced)
#'
#' @return Selected move object
#' @export
get_ai_move <- function(board, player, level = 3) {
  legal_moves <- get_legal_moves(board, player)

  if (length(legal_moves) == 0) return(NULL)
  if (length(legal_moves) == 1) return(legal_moves[[1]])

  switch(as.character(level),
         "1" = ai_beginner(board, player, legal_moves),
         "2" = ai_minimax(board, player, legal_moves, depth = 3),
         "3" = ai_minimax(board, player, legal_moves, depth = 5),
         ai_beginner(board, player, legal_moves)
  )
}

# --- Level 1: Beginner (Random Safe Move) ---

#' @noRd
ai_beginner <- function(board, player, legal_moves) {
  # Фильтруем небезопасные ходы
  safe_moves <- Filter(function(m) !is_move_unsafe(board, player, m), legal_moves)

  # Если все ходы опасны, берём любой
  candidate_moves <- if (length(safe_moves) > 0) safe_moves else legal_moves

  # Случайный выбор
  candidate_moves[[sample.int(length(candidate_moves), 1)]]
}

#' @noRd
is_move_unsafe <- function(board, player, move) {
  # Применяем ход виртуально
  temp_board <- apply_move_virtual(board, move)
  opponent <- get_opponent(player)

  # Проверяем, может ли противник побить фигуру на новой позиции
  opponent_captures <- get_all_capture_moves(temp_board, opponent)

  any(sapply(opponent_captures, function(cm) {
    any(sapply(cm$captures, function(cap) {
      identical(cap, move$to)
    }))
  }))
}

# --- Level 2/3: Minimax with Alpha-Beta Pruning ---

#' @noRd
ai_minimax <- function(board, player, legal_moves, depth) {
  # Оцениваем каждый ход
  scores <- vapply(legal_moves, function(move) {
    temp_board <- apply_move_virtual(board, move)
    minimax(temp_board, depth - 1, -Inf, Inf, FALSE, player)
  }, numeric(1))

  # Выбираем лучший
  best_score <- max(scores)
  best_indices <- which(scores == best_score)

  legal_moves[[sample(best_indices, 1)]]
}

#' @noRd
minimax <- function(board, depth, alpha, beta, is_maximizing, ai_player) {
  # Терминальные условия
  if (depth == 0) return(evaluate_position(board, ai_player))

  current_player <- if (is_maximizing) ai_player else get_opponent(ai_player)
  moves <- get_legal_moves(board, current_player)

  if (length(moves) == 0) {
    # Нет ходов = проигрыш
    return(if (is_maximizing) -10000 else 10000)
  }

  if (is_maximizing) {
    max_eval <- -Inf
    for (move in moves) {
      temp_board <- apply_move_virtual(board, move)
      eval <- minimax(temp_board, depth - 1, alpha, beta, FALSE, ai_player)
      max_eval <- max(max_eval, eval)
      alpha <- max(alpha, eval)
      if (beta <= alpha) break  # Alpha-beta отсечение
    }
    return(max_eval)
  } else {
    min_eval <- Inf
    for (move in moves) {
      temp_board <- apply_move_virtual(board, move)
      eval <- minimax(temp_board, depth - 1, alpha, beta, TRUE, ai_player)
      min_eval <- min(min_eval, eval)
      beta <- min(beta, eval)
      if (beta <= alpha) break
    }
    return(min_eval)
  }
}

# --- Evaluation Function ---

#' @noRd
evaluate_position <- function(board, player) {
  opponent <- get_opponent(player)

  # Подсчёт материала
  piece_values <- c(
    `1` = 100,  # Белая простая
    `2` = 100,  # Чёрная простая
    `3` = 300,  # Белая дамка
    `4` = 300   # Чёрная дамка
  )

  # Векторизованный подсчёт
  player_pieces <- board[board %in% c(if (player == 1) c(1, 3) else c(2, 4))]
  opponent_pieces <- board[board %in% c(if (opponent == 1) c(1, 3) else c(2, 4))]

  player_score <- sum(piece_values[as.character(player_pieces)])
  opponent_score <- sum(piece_values[as.character(opponent_pieces)])

  # Бонусы за позицию (центр, продвижение)
  position_bonus <- sum(apply(which(board == player | board == player + 2, arr.ind = TRUE), 1, function(pos) {
    row <- pos[1]
    col <- pos[2]
    center_dist <- abs(4.5 - row) + abs(4.5 - col)
    advancement <- if (player == 1) 8 - row else row - 1
    5 * advancement - 2 * center_dist
  }))

  player_score + position_bonus - opponent_score
}

# --- Helper: Apply Move Virtually ---

#' @noRd
apply_move_virtual <- function(board, move) {
  temp_board <- board
  from <- move$from
  to <- move$to

  piece <- temp_board[from[1], from[2]]
  temp_board[from[1], from[2]] <- 0

  # Удаляем побитые
  for (cap in move$captures) {
    temp_board[cap[1], cap[2]] <- 0
  }

  # Превращение в дамку
  if (!is.null(move$is_king) && move$is_king != 0) {
    piece <- move$is_king
  }

  temp_board[to[1], to[2]] <- piece
  temp_board
}
#'
#' # --- Helper: Get Opponent ---
#'
#' #' @noRd
#' get_opponent <- function(player) {
#'   if (player == 1) 2 else 1
#' }




#' #' Get AI Move
#' #'
#' #' Selects a move for AI player based on difficulty level.
#' #'
#' #' @param board Matrix 8x8
#' #' @param player 1 (White) or 2 (Black)
#' #' @param level Integer: 1 (novice), 2 (amateur), 3 (advanced)
#' #'
#' #' @return A move object (same structure as from get_legal_moves)
#' #' @export
#' get_ai_move <- function(board, player, level = 3) {
#'   moves <- get_legal_moves(board, player)
#'
#'   if (length(moves) == 0) {
#'     stop("No legal moves available for AI")
#'   }
#'
#'   switch(level,
#'          "1" = select_safe_random_move(board, player, moves),
#'          "2" = select_best_move(board, player, moves, depth = 3),
#'          "3" = select_best_move(board, player, moves, depth = 6),
#'          select_safe_random_move(board, player, moves)
#'   )
#' }
#'
#' #' Select Safe Random Move (Level 1)
#' #'
#' #' Picks a random move that doesn't expose the piece to immediate capture.
#' #'
#' #' @noRd
#' select_safe_random_move <- function(board, player, moves) {
#'   # Filter out moves that lead to immediate capture
#'   safe_moves <- Filter(function(m) {
#'     test_board <- apply_move(board, m)
#'     !is_position_under_attack(test_board, m$to, player)
#'   }, moves)
#'
#'   # If all moves are risky, just pick any
#'   candidate_moves <- if (length(safe_moves) > 0) safe_moves else moves
#'
#'   candidate_moves[[sample.int(length(candidate_moves), 1)]]
#' }
#'
#' #' Check if Position is Under Attack
#' #'
#' #' @param board Matrix 8x8
#' #' @param pos Vector c(row, col)
#' #' @param player Piece owner (1 or 2)
#' #'
#' #' @return Logical
#' #' @noRd
#' is_position_under_attack <- function(board, pos, player) {
#'   opponent <- get_opponent(player)
#'   opponent_moves <- get_all_capture_moves(board, opponent)
#'
#'   # Check if any opponent capture targets this position
#'   any(vapply(opponent_moves, function(m) {
#'     any(vapply(m$captures, function(cap) {
#'       identical(cap, pos)
#'     }, logical(1)))
#'   }, logical(1)))
#' }
#'
#' #' Select Best Move (Levels 2-3)
#' #'
#' #' Uses minimax algorithm with alpha-beta pruning to find optimal move.
#' #'
#' #' @noRd
#' select_best_move <- function(board, player, moves, depth) {
#'   # Evaluate each move
#'   scores <- vapply(moves, function(m) {
#'     test_board <- apply_move(board, m)
#'     -minimax(test_board, get_opponent(player), depth - 1,
#'              -Inf, Inf, maximizing = FALSE)
#'   }, numeric(1))
#'
#'   # Select move(s) with best score
#'   best_score <- max(scores)
#'   best_indices <- which(scores == best_score)
#'
#'   # Random selection among equally good moves
#'   moves[[sample(best_indices, 1)]]
#' }
#'
#' #' Minimax Algorithm with Alpha-Beta Pruning
#' #'
#' #' @param board Current board state
#' #' @param player Current player
#' #' @param depth Remaining search depth
#' #' @param alpha Best value for maximizer
#' #' @param beta Best value for minimizer
#' #' @param maximizing Logical: TRUE if maximizing player
#' #'
#' #' @return Numeric score
#' #' @noRd
#' minimax <- function(board, player, depth, alpha, beta, maximizing) {
#'   # Terminal conditions
#'   if (depth == 0) {
#'     return(evaluate_position(board, player))
#'   }
#'
#'   moves <- get_legal_moves(board, player)
#'
#'   if (length(moves) == 0) {
#'     # Game over: massive penalty/bonus
#'     return(if (maximizing) -10000 else 10000)
#'   }
#'
#'   if (maximizing) {
#'     max_eval <- -Inf
#'     for (m in moves) {
#'       test_board <- apply_move(board, m)
#'       eval <- minimax(test_board, get_opponent(player), depth - 1,
#'                       alpha, beta, FALSE)
#'       max_eval <- max(max_eval, eval)
#'       alpha <- max(alpha, eval)
#'       if (beta <= alpha) break  # Beta cutoff
#'     }
#'     return(max_eval)
#'   } else {
#'     min_eval <- Inf
#'     for (m in moves) {
#'       test_board <- apply_move(board, m)
#'       eval <- minimax(test_board, get_opponent(player), depth - 1,
#'                       alpha, beta, TRUE)
#'       min_eval <- min(min_eval, eval)
#'       beta <- min(beta, eval)
#'       if (beta <= alpha) break  # Alpha cutoff
#'     }
#'     return(min_eval)
#'   }
#' }
#'
#' #' Evaluate Board Position
#' #'
#' #' Returns a score from the perspective of the given player.
#' #' Positive = good for player, Negative = bad for player.
#' #'
#' #' @param board Matrix 8x8
#' #' @param player Player to evaluate for (1 or 2)
#' #'
#' #' @return Numeric score
#' #' @noRd
#' evaluate_position <- function(board, player) {
#'   opponent <- get_opponent(player)
#'
#'   # Material values
#'   piece_values <- c(0, 1, 1, 3, 3)  # 0=empty, 1=white_man, 2=black_man, 3=white_king, 4=black_king
#'
#'   # Count material for both sides
#'   player_material <- sum(piece_values[board[get_piece_owner(board) == player] + 1])
#'   opponent_material <- sum(piece_values[board[get_piece_owner(board) == opponent] + 1])
#'
#'   # Positional bonuses
#'   player_position <- calculate_position_bonus(board, player)
#'   opponent_position <- calculate_position_bonus(board, opponent)
#'
#'   # Combined evaluation
#'   material_diff <- player_material - opponent_material
#'   position_diff <- (player_position - opponent_position) * 0.1
#'
#'   material_diff + position_diff
#' }
#'
#' #' Calculate Positional Bonus
#' #'
#' #' Rewards center control and piece advancement.
#' #'
#' #' @noRd
#' calculate_position_bonus <- function(board, player) {
#'   # Center squares (rows 3-6, cols 3-6) worth more
#'   center_mask <- matrix(0, 8, 8)
#'   center_mask[3:6, 3:6] <- 1
#'
#'   # Advancement bonus (pieces closer to promotion)
#'   advancement_weights <- if (player == 1) {
#'     matrix(rep(8:1, each = 8), nrow = 8, byrow = TRUE)
#'   } else {
#'     matrix(rep(1:8, each = 8), nrow = 8, byrow = TRUE)
#'   }
#'
#'   player_mask <- get_piece_owner(board) == player
#'
#'   # Calculate bonuses
#'   center_bonus <- sum(center_mask * player_mask)
#'   advancement_bonus <- sum(advancement_weights * player_mask * (board < 3)) * 0.1
#'
#'   center_bonus + advancement_bonus
#' }
