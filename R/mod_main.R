#' Game Module UI
#' @param id Module ID
#' @export
rcheckers_ui <- function(id) {
  ns <- NS(id)

  css <- "
     .last-move-from {
    background-color: rgba(106, 168, 79, 0.35) !important;
    box-shadow: inset 0 0 8px rgba(106, 168, 79, 0.6);
    animation: fadeIn 0.5s ease-in;
    }

    .last-move-to {
    background-color: rgba(106, 168, 79, 0.5) !important;
    box-shadow: inset 0 0 12px rgba(106, 168, 79, 0.8);
    animation: fadeIn 0.5s ease-in;
    }

    .last-move-path {
    background-color: rgba(106, 168, 79, 0.25) !important;
    box-shadow: inset 0 0 6px rgba(106, 168, 79, 0.4);
    animation: fadeIn 0.5s ease-in;
    }

    @keyframes fadeIn {
    from {
      opacity: 0;
      transform: scale(0.95);
    }
    to {
      opacity: 1;
      transform: scale(1);
    }
    }

  .board-container {
    display: grid;
    grid-template-columns: repeat(8, 50px);
    grid-template-rows: repeat(8, 50px);
    border: 5px solid #4e342e;
    width: fit-content;
    margin: 0 auto;
  }
  .board-cell {
    width: 50px;
    height: 50px;
    display: flex;
    justify-content: center;
    align-items: center;
    cursor: pointer;
    font-size: 32px;
    user-select: none;
    transition: background-color 0.15s ease;
  }
  .cell-light { background-color: #f0d9b5; }
  .cell-dark { background-color: #b58863; }
  .piece { transition: none; }
  .piece-white { color: #fff; text-shadow: 0 0 2px #000; }
  .piece-black { color: #000; text-shadow: 0 0 1px #fff; }
  .piece-king::after { content: '👑'; position: absolute; font-size: 12px; color: gold; }
  .selected { background-color: #7b6f3c !important; }
  .highlight { background-color: #6f7b3c !important; box-shadow: inset 0 0 10px #ffeb3b; }
  .last-move { background-color: rgba(255, 255, 0, 0.3) !important; }
  .game-info { text-align: center; margin-bottom: 15px; }
  .controls { margin-top: 15px; display: flex; gap: 10px; justify-content: center; }
  "

  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(css))),

    div(class = "container-fluid",
        div(class = "game-info",
            h3(textOutput(ns("status_text"))),
            h4(textOutput(ns("score_text")))
        ),

        # Статичная структура доски (рендерится 1 раз)
        div(class = "board-container", id = ns("board_container"),
            lapply(1:8, function(r) {
              lapply(1:8, function(c) {
                is_black_cell <- (r + c) %% 2 != 0
                cell_class <- if (is_black_cell) "cell-dark" else "cell-light"
                cell_id <- ns(paste0("cell_", r, "_", c))

                if (!is_black_cell) {
                  div(class = paste("board-cell", cell_class), id = cell_id)
                } else {
                  onclick_str <- sprintf("Shiny.setInputValue('%s', '%d_%d', {priority: 'event'})",
                                         ns("board_click"), r, c)
                  div(class = paste("board-cell", cell_class),
                      id = cell_id,
                      onclick = onclick_str)
                }
              })
            })
        ),

        div(class = "controls",
            actionButton(ns("btn_new_game"), get_localization("btn_new_game")),
            actionButton(ns("btn_surrender"), get_localization("btn_surrender")),
            actionButton(ns("btn_offer_draw"), get_localization("btn_offer_draw"))
        )
    )
  )
}


#' Game Module Server
#' @param id Module ID
#' @export
rcheckers_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # === Рекативные переменные ====
    game <- reactiveValues(
      board = init_board(),
      turn = 1,
      selected = NULL,
      legal_moves = NULL,
      game_over = FALSE,
      winner = NULL,
      score = c(0, 0),
      mode = "pvp",
      last_move = NULL,
      force_update = 0,  # Триггер для принудительного обновления

      # Счетчик тихих ходов. Для правил ничьи 5, 15, 30, 60 ходов
      moves_quiet = 0,

      # История позиций. Для проверки троекратного повторения позиции
      position_history = list()
    )

    # === Observer для обновления клеток ====
    observe({
      board <- game$board
      sel <- game$selected
      moves <- game$legal_moves
      trigger <- game$force_update  # Чтобы сработало даже при начальной загрузке

      # Задержка для гарантии загрузки DOM
      shinyjs::delay(50, {
        for (r in 1:8) {
          for (c in 1:8) {
            is_black_cell <- (r + c) %% 2 != 0
            if (!is_black_cell) next

            cell_id <- ns(paste0("cell_", r, "_", c))

            # Определяем CSS классы
            cell_classes <- c("board-cell", "cell-dark")

            if (!is.null(game$last_move)) {
              # Начальная клетка (откуда)
              if (game$last_move$from[1] == r && game$last_move$from[2] == c) {
                cell_classes <- c(cell_classes, "last-move-from")
              }
              # Конечная клетка (куда) - более яркая
              else if (game$last_move$to[1] == r && game$last_move$to[2] == c) {
                cell_classes <- c(cell_classes, "last-move-to")
              }
              # Промежуточные клетки маршрута - самые тусклые
              else if (!is.null(game$last_move$detail)) {
                for (step in game$last_move$detail) {
                  if (step[1] == r && step[2] == c) {
                    cell_classes <- c(cell_classes, "last-move-path")
                    break
                  }
                }
              }
            }


            # Подсветка выбранной клетки
            if (!is.null(sel) && sel[1] == r && sel[2] == c) {
              cell_classes <- c(cell_classes, "selected")
            }

            # Подсветка возможных ходов
            is_target <- FALSE

            # Подсветка промежуточных шагов (detail)
            is_path <- FALSE
            if (!is.null(sel) && !is.null(moves)) {
              for (m in moves) {
                if (m$from[1] == sel[1] && m$from[2] == sel[2]) {
                  if (!is.null(m$detail)) {
                    for (step in m$detail) {
                      if (step[1] == r && step[2] == c) {
                        is_path <- TRUE
                        break
                      }
                    }
                  }
                  if (is_path) break
                }
              }
            }

            if (is_path) {
              cell_classes <- c(cell_classes, "highlight")  # Или создать новый класс "path-highlight"
            }

            if (!is.null(sel) && !is.null(moves)) {
              for (m in moves) {
                if (m$from[1] == sel[1] && m$from[2] == sel[2] &&
                    m$to[1] == r && m$to[2] == c) {
                  is_target <- TRUE
                  break
                }
              }
            }
            if (is_target) {
              cell_classes <- c(cell_classes, "highlight")
            }

            # Определяем содержимое (фигура)
            piece <- board[r, c]
            piece_html <- ""
            if (piece != 0) {
              color_class <- if (get_piece_owner(piece) == 1) "piece-white" else "piece-black"
              king_class <- if (piece > 2) " piece-king" else ""
              # Экранируем кавычки для JS
              piece_html <- paste0("<span class='piece ", color_class, king_class, "'>●</span>")
            }

            # Формируем безопасную строку для JS (экранируем спецсимволы)
            safe_html <- gsub("'", "\\\\'", piece_html)
            safe_classes <- paste(cell_classes, collapse = " ")

            # Обновляем клетку через JavaScript
            js_code <- sprintf(
              "var cell = document.getElementById('%s');
               if (cell) {
                 cell.className = '%s';
                 cell.innerHTML = '%s';
               }",
              cell_id, safe_classes, safe_html
            )

            shinyjs::runjs(js_code)
          }
        }
      })
    })

    # === Новая игра ====
    observeEvent(input$btn_new_game, {
      showModal(modalDialog(
        title = get_localization("app_title"),
        radioButtons(ns("mode_select"), "Mode",
                     choices = c("PvP" = "pvp", "PvE (AI)" = "pve")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("start_confirm"), "Start")
        )
      ))
    })

    observeEvent(input$start_confirm, {
      game$board <- init_board()
      game$turn <- 1
      game$selected <- NULL
      game$game_over <- FALSE
      game$winner <- NULL
      game$mode <- input$mode_select
      game$legal_moves <- get_legal_moves(game$board, game$turn)
      game$force_update <- game$force_update + 1  # Триггер обновления
      removeModal()
    })

    # === Инициализация при загрузке приложения ====
    observe({
      # Срабатывает один раз при старте сессии
      isolate({
        game$legal_moves <- get_legal_moves(game$board, game$turn)
        game$force_update <- game$force_update + 1
      })
    }) %>%
      bindEvent(session$clientData$url_hostname, once = TRUE, ignoreInit = FALSE)

    # === Клик по доске =====
    observeEvent(input$board_click, {
      if (game$game_over) return()

      if (!is.null(game$last_move) && game$turn == 1) {
        game$last_move <- NULL
      }

      coords <- as.numeric(strsplit(input$board_click, "_")[[1]])
      r <- coords[1]
      c <- coords[2]

      clicked_owner <- get_piece_owner(game$board[r, c])

      if (clicked_owner == game$turn) {
        can_select <- FALSE
        for (m in game$legal_moves) {
          if (m$from[1] == r && m$from[2] == c) {
            can_select <- TRUE
            break
          }
        }

        if (can_select) {
          game$selected <- c(r, c)
        } else {
          showNotification(get_localization("error_illegal_move"), type = "warning", duration = 2)
        }
      } else if (!is.null(game$selected)) {
        move_to_apply <- NULL
        for (m in game$legal_moves) {
          if (m$from[1] == game$selected[1] && m$from[2] == game$selected[2] &&
              m$to[1] == r && m$to[2] == c) {
            move_to_apply <- m
            break
          }
        }

        if (!is.null(move_to_apply)) {
          game$board <- apply_move(game$board, move_to_apply)
          game$selected <- NULL

          # Обновляем счетчик тихих ходов
          if (length(move_to_apply$captures) > 0) {
            game$moves_quiet <- 0  # Сброс при взятии
          } else {
            game$moves_quiet <- game$moves_quiet + 1
          }

          # Добавляем текущую позицию в историю
          position_key <- paste(c(as.vector(game$board), game$turn), collapse = "")
          game$position_history[[length(game$position_history) + 1]] <- position_key

          multi_jump_available <- FALSE
          if (length(move_to_apply$captures) > 0) {
            next_captures <- get_all_capture_moves(game$board, game$turn)
            this_piece_captures <- list()
            for (nm in next_captures) {
              if (nm$from[1] == move_to_apply$to[1] && nm$from[2] == move_to_apply$to[2]) {
                this_piece_captures[[length(this_piece_captures) + 1]] <- nm
              }
            }

            if (length(this_piece_captures) > 0) {
              multi_jump_available <- TRUE
              game$legal_moves <- this_piece_captures
              game$selected <- move_to_apply$to
              showNotification("Multi-jump required!", type = "message")
            }
          }

          if (!multi_jump_available) {
            # 1. Сначала проверяем условия ничьей
            draw_check <- check_draw_conditions(game)

            if (draw_check$is_draw) {
              # --- НИЧЬЯ ---
              game$game_over <- TRUE
              game$winner <- "draw"

              # Обновим счет
              game$score <- game$score + 0.5

              showModal(modalDialog(
                title = "Game Over",
                paste("Draw:", draw_check$reason), # Выводим причину (15 ходов, повтор и т.д.)
                footer = modalButton("Close")
              ))

            } else {
              # --- НЕ НИЧЬЯ -> Идем по стандартному пути ---

              next_player <- get_opponent(game$turn)
              status <- check_game_state(game$board, next_player) # Проверяем, может ли ходить соперник

              if (status != "active") {
                # --- ПОБЕДА / ПОРАЖЕНИЕ ---
                game$game_over <- TRUE
                game$winner <- status

                # ИСПРАВЛЕНИЕ: Безопасный подсчет очков
                if (status == "white_won") {
                  game$score[1] <- game$score[1] + 1
                } else if (status == "black_won") {
                  game$score[2] <- game$score[2] + 1
                }

                showModal(modalDialog(
                  title = "Game Over",
                  paste(get_localization(paste0("status_", status))),
                  footer = modalButton("Close")
                ))

              } else {
                # --- ИГРА ПРОДОЛЖАЕТСЯ ---
                game$turn <- next_player
                game$legal_moves <- get_legal_moves(game$board, next_player)

                # Если сейчас ход ИИ
                if (game$mode == "pve" && game$turn == 2) {
                  shinyjs::delay(500, { run_ai_turn() })
                }
              }
            }
          }
        }
      }
    })

    run_ai_turn <- function() {
      if (game$game_over || game$turn != 2) return()

      ai_move <- get_ai_move(game$board, 2)

      if (!is.null(ai_move)) {
        game$board <- apply_move(game$board, ai_move)

        game$last_move <- list(from = ai_move$from, to = ai_move$to,
                               detail = ai_move$detail)

        next_player <- 1
        status <- check_game_state(game$board, next_player)

        if (status != "active") {
          game$game_over <- TRUE
          game$winner <- status
          game$score[2] <- game$score[2] + 1
          showModal(modalDialog(title = "Game Over", "AI Wins!",
                                footer = modalButton("Close")))
        } else {
          game$turn <- next_player
          game$legal_moves <- get_legal_moves(game$board, next_player)
        }
      }
    }

    # === Текс при завершении игры ====
    output$status_text <- renderText({
      if (game$game_over)
        return(get_localization(paste0("status_", game$winner)))
      if (game$turn == 1) get_localization("status_white_turn")
      else get_localization("status_black_turn")
    })

    #  === Счет ====
    output$score_text <- renderText({
      paste(get_localization("score_label"), ": ", game$score[1], " - ", game$score[2])
    })

    # ==== Модальное окно о завершении игры ====
    observeEvent(input$btn_surrender, {
      if (game$game_over) return()
      loser <- game$turn
      winner <- get_opponent(loser)
      game$game_over <- TRUE
      game$winner <- if (winner == 1) "white_won" else "black_won"
      if (winner == 1) game$score[1] <- game$score[1] + 1 else game$score[2] <- game$score[2] + 1
      showModal(modalDialog(
        title = "Game Over",
        paste0("Player ", loser, " surrendered!"),
        footer = modalButton("Close")
      ))
    })

    # === Предложение ничьи ====
    observeEvent(input$btn_offer_draw, {
      if (game$game_over) return()
      current_player <- game$turn

      if (game$mode == "pve" && current_player == 1) {
        ai_pieces <- sum(game$board == 2 | game$board == 4)
        player_pieces <- sum(game$board == 1 | game$board == 3)

        if (ai_pieces <= player_pieces) {
          game$game_over <- TRUE
          game$winner <- "draw"
          showModal(modalDialog(title = "Game Over", "AI accepted the draw.",
                                footer = modalButton("Close")))
        } else {
          showNotification("Computer refused the draw!", type = "warning")
        }
      } else {
        opponent_name <- if (current_player == 1) "Black" else "White"
        showModal(modalDialog(
          title = "Draw Offer",
          paste0("Player ", current_player, " offers a draw. ",
                 opponent_name, ", do you accept?"),
          footer = tagList(
            actionButton(ns("btn_draw_accept"), "Accept"),
            modalButton("Decline")
          )
        ))
      }
    })

    observeEvent(input$btn_draw_accept, {
      removeModal()
      game$game_over <- TRUE
      game$winner <- "draw"
      showModal(modalDialog(title = "Game Over", "Draw agreed!",
                            footer = modalButton("Close")))
    })
  })
}
