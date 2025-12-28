#' Game Module UI
#' @param id Module ID
#' @export
rcheckers_ui <- function(id) {
  ns <- NS(id)

  css <- "
    /* === АНИМАЦИИ === */
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

    @keyframes pieceClick {
      0% { transform: translateY(0) scale(1); }
      50% { transform: translateY(-5px) scale(1.05); }
      100% { transform: translateY(0) scale(1); }
    }

    @keyframes hexagonPulse {
      0%, 100% { opacity: 0.9; transform: scale(1); }
      50% { opacity: 1; transform: scale(1.1); }
    }

    /* === ПОДСВЕТКА ПОСЛЕДНЕГО ХОДА === */
    .last-move-from {
      background: radial-gradient(circle at center, rgba(106, 168, 79, 0.4), rgba(106, 168, 79, 0.2)) !important;
      box-shadow: inset 0 0 15px rgba(106, 168, 79, 0.7), 0 4px 8px rgba(0,0,0,0.2);
      animation: fadeIn 0.5s ease-in;
    }

    .last-move-to {
      background: radial-gradient(circle at center, rgba(106, 168, 79, 0.6), rgba(106, 168, 79, 0.3)) !important;
      box-shadow: inset 0 0 20px rgba(106, 168, 79, 0.9), 0 6px 12px rgba(0,0,0,0.3);
      animation: fadeIn 0.5s ease-in;
    }

    .last-move-path {
      background: radial-gradient(circle at center, rgba(106, 168, 79, 0.3), rgba(106, 168, 79, 0.15)) !important;
      box-shadow: inset 0 0 10px rgba(106, 168, 79, 0.5), 0 2px 6px rgba(0,0,0,0.15);
      animation: fadeIn 0.5s ease-in;
    }

    /* === КОНТЕЙНЕР ДОСКИ === */
    .board-container {
      display: grid;
      grid-template-columns: repeat(8, 60px);
      grid-template-rows: repeat(8, 60px);
      border: 8px solid #3e2723;
      border-radius: 4px;
      width: fit-content;
      margin: 0 auto;
      box-shadow:
        0 10px 30px rgba(0, 0, 0, 0.5),
        0 20px 60px rgba(0, 0, 0, 0.3),
        inset 0 0 20px rgba(0, 0, 0, 0.2);
      background: linear-gradient(145deg, #2c1810, #4e342e);
      padding: 8px;
    }

    /* === КЛЕТКИ ДОСКИ === */
    .board-cell {
      width: 60px;
      height: 60px;
      display: flex;
      justify-content: center;
      align-items: center;
      cursor: pointer;
      user-select: none;
      position: relative;
      transition: all 0.2s ease;
      box-shadow: inset 0 2px 4px rgba(0,0,0,0.1);
    }

    .cell-light {
      background: linear-gradient(145deg, #f5e6d3, #e8d4b8);
      box-shadow:
        inset 2px 2px 4px rgba(255,255,255,0.4),
        inset -2px -2px 4px rgba(0,0,0,0.1);
    }

    .cell-dark {
      background: linear-gradient(145deg, #c7a882, #a88f6f);
      box-shadow:
        inset 2px 2px 4px rgba(255,255,255,0.2),
        inset -2px -2px 4px rgba(0,0,0,0.2);
    }

    .cell-dark:hover {
      transform: translateY(-1px);
      box-shadow:
        inset 2px 2px 4px rgba(255,255,255,0.25),
        inset -2px -2px 4px rgba(0,0,0,0.25),
        0 4px 8px rgba(0,0,0,0.2);
    }

    /* === ПОДСВЕТКА ВЫБРАННОЙ КЛЕТКИ === */
    .selected {
      background: radial-gradient(circle at center, #8b7355, #6d5d4a) !important;
      box-shadow:
        inset 0 0 20px rgba(255, 235, 59, 0.5),
        0 0 15px rgba(255, 235, 59, 0.4),
        0 4px 12px rgba(0,0,0,0.4) !important;
      animation: hexagonPulse 1s ease-in-out infinite;
    }

    .highlight {
      background: radial-gradient(circle at center, #8fa870, #7b9461) !important;
      box-shadow:
        inset 0 0 15px rgba(139, 195, 74, 0.6),
        0 0 12px rgba(139, 195, 74, 0.5),
        0 4px 10px rgba(0,0,0,0.3) !important;
    }

    /* === ФИГУРЫ 3D === */
    .piece-container {
      position: relative;
      width: 48px;
      height: 48px;
      display: flex;
      justify-content: center;
      align-items: center;
      pointer-events: none;
    }

    .piece {
      position: relative;
      width: 44px;
      height: 44px;
      border-radius: 50%;
      display: flex;
      justify-content: center;
      align-items: center;
      transition: transform 0.15s ease;
      cursor: pointer;
      pointer-events: all;
    }

    .board-cell:active .piece {
      animation: pieceClick 0.3s ease;
    }

    /* === БЕЛЫЕ ФИГУРЫ === */
    .piece-white {
      background:
        radial-gradient(circle at 30% 30%, #ffffff, #e8e8e8 50%, #c0c0c0);
      box-shadow:
        0 6px 12px rgba(0, 0, 0, 0.4),
        inset 0 2px 4px rgba(255, 255, 255, 0.8),
        inset 0 -2px 4px rgba(0, 0, 0, 0.2),
        0 0 0 2px rgba(200, 200, 200, 0.3);
      border: 2px solid #b8b8b8;
    }

    .piece-white::before {
      content: '';
      position: absolute;
      top: 3px;
      left: 8px;
      width: 15px;
      height: 15px;
      background: radial-gradient(circle, rgba(255,255,255,0.9), transparent);
      border-radius: 50%;
      pointer-events: none;
    }

    /* === ЧЕРНЫЕ ФИГУРЫ === */
    .piece-black {
      background:
        radial-gradient(circle at 30% 30%, #4a4a4a, #2a2a2a 50%, #0a0a0a);
      box-shadow:
        0 6px 12px rgba(0, 0, 0, 0.6),
        inset 0 2px 4px rgba(255, 255, 255, 0.15),
        inset 0 -2px 4px rgba(0, 0, 0, 0.5),
        0 0 0 2px rgba(80, 80, 80, 0.4);
      border: 2px solid #1a1a1a;
    }

    .piece-black::before {
      content: '';
      position: absolute;
      top: 3px;
      left: 8px;
      width: 12px;
      height: 12px;
      background: radial-gradient(circle, rgba(255,255,255,0.2), transparent);
      border-radius: 50%;
      pointer-events: none;
    }

    /* === ШЕСТИГРАННИК В ЦЕНТРЕ === */
    .hexagon {
      position: absolute;
      width: 18px;
      height: 18px;
      display: flex;
      justify-content: center;
      align-items: center;
      pointer-events: none;
      z-index: 10;
    }

    .hexagon svg {
      width: 100%;
      height: 100%;
      filter: drop-shadow(0 1px 2px rgba(0,0,0,0.3));
    }

    .piece-white .hexagon svg {
      fill: url(#hexGradientWhite);
      stroke: #999;
      stroke-width: 0.5;
    }

    .piece-black .hexagon svg {
      fill: url(#hexGradientBlack);
      stroke: #666;
      stroke-width: 0.5;
    }

    /* === КОРОНА ДЛЯ ДАМОК === */
    .piece-king::after {
      content: '♔';
      position: absolute;
      top: -8px;
      font-size: 20px;
      color: #ffd700;
      text-shadow:
        0 2px 4px rgba(0,0,0,0.5),
        0 0 8px rgba(255, 215, 0, 0.6);
      z-index: 20;
      pointer-events: none;
      animation: hexagonPulse 2s ease-in-out infinite;
    }

    /* === ИНТЕРФЕЙС === */
    .game-info {
      text-align: center;
      margin-bottom: 20px;
      font-family: 'Georgia', serif;
      color: #3e2723;
    }

    .game-info h3 {
      font-size: 24px;
      font-weight: bold;
      margin-bottom: 8px;
      text-shadow: 1px 1px 2px rgba(0,0,0,0.1);
    }

    .game-info h4 {
      font-size: 18px;
      color: #5d4037;
    }

    .controls {
      margin-top: 20px;
      display: flex;
      gap: 12px;
      justify-content: center;
    }

    .controls button {
      padding: 10px 20px;
      font-size: 14px;
      border-radius: 6px;
      border: none;
      background: linear-gradient(145deg, #6d4c41, #5d4037);
      color: white;
      cursor: pointer;
      box-shadow: 0 4px 8px rgba(0,0,0,0.3);
      transition: all 0.2s ease;
    }

    .controls button:hover {
      transform: translateY(-2px);
      box-shadow: 0 6px 12px rgba(0,0,0,0.4);
      background: linear-gradient(145deg, #7d5c51, #6d4c41);
    }

    .controls button:active {
      transform: translateY(0);
      box-shadow: 0 2px 4px rgba(0,0,0,0.3);
    }

    /* === SVG ГРАДИЕНТЫ (скрытые) === */
    .svg-defs {
      position: absolute;
      width: 0;
      height: 0;
      overflow: hidden;
    }
  "

  tagList(
    shinyjs::useShinyjs(),
    tags$head(tags$style(HTML(css))),

    # SVG градиенты для шестигранников
    tags$div(class = "svg-defs",
             HTML('
        <svg xmlns="http://www.w3.org/2000/svg">
          <defs>
            <linearGradient id="hexGradientWhite" x1="0%" y1="0%" x2="100%" y2="100%">
              <stop offset="0%" style="stop-color:#e0e0e0;stop-opacity:1" />
              <stop offset="100%" style="stop-color:#a0a0a0;stop-opacity:1" />
            </linearGradient>
            <linearGradient id="hexGradientBlack" x1="0%" y1="0%" x2="100%" y2="100%">
              <stop offset="0%" style="stop-color:#505050;stop-opacity:1" />
              <stop offset="100%" style="stop-color:#202020;stop-opacity:1" />
            </linearGradient>
          </defs>
        </svg>
      ')
    ),

    div(class = "container-fluid",
        div(class = "game-info",
            h3(textOutput(ns("status_text"))),
            h4(textOutput(ns("score_text")))
        ),

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
      force_update = 0,
      moves_quiet = 0,
      position_history = list()
    )

    # === ФУНКЦИЯ ГЕНЕРАЦИИ HTML ФИГУРЫ ===
    generate_piece_html <- function(piece) {
      if (piece == 0) return("")

      color_class <- if (get_piece_owner(piece) == 1) "piece-white" else "piece-black"
      king_class <- if (piece > 2) " piece-king" else ""

      # Шестигранник SVG
      hexagon_svg <- '<div class="hexagon"><svg viewBox="0 0 100 100" xmlns="http://www.w3.org/2000/svg">
        <polygon points="50,5 93.3,27.5 93.3,72.5 50,95 6.7,72.5 6.7,27.5" />
      </svg></div>'

      sprintf(
        "<div class='piece-container'><div class='piece %s%s'>%s</div></div>",
        color_class, king_class, hexagon_svg
      )
    }

    # === ОПТИМИЗИРОВАННЫЙ OBSERVER ДЛЯ ОБНОВЛЕНИЯ ===
    observe({
      board <- game$board
      sel <- game$selected
      moves <- game$legal_moves
      trigger <- game$force_update

      shinyjs::delay(50, {
        for (r in 1:8) {
          for (c in 1:8) {
            is_black_cell <- (r + c) %% 2 != 0
            if (!is_black_cell) next

            cell_id <- ns(paste0("cell_", r, "_", c))
            cell_classes <- c("board-cell", "cell-dark")

            # Подсветка последнего хода
            if (!is.null(game$last_move)) {
              if (game$last_move$from[1] == r && game$last_move$from[2] == c) {
                cell_classes <- c(cell_classes, "last-move-from")
              } else if (game$last_move$to[1] == r && game$last_move$to[2] == c) {
                cell_classes <- c(cell_classes, "last-move-to")
              } else if (!is.null(game$last_move$detail)) {
                for (step in game$last_move$detail) {
                  if (step[1] == r && step[2] == c) {
                    cell_classes <- c(cell_classes, "last-move-path")
                    break
                  }
                }
              }
            }

            # Выбранная клетка
            if (!is.null(sel) && sel[1] == r && sel[2] == c) {
              cell_classes <- c(cell_classes, "selected")
            }

            # Подсветка возможных ходов
            is_target <- FALSE
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
                  if (m$to[1] == r && m$to[2] == c) {
                    is_target <- TRUE
                    break
                  }
                }
              }
            }

            if (is_path || is_target) {
              cell_classes <- c(cell_classes, "highlight")
            }

            # Генерация HTML фигуры
            piece <- board[r, c]
            piece_html <- generate_piece_html(piece)

            safe_html <- gsub("'", "\\\\'", piece_html)
            safe_html <- gsub('"', '\\\\"', safe_html)
            safe_html <- gsub("\n", "", safe_html)
            safe_classes <- paste(cell_classes, collapse = " ")

            # Обновление БЕЗ МЕРЦАНИЯ
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

    # === НОВАЯ ИГРА ===
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
      game$last_move <- NULL
      game$moves_quiet <- 0
      game$position_history <- list()
      game$force_update <- game$force_update + 1
      removeModal()
    })

    # === ИНИЦИАЛИЗАЦИЯ ПРИ ЗАГРУЗКЕ ===
    observe({
      isolate({
        game$legal_moves <- get_legal_moves(game$board, game$turn)
        game$force_update <- game$force_update + 1
      })
    }) %>%
      bindEvent(session$clientData$url_hostname, once = TRUE, ignoreInit = FALSE)

    # === КЛИК ПО ДОСКЕ ===
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
          game$last_move <- list(from = move_to_apply$from, to = move_to_apply$to,
                                 detail = move_to_apply$detail)
          game$selected <- NULL

          if (length(move_to_apply$captures) > 0) {
            game$moves_quiet <- 0
          } else {
            game$moves_quiet <- game$moves_quiet + 1
          }

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
            draw_check <- check_draw_conditions(game)
            if (draw_check$is_draw) {
              game$game_over <- TRUE
              game$winner <- "draw"
              game$score <- game$score + 0.5
              showModal(modalDialog(
                title = "Game Over",
                paste("Draw:", draw_check$reason),
                footer = modalButton("Close")
              ))
            } else {
              next_player <- get_opponent(game$turn)
              status <- check_game_state(game$board, next_player)

              if (status != "active") {
                game$game_over <- TRUE
                game$winner <- status

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
                game$turn <- next_player
                game$legal_moves <- get_legal_moves(game$board, next_player)

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

    output$status_text <- renderText({
      if (game$game_over)
        return(get_localization(paste0("status_", game$winner)))
      if (game$turn == 1) get_localization("status_white_turn")
      else get_localization("status_black_turn")
    })

    output$score_text <- renderText({
      paste(get_localization("score_label"), ": ", game$score[1], " - ", game$score[2])
    })

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
