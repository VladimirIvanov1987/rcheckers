#' Game Module UI
#' @param id Module ID
#' @export
rcheckers_ui <- function(id) {
  ns <- NS(id)
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "assets/style.css")
  )


  tagList(
    shinyjs::useShinyjs(),
    # tags$head(tags$style(HTML(css))),
    shiny::includeCSS(system.file("www", "style.css", package = "rcheckers")),

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

    observeEvent(input$..., {
      if (currentPlayer == "white") {
        shinyjs::addClass("white-panel", "active")
        shinyjs::removeClass("black-panel", "active")
      } else {
        shinyjs::addClass("black-panel", "active")
        shinyjs::removeClass("white-panel", "active")
      }
    })

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
