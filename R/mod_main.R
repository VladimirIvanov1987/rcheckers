# Основной модуль игры ====

#' @import shiny
#' @import htmltools
NULL

#' Game Module UI
#'
#' @param id Module ID
#'
#' @export
rcheckers_ui <- function(id) {
  ns <- NS(id)

  # CSS для доски и фигур
  css <- "
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
    font-size: 32px; /* Размер шашки */
    user-select: none;
  }
  .cell-light { background-color: #f0d9b5; }
  .cell-dark { background-color: #b58863; }

  .piece { transition: transform 0.1s; }
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
    shinyjs::useShinyjs(), # <--- ВАЖНО: Добавляем активацию shinyjs
    tags$head(tags$style(HTML(css))),

    tags$head(tags$style(HTML(css))),

    div(class = "container-fluid",
        div(class = "game-info",
            h3(textOutput(ns("status_text"))),
            h4(textOutput(ns("score_text")))
        ),

        # Сама доска
        uiOutput(ns("board_ui")),

        # Кнопки управления
        div(class = "controls",
            actionButton(ns("btn_new_game"), get_localization("btn_new_game")),
            actionButton(ns("btn_surrender"), get_localization("btn_surrender")),
            actionButton(ns("btn_offer_draw"), get_localization("btn_offer_draw"))
        ),

        # Скрытые инпуты для взаимодействия
        # Мы будем генерировать actionLink'и, которые сами будут слать инпуты,
        # либо используем JS. Для простоты - actionButton внутри renderUI.
    )
  )
}

#' Game Module Server
#'
#' @param id Module ID
#'
#' @export
rcheckers_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # --- State ----
    # Состояние игры
    game <- reactiveValues(
      board = init_board(),
      turn = 1,              # 1=White, 2=Black
      selected = NULL,       # c(r, c) or NULL
      legal_moves = NULL,    # List of legal moves for current player
      game_over = FALSE,
      winner = NULL,
      score = c(0, 0),       # White, Black
      mode = "pvp"           # "pvp" or "pve"
      # TODO: Добавить историю для "отмены хода" если нужно
    )

    # --- Initialization ----
    observeEvent(input$btn_new_game, {
      # Сброс
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

      # Предрасчет ходов для первого игрока
      game$legal_moves <- get_legal_moves(game$board, game$turn)
      removeModal()
    })

    # --- Helper: Render Board Grid ----
    # Мы рендерим сетку один раз (структуру), но контент обновляем реактивно.
    # Но так как 'renderUI' перерисовывает все целиком, сделаем это умно.
    # Создадим матрицу кнопок.

    output$board_ui <- renderUI({
      board <- game$board
      sel <- game$selected
      moves <- game$legal_moves

      # Сетка 8x8
      grid_items <- lapply(1:8, function(r) {
        lapply(1:8, function(c) {
          # Определяем цвет клетки
          is_black_cell <- (r + c) %% 2 != 0
          cell_class <- if (is_black_cell) "cell-dark" else "cell-light"

          # Фигура
          piece <- board[r, c]
          piece_html <- ""
          if (piece != 0) {
            color_class <- if (get_piece_owner(piece) == 1) "piece-white" else "piece-black"
            king_class <- if (piece > 2) " piece-king" else ""
            symbol <- "●" # Unicode Circle
            piece_html <- span(class = paste("piece", color_class, king_class), symbol)
          }

          # Подсветка
          # 1. Если клетка выбрана
          if (!is.null(sel) && sel[1] == r && sel[2] == c) {
            cell_class <- paste(cell_class, "selected")
          }

          # 2. Если это валидный ход (Target)
          is_target <- FALSE
          if (!is.null(sel)) {
            # Ищем, есть ли ход из sel в текущую (r,c)
            for (m in moves) {
              if (m$from[1] == sel[1] && m$from[2] == sel[2] &&
                  m$to[1] == r && m$to[2] == c) {
                is_target <- TRUE
                break
              }
            }
          }
          if (is_target) {
            cell_class <- paste(cell_class, "highlight")
          }

          # Создаем кликабельный элемент
          # actionButton слишком тяжелый, используем div + onclick + shiny input
          id_val <- paste0("cell_", r, "_", c)

          # Если это светлая клетка - она неактивна
          if (!is_black_cell) {
            div(class = paste("board-cell", cell_class))
          } else {
            # Мы используем onclick чтобы послать input
            # Input будет вида: {id: "click_board", val: "r_c"}
            onclick_str <- sprintf("Shiny.setInputValue('%s', '%d_%d', {priority: 'event'})",
                                   ns("board_click"), r, c)

            div(class = paste("board-cell", cell_class),
                onclick = onclick_str,
                piece_html)
          }
        })
      })

      # Flatten list and wrap in container
      div(class = "board-container", grid_items)
    })

    # --- Game Logic: Click Handler ----
    observeEvent(input$board_click, {
      if (game$game_over) return()

      # Парсим координаты "r_c"
      coords <- as.numeric(strsplit(input$board_click, "_")[[1]])
      r <- coords[1]
      c <- coords[2]

      clicked_owner <- get_piece_owner(game$board[r, c])

      # Логика клика:

      # Сценарий А: Кликнули по СВОЕЙ фигуре -> Выбор (Select)
      if (clicked_owner == game$turn) {
        # Проверяем, есть ли у этой фигуры ходы
        # (В русских шашках: если есть обязательное битье другой фигурой, эту выбрать нельзя)
        # Наш get_legal_moves уже отфильтровал всё лишнее.

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
          # Визуально можно показать ошибку "Есть обязательное взятие другой фигурой"
          showNotification(get_localization("error_illegal_move"), type = "warning", duration = 2)
        }
      }

      # Сценарий Б: Кликнули по ПУСТОЙ клетке (или врагу?) -> Попытка хода (Move)
      else if (!is.null(game$selected)) {
        # Пытаемся найти ход из selected в (r,c)
        move_to_apply <- NULL
        for (m in game$legal_moves) {
          if (m$from[1] == game$selected[1] && m$from[2] == game$selected[2] &&
              m$to[1] == r && m$to[2] == c) {
            move_to_apply <- m
            break
          }
        }

        if (!is.null(move_to_apply)) {
          # 1. Применяем ход
          game$board <- apply_move(game$board, move_to_apply)
          game$selected <- NULL # Сброс выбора

          # 2. Проверка на МУЛЬТИ-ВЗЯТИЕ (Цепочка)
          # В русских шашках: если мы били, и той же фигурой можно бить дальше -> ход не переходит!
          multi_jump_available <- FALSE

          if (length(move_to_apply$captures) > 0) {
            # Проверяем, может ли эта фигура (теперь она на to) бить дальше
            # Важно: мы передаем "to" как стартовую позицию и проверяем только взятия
            next_captures <- get_all_capture_moves(game$board, game$turn)

            # Фильтруем: нас интересуют только взятия именно ЭТОЙ фигурой
            this_piece_captures <- list()
            for (nm in next_captures) {
              if (nm$from[1] == move_to_apply$to[1] && nm$from[2] == move_to_apply$to[2]) {
                this_piece_captures[[length(this_piece_captures) + 1]] <- nm
              }
            }

            if (length(this_piece_captures) > 0) {
              multi_jump_available <- TRUE
              game$legal_moves <- this_piece_captures
              game$selected <- move_to_apply$to # Авто-выбор фигуры для продолжения

              showNotification("Multi-jump required!", type = "message")
            }
          }

          # 3. Передача хода (если нет серии)
          if (!multi_jump_available) {
            next_player <- get_opponent(game$turn)

            # Проверка победы (есть ли ходы у следующего?)
            status <- check_game_state(game$board, next_player)

            if (status != "active") {
              game$game_over <- TRUE
              game$winner <- status
              # Обновляем счет
              if (status == "white_won") game$score[1] <- game$score[1] + 1
              else game$score[2] <- game$score[2] + 1

              showModal(modalDialog(
                title = "Game Over",
                paste(get_localization(paste0("status_", status))),
                footer = modalButton("Close")
              ))
            } else {
              # Смена хода
              game$turn <- next_player
              game$legal_moves <- get_legal_moves(game$board, next_player)

              # --- AI TURN TRIGGER ---
              if (game$mode == "pve" && game$turn == 2) {
                # Задержка для реализма (не блокирующая в идеале, но тут через invalidateLater сложно)
                # Просто вызовем обработку AI
                shinyjs::delay(500, run_ai_turn()) # Нужен shinyjs useShinyjs() в UI
              }
            }
          }
        }
      }
    })

    # --- AI Logic Execution ----
    # Вынесем в отдельную reactive или функцию внутри сервера
    run_ai_turn <- function() {
      # Защита от бесконечного цикла
      if (game$game_over || game$turn != 2) return()

      ai_move <- get_ai_move(game$board, 2) # Из logic_ai.R

      if (!is.null(ai_move)) {
        game$board <- apply_move(game$board, ai_move)

        # Проверка мульти-джампа для AI (пока упростим: AI в нашей логике get_ai_move
        # должен возвращать полные цепочки? Нет, мы реализовали пошаговость).
        # TODO: Реализовать логику мульти-джампа для AI (аналогично игроку).
        # Для простоты MVP AI делает 1 шаг. Доработка: цикл while can_capture.

        next_player <- 1
        status <- check_game_state(game$board, next_player)

        if (status != "active") {
          game$game_over <- TRUE
          game$winner <- status
          game$score[2] <- game$score[2] + 1
          showModal(modalDialog(title = "Game Over", "AI Wins!", footer = modalButton("Close")))
        } else {
          game$turn <- next_player
          game$legal_moves <- get_legal_moves(game$board, next_player)
        }
      }
    }

    # --- UI Outputs ----
    output$status_text <- renderText({
      if (game$game_over) return(get_localization(paste0("status_", game$winner)))
      if (game$turn == 1) get_localization("status_white_turn") else get_localization("status_black_turn")
    })

    output$score_text <- renderText({
      paste(get_localization("score_label"), ": ", game$score[1], " - ", game$score[2])
    })

    # --- Surrender Logic ---
    observeEvent(input$btn_surrender, {
      if (game$game_over) return()

      # Сдается тот, чей сейчас ход (или тот, кто нажал кнопку, но в локальном режиме это одно и то же)
      loser <- game$turn
      winner <- get_opponent(loser)

      game$game_over <- TRUE
      game$winner <- if (winner == 1) "white_won" else "black_won"

      # Обновляем счет
      if (winner == 1) game$score[1] <- game$score[1] + 1 else game$score[2] <- game$score[2] + 1

      showModal(modalDialog(
        title = "Game Over",
        paste0("Player ", loser, " surrendered!"),
        footer = modalButton("Close")
      ))
    })

    # --- Offer Draw Logic ---
    observeEvent(input$btn_offer_draw, {
      if (game$game_over) return()

      current_player <- game$turn

      if (game$mode == "pve" && current_player == 1) {
        # Логика AI для ничьей:
        # Соглашается, если у него меньше или поровну фигур.
        # Отказывается, если у него преимущество.

        ai_pieces <- sum(game$board == 2 | game$board == 4)
        player_pieces <- sum(game$board == 1 | game$board == 3)

        if (ai_pieces <= player_pieces) {
          game$game_over <- TRUE
          game$winner <- "draw"
          showModal(modalDialog(title = "Game Over", "AI accepted the draw.", footer = modalButton("Close")))
        } else {
          showNotification("Computer refused the draw!", type = "warning")
        }

      } else {
        # PvP режим: Спрашиваем второго игрока
        opponent_name <- if (current_player == 1) "Black" else "White"

        showModal(modalDialog(
          title = "Draw Offer",
          paste0("Player ", current_player, " offers a draw. ", opponent_name, ", do you accept?"),
          footer = tagList(
            actionButton(ns("btn_draw_accept"), "Accept"),
            modalButton("Decline")
          )
        ))
      }
    })

    # Обработка согласия на ничью (для PvP)
    observeEvent(input$btn_draw_accept, {
      removeModal()
      game$game_over <- TRUE
      game$winner <- "draw"
      showModal(modalDialog(title = "Game Over", "Draw agreed!", footer = modalButton("Close")))
    })
  })
}
