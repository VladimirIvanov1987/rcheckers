#' Dictionary for rcheckers
#' @keywords internal
i18n_dict <- list(
  # --- НИЧЬЯ И AI (DRAW & AI) ----
  msg_ai_draw_accept = list(
    en = "AI accepted the draw.",
    ru = "Компьютер согласился на ничью.",
    cn = "电脑接受了和棋。"
  ),
  msg_ai_draw_refuse = list(
    en = "Computer refused the draw!",
    ru = "Компьютер отказался от ничьей!",
    cn = "电脑拒绝了和棋！"
  ),
  msg_draw_offer_title = list(
    en = "Draw Offer",
    ru = "Предложение ничьей",
    cn = "提议和棋"
  ),
  msg_offers_draw = list(
    en = "offers a draw.",
    ru = "предлагает ничью.",
    cn = "提议和棋。"
  ),
  msg_do_you_accept = list(
    en = "do you accept?",
    ru = "вы согласны?",
    cn = "你接受吗？"
  ),
  msg_draw_agreed = list(
    en = "Draw agreed!",
    ru = "Ничья принята!",
    cn = "同意和棋！"
  ),
  msg_ai_wins = list(
    en = "AI Wins!",
    ru = "Компьютер победил!",
    cn = "电脑获胜！"
  ),

  # --- НОВАЯ ИГРА (NEW GAME) ----
  app_title = list(
    en = "Checkers",
    ru = "Шашки",
    cn = "国际跳棋"
  ),
  label_mode = list(
    en = "Select Mode",
    ru = "Выберите режим",
    cn = "选择模式"
  ),
  mode_pvp = list(
    en = "Player vs Player",
    ru = "Игрок против Игрока",
    cn = "玩家对战"
  ),
  mode_pve = list(
    en = "Player vs AI",
    ru = "Игрок против ИИ",
    cn = "人机对战"
  ),

  # Кнопки (если btn_cancel и btn_start еще нет, добавь их)
  btn_start = list(
    en = "Start Game",
    ru = "Начать игру",
    cn = "开始游戏"
  ),
  btn_cancel = list(
    en = "Cancel",
    ru = "Отмена",
    cn = "取消"
  ),

  # --- ОШИБКИ И ПРАВИЛА (ERRORS & RULES) ----
  error_must_capture = list(
    en = "Move impossible. Capture is mandatory!",
    ru = "Ход невозможен. Взятие обязательно!",
    cn = "无法移动。必须要吃子！"
  ),
  msg_multi_jump = list(
    en = "Multi-jump required!",
    ru = "Нужно бить дальше!",
    cn = "必须连跳！"
  ),

  # --- РЕЗУЛЬТАТЫ (RESULTS) ----
  msg_draw_prefix = list( # Префикс для ничьей
    en = "Draw: ",
    ru = "Ничья: ",
    cn = "平局："
  ),
  status_white_won = list(
    en = "White won!",
    ru = "Белые победили!",
    cn = "白方获胜！"
  ),
  status_black_won = list(
    en = "Black won!",
    ru = "Черные победили!",
    cn = "黑方获胜！"
  ),

  # --- КНОПКИ (BUTTONS) ----
  btn_new_game = list(
    en = "New Game",
    ru = "Новая игра",
    cn = "新游戏"
  ),
  btn_surrender = list(
    en = "Surrender",
    ru = "Сдаться",
    cn = "投降"
  ),
  btn_offer_draw = list(
    en = "Offer Draw",
    ru = "Предложить ничью",
    cn = "提议和棋"
  ),
  btn_rules = list(
    en = "Rules",
    ru = "Правила",
    cn = "规则"
  ),
  btn_accept = list(
    en = "Accept",
    ru = "Принять",
    cn = "接受"
  ),
  btn_decline = list(
    en = "Decline",
    ru = "Отклонить",
    cn = "拒绝"
  ),
  btn_close = list(
    en = "Close",
    ru = "Закрыть",
    cn = "关闭"
  ),

  # --- СООБЩЕНИЯ (MODALS) ----
  msg_game_over = list(
    en = "Game Over",
    ru = "Игра окончена",
    cn = "游戏结束"
  ),
  msg_surrendered = list(
    en = "surrendered!",
    ru = "сдались!",
    cn = "投降！"
  ),
  score_label = list(
    en = "Score",
    ru = "Счет",
    cn = "比分"
  ),

  # --- НАЗВАНИЯ СТОРОН -----
  team_white = list(
    en = "White",
    ru = "Белые",
    cn = "白方"
  ),
  team_black = list(
    en = "Black",
    ru = "Черные",
    cn = "黑方"
  ),

  # --- ИГРОВОЙ СТАТУС -----
  status_white_move = list(
    en = "White's move",
    ru = "Ход белых",
    cn = "白方走棋"
  ),
  status_black_move = list(
    en = "Black's move",
    ru = "Ход черных",
    cn = "黑方走棋"
  ),
  status_game_over = list(
    en = "Game Over",
    ru = "Игра окончена",
    cn = "游戏结束"
  ),
  status_won = list(
    en = "won!",
    ru = "победили!",
    cn = "获胜!"
  ),
  status_draw = list(
    en = "Draw!",
    ru = "Ничья!",
    cn = "平局!"
  ),

  # --- СЧЕТ И ИНФО ----
  score_pattern = list( # Используем sprintf формат
    en = "Score (W:B) %d : %d",
    ru = "Счет (Б:Ч) %d : %d",
    cn = "比分 (白:黑) %d : %d"
  ),

  # --- ИНТЕРФЕЙС ----
  lbl_language = list(
    en = "Language",
    ru = "Язык",
    cn = "语言"
  ),
  # --- ПРИЧИНЫ НИЧЬЕЙ (DRAW REASONS) ----
  draw_repetition = list(
    en = "Threefold repetition",
    ru = "Троекратное повторение позиции",
    cn = "三次重复局面"
  ),
  draw_15_kings = list(
    en = "15 moves rule (Kings only)",
    ru = "Правило 15 ходов (только дамки)",
    cn = "15步规则（仅王棋）"
  ),
  draw_15_adv = list(
    en = "15 moves rule (3+ vs 1)",
    ru = "Правило 15 ходов (3+ против 1)",
    cn = "15步规则（3子对1子）"
  ),
  draw_moves_limit = list(
    en = "moves rule",
    ru = "ходов без взятия",
    cn = "步规则"
  )
)

#' Get translation helper
#' @param key String key from dictionary
#' @param lang Language code ("en", "ru", "cn")
#' @noRd
tr <- function(key, lang = "en") {
  # Проверка на существование ключа
  if (is.null(i18n_dict[[key]])) {
    return(key)
  }

  # Попытка найти перевод
  res <- i18n_dict[[key]][[lang]]

  # Фоллбэк на английский, если перевода нет
  if (is.null(res)) res <- i18n_dict[[key]][["en"]]

  return(res)
}
