# tests/testthat/test-logic_moves.R

# library(testthat)

# Вспомогательная функция для создания пустой доски
create_empty_board <- function() {
  matrix(0, nrow = 8, ncol = 8)
}

# ============================================================================
# ТЕСТЫ ДЛЯ get_all_quiet_moves (тихие ходы)
# ============================================================================

test_that("Простая белая шашка ходит только вперед по диагонали", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая на (6,2)

  moves <- get_all_quiet_moves(board, player = 1)

  expect_equal(length(moves), 2)
  # Проверяем, что ходы только вперед (уменьшение row)
  expect_true(all(sapply(moves, function(m) m$to[1] == 5)))
})

test_that("Простая черная шашка ходит только вперед по диагонали", {
  board <- create_empty_board()
  board[3, 2] <- 2  # Черная простая на (3,2)

  moves <- get_all_quiet_moves(board, player = 2)

  expect_equal(length(moves), 2)
  # Проверяем, что ходы только вперед (увеличение row)
  expect_true(all(sapply(moves, function(m) m$to[1] == 4)))
})

test_that("Простая шашка не ходит, если клетки заняты", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая
  board[5, 1] <- 1  # Блокируем одно направление
  board[5, 3] <- 2  # Блокируем второе направление

  moves <- get_all_quiet_moves(board, player = 1)

  expect_equal(length(moves), 0)
})

test_that("Дамка ходит во все стороны по диагонали на любое расстояние", {
  board <- create_empty_board()
  board[4, 4] <- 3  # Белая дамка в центре

  moves <- get_all_quiet_moves(board, player = 1)

  # 4 направления, суммарно: 3+3+3+3 = 12 ходов
  expect_equal(length(moves), 12)
})

test_that("Дамка останавливается перед своей фигурой", {
  board <- create_empty_board()
  board[4, 4] <- 3  # Белая дамка
  board[2, 2] <- 1  # Своя фигура блокирует

  moves <- get_all_quiet_moves(board, player = 1)

  # В направлении (-1,-1) только 1 ход вместо 3
  moves_upleft <- Filter(function(m) m$to[1] < 4 && m$to[2] < 4, moves)
  expect_equal(length(moves_upleft), 1)
  expect_equal(moves_upleft[[1]]$to, c(3, 3))
})

test_that("Дамка останавливается перед чужой фигурой", {
  board <- create_empty_board()
  board[4, 4] <- 3  # Белая дамка
  board[2, 2] <- 2  # Чужая фигура блокирует

  moves <- get_all_quiet_moves(board, player = 1)

  moves_upleft <- Filter(function(m) m$to[1] < 4 && m$to[2] < 4, moves)
  expect_equal(length(moves_upleft), 1)
  expect_equal(moves_upleft[[1]]$to, c(3, 3))
})

# ============================================================================
# ТЕСТЫ ДЛЯ get_all_capture_moves (взятия простой шашкой)
# ============================================================================

test_that("Простая шашка бьет вперед", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая
  board[5, 3] <- 2  # Черная для битья

  moves <- get_all_capture_moves(board, player = 1)

  expect_equal(length(moves), 1)
  expect_equal(moves[[1]]$from, c(6, 2))
  expect_equal(moves[[1]]$to, c(4, 4))
  expect_equal(moves[[1]]$captures, list(c(5, 3)))
})

test_that("Простая шашка бьет назад", {
  board <- create_empty_board()
  board[4, 4] <- 1  # Белая простая
  board[5, 3] <- 2  # Черная сзади

  moves <- get_all_capture_moves(board, player = 1)

  expect_equal(length(moves), 1)
  expect_equal(moves[[1]]$to, c(6, 2))
  expect_equal(moves[[1]]$captures, list(c(5, 3)))
})

test_that("Простая шашка не бьет свою фигуру", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая
  board[5, 3] <- 1  # Своя фигура

  moves <- get_all_capture_moves(board, player = 1)

  expect_equal(length(moves), 0)
})

test_that("Простая шашка не бьет, если за противником нет места", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая
  board[5, 3] <- 2  # Черная
  board[4, 4] <- 1  # Место занято

  moves <- get_all_capture_moves(board, player = 1)

  expect_equal(length(moves), 0)
})

test_that("Последовательное взятие простой шашкой", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая
  board[5, 3] <- 2  # Первая жертва
  board[3, 5] <- 2  # Вторая жертва

  moves <- get_all_capture_moves(board, player = 1)

  expect_equal(length(moves), 1)
  expect_equal(moves[[1]]$to, c(2, 6))
  expect_equal(length(moves[[1]]$captures), 2)
})

test_that("Запрещено бить одну шашку дважды", {
  board <- create_empty_board()
  board[5, 5] <- 1  # Белая простая
  board[4, 4] <- 2  # Черная

  moves <- get_all_capture_moves(board, player = 1)

  # Можно побить только один раз
  expect_true(all(sapply(moves, function(m) length(m$captures) == 1)))
})

test_that("Превращение в дамку при взятии с продолжением боя", {
  board <- create_empty_board()
  board[2, 2] <- 1  # Белая простая
  board[1, 3] <- 2  # Черная на последней линии, будем бить через нее
  # На самом деле нужна такая позиция:
  board <- create_empty_board()
  board[3, 3] <- 1  # Белая
  board[2, 4] <- 2  # Бьем, попадаем на 1-ю линию
  board[1, 7] <- 2  # И можем продолжить как дамка

  moves <- get_all_capture_moves(board, player = 1)

  # Должно быть взятие с превращением и продолжением
  expect_true(length(moves) > 0)
  multi_capture <- Filter(function(m) length(m$captures) > 1, moves)
  expect_true(length(multi_capture) > 0)
})

# ============================================================================
# ТЕСТЫ ДЛЯ get_all_capture_moves (взятия дамкой)
# ============================================================================

test_that("Дамка бьет на расстоянии", {
  board <- create_empty_board()
  board[8, 2] <- 3  # Белая дамка
  board[5, 5] <- 2  # Черная на расстоянии

  moves <- get_all_capture_moves(board, player = 1)

  expect_true(length(moves) > 0)
  # Дамка может приземлиться на любое поле за противником
  expect_true(any(sapply(moves, function(m) m$captures[[1]][1] == 5 && m$captures[[1]][2] == 5)))
})

test_that("Дамка приземляется на разные поля за противником", {
  board <- create_empty_board()
  board[8, 2] <- 3  # Белая дамка
  board[6, 4] <- 2  # Черная

  moves <- get_all_capture_moves(board, player = 1)

  # Дамка может приземлиться на (5,5), (4,6), (3,7)
  landing_positions <- lapply(moves, function(m) m$to)
  expect_true(length(landing_positions) >= 3)
})

test_that("Дамка не может перепрыгнуть две фигуры подряд", {
  board <- create_empty_board()
  board[8, 2] <- 3  # Белая дамка
  board[6, 4] <- 2  # Первая черная
  board[5, 5] <- 2  # Вторая черная сразу за первой

  moves <- get_all_capture_moves(board, player = 1)

  # Не должно быть хода, бьющего обе фигуры в одном прыжке
  expect_equal(length(moves), 0)
})

test_that("Последовательное взятие дамкой", {
  board <- create_empty_board()
  board[8, 2] <- 3  # Белая дамка
  board[6, 4] <- 2  # Первая жертва
  board[3, 7] <- 2  # Вторая жертва

  moves <- get_all_capture_moves(board, player = 1)

  multi_captures <- Filter(function(m) length(m$captures) == 2, moves)
  expect_true(length(multi_captures) > 0)
})

test_that("Дамка может проходить через одно поле несколько раз", {
  # Сложная позиция, где дамка возвращается через то же поле
  board <- create_empty_board()
  board[4, 4] <- 3  # Белая дамка
  board[3, 3] <- 2  # Противник 1
  board[3, 5] <- 2  # Противник 2

  moves <- get_all_capture_moves(board, player = 1)

  # Проверяем, что есть последовательное взятие
  expect_true(length(moves) > 0)
})

# ============================================================================
# ТЕСТЫ ДЛЯ get_legal_moves (обязательное битье)
# ============================================================================

test_that("Обязательное битье: возвращаются только взятия", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая
  board[5, 3] <- 2  # Можно бить
  board[6, 4] <- 1  # Другая белая может тихо ходить

  moves <- get_legal_moves(board, player = 1)

  # Должны быть только взятия
  expect_true(all(sapply(moves, function(m) length(m$captures) > 0)))
})

test_that("Нет взятий: возвращаются тихие ходы", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая

  moves <- get_legal_moves(board, player = 1)

  # Только тихие ходы
  expect_true(all(sapply(moves, function(m) length(m$captures) == 0)))
})

test_that("Выбор направления взятия предоставлен игроку", {
  board <- create_empty_board()
  board[5, 5] <- 1  # Белая простая
  board[4, 4] <- 2  # Можно бить влево
  board[4, 6] <- 2  # Можно бить вправо

  moves <- get_legal_moves(board, player = 1)

  # Должно быть 2 варианта взятия
  expect_equal(length(moves), 2)
})

# ============================================================================
# ТЕСТЫ ГРАНИЧНЫХ СЛУЧАЕВ
# ============================================================================

test_that("Шашка на краю доски", {
  board <- create_empty_board()
  board[6, 1] <- 1  # Белая на левом краю

  moves <- get_all_quiet_moves(board, player = 1)

  # Только 1 ход (вправо-вверх)
  expect_equal(length(moves), 1)
})

test_that("Дамка в углу доски", {
  board <- create_empty_board()
  board[1, 1] <- 3  # Белая дамка в углу

  moves <- get_all_quiet_moves(board, player = 1)

  # Только 1 диагональ доступна
  expect_equal(length(moves), 7)
})

test_that("Нет легальных ходов", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая
  board[5, 1] <- 1  # Блок
  board[5, 3] <- 1  # Блок

  moves <- get_legal_moves(board, player = 1)

  expect_equal(length(moves), 0)
})

test_that("Превращение простой в дамку без взятия", {
  board <- create_empty_board()
  board[2, 2] <- 1  # Белая простая

  moves <- get_all_quiet_moves(board, player = 1)

  # Может дойти до первой линии
  promotion_moves <- Filter(function(m) m$to[1] == 1, moves)
  expect_equal(length(promotion_moves), 2)
})
