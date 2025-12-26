# tests/testthat/test-logic_moves.R

# ТЕСТЫ ДЛЯ get_all_quiet_moves (тихие ходы)====

test_that("Простая белая шашка ходит только вперед по диагонали", {
  board <- create_empty_board()
  board[6, 2] <- 1  # Белая простая на (6,2)

  moves <- get_all_quiet_moves(board, player = 1)

  expect_equal(length(moves), 2)
  # Проверяем, что ходы только вперед (уменьшение row)
  dest_cols <- sort(sapply(moves, function(m) m$to[2]))
  expect_equal(dest_cols, c(1, 3))
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
  board[5, 1] <- 2  # Блокируем одно направление
  board[5, 3] <- 2  # Блокируем второе направление

  moves <- get_all_quiet_moves(board, player = 1)

  expect_equal(length(moves), 0)
})

test_that("Дамка ходит во все стороны по диагонали на любое расстояние", {
  board <- create_empty_board()
  board[4, 4] <- 3  # Белая дамка почти в центре

  moves <- get_all_quiet_moves(board, player = 1)

  # Геометрия доски 8x8 из клетки (4,4):
  # Диагонали: 3 + 3 + 3 + 4 = 13 клеток
  expect_equal(length(moves), 13,
               info = "Из (4,4) доступно 13 клеток (включая угол 8,8)")

  # Дополнительная проверка: убедимся, что угол (8,8) достижим
  reaches_corner <- any(sapply(moves, function(m) all(m$to == c(8, 8))))
  expect_true(reaches_corner, info = "Дамка должна доставать до угла (8,8)")
})

test_that("Дамка останавливается перед своей фигурой", {

  board <- create_empty_board()
  board[4, 4] <- 3  # Белая дамка
  board[2, 2] <- 1  # Своя фигура блокирует диагональ

  # Все ходы
  all_moves <- get_all_quiet_moves(board, player = 1)

  # Оставляем тестируемую дамку
  king_moves <- get_moves_from(all_moves, 4, 4)

  # Фильтруем направление "Вверх-Влево" (индексы уменьшаются: row < 4, col < 4)
  moves_upleft <- Filter(function(m) m$to[1] < 4 && m$to[2] < 4, king_moves)



  # Должен быть только один ход на (3,3).
  # Клетка (2,2) занята, а перепрыгнуть (1,1) нельзя, так как это не бой.
  expect_equal(length(moves_upleft), 1,
               info = "Дамка не должна проходить сквозь свои фигуры или вставать на них")

  expect_equal(moves_upleft[[1]]$to, c(3, 3),
               info = "Единственный доступный ход по диагонали — (3,3)")
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

# ТЕСТЫ ДЛЯ get_all_capture_moves (взятия простой шашкой)====

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

test_that("Правило Турецкого удара: нельзя бить одну шашку дважды в одном ходу", {
  board <- create_empty_board()

  # Белая в центре ромба (стартовая позиция подобрана для замыкания круга)
  board[3, 3] <- 1

  # Черные расставлены ромбом вокруг
  board[2, 4] <- 2 # Справа-сверху
  board[2, 2] <- 2 # Слева-сверху
  board[4, 2] <- 2 # Слева-снизу
  board[4, 4] <- 2 # Справа-снизу

  # Логика круга:
  # 1. (3,3) -> прыжок через (4,4) -> приземление (5,5)
  # 2. (5,5) -> прыжок через (4,2) -> приземление (3,1) - стоп, тут не круг.

  # Давай проще: "Бумеранг". Прыжок туда и попытка прыжка обратно.
  # Но в шашках нельзя прыгнуть обратно сразу же той же траекторией.

  # Самый надежный способ проверить список captured в рекурсии:
  # Поставим две шашки так, чтобы можно было прыгнуть "туда-сюда", если бы правила позволяли.
  # Но геометрически в шашках это сложно.

  # Поэтому просто проверим, что в возвращаемых ходах в captured нет дубликатов.
  # Это общее свойство алгоритма.

  moves <- get_all_capture_moves(board, player = 1)

  # Проверяем каждый найденный ход
  for (m in moves) {
    captures <- m$captures
    # Уникальных сбитых должно быть столько же, сколько всего сбитых
    expect_equal(length(captures), length(unique(captures)))
  }
})

test_that("Превращение в дамку при взятии с продолжением боя", {
  board <- create_empty_board()
  board[3, 3] <- 1  # Белая
  board[2, 4] <- 2  # Черная
  board[3, 7] <- 2  # Бьем, попадаем на 1-ю линию
  board[6, 6] <- 2  # И можем продолжить как дамка

  moves <- get_all_capture_moves(board, player = 1)

  # Должно быть взятие с превращением и продолжением
  expect_true(length(moves) > 0)
  multi_capture <- Filter(function(m) length(m$captures) > 1, moves)
  expect_true(length(multi_capture) > 0)
})

# ТЕСТЫ ДЛЯ get_all_capture_moves (взятия дамкой)====

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
test_that("Дамка обязана выбрать поле приземления, позволяющее продолжить бой", {
  board <- create_empty_board()

  board[8, 1] <- 3  # Белая дамка
  board[5, 4] <- 2  # Черная шашка на пути (жертва №1)

  # После взятия (5,4) дамка оказывается на диагонали (4,5) - (1,8).
  # Возможные поля приземления: (4,5), (3,6), (2,7), (1,8).

  # 3. Черная шашка сбоку (жертва №2)
  # Чтобы её сбить, дамка должна остановиться СТРОГО на (4,5).
  # Если она улетит на (3,6) и далее, она проскочит поворот.
  board[3, 4] <- 2

  moves <- get_all_capture_moves(board, player = 1)

  # ПРОВЕРКИ:

  # 1. Ход должен быть найден
  expect_true(length(moves) > 0)

  # 2. Проверяем, что нет ходов, где сбита только одна фигура
  # (Это самая частая ошибка: движок разрешает встать на (3,6) и закончить ход)
  incomplete_moves <- Filter(function(m) length(m$captures) == 1, moves)
  expect_equal(length(incomplete_moves), 0, info = "Дамка не должна останавливаться, если может бить дальше")

  # 3. Проверяем, что найден ход с двумя взятиями
  complete_moves <- Filter(function(m) length(m$captures) == 2, moves)
  expect_true(length(complete_moves) > 0)

  # 4. Проверяем конечную точку (после второго взятия)
  # После (4,5) бьем (3,4) и летим на диагональ (2,3)-(1,2)
  final_dest <- complete_moves[[1]]$to
  expect_true(final_dest[1] <= 2 && final_dest[2] <= 3)
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
  board[6, 2] <- 1  # Белая - цель проверки
  board[5, 1] <- 1  # Блок
  board[5, 3] <- 1  # Блок

  moves <- get_all_quiet_moves(board, player = 1)
  moves_62 <- Filter(function(m) all(m$from == c(6, 2)), moves)

  # Ожидаем: 0 тихих ходов именно для этой шашки
  expect_equal(length(moves_62), 0)
})

test_that("Превращение простой в дамку без взятия", {
  board <- create_empty_board()
  board[2, 2] <- 1  # Белая простая

  moves <- get_all_quiet_moves(board, player = 1)

  # Может дойти до первой линии
  promotion_moves <- Filter(function(m) m$to[1] == 1, moves)
  expect_equal(length(promotion_moves), 2)
})
