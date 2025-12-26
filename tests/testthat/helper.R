# Вспомогательная функция для создания пустой доски
create_empty_board <- function() {
  matrix(0, nrow = 8, ncol = 8)
}

# Функция фильтрует список ходов, оставляя только те,
# которые начинаются с клетки (row, col)
get_moves_from <- function(moves, row, col) {
  Filter(function(m) {
    # Проверяем, что координаты from совпадают
    # Предполагаем, что m$from это вектор c(r, c)
    m$from[1] == row && m$from[2] == col
  }, moves)
}
