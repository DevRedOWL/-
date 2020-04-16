# ================================
#             Матрицы
# ================================

# Задание 1
task1 <- function(){
    income <- 400000
    log_income <- log(income)
    income_pre <- 500000

    cat("Доход был больше в", ifelse(income > income_pre, "текущем", "предыдущем"), "месяце")
}

# Задание 2
task2 <- function(){
    x <- 2
    y <- 4

    z <- x
    x <- y
    y <- z

    cat('Значения x и y после обмена:', x, y)
}

# Задание 3
task3 <- function(){
    x <- 3.5
    y <- "2,6"
    z <- 1.78
    h <- TRUE

    cat("Типы переменных:",class(x), class(y), class(z), class(h))
}

# Задание 4
task4 <- function(){
    q <- c(4, 7, -1, 21, 2, 0, 14)

    q_sq <- q * q
    cat("Квадраты:",q_sq, '\n')

    q_log <- log(q)
    cat("Логарифмы:", q_log, '\n')
    # Логарифм не существует при нулевом или отрицательном основании
    
    cat("Неотрицательные значения:", q[q>=0], '\n')

    cat("Кратные семи:", q[q%%7==0], '\n')

    cat("Логарифмы, кратные 2 и больше 5:", q_log[q_log%%2==0 && q_log>5], '\n')
    # Кажется не существуют
}

# Задание 5
task5 <- function(){
    turnout <- c(100, 124, 121, 130, 150, 155, 144, 132, 189, 145, 125, 110, 118, 129, 127)

    strangeCount <- which(turnout%%5==0)
    cat("Индексы подозрительных участков:", strangeCount, "\n")
    
    result = round((length(strangeCount) / length(turnout)) * 100, 2)
    cat("Доля подозрительных участков:", result)
    cat("%")
}

# Задание 6
task6 <- function(){
    z <- c(8, NA, 7, 10, NA, 15, NA, 0, NA, NA, 87)
    
    cat("Индексы пропущенных элементов:",which(is.na(z)));
}

# Задание 7
task7 <- function(){
    s <- c("4,5", "6,8", "9,2", "1,75")

    s <- as.numeric(sub(",", ".", s));
    
    cat("Вектор s преобразован в", class(s))
}

# Задание 8
task8 <- function(){
    a <- c(1, 50, 1, 75)
    dim(a) <- c(2,2) 
    b <- c(100, 6625)
    cat("Решение системы:", solve(a, b), '\n')

    A <- c(1, 2, 3, 4, 2, 7, 6, 9, 3, 6, 3, 8, 4, 9, 8, 2)
    dim(A) <- c(4,4) 
    # Обратная матрица
    cat("Обратная матрица:\n")
    print(solve(A)) # -1 тоже можно
    cat("Транспонированная матрица:\n")
    print(t(A))
    cat("След матрицы:", sum(diag(A)),"\n")
    cat("Определитель матрицы:", det(A),"\n")
    cat("Алгебраичесткое дополнение A(2,3)",-det(A[-2,-3]))
}

# ================================
#          Запуск задания
# ================================

# Метод запуска заданияS
startTask <- function(arg){
    shell("cls");
    switch(
      arg, 
      "1" = task1(),
      "2" = task2(),
      "3" = task3(),
      "4" = task4(),
      "5" = task5(),
      "6" = task6(),
      "7" = task7(),
      "8" = task8()
    )
    cat('\n');
}

# Непосредственно запуск задания
# startTask("1")
# startTask("2")
# startTask("3")
# startTask("4")
# startTask("5")
# startTask("6")
# startTask("7")
# startTask("8")