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
  
}

# Задание 6
task6 <- function(){
    
}

# Задание 7
task7 <- function(){
   
}

# Задание 8
task8 <- function(){
   
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