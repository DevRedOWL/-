# ================================
#             Матрицы
# ================================

# Задание 1
task1 <- function(){
    
}

# Задание 2
task2 <- function(){
   
}

# Задание 3
task3 <- function(){
    
}
# Задание 4
task4 <- function(){
   
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
startTask("1")
# startTask("2")
# startTask("3")
# startTask("4")
# startTask("5")
# startTask("6")
# startTask("7")
# startTask("8")