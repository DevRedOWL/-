# ================================
#      Устал декомпозировать
# ================================

# Задание 1
task1 <- function(){
    setwd("C:\Projects\R-epo\data-l4")

}

# Задание 2
task2 <- function(){
   
}

# Задание 3
task3 <- function(){
    
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
      "3" = task3()
    )
    cat('\n');
}

# Непосредственно запуск задания
startTask("1")
# startTask("2")
# startTask("3")