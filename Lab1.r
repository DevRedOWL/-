# ================================
#           Ввод и вывод
# ================================

# Вспомогательные методы
validate <- function(method, format = "default") 
{
    # Залепил крутейший метод, который будет проверять входные данные на валидность просто потому что могу
    repeat{     
        switch(
            format,
            "inputName" = { input = readline(prompt = "Введите ваше имя: ")},
            "clearNumeric" = { input = sub(",", ".", readline(prompt = "Введите дробное число: ")) },
            "vehicleSpeed" = { input =  readline(prompt = "Введите скорость целым числом в км/ч: ") },
            "default" = { input = readline(prompt = "Введите строку: ") }
        )

        result <- method(input)  
        if(!is.na(result)){           
            return(result)
        }
    }
}

# Задание 1.1
task11 <- function(){
    name <- validate(as.character, "inputName")

    cat(sprintf("Hello, %s!", name))
}

# Задание 1.2
task12 <- function(){
    # Можно пытаться ввести даже строку, все равно прога не даст
    num1 <- validate(as.numeric, "clearNumeric")
    num2 <- validate(as.numeric, "clearNumeric")

    cat("Сумма чисел =", num1 + num2);
}

# Задание 1.3
task13 <- function(){
    spd <- validate(as.integer, "vehicleSpeed")

    cat("Преобразованная скорость =", (spd*1000)/3600, "м/с")
}

# ================================
#             Векторы
# ================================

# Задание 2.1
task21 <- function(){

}

# Задание 2.2
task22 <- function(){

}

# Задание 2.3
task23 <- function(){

}

# Задание 2.4
task24 <- function(){

}

# Задание 2.5
task25 <- function(){

}

# Задание 2.6
task26 <- function(){

}

# Задание 2.7
task27 <- function(){

}

# Метод запуска задания
startTask <- function(arg){
    shell("cls");
    switch(
      arg, 
      "1.1" = task11(),
      "1.2" = task12(),
      "1.3" = task13(),
      "2.1" = task21(),
      "2.2" = task22(),
      "2.3" = task23(),
      "2.4" = task24(),
      "2.5" = task25(),
      "2.6" = task26(),
      "2.7" = task27(),
    )
    cat('\n');
}

# Непосредственно запуск задания
startTask("1.1")