# ================================
#      Устал декомпозировать
# ================================

# Задание 1
task1 <- function(){
    # С болью читаем чертов эксель
    library("readxl")
    path <- file.path("C:","R-epo","data-l4")
    setwd("C:\\Users\\vdape\\Desktop\\ВШЭ\\2 курс\\Не рычи на меня\\data-l4")
    df <- read_excel("AppleStore.xlsx")

    # Чистим от ид и валюты
    df2 <- subset(df, select = -c(id, currency)) 
    #View(df2)   

    # Изучаем структуру таблицы:
    cat(str(df2),"\n")
    # Единицей наблюдения является приложение в маркетплейсе AppleStore
    # Наблюдений 7187, переменных 7 и их сигнатуры выведет функция выше
    
    # Анализ суммарной статистики выбранных переменных
    df2sum <- summary(subset(df2, select = c(price, user_rating, lang_num, size_bytes)))
    print(df2sum)

    # Приложение с максимальным количеством языков
    cat("\nНазвание приложения с максимальным количеством языков:", as.character(subset(df2, lang_num == max(df2$lang_num))[1]),"\n")

    # Определение квантилей указанных переменных
    cat('Квантили:\n', quantile(df2$price),'\n',quantile(df2$user_rating),'\n',quantile(df2$lang_num),'\n')

    # Всякие там коэфициенты
    # install.packages(c("moments")) 
    library(moments)
    
    analysis <- data.frame()
    for(c in 1:length(df2))
      if(is.numeric(df2[[c]])){
        data <- data.frame(
          "Имя" = colnames(df2)[c],
          "Эксцесс" = kurtosis(df2[[c]]), 
          "Ассиметрия" = skewness(df2[[c]]), 
          "Коэфициент вариации" = sd(df2[[c]]) / mean(df2[[c]]) * 100
        )
        analysis <- rbind(analysis, data)
        # var() - Дисперсия, sd() - Станд. отклонение     
      }
    print(analysis)



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