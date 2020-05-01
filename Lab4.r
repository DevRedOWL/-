# ================================
#      Устал декомпозировать
# ================================

# Задание 1

# Метод для получения моды 
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

task1 <- function(){
    # С болью читаем чертов эксель
    library("readxl")
    # Пришлось сделать 2 пути, т.к. работаю с двух разных устройств
    pathPC <- file.path("C:","Projects","R-epo","data-l4")
    pathLT <- file.path("C:","Users","vdape","Desktop","ВШЭ","2 курс","Не рычи на меня","data-l4")
    setwd(pathPC)
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
    # Соглано данным о коэфициентах вариации, информация неоднородна
    # Эксцесс всех полей больше нуля, следовательно все графики островершинны
    # Коэфициенты ассиметрии всех графиков, кроме пользовательского рейтинга больше нуля, значит их левый хвост длиннее

    # Пойдем рисовать графики!
    library(ggplot2)
    wplot1 <- ggplot(df2, aes(x=size_bytes, y=factor(price))) + geom_boxplot()
    # Распределение более дешевых приложений хаотично
    wplot2 <- ggplot(df2, aes(x=price, y=factor(prime_genre))) + geom_boxplot()
    # Много приложений стоят до 25$ но редкие сущности могут иметь стоимость значительно выше, слишком дорогие неободимо исключать
    wplot3 <- ggplot(df2, aes(x=rating_count_tot, y=factor(user_rating))) + geom_boxplot()
    # Интересная получается ситуация, по графику можно сделать вывод, что, как правильно, большинство приложений
    # имеют достаточно низкое количество оцениваний и для объективной оценки взаимосвязи рейтинга и количества оцениваний
    # необходимо выбирать группы однородных данных
    wplot4 <- ggplot(df2, aes(x=user_rating, y=factor(prime_genre))) + geom_boxplot()
    wplot5 <- ggplot(df2, aes(x=lang_num, y=factor(user_rating))) + geom_boxplot()
    # Более дорогие приложения имеют большее число локализаций
    print(wplot5)

    # Пирожковые диаграммы
    pie(table(df2$user_rating), main="Не очень интересная диаграмма рейтингов")
    pie(table(df2$prime_genre), main="Не очень интересная диаграмма жанров")

    # Нормальное распределение
    hist(df2$size_bytes, freq = FALSE); curve(dnorm(x, mean(df2$size_bytes), sd = sd(df2$size_bytes)), add = TRUE)
    # Чаще встречаются приложения с маленьким весом
    hist(df2$price, freq = FALSE); curve(dnorm(x, mean(df2$price), sd = sd(df2$price)), add = TRUE)
    # Подавляющее число приложений - бесплатные
    hist(df2$rating_count_tot, freq = FALSE); curve(dnorm(x, mean(df2$rating_count_tot), sd = sd(df2$rating_count_tot)), add = TRUE)
    # Не так много приложений были оценены пользователями
    hist(df2$user_rating, freq = FALSE); curve(dnorm(x, mean(df2$user_rating), sd = sd(df2$user_rating)), add = TRUE)
    # Большинство отзывов колеблется на уровне 4.5 звезд, не ниже благодаря модерации магазина и политике Apple,
    # а не 5, вероятно из за психологической отметки не позволяющей поставить высший балл
    hist(df2$lang_num, freq = FALSE); curve(dnorm(x, mean(df2$lang_num), sd = sd(df2$lang_num)), add = TRUE)
    # Не часто приложения локализуются для более, чем 5 языков, но значительно реже встречается локализация 20 и более

    genreMode <- Mode(df2$prime_genre)
    cat("\nСамый популярный жанр:", genreMode, "\n\n")
    # Самым популярным жанром оказались игры

    # Создаем сабсет из самого популярного жанра
    library("dplyr")
    df3 <- filter(df2, prime_genre == genreMode) 
    df3sum <- summary(subset(df3, select = c(price, user_rating, lang_num, size_bytes)))
    print(df3sum)
    # Выводы:
    # Средняя цена остается схожей с полной выборкой, но медианная равна нулю долларов, 
    # так же самое дорогое приложение в 4 раза дешевле самого дорогого приложения в полной выборке
    # Со средним значением рейтинга так же ситуация, Медианное на 0.5 выше чем, в полной
    # Количество локализаций в играх в среднем меньше, чем в других приложениях, 
    # большинство игр имеют только 1 язык
    
    # Можно вывести статистику по группам в соответствии со столбцом
    # print(describeBy(df2$price, group = df2$prime_genre))
    
    KSGroups <- data.frame()
    for(genreGroup in levels(factor(df2$prime_genre))){
      # Делаем выборку из df2 по этому жанру
      thisFrame <- df2[df2$prime_genre == genreGroup,]
      # Находим среднее значение и стандартное отклонение
      thisMean <- mean(thisFrame$price)
      thisSd <- sd(thisFrame$price)
      # Считаем по Колмогорову-Смирнову
      result <- ks.test(thisFrame$price, "pnorm", thisMean, thisSd)
      
      # print(result)
      # print(names(result)[2])
      # print(result[[2]])

      # names(x)
      # for(KSPID in 1:length(result)){           
      #    Some <- cbind(Some, rbind(), result[[KSPID]])
      # } 
      # for(KSPID in 1:length(result)){           
      #   Some <- cbind(Some, rbind(), result[[KSPID]]))
      # } 
      # Some[names(result[1])] <- 1;
      # print(Some)

      # for(KSPID in 1:length(result)){     
      #   Some <- с(Some, t(c(names(result[KSPID]),result[[KSPID]])))
      #   # Some <- cbind(Some, rbind(), result[[KSPID]]))
      # } 
      
      KSGroups <- rbind(KSGroups, unlist(result, use.names = FALSE))
      colnames(KSGroups) <- names(result)

      # KSGroups <- rbind(KSGroups, data.frame(
      #   as.character(names(result[1])) = result[[1]]
      # ))

      
    }
    View(KSGroups)
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

# Метод запуска задания
clear <- function() cat(rep("\n", 50))
startTask <- function(arg){
    clear();
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