install.packages("ggplot2")
install.packages("purrr")
install.packages("randomForest")
install.packages("caret")

skrypt <- function(metoda,typ) {
  library(ggplot2)
  library(caret)
  college = read.csv("College.csv", header = TRUE, sep=",")
  college[,2] = (college[,2]=="Yes")*1
  #Sprawdzenie poprawnosci danych
  print(paste("Czy sa brakujace wartosci: ", any(is.na(college))))
  print(paste("Czy dane sa kompletne: ", all(complete.cases(college))))
  #Ziarno generatora liczb losowych
  numery_indeksow=303890
  set.seed(numery_indeksow)
  indeksy <- sample(2,nrow(college), replace = TRUE, prob=c(0.7,0.3))
  X <- college[,2:(ncol(college)-1)]
  y <- college[,ncol(college)]
  X <- scale(X)
  trainX <- X[indeksy==1,]
  testX <- X[indeksy==2,]
  trainY <- y[indeksy==1]
  testY <- y[indeksy==2]
  if(metoda=="knn" || metoda=="rf"){
    model <- train(trainX, trainY, method = metoda, ntree = 50)
    print(model)
    print("Zbior uczacy: ")
    y_pred <- predict(model,trainX)
    #Blad sredniokwadratowy (RMSE):
    RMSE <- caret::RMSE(y_pred,trainY)
    #Srednia bezwzglena roznica (MAE):
    MAE <- caret::MAE(y_pred,trainY)
    #Sredni procentowy blad (MAPE):
    MAPE <- mean(abs((y_pred-trainY)/trainY))*100
    print(paste("RMSE_train: ", RMSE))
    print(paste("MAE_train: ", MAE))
    print(paste("MAPE_train: ", MAPE, "%"))
    cat("\n")
    print("Zbior testowy: ")
    y_pred <- predict(model,testX)
    #Blad sredniokwadratowy (RMSE):
    RMSE <- caret::RMSE(y_pred,testY)
    #Srednia bezwzglena roznica (MAE):
    MAE <- caret::MAE(y_pred,testY)
    #Sredni procentowy blad (MAPE):
    MAPE <- mean(abs((y_pred-testY)/testY))*100
    print(paste("RMSE_test: ", RMSE))
    print(paste("MAE_test: ", MAE))
    print(paste("MAPE_test: ", MAPE, "%"))
    #Utworz ramke danych z obserwowanymi i przewidywanymi wartosciami
    dane <- data.frame(observed = testY, predicted = y_pred)
    #Wykres wartosci obserwowanych i przewidywanych
    ggp <- ggplot(dane, aes(x = observed, y = predicted)) + geom_point() + 
    geom_abline(intercept = 0, slope = 1)
    if(typ=="po") {
      show(ggp)
    } else if(typ=="chart") {
      #Ocena waznosci zmiennnych
      importance <- varImp(model)
      plot(importance)
      show(plot(importance))
      }
    } else if(metoda=="regresja"){
      train_ind <- createDataPartition(college$Grad.Rate, p = 0.7, list = FALSE)
      train_data <- college[train_ind, ]
      test_data <- college[-train_ind, ]
      y_train <- train_data$Grad.Rate
      x_train <- train_data[,c("Apps","Accept","Enroll","Top10perc","Top25perc","F.Undergrad","P.Undergrad","Outstate","Room.Board","Books","Personal","PhD","Terminal","S.F.Ratio","perc.alumni","Expend")]
      model <- lm(y_train ~., data = x_train)
      summary(model)
      y_test <- test_data$Grad.Rate
      x_test <- test_data[,c("Apps","Accept","Enroll","Top10perc","Top25perc","F.Undergrad", "P.Undergrad","Outstate","Room.Board","Books","Personal","PhD", "Terminal","S.F.Ratio","perc.alumni","Expend")]
      y_pred <- predict(model, newdata = x_test)
      print(y_pred)
      r_squared <- 1 - sum((y_test - y_pred)^2)/sum((y_test - mean(y_test))^2)
      r_squared
      library(ggplot2)
      ggp <- ggplot() + geom_point(aes(x = x_test$Apps, y = y_test), color = "blue") + geom_line(aes(x = x_test$Apps, y = y_pred), color = "red")
      show(ggp)
      library(caret)
      rmse_train <- sqrt(mean((y_train - predict(model, x_train))^2))
      rmse_test <- sqrt(mean((y_test - y_pred)^2))
      mae_train <- mean(abs(y_train - predict(model, x_train)))
      mae_test <- mean(abs(y_test - y_pred))
      mape_train <- mean(abs((y_train - predict(model, x_train))/y_train))*100
      mape_test <- mean(abs((y_test - y_pred)/y_test))*100
      print("Zbior uczacy: ")
      cat("RMSE_train:",rmse_train,"\n")
      cat("MAE_train:",mae_train,"\n")
      cat("MAPE_train:",mape_train,"%\n")
      cat("\n")
      print("Zbior testowy: ")
      cat("RMSE_test:",rmse_test,"\n")
      cat("MAE_test:",mae_test,"\n")
      cat("MAPE_test:",mape_test,"%\n")
    }
  cat("\n\nskrypt(\"knn\", \"po\"), skrypt(\"knn\", \"chart\"),
  skrypt(\"rf\", \"po\"), skrypt(\"rf\", \"chart\"),
      skrypt(\"regresja\")\n\n")
}

cat("\n\n
jezeli wyswietlil sie ten komunikat oznacza, ze wszystko dziala poprawnie

program przedstawia 2 algorytmy: k najblizszych sasiadow oraz drzewa losowe
    
uzycie programu w konsoli:
skrypt(\"knn\", \"po\"), skrypt(\"knn\", \"chart\"),
skrypt(\"rf\", \"po\"), skrypt(\"rf\", \"chart\"),
      skrypt(\"regresja\")\n\n")














