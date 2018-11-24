# Autor: Iryna Mikutskaya

setwd("E:/med")

library(caret)

fknn <- function(trainData1, test_one, col_val, k){
  
  error <-0
  miary<-0
  miary[1:4] <- 0
  
  test_matrix<-trainData1
  test_matrix[,1:col_val]=0
  test_matrix[,1:col_val] = lapply(test_one[1,1:col_val], function (x)
  {
    x  
  })
  
  data_difference=trainData1 # tworzymy zbior danych dla przechowywznia ruznic
  data_difference[,1:col_val]=0 
  
  data_difference$distance <- 0 # dodajemy pole odleglosci
  
  # ------------------------------ obliczenie odleglosci --------------------------
  
  # vyliczmy ruznice miedzy wartosciami parametrow tostowej probki i zbioru treningowego, i od razu podnosimy do potegi 2
  data_difference[,1:col_val] <- (test_matrix[,1:col_val] - trainData1[,1:col_val])^2
  
  # obliczamy odleglosc od testowej prubki do kazdego obiektu ze zbioru treningowego wedlug wzoru odleglosci Euklidesowej
  data_difference$distance <- sqrt(rowSums(data_difference[,1:col_val]))
  
  # ----------------------- sortowanie wedlug odleglosci rosnaco ------------------
  
  data_difference <- data_difference[order(data_difference$distance),]
  
  # ---------------------- wybieramy k najblizszych sasiadow ----------------------
  
  data_k <- data_difference[1:k,]
  
  # ------------------------- pszydzielenie obiektu do klasy ----------------------
  
  # tworzymy wektor wystapien kazdej klasy
  kol_wystapen <- 0
  wystapione_klasy <- levels(as.factor(data_k[,(ncol(data_k))-1])) # wektor z nazwami klas
  
  # liczenie ilosci wystapien kazdej klasy
  for (i in 1:length(wystapione_klasy)){
    kol_wystapen[i] <- nrow(subset(data_k, data_k[,(ncol(data_k))-1]==wystapione_klasy[i]))
  }
  names(kol_wystapen) <- wystapione_klasy
  
  m <- which.max(kol_wystapen)
  
  predict <- wystapione_klasy[m]
  
  if (predict != test_one[,ncol(test_one)])
  {
    error <- 1
  }
  
  return(error)
  
}


###------------------------------------------------------------------------------###
###                           testowanie na zbiorze pima-indians-diabetes.data   ###
###------------------------------------------------------------------------------###

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data', 'pima-indians-diabetes.data')
pid_ds = read.csv("pima-indians-diabetes.data", header = FALSE, col.names = c('Number.pregnant', 'Plasma.glucose', 'Diastolic.blood.pressure', 'Triceps.skin.fold.thickness', 'serum.insulin', 'Body.mass.index', 'Diabetes.pedigree.function', 'Age', 'Class') )

str(pid_ds)
summary(pid_ds)
set.seed(1235)

col_val <- ncol(pid_ds)-1 # ilosc kolumn z wartosciami do obliczenia

miary<-0
miary[1:4] <- 0
names(miary) <- c("TP(PP)", "FN(PN)", "TN(NN)", "FP(NP)")

n <-5 # ilosc walidacji

accuracy <- 0
accuracy[1:n] <- 0
czulosc <- 0
czulosc[1:n] <- 0
specyficznosc <- 0
specyficznosc[1:n] <- 0
F1_score <- 0
F1_score[1:n] <- 0


k <- 10  # ilosc sasiadow


# -------------------------- kroswalidacja -------------------------------
for (i in 0:(n-1)){
  
  trainData <- pid_ds[as.integer(rownames(pid_ds)) %% n != i,] #zbior treningory
  testData <- pid_ds[as.integer(rownames(pid_ds)) %% n == i,]  # zbior tetstowy
  
  test1=testData[,]  # testowanie na 10 obiektach
  miary[1:4] <- 0
  erro <- 0 # ilosc blednych klasyfikacji
  
  # ========== klasyfikacja na zbiorze testowym ============
  for (ind in 1:nrow(test1)){
    
    test_one=test1[ind,]
    
    err <- fknn(trainData, test_one, col_val, k)
    
    erro = erro+err
    
    if (test_one$Class == 0) 
    {
      if (err == 0) {miary[1] = miary[1] + 1}
      if (err == 1) {miary[2] = miary[2] + 1}
    }
    if (test_one$Class == 1)
    {
      if (err == 0) {miary[3] = miary[3] + 1}
      if (err == 1) {miary[4] = miary[4] + 1}
    }
  }
  
  # ========= miary oceny jakosci klasyfikacji ==============
  
  # dokladnosc Acc <- (TP+TN)/(TP+FN+FP+TN)
  ACC <- (miary[1]+miary[3])/sum(miary)
  accuracy[i+1] <- ACC
  
  # czulosc TPR =  TP/(TP+FN)
  TPR <- miary[1]/(miary[1]+miary[2])
  czulosc[i+1] <- TPR
  
  # specyficznosc TNR = TN/(FP+TN)
  TNR <- miary[3]/(miary[4]+miary[3])
  specyficznosc[i+1] <- TNR
  
  # miara F1 = 2(PPV*TPR)/(PPV+TPR) = 2TP/(2TP+FP+FN)
  F1 <- 2*miary[1]/(2*miary[1]+miary[4]+miary[2])
  F1_score[i+1] <- F1
  
}
# -----------------------------------------------------------------------

mean_accuracy <- mean(accuracy)
mean_accuracy
mean_czulosc <- mean(czulosc)
mean_czulosc
mean_specyficznosc <- mean(specyficznosc)
mean_specyficznosc
mean_F1_score <- mean(F1_score)
mean_F1_score


##############################################################################################

###------------------------------------------------------------------------------###
###  testowanie na zbiorze pima-indians-diabetes.data + skalowanie atrybutow   ###
###------------------------------------------------------------------------------###

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/pima-indians-diabetes/pima-indians-diabetes.data', 'pima-indians-diabetes.data')
pid_ds = read.csv("pima-indians-diabetes.data", header = FALSE, col.names = c('Number.pregnant', 'Plasma.glucose', 'Diastolic.blood.pressure', 'Triceps.skin.fold.thickness', 'serum.insulin', 'Body.mass.index', 'Diabetes.pedigree.function', 'Age', 'Class') )

pid_ds[,1:(ncol(pid_ds)-1)] <- scale(pid_ds[,1:(ncol(pid_ds)-1)])

str(pid_ds)
summary(pid_ds)
set.seed(1235)

col_val <- ncol(pid_ds)-1 # ilosc kolumn z wartosciami do obliczenia

miary<-0
miary[1:4] <- 0
names(miary) <- c("TP(PP)", "FN(PN)", "TN(NN)", "FP(NP)")

n <-5 # ilosc walidacji

accuracy <- 0
accuracy[1:n] <- 0
czulosc <- 0
czulosc[1:n] <- 0
specyficznosc <- 0
specyficznosc[1:n] <- 0
F1_score <- 0
F1_score[1:n] <- 0


k <- 10  # ilosc sasiadow

# -------------------------- kroswalidacja -------------------------------
for (i in 0:(n-1)){
  
  trainData <- pid_ds[as.integer(rownames(pid_ds)) %% n != i,] #zbior treningory
  testData <- pid_ds[as.integer(rownames(pid_ds)) %% n == i,]  # zbior tetstowy
  
  #pid_ds$group <- as.integer(rownames(pid_ds)) %% 10
  
  test1=testData[,]  # testowanie na 10 obiektach
  miary[1:4] <- 0
  erro <- 0 # ilosc blednych klasyfikacji
  
  # ========== klasyfikacja na zbiorze testowym ============
  for (ind in 1:nrow(test1)){
    
    test_one=test1[ind,]
    
    err <- fknn(trainData, test_one, col_val, k)
    
    erro = erro+err
    
    if (test_one$Class == 0) 
    {
      if (err == 0) {miary[1] = miary[1] + 1}
      if (err == 1) {miary[2] = miary[2] + 1}
    }
    if (test_one$Class == 1)
    {
      if (err == 0) {miary[3] = miary[3] + 1}
      if (err == 1) {miary[4] = miary[4] + 1}
    }
  }
  
  # ========= miary oceny jakosci klasyfikacji ==============
  
  # dokladnosc Acc <- (TP+TN)/(TP+FN+FP+TN)
  ACC <- (miary[1]+miary[3])/sum(miary)
  accuracy[i+1] <- ACC
  
  # czulosc TPR =  TP/(TP+FN)
  TPR <- miary[1]/(miary[1]+miary[2])
  czulosc[i+1] <- TPR
  
  # specyficznosc TNR = TN/(FP+TN)
  TNR <- miary[3]/(miary[4]+miary[3])
  specyficznosc[i+1] <- TNR
  
  # miara F1 = 2(PPV*TPR)/(PPV+TPR) = 2TP/(2TP+FP+FN)
  F1 <- 2*miary[1]/(2*miary[1]+miary[4]+miary[2])
  F1_score[i+1] <- F1
  
}
# -----------------------------------------------------------------------

mean_accuracy <- mean(accuracy)
mean_accuracy
mean_czulosc <- mean(czulosc)
mean_czulosc
mean_specyficznosc <- mean(specyficznosc)
mean_specyficznosc
mean_F1_score <- mean(F1_score)
mean_F1_score



################################################################################################



###------------------------------------------------------------------------------###
###               testowanie na zbiorze data_banknote_authentication.txt         ###
###------------------------------------------------------------------------------###

download.file('http://archive.ics.uci.edu/ml/machine-learning-databases/00267/data_banknote_authentication.txt', 'data_banknote_authentication.txt')
ba_ds = read.csv("data_banknote_authentication.txt", header = FALSE, col.names = c('variance', 'skewness', 'curtosis', 'entropy', 'class') )

str(ba_ds)
summary(ba_ds)
set.seed(1235)

col_val <- ncol(ba_ds)-1 # ilosc kolumn z wartosciami do obliczenia

miary<-0
miary[1:4] <- 0
names(miary) <- c("TP(PP)", "FN(PN)", "TN(NN)", "FP(NP)")

n <-5 # ilosc walidacji

accuracy <- 0
accuracy[1:n] <- 0
czulosc <- 0
czulosc[1:n] <- 0
specyficznosc <- 0
specyficznosc[1:n] <- 0
F1_score <- 0
F1_score[1:n] <- 0


k <- 10  # ilosc sasiadow

# -------------------------- kroswalidacja -------------------------------
for (i in 0:(n-1)){
  
  trainData <- ba_ds[as.integer(rownames(ba_ds)) %% n != i,] #zbior treningory
  testData <- ba_ds[as.integer(rownames(ba_ds)) %% n == i,]  # zbior tetstowy
  
  test1=testData[,]  # testowanie na 10 obiektach
  miary[1:4] <- 0
  erro <- 0 # ilosc blednych klasyfikacji
  
  # ========== klasyfikacja na zbiorze testowym ============
  for (ind in 1:nrow(test1)){
    
    test_one=test1[ind,]
    
    err <- fknn(trainData, test_one, col_val, k)
    
    erro = erro+err
    
    if (test_one$class == 0) 
    {
      if (err == 0) {miary[1] = miary[1] + 1}
      if (err == 1) {miary[2] = miary[2] + 1}
    }
    if (test_one$class == 1)
    {
      if (err == 0) {miary[3] = miary[3] + 1}
      if (err == 1) {miary[4] = miary[4] + 1}
    }
  }
  
  # ========= miary oceny jakosci klasyfikacji ==============

  # dokladnosc Acc <- (TP+TN)/(TP+FN+FP+TN)
  ACC <- (miary[1]+miary[3])/sum(miary)
  accuracy[i+1] <- ACC
  
  # czulosc TPR =  TP/(TP+FN)
  TPR <- miary[1]/(miary[1]+miary[2])
  czulosc[i+1] <- TPR
  
  # specyficznosc TNR = TN/(FP+TN)
  TNR <- miary[3]/(miary[4]+miary[3])
  specyficznosc[i+1] <- TNR
  
  # miara F1 = 2(PPV*TPR)/(PPV+TPR) = 2TP/(2TP+FP+FN)
  F1 <- 2*miary[1]/(2*miary[1]+miary[4]+miary[2])
  F1_score[i+1] <- F1
  
}
# -----------------------------------------------------------------------

mean_accuracy <- mean(accuracy)
mean_accuracy
mean_czulosc <- mean(czulosc)
mean_czulosc
mean_specyficznosc <- mean(specyficznosc)
mean_specyficznosc
mean_F1_score <- mean(F1_score)
mean_F1_score





