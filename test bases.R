setwd("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/experimento")
remove(list=ls())
gc()

library(data.table)
library(dplyr)
library(beepr)
require("lightgbm")


marzo_3_123 <- fread("bases test/exp_TP_experimento_marzo3.csv.gz")
mayo_3_123 <- fread("bases test/exp_TP_experimento_mayo3.csv.gz")

marzo_6_1212 <- fread("bases test/exp_TP_experimento_marzo6.csv.gz")
mayo_6_1212 <- fread("bases test/exp_TP_experimento_mayo6.csv.gz")

marzo_3_1212 <- fread("bases test/exp_TP_experimento_marzo12.csv.gz")
mayo_3_1212 <- fread("bases test/exp_TP_experimento_mayo12.csv.gz")

left_join(marzo_3_123,marzo_3_1212, by=c("numero_de_cliente"))->marzo_3_12
left_join(mayo_3_123,mayo_3_1212, by=c("numero_de_cliente"))->mayo_3_12

left_join(marzo_3_12,marzo_6_1212, by=c("numero_de_cliente"))->marzo_3_12
left_join(mayo_3_12,mayo_6_1212, by=c("numero_de_cliente"))->mayo_3_12

remove(mayo_3_123,marzo_3_123,marzo_3_1212,mayo_3_1212, mayo_6_1212, marzo_6_1212)
gc()

marzo_ter <- fread("bases mes/M202103.csv.gz")
marzo_ter %>% select(numero_de_cliente, clase_ternaria)->marzo_ter
left_join(marzo_3_12,marzo_ter)->marzo_3_12

mayo_ter <- fread("bases mes/M202105.csv.gz")
mayo_ter %>% select(numero_de_cliente, clase_ternaria)->mayo_ter
left_join(mayo_3_12,mayo_ter)->mayo_3_12

remove(marzo_ter,mayo_ter)
gc()
pruebasv1 <- read.delim("C:/Users/lnfernandez/Desktop/posgrado/DM EyN/TP2_dmEyF/exp/pruebas v1/HT7231.txt")

maximo_marzo<-sum(marzo_3_12$clase_ternaria=="BAJA+2")*78000
maximo_maryo<-sum(mayo_3_12$clase_ternaria=="BAJA+2")*78000

#### train lgbm ####
# Clase BAJA+1 y BAJA+2 juntas
clase_binaria <- ifelse(marzo_3_12$clase_ternaria == "CONTINUA", 0, 1)
clase_real <- marzo_3_12$clase_ternaria
marzo_3_12$clase_ternaria <- NULL

dtrain  <- lgb.Dataset(data   = data.matrix(marzo_3_12),
                       label  = clase_binaria,
                       # Truco jedi!
                       weight = ifelse(clase_real == "BAJA+2", 1.0000001, 1.0))

# Veremos en detalle esta funciÃ³n un poco mÃ¡s adelante
ganancia_lgbm  <- function(probs, datos) {
  vlabels  <- get_field(datos, "label")
  vpesos   <- get_field(datos, "weight")
  
  tbl  <- as.data.table( list( "prob"= probs,
                               "gan" = ifelse( vlabels==1 & vpesos > 1,
                                               78000,
                                               -2000  ) ) )
  
  setorder( tbl, -prob )
  tbl[ , posicion := .I ]
  tbl[ , gan_acum :=  cumsum( gan ) ]
  
  gan  <-  tbl[ , max(gan_acum) ]
  
  pos  <- which.max(  tbl[ , gan_acum ] )
  # vpos_optima   <<- c( vpos_optima, pos )
  # vprob_optima  <<- c( vprob_optima, tbl[ pos, prob ] )
  
  rm( tbl )
  return( list( "name"= "ganancia",
                "value"=  gan,
                "higher_better"= TRUE ) )
}

for (semilla in 1:20) {
  set.seed(semilla)
  
  pruebasv1[pruebasv1$ganancia==max(pruebasv1$ganancia),]->maxgan
  
  model_lgbm_cv <- lgb.cv(data = dtrain,
                          eval = ganancia_lgbm,
                          stratified = TRUE,
                          nfold = 5,
                          param = list(objective = "binary",
                                       max_depth=(-1),
                                       max_bin = 31,
                                       min_data_in_leaf = maxgan$min_data_in_leaf,
                                       learning_rate = maxgan$learning_rate,
                                       feature_fraction=maxgan$feature_fraction,
                                       num_leaves=maxgan$num_leaves,
                                       num_iterations = round(maxgan$num_iterations*1.2,0), #ver esto
                                       seed=semilla
                          )
  )
  
  # Mejor iteración
  model_lgbm_cv$best_iter
  
  # Ganancia de la mejor iteración
  unlist(model_lgbm_cv$record_evals$valid$ganancia$eval)[model_lgbm_cv$best_iter]/maximo_marzo/0.2->gan_train
  
  
  # Una vez que elegimos los parámetros tenemos que entrenar con todos.
  model_lgm <- lightgbm(data = dtrain,
                        nrounds = model_lgbm_cv$best_iter, # <--- OJO! Double Descent alert
                        params = list(objective = "binary",
                                      max_depth=(-1),
                                      max_bin = 31,
                                      min_data_in_leaf = maxgan$min_data_in_leaf,
                                      learning_rate = maxgan$learning_rate,
                                      feature_fraction=maxgan$feature_fraction,
                                      num_leaves=maxgan$num_leaves),
                        verbose = -1)
  
  
  mayo_3_12$pred <- predict(model_lgm, data.matrix(mayo_3_12[, -c("clase_ternaria")]))
  
  mayo_3_12[order(mayo_3_12$pred, # Sequence of vectors of the same length
                decreasing = TRUE),]->mayo_3_12
  
  mayo_3_12[ 1:9000, Predicted := 1 ]
  mayo_3_12[ 9001:nrow(mayo_3_12), Predicted := 0 ]
  
  sum((mayo_3_12$Predicted ==1) *
        ifelse(mayo_3_12$clase_ternaria == "BAJA+2", 78000, -2000))/maximo_maryo->gan_test
  
  nuevaFila<-data.frame(semilla,train=round(gan_train*100,2),test=round(gan_test*100,2))
  
  if(semilla==1){
    baseF<-nuevaFila
  }else{
    baseF<-rbind(baseF, nuevaFila)
  }
  
  mayo_3_12$Predicted<-NULL
  mayo_3_12$pred<-NULL
  
  if(semilla==20){
    beep(sound = 1, expr = NULL)
  }
}


# También tiene su importancia de variables
lgb.importance(model_lgm, percentage = TRUE)->importancia

write.csv(importancia, "importancia_3_6_12.csv", row.names = FALSE)
write.csv(baseF, "ganancia_3_6_12.csv", row.names = FALSE)

mean(baseF$train)
mean(baseF$test)


