get.error <- function(diagnosis,pred){
  cont.tab <- table(diagnosis,pred)
  print(cont.tab)
  return((cont.tab[2,1]+cont.tab[1,2])/(sum(cont.tab)))
}

get.sensitivity <- function(diagnosis,pred){
  cont.tab <- table(diagnosis,pred)
  return((cont.tab[2,2])/(sum(cont.tab[2,])))
}


get.specificity <- function(diagnosis,pred){
  cont.tab <- table(diagnosis,pred)
  return((cont.tab[1,1])/(sum(cont.tab[1,])))
}

require("ISLR")
require("class")
require("MASS")
require("rpart.plot")
require("rpart")
require("readxl")

d <- read_excel("2021-2022 M2 INSFA/Projet Shiny/data.xlsx", 
                col_types = c("text", "text", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", 
                              "numeric", "numeric"))
dt <- data.frame(d[,-c(1,2)])
dt <- data.frame(scale(dt, center = TRUE, scale = TRUE))
summary(dt)

data <- cbind("diagnosis"=d$diagnosis,dt)
summary(data)


rpart(diagnosis~.,data=data)


rpart.plot(rpart(diagnosis~.,data=data))


############################
di <- data.frame(d[,c(1,2)]) ; class(di)
dv <- data.frame(d[,3:32]) ; class(dv)

dv <- data.frame(scale(dv, center = TRUE, scale = TRUE))
summary(dv)
class(dv)

cv = ncol(dv)
lv = nrow(dv)
############################


shapiro.test(dv)

##### VISUALITAION ####
names(d)

boxplot(diagnosis~., data =d)

boxplot(radius_mean~diagnosis, data = d)
boxplot(texture_mean~diagnosis, data = d)
boxplot(perimeter_mean~diagnosis, data = d)
boxplot(area_mean~diagnosis, data = d)
boxplot(smoothness_mean~diagnosis, data = d)
boxplot(compactness_mean~diagnosis, data = d)
boxplot(concavity_mean~diagnosis, data = d)
boxplot(concavepoints_mean~diagnosis, data = d)
boxplot(symmetry_mean~diagnosis, data = d)
boxplot(fractal_dimension_mean~diagnosis, data = d)
boxplot(radius_se~diagnosis, data = d)
boxplot(texture_se~diagnosis, data = d)
boxplot(perimeter_se~diagnosis, data = d)
boxplot(area_se~diagnosis, data = d)



#### TEST/TRAIN ####
n <- nrow(data)
n.train <- n/4
n.test <- n-n.train
ind.train <- sample(1:nrow(data),n.train)
data.train <- data[ind.train,]
data.test <- data[-ind.train,]

##### LDA #####
mod.lda <- lda(diagnosis ~ .,data=data.train)
pred.lda.test <- predict(mod.lda,newdata=data.test)$class # la fonction predict detecte le type de l'objet "mod.lda" et adapte sa fonction
get.error(data.test$diagnosis,pred.lda.test) # fonctions implémentées (voir haut du code)
get.specificity(data.test$diagnosis,pred.lda.test)
get.sensitivity(data.test$diagnosis,pred.lda.test)

#### QDA ####
mod.qda <- qda(diagnosis ~ .,data=data.train)
pred.qda.test <- predict(mod.qda,newdata=data.test)$class
get.error(data.test$diagnosis,pred.qda.test) # le modèle qda prédit un peu plus de positif que lda
get.specificity(data.test$diagnosis,pred.qda.test)
get.sensitivity(data.test$diagnosis,pred.qda.test)