rm(list = ls())#Removendo todos os objetos
ls()#Área de trabalho vazia

#Importação de dados

#Carregando o pacote MASS
#install.packages("MASS")
library(MASS)

fix(Boston)
names(Boston)

Boston#Conferência
head(Boston)#Conferindo a importação e mostrando as seis primeiras linhas
dim(Boston)#Mostrando o número de linhas e colunas
View(Boston)
attach(Boston)#Salvando a importação

#Carregando demais pacotes necessários

#install.packages("ISLR")
#install.packages("leaps")
library(ISLR)
library(leaps)

#Separação de amostras treinamento/teste

#Separando de maneira randômica utilizando a função sample, ou seja, gerando valores aleatórios
#sample(números que podem ser escolhidos, quantidade de números disponíveis)
linhas<-sample(1:length(Boston$medv),length(Boston$medv)*0.803)

#Dados de treino (406 instâncias)
treino=Boston[linhas,]

#Dados de teste (100 instâncias)
teste=Boston[-linhas,]

#Conferência da aleatoriedade
View(treino)
View(teste)
length(treino$medv)+length(teste$medv)#Verificando se está com o comprimento total correto

#### Letra A####

#Encontrando o melhor modelo utilizando a função regsubsets()
is.na(treino)
fix(treino)
names(treino)
dim(treino)
sum(is.na(treino$medv))

#Verificou-se que não há dados faltantes

#A função regsubsets() identifica o melhor modelo que contém determinado número de preditores, onde é melhor quantificado pelo RSS (Soma dos quadrados dos resíduos ou Soma residual dos quadrados) é uma medida da discrepância entre os dados e um modelo de estimativa

regfit.full=regsubsets(medv~.,treino)
summary(regfit.full)

#Um asterisco indica que uma determinada variável está incluída no modelo
#Ex.: esta saída indicou que o melhor modelo de duas variáveis contém apenas "rm" e "lstat"
#Com a opção nvmax pode-se retornar quantas variáveis forem desejadas
#Ajustando um modelo com 13 variáveis

regfit.full=regsubsets(medv~.,data=treino, nvmax=13)
reg.summary=summary(regfit.full)
summary(regfit.full)

#Verificando as estatísticas para definição do melhor modelo
names(reg.summary)

#Analisando as estatísticas

reg.summary$cp#Medida Cp para cada atributo
which.min(reg.summary$cp)#Conferência

reg.summary$bic#Medida BIC para cada atributo
which.min(reg.summary$bic)#Conferência

reg.summary$adjr2#Medida R² Ajustado para cada atributo
which.max(reg.summary$adjr2)#Conferência

#Identificou-se o melhor modelo com 11 atributos

par(mfrow=c(2,2))#Plotando os gráficos das estatísticas CP, BIC e R² Ajustado considerando todas as variáveis

plot(reg.summary$cp, xlab="Número de variáveis", ylab="CP", type="l")
melhor_cp<-which.min(reg.summary$cp)
points(melhor_cp, reg.summary$cp[melhor_cp], col="red", cex=2, pch=20)
abline(v=melhor_cp, col="red", lty=2)
text(melhor_cp, 50, labels=c("Melhor"), pos=4)

plot(reg.summary$bic, xlab="Número de variáveis", ylab="BIC", type="l")
melhor_bic<-which.min(reg.summary$bic)
points(melhor_bic, reg.summary$bic[melhor_bic], col="red", cex=2, pch=20)
abline(v=melhor_bic, col="red", lty=2)
text(melhor_bic, -425, labels=c("Melhor"), pos=4)

plot(reg.summary$adjr2, xlab="Número de variáveis", ylab="R² Ajustado", type="l")
melhor_adjr2<-which.max(reg.summary$adjr2)
points(melhor_adjr2, reg.summary$adjr2[melhor_adjr2], col="red", cex=2, pch=20)
abline(v=melhor_adjr2, col="red", lty=2)
text(melhor_adjr2, 0.7, labels=c("Melhor"), pos=4)

#Identificou-se o melhor modelo com 11 atributos

coef(regfit.full,11)#Coeficientes do melhor modelo (menor CP, menor BIC e maior R² Ajustado)

#### Letra B####

#Utilizando a função regsubsets() para executar a seleção progressiva

regfit.fwd=regsubsets(medv~., data = treino, nvmax=13, method = "forward")
summary(regfit.fwd)

#Verificando as estatísticas para definição do melhor modelo
names(summary(regfit.fwd))

#Analisando as estatísticas

summary(regfit.fwd)$cp#Medida Cp para cada atributo
which.min(summary(regfit.fwd)$cp)#Conferência

summary(regfit.fwd)$bic#Medida BIC para cada atributo
which.min(summary(regfit.fwd)$bic)#Conferência

summary(regfit.fwd)$adjr2#Medida R² Ajustado para cada atributo
which.max(summary(regfit.fwd)$adjr2)#Conferência

#Identificou-se o melhor modelo com 11 atributos

#Utilizando a função regsubsets() para executar a seleção regressiva

regfit.bwd=regsubsets(medv~., data = treino, nvmax=13, method = "backward")
summary(regfit.bwd)

#Verificando as estatísticas para definição do melhor modelo
names(summary(regfit.bwd))

#Analisando as estatísticas

summary(regfit.bwd)$cp#Medida Cp para cada atributo
which.min(summary(regfit.bwd)$cp)#Conferência

summary(regfit.bwd)$bic#Medida BIC para cada atributo
which.min(summary(regfit.bwd)$bic)#Conferência

summary(regfit.bwd)$adjr2#Medida R² Ajustado para cada atributo
which.max(summary(regfit.bwd)$adjr2)#Conferência

#Identificou-se o melhor modelo com 11 atributos

#Resultados

coef(regfit.fwd, 11)#Coeficientes do melhor modelo usando a seleção progessiva
coef(regfit.bwd, 11)#Coeficientes do melhor modelo usando a seleção regressiva
coef(regfit.full, 11)#Coeficientes do melhor modelo anteriormente testado

#### Letra C####

#Separação dos dados de treino e de teste

set.seed(1)
train=sample(c(TRUE, FALSE), nrow(Boston), rep=TRUE)
test=(!train)

library(ISLR)
library(leaps)

regfit.best=regsubsets(medv~., data=Boston[train,], nvmax=13)
test.mat=model.matrix(medv~., data=Boston[test,])

val.errors=rep(NA, 13)

for(i in 1:13){
  coefi=coef(regfit.best, id=i)
  pred=test.mat[,names(coefi)]%*%coefi
  val.errors[i]=mean((Boston$medv[test]-pred)^2)
}

val.errors

which.min(val.errors)

coef(regfit.best,which.min(val.errors))

predict.regsubsets=function(object, newdata, id, ...){
  form=as.formula(object$call[[2]])
  mat=model.matrix(form, newdata)
  coefi=coef(object, id=id)
  xvars=names(coefi)
  mat[,xvars]%*%coefi
}

regfit.best=regsubsets(medv~., data=Boston, nvmax=13)
which.min(val.errors)
coef(regfit.best,which.min(val.errors))

#Validação cruzada

k=11
set.seed(1)
folds=sample(1:k, nrow(Boston), replace=TRUE)
cv.errors=matrix(NA, k, 13, dimnames=list(NULL, paste(1:13)))

for(j in 1:k){
  best.fit=regsubsets(medv~.,data=Boston[folds!=j,], nvmax=13)
  for(i in 1:13){
    pred=predict(best.fit, Boston[folds==j,], id=i)
    cv.errors[j,i]=mean((Boston$medv[folds==j]-pred)^2)
  }
}

mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors

par(mfrow=c(1,1))
plot(mean.cv.errors, type='b')

reg.best=regsubsets(medv~., data=Boston, nvmax=13)
coef(reg.best,11)

#Identificou-se o melhor modelo com 11 atributos

#### Letra D####

#Repetir procedimento para conjunto de teste