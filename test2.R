#TESTE 2 - 13/11/2019
# Bruno Araújo 80852
# Edgar Basto 93575
setwd("/home/president/ISCTE/IADR/aulas/")
getwd()


library(xlsx)
bebidas = read.xlsx("BEB2_2018.xls", 1)  

head(bebidas)
View(bebidas)
summary(bebidas)
attach(bebidas)

#A)
counts <- table(IdadeCEO, GeneroCEO, dnn=c('IdadeCEO', 'GeneroCEO'))
counts

row.margin <- round(prop.table(counts, margin=2), 3)*100
row.margin

#B)
par(mfrow=c(1,2))
# Pode avaliar-se a normalidade de uma vari?vel usando o gr?fico Q-Q
qqnorm(Vendas, col = "red")
qqline(Vendas, col = "blue") # Grandes desvios da linha sugerem desvios ? normalidade.
#ou usando o histograma com a curva da Normal
x<-rnorm(1000000, mean = mean(Vendas), sd = sd(Vendas)) 
hist(Vendas, col = "lightblue", prob = TRUE, border = "black", xlab = "Vendas", ylab = "Frequência relativa", main = "Distribuição nas Vendas") 
box(which="plot", lty = "solid", col="black") 
lines(density(x), col = "red") #Adiciona a curva da fun??o densidade da Normal 

install.packages('moments')
library(moments)
skewness(Vendas)

#C)
error <- qt(0.975,df=length(Vendas)-1)*sd(Vendas)/sqrt(length(Vendas))
error
left <- mean(Vendas) - error
right <- mean(Vendas) + error
left; right


#D)
num.genero <- sum(GeneroCEO == 0)
num.genero

sample.size <- dim(bebidas)[1]
sample.size

p <- num.genero/sample.size    #Estimativa pontual
p

error <- qnorm(0.995, mean=0,sd=1)*sqrt((p*(1-p))/sample.size) #qnorm ? o quantil (z-score)
error
lower.bound <- p-error
upper.bound <- p+error
lower.bound; upper.bound      #IC

# Se quisermos o teste para a propor??o: H0: pi=0,40 vs pi diferente de 0,40
Z_data <-  (p - 0.40)/sqrt((0.40*(1-0.40))/sample.size)
p.value <- 2*pnorm(Z_data, mean=0, sd=1)
Z_data; p.value               #Estat?stica de teste e p_value

#E)

#Criação de subsets

rec.fem <- subset(bebidas, GeneroCEO==0)
rec.masc <-subset(bebidas, GeneroCEO==1)
rec.fem
rec.masc
dim(rec.fem)
dim(rec.masc)

#Teste à normalidade
library(nortest)
lillie.test(rec.fem$RecEmp)
lillie.test(rec.masc$RecEmp)

#Teste à igualdade das variâncias
bartlett.test(RecEmp~GeneroCEO) #Teste à igualdade de variâncias

#Usa o teste de wilcoxon como alternativo ao teste t
library(MASS)  
wilcox.test(RecEmp~GeneroCEO, conf.int=TRUE, conf.level=0.95)
wilcox.test(rec.fem$RecEmp, rec.masc$RecEmp, conf.int=TRUE, conf.level=0.95)

#F)
par(mfrow = c(1,1))
boxplot(RecEmp~CAE, xlab = "CAE", main = "Receitas por Empregado por CAE", col='blue')












