# a)
setwd('/home/president/ISCTE/IADR/teste1/')
getwd()
library(xlsx)
bebidas = read.xlsx("BEB_2018.xls", 1)  

head(bebidas)


# b)
summary(bebidas)

# c)
mean(is.na(bebidas$RecEmp)) #Proporção de missing values

media <- mean(na.omit(bebidas$RecEmp)) # Média da Variável sem os missing values
media 

which(is.na(bebidas$RecEmp)) #index dos missing values
bebidas$RecEmp[14] <- bebidas$RecEmp[44] <- bebidas$RecEmp[82] <- media

mean(bebidas$RecEmp) #Mantém-se a média

# d) 
summary(bebidas$RecEmp)
m <- mean(bebidas$RecEmp); s <- sd(bebidas$RecEmp)
m; s 
z.weight <- (bebidas$RecEmp - m) / s
z.weight
summary(z.weight)
min(z.weight)
max(z.weight)

# e)
summary(bebidas$Vendas)
media_vendas <- mean(bebidas$Vendas)
media_vendas
mediana_vendas <- median(bebidas$Vendas)
mediana_vendas

our_table <- table(bebidas$Vendas)
our_table
our_table == max(our_table)
our_mode <- names(our_table)[our_table == max(our_table)]
our_mode #Apesar de a moda não fazer muito sentido nesta variável quantitativa, verifica-se a repetição de um dos valores.


q1 <- as.vector(quantile(bebidas$Vendas, prob = c(0.25)))
q3 <- as.vector(quantile(bebidas$Vendas, prob = c(0.75)))
q1; q3 #P25 e P75

iqr <- q3 - q1
iqr #intervalo interquartilíco

s_vendas <- sd(bebidas$Vendas)
s_vendas

q85 <- as.vector(quantile(bebidas$Vendas, prob = c(0.85)))
q85

#primeiro coeficiente de assimetria AS= média - moda / desvio padrão
AS1 <- (media_vendas - max(our_table)) / s_vendas
AS1
(3*(media_vendas) - mediana_vendas)/s_vendas


# f)
malte <- subset(bebidas, bebidas[9] == 2082)
vinhos <- subset(bebidas, bebidas[9] == 2084)
head(malte)
head(vinhos)
cv_malte <- sd(malte$Vendas)/ mean(malte$Vendas)*100
cv_vinhos <- sd(vinhos$Vendas)/ mean(vinhos$Vendas)*100
cv_malte; cv_vinhos

# g)
p25 <- as.vector(quantile(bebidas$RecEmp, prob = c(0.25))) 
p75 <- as.vector(quantile(bebidas$RecEmp, prob = c(0.75))) 
iqr_recemp <-p75-p25
p25; p75; iqr_recemp

li<-p25-1.5*iqr_recemp
lis<-p25-3*iqr_recemp
ls<-p75+1.5*iqr_recemp
lss<-p75+3*iqr_recemp

#outliers moderados
bebidas$RecEmp[bebidas$RecEmp<li]; 
bebidas$RecEmp[bebidas$RecEmp>ls];

#outliers severos
bebidas$RecEmp[bebidas$RecEmp<lis]; bebidas$RecEmp[bebidas$RecEmp>lss]

# h)
par(mfrow = c(1,2)) 
boxplot(z.weight, xlab = "RecEmp Sandardizada", main = "Receitas por Empregado", col='blue', ylim = c(-3,3))
boxplot(z.weight ~bebidas$GeneroCEO, xlab = "RecEmp Std", main = "RecEmp Std/Gestor", col='red', ylim = c(-3,3), las=1)


# i)
par(mfrow = c(1,3))
summary(bebidas$Activo)
summary(bebidas$Passivo)
hist(bebidas$Activo, col='red', xlim = c(0,45000), breaks=20, border='black', ylim = c(0,15))
box(which = "plot", lty = "solid", col = "black")   
hist(bebidas$Passivo, col='blue', xlim = c(0,4000), breaks=20, border='black', ylim = c(0,15))
box(which = "plot", lty = "solid", col = "black")   
plot(bebidas$Activo, bebidas$Passivo, main="Activo vs Passivo", xlab="Ativo ", ylab="Passivo", pch=19, xlim=c(0,45000), ylim=c(0,4000))

ggsave("grafico.pdf", width = 20, height = 20, units = "cm")  
