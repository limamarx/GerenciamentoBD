#Leitura dos microdados da PNAD cont�nua divulga��o anual 2018
#Exerc�cio 1

#Para saber aonde ser�o salvos os arquivos que voc� ir� criar

getwd()

#Mas se voc� quiser definir o lugar aonde os arquivos que voc� ir� criar ser�o salvos, utilize a fun��o:

setwd("E:\\PNADC2018")

#Para retirar a nota��o cient�fica

options(scipen=999)

#Para salvar os gr�ficos com n�meros separados por v�rgula

options(OutDec=",")

#Para fazer o exerc�cio 1 voc� precisa:

#instalar e carregar o pacote survey

install.packages("survey")

library(survey)

#Abrir a base de dados salva na �ltima aula (1� visita)

load("Pnadc2018V1_REDUZ.RData")

#Recompor o plano amostral para as an�lises

sample.pnadc <- svydesign(ids = ~UPA, strata = ~ESTRATO, weights = ~peso, data = Pnadc2018V1_REDUZ, na.rm=TRUE, nest = TRUE)

# para evitar erro "has only one PSU at stage 1"

options(survey.lonely.psu = "adjust")

######Exerc�cios - gere para Brasil os seguintes gr�ficos e interprete

#a) Taxa de atividade econ�mica - Brasil

a <- svymean(~condat, sample.pnadc, na.rm=TRUE)

a <- a*100

barplot(a, ylim = c(0, 100), names.arg=c("PEA","PIA"), col=("purple"), border="black", density=15, angle=c(90,45),
        main="Pop. de 14 anos ou mais por condi��o de atividade, Brasil, 2018", font.main = 4, col.main = "red", cex.main = 0.7, 
        ylab="%", xlab="PNADC 2018", font.lab = 2, col.lab = "brown", cex.lab = 0.9)

#b) Taxa de atividade econ�mica por trimestre- Brasil

b<-svyby(~condat, ~TRIMESTRE, sample.pnadc, svymean, na.rm=TRUE)

barplot(b, beside=TRUE,  main = "Pop. de 14 anos ou mais por condi��o de atividade, Brasil, Trimestres de 2018", font.main = 2, col.main = "black", cex.main = 0.6,
        col = c("grey", "yellow"), density=c(20,25),angle=c(90,45),
        xlab = "PNADC 2018", names = c("1� Tri", "2� Tri", "3� Tri", "4� Tri"), 
        ylab = "Prop", legend = c("PEA", "PIA"),font.lab = 1, col.lab = "black", cex.lab = 0.8,
        args.legend = list(title = "Condi��o de atividade", x = "top", cex = .7), ylim = c(0, 1))

#c) Taxa de Taxa de desocupa��o - Brasil

c <- svymean(~condocup, sample.pnadc, na.rm=TRUE)

c <- c*100

barplot(c, ylim = c(0, 100), names.arg=c("Ocupados","Desocupados"), col="orange", border="green", density=20, 
        main="Pop. de 14 anos ou mais por condi��o de ocupa��o, Brasil, 2018", font.main = 2, col.main = "black", cex.main = 0.7, 
        ylab="%", xlab="PNADC 2018", font.lab = 2, col.lab = "black", cex.lab = 0.9)
        

#d) Taxa de desocupa��o por trimestre- Brasil

d<-svyby(~condocup, ~TRIMESTRE, sample.pnadc, svymean, na.rm=TRUE)

barplot(d, beside=TRUE,  main = "Pop. de 14 anos ou mais por condi��o de ocupa��o, Brasil, Trimestres de 2018", font.main = 2, col.main = "black", cex.main = 0.6,
        col = c("mediumvioletred", "navy"),
        xlab = "PNADC 2018", names = c("1� Tri", "2� Tri", "3� Tri", "4� Tri"), 
        ylab = "Prop", legend = c("Ocupados","Desocupados"),font.lab = 2, col.lab = "black", cex.lab = 0.9,
        args.legend = list(title = "Condi��o de ocupa��o", x = "right", cex = .5), ylim = c(0, 1))

#e) Distribui��o da renda do trabalho principal deflacionada para pre�os m�dios de 2018

svyhist(~rendadefl, sample.pnadc, breaks=100,xlim=c(0,50000), col="red",
        main="Histograma de frequ�ncia da renda do trabalho principal, Brasil, 2018", font.main = 2, col.main = "black", cex.main = 0.6,
        xlab="PNAD C 2018", font.lab = 2, col.lab = "black", cex.lab = 0.9)

#f) Distribui��o da renda do trabalho principal deflacionada por idade discreta

svyplot(idade ~ rendadefl,
        design = sample.pnadc,
        style = "bubble", 
        basecol="darkolivegreen",
        cex=1.5,
        pch=5,
        alpha = c(0,8),
        main="Gr�fico de dispers�o - Renda e idade, Brasil, 2018", font.main = 4, col.main = "black", cex.main = 0.8,
        xlab="Renda deflacionada",ylab="Idade",
        ylim = c(14, 100))


#g) Boxplot - renda do trabalho principal deflacionada e sexo       

svyboxplot(~rendadefl~factor(sexo), design=sample.pnadc, xlab='PNAD C 2018', ylim=c(0,5000), ylab='Renda', col=c("orange", "brown"), colnames=c("Homem", "Mulher"),
           main="Boxplot - Renda do trabalho principal por sexo, Brasil, 2018", font.main = 4, col.main = "black", cex.main = 0.5)


#H) Boxplot - renda do trabalho principal deflacionada e faixa et�ria       

svyboxplot(~rendadefl~factor(IDADECAT), design=sample.pnadc, xlab='PNAD C 2018', ylim=c(0,8000), col="pink", colnames=c("14-19","20-24","25-29","30-34","35-39","40-44","45-49", "50-54","55-59", "60-64","65+"), ylab='Renda',
           main="Boxplot - Renda do trabalho principal e idade, Brasil, 2018", font.main = 4, col.main = "black", cex.main = 0.8) 
          


 




