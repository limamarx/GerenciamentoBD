#Leitura dos microdados da Rela��o Anual de Informa��es Sociais (Rais)

#Para saber aonde ser�o salvos os arquivos que voc� ir� criar

getwd()

#Mas se voc� quiser definir o lugar aonde os arquivos que voc� ir� criar ser�o salvos, utilize a fun��o:

setwd("F:\\UFRN\\Disciplinas\\2019\\2019.2\\Gerenciamento de banco de dados\\Aulas\\Rais")


#Acesso aos microdados da RAIS (RN2017) - fa�a o download e retire do winzip!

# http://pdet.mte.gov.br/microdados-rais-e-caged #

#Para retirar a nota��o cient�fica

options(scipen=999)

#Para salvar a tabela com n�meros separados por v�rgula

options(OutDec=",")

#Lendo a base de dados da RAIS em formato csv 

RN <-read.csv2('RN2017.txt',sep=';', dec=',')

#Visualizando a base de dados

View(RN)

#Deletando as vari�veis que se aplicam a s�o Paulo, Fortaleza e Rio de Janeiro

RN<-RN[,-c(1,2,3)]

#Visualizando as vari�veis contidas nela

names(RN)

#Visualizando os seis primeiros casos de cada vari�vel 

head(RN)

#An�lise descritiva b�sica dos dados

summary(RN)

######Trabalhando com as vari�veis

#Pedindo a an�lise descritiva da Remunera��o m�dia do trabalhador no ano (valor nominal)

summary(RN$Vl.Remun.M�dia.Nom)

#Colocando as pessoas com rendimento m�dio no ano igual a zero como missing

NA -> RN$Vl.Remun.M�dia.Nom[RN$Vl.Remun.M�dia.Nom==0]

#Conferindo se deu certo

summary(RN$Vl.Remun.M�dia.Nom)

##Quest�o 1: Remunera��o m�dia real no ano:

#Criando uma vari�vel com a renda deflacionada (Utilizando fator de corre��o segundo o INPC de 1� de julho de 2019)

RN$rendamedia_real<-RN$Vl.Remun.M�dia.Nom*1.060764398

summary(RN$rendamedia_real)

#Renda m�dia real RN= 2.335,40 reais
#Renda m�dia real meu munic�pio= xxxx reais

#####Colocando os r�tulos da vari�vel sexo do indiv�duo
                     
table(RN$Sexo.Trabalhador)
                     
RN$Sexo.Trabalhador<- factor(RN$Sexo.Trabalhador,labels=c('Masculino', 'Feminino'))

#Conferindo se deu certo

table(RN$Sexo.Trabalhador)

#Calculando uma distribui��o percentual

100*table(RN$Sexo.Trabalhador)/length(RN$Sexo.Trabalhador)

#Quest�o 2: Calculando o rendimento m�dio anual real por sexo

q2=tapply(RN$rendamedia_real,RN$Sexo.Trabalhador, mean, na.rm=TRUE)

write.table(q2, file="questao2.xls", sep = "\t", dec = ",")

###Trabalhando com a vari�vel faixa et�ria

table(RN$Faixa.Et�ria)
                     
#Coloque os r�tulos da vari�vel faixa et�ria (crie uma vari�vel)
                     
RN$FXIDADE<- ordered(RN$Faixa.Et�ria, levels = c(1,2,3,4,5,6,7,8), labels = c('10-14', '15-17', '18-24', '25-29', '30-39', '40-49','50-64', '65+'))

#Verificando se deu certo 

table(RN$FXIDADE)

#E agora, pedindo uma distribui��o percentual

100*prop.table(table(RN$FXIDADE))
                     
###Quest�o 3:	Remunera��o m�dia real no ano por faixas et�rias 

##Calculando o rendimento m�dio anual por faixa et�ria
                     
q3=tapply(RN$rendamedia_real,RN$FXIDADE, mean, na.rm=TRUE) 

write.table(q3, file="questao3.xls", sep = "\t", dec = ",")

####Trabalhando com a vari�vel causa do afastamento
                     
#Pedindo a an�lise descritiva da vari�vel causa do 1� afastamento
                     
table(RN$Causa.Afastamento.1)

#Retirando os missings (99=Causa do afastamento n�o declarada) 

NA -> RN$Causa.Afastamento.1[RN$Causa.Afastamento.1>=99]
                     
#Colocando os r�tulos da vari�vel causa do 1� afastamento. � preciso baixar o seguinte arquivo para ter os c�gidos:
#http://www.rais.gov.br/sitio/rais_ftp/ManualRAIS2017.pdf
                     
RN$Causa.Afastamento.1<- factor(RN$Causa.Afastamento.1,labels=c('Acidente do trabalho t�pico', 'Acidente do trabalho de trajeto', 'Doen�a relacionada ao trabalho', 'Doen�a n�o relacionada ao trabalho', 'Licen�a-maternidade', 'Servi�o militar obrigat�rio', 'Licen�a sem vencimento/sem remunera��o'))

#Conferindo

table(RN$Causa.Afastamento.1)

#Quest�o 4:	Causa do 1� afastamento - Obtendo a distribui��o percentual

q4=100*prop.table(table(RN$Causa.Afastamento.1))
write.table(q4, file="questao4.xls", sep = "\t", dec = ",")
                     
#####Obtendo a idade m�dia por categorias de causa do 1� afastamento 

##Pedindo a an�lise descritiva 

summary(RN$Idade)
NA -> RN$Idade[RN$Idade==99]

###Quest�o 5: M�dia de idade de trabalhadores que se afastam (1� afastamento) 

q5=tapply(RN$Idade, RN$Causa.Afastamento.1, mean)
write.table(q5, file="questao5.xls", sep = "\t", dec = ",")

#Para salvar o banco de dados em formato RDATA

save(RN,file="RN2017.Rdata")

#E quando eu quiser abri-lo

load("RN2017.Rdata")
                     
#Agora crie um banco de dados com casos apenas para o seu munic�pio (vide c�digo no roteiro da aula)

CEARAMIRIM <-RN[RN$Munic�pio==240260,]
save(CEARAMIRIM,file="CEARAMIRIM2017.Rdata")

#E gere os indicadores conforme pedem as quest�es de 1 a 5 para seu munic�pio. Compare com os dados do RN.
                     