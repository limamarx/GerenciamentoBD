#Leitura dos microdados da Relação Anual de Informações Sociais (Rais)

#Para saber aonde serão salvos os arquivos que você irá criar

getwd()

#Mas se você quiser definir o lugar aonde os arquivos que você irá criar serão salvos, utilize a função:

setwd("F:\\UFRN\\Disciplinas\\2019\\2019.2\\Gerenciamento de banco de dados\\Aulas\\Rais")


#Acesso aos microdados da RAIS (RN2017) - faça o download e retire do winzip!

# http://pdet.mte.gov.br/microdados-rais-e-caged #

#Para retirar a notação científica

options(scipen=999)

#Para salvar a tabela com números separados por vírgula

options(OutDec=",")

#Lendo a base de dados da RAIS em formato csv 

RN <-read.csv2('RN2017.txt',sep=';', dec=',')

#Visualizando a base de dados

View(RN)

#Deletando as variáveis que se aplicam a são Paulo, Fortaleza e Rio de Janeiro

RN<-RN[,-c(1,2,3)]

#Visualizando as variáveis contidas nela

names(RN)

#Visualizando os seis primeiros casos de cada variável 

head(RN)

#Análise descritiva básica dos dados

summary(RN)

######Trabalhando com as variáveis

#Pedindo a análise descritiva da Remuneração média do trabalhador no ano (valor nominal)

summary(RN$Vl.Remun.Média.Nom)

#Colocando as pessoas com rendimento médio no ano igual a zero como missing

NA -> RN$Vl.Remun.Média.Nom[RN$Vl.Remun.Média.Nom==0]

#Conferindo se deu certo

summary(RN$Vl.Remun.Média.Nom)

##Questão 1: Remuneração média real no ano:

#Criando uma variável com a renda deflacionada (Utilizando fator de correção segundo o INPC de 1º de julho de 2019)

RN$rendamedia_real<-RN$Vl.Remun.Média.Nom*1.060764398

summary(RN$rendamedia_real)

#Renda média real RN= 2.335,40 reais
#Renda média real meu município= xxxx reais

#####Colocando os rótulos da variável sexo do indivíduo
                     
table(RN$Sexo.Trabalhador)
                     
RN$Sexo.Trabalhador<- factor(RN$Sexo.Trabalhador,labels=c('Masculino', 'Feminino'))

#Conferindo se deu certo

table(RN$Sexo.Trabalhador)

#Calculando uma distribuição percentual

100*table(RN$Sexo.Trabalhador)/length(RN$Sexo.Trabalhador)

#Questão 2: Calculando o rendimento médio anual real por sexo

q2=tapply(RN$rendamedia_real,RN$Sexo.Trabalhador, mean, na.rm=TRUE)

write.table(q2, file="questao2.xls", sep = "\t", dec = ",")

###Trabalhando com a variável faixa etária

table(RN$Faixa.Etária)
                     
#Coloque os rótulos da variável faixa etária (crie uma variável)
                     
RN$FXIDADE<- ordered(RN$Faixa.Etária, levels = c(1,2,3,4,5,6,7,8), labels = c('10-14', '15-17', '18-24', '25-29', '30-39', '40-49','50-64', '65+'))

#Verificando se deu certo 

table(RN$FXIDADE)

#E agora, pedindo uma distribuição percentual

100*prop.table(table(RN$FXIDADE))
                     
###Questão 3:	Remuneração média real no ano por faixas etárias 

##Calculando o rendimento médio anual por faixa etária
                     
q3=tapply(RN$rendamedia_real,RN$FXIDADE, mean, na.rm=TRUE) 

write.table(q3, file="questao3.xls", sep = "\t", dec = ",")

####Trabalhando com a variável causa do afastamento
                     
#Pedindo a análise descritiva da variável causa do 1º afastamento
                     
table(RN$Causa.Afastamento.1)

#Retirando os missings (99=Causa do afastamento não declarada) 

NA -> RN$Causa.Afastamento.1[RN$Causa.Afastamento.1>=99]
                     
#Colocando os rótulos da variável causa do 1º afastamento. É preciso baixar o seguinte arquivo para ter os cógidos:
#http://www.rais.gov.br/sitio/rais_ftp/ManualRAIS2017.pdf
                     
RN$Causa.Afastamento.1<- factor(RN$Causa.Afastamento.1,labels=c('Acidente do trabalho típico', 'Acidente do trabalho de trajeto', 'Doença relacionada ao trabalho', 'Doença não relacionada ao trabalho', 'Licença-maternidade', 'Serviço militar obrigatório', 'Licença sem vencimento/sem remuneração'))

#Conferindo

table(RN$Causa.Afastamento.1)

#Questão 4:	Causa do 1º afastamento - Obtendo a distribuição percentual

q4=100*prop.table(table(RN$Causa.Afastamento.1))
write.table(q4, file="questao4.xls", sep = "\t", dec = ",")
                     
#####Obtendo a idade média por categorias de causa do 1º afastamento 

##Pedindo a análise descritiva 

summary(RN$Idade)
NA -> RN$Idade[RN$Idade==99]

###Questão 5: Média de idade de trabalhadores que se afastam (1º afastamento) 

q5=tapply(RN$Idade, RN$Causa.Afastamento.1, mean)
write.table(q5, file="questao5.xls", sep = "\t", dec = ",")

#Para salvar o banco de dados em formato RDATA

save(RN,file="RN2017.Rdata")

#E quando eu quiser abri-lo

load("RN2017.Rdata")
                     
#Agora crie um banco de dados com casos apenas para o seu município (vide código no roteiro da aula)

CEARAMIRIM <-RN[RN$Município==240260,]
save(CEARAMIRIM,file="CEARAMIRIM2017.Rdata")

#E gere os indicadores conforme pedem as questões de 1 a 5 para seu município. Compare com os dados do RN.
                     