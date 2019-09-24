#Leitura dos microdados da PNAD contínua divulgação anual - 1ª e 5ª visita 

#Para saber aonde serão salvos os arquivos que você irá criar

getwd()

#Mas se você quiser definir o lugar aonde os arquivos que você irá criar serão salvos, utilize a função:

setwd("")

#Baixando o pacaote para ler a base de dados disponibilizada em formato TXT e dicionário dos dados em formato SAS

# Instalar o pacote para importação de dados

install.packages("readr")

#Carregar o pacote que vc acabou de instalar

library(readr)

# Instalar o pacote para ler dicionário de dados no formato SAS

install.packages("SAScii")

#Carregar o pacote que vc acabou de instalar

library(SAScii)

# Instalar o pacote para empilhar bases de dados com colunas diferentes

install.packages("plyr")

#Carregar o pacote que vc acabou de instalar

library(plyr)

#Abrindo o dicionário da 1ª visita que está na linguagem SAS 
#Ler a base de dados utilizando as informações de tamanho e nome das colunas do dicionário, e atribuindo as colunas o formato character

dicPnad2018V1=parse.SAScii("Input_PNADC_1_visita_2018.txt")

Pnad2018V1<-read_fwf(file="PNADC_2018_visita1.txt", col_positions=fwf_widths(dicPnad2018V1$width,col_names=dicPnad2018V1$varname),col_types = cols(.default = "c"), progress = T)

#Selecionando algumas variáveis dessa base de dados

visita1<- c("ANO", "TRIMESTRE", "UF", "UPA","ESTRATO", "V1032", "V2007","V2009", "VD4001", "VD4002", "VD4004A", "VD4005", "VD4009", "VD4016", "V4032")

Pnadc2018V1_REDUZ <- Pnad2018V1[visita1]


#Abrindo o dicionário da 5ª visita que está na linguagem SAS 
#Ler a base de dados utilizando as informações de tamanho e nome das colunas do dicionário, e atribuindo as colunas o formato character

dicPnad2018V5=parse.SAScii("Input_PNADC_5_visita_2018_20190516.txt")

Pnad2018V5<-read_fwf(file="PNADC_2018_visita5_ 20190516.txt", col_positions=fwf_widths(dicPnad2018V5$width,col_names=dicPnad2018V5$varname),col_types = cols(.default = "c"), progress = T)


#Selecionando algumas variáveis dessa base de dados

visita5<- c("ANO", "TRIMESTRE", "UF", "UPA","ESTRATO", "V4111", "V4120", "V4117A")

Pnadc2018V5_REDUZ <- Pnad2018V5[visita5]

## Empilhando os bancos de dados das visitas 1 e 5 da PNAD 2018 ###

PNADC2018_15 <- rbind.fill(Pnadc2018V1_REDUZ,Pnadc2018V5_REDUZ)

#Transformar a variável fator de expansão pra numérica

PNADC2018_15$peso <- as.numeric(PNADC2018_15$V1032)

#Colocando os rótulos da variável condição de atividade

table(PNADC2018_15$VD4001)

PNADC2018_15$condat<-factor(PNADC2018_15$VD4001,labels=c('Pessoas na força de trabalho', 'Pessoas fora da força de trabalho'))

100*prop.table(table(PNADC2018_15$condat))


#Colocando os rótulos da variável condição de ocupação

table(PNADC2018_15$VD4002)

PNADC2018_15$condocup<-factor(PNADC2018_15$VD4002,labels=c('Ocupadas', 'Desocupadas'))

100*prop.table(table(PNADC2018_15$condocup))

#Agora faça o mesmo para as demais variáveis
#V2007 - sexo
#VD4004A - Subocupação por insuficiência de horas habitualmente trabalhadas em todos os trabalhos
#VD4005 - Pessoas desalentadas na semana de referência
#VD4009 - Posição na ocupação e categoria do emprego do trabalho principal da semana de referência para pessoas de 14 anos ou mais de idade
#V4032 - Era contribuinte de instituto de previdência por esse trabalho 
#V4111 - Se na semana de referência trabalhou, durante pelo menos uma hora, voluntariamente e sem remuneração
#V4120 - Se na semana de referência fez tarefas domésticas para o próprio domicílio
#V4117A - Se na semana de referência realizou tarefas de cuidados a moradores deste domicilio que eram crianças, idosos, enfermos ou pessoas com necessidades especiais


