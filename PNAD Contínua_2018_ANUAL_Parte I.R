#Leitura dos microdados da PNAD cont�nua divulga��o anual - 1� e 5� visita 

#Para saber aonde ser�o salvos os arquivos que voc� ir� criar

getwd()

#Mas se voc� quiser definir o lugar aonde os arquivos que voc� ir� criar ser�o salvos, utilize a fun��o:

setwd("")

#Baixando o pacaote para ler a base de dados disponibilizada em formato TXT e dicion�rio dos dados em formato SAS

# Instalar o pacote para importa��o de dados

install.packages("readr")

#Carregar o pacote que vc acabou de instalar

library(readr)

# Instalar o pacote para ler dicion�rio de dados no formato SAS

install.packages("SAScii")

#Carregar o pacote que vc acabou de instalar

library(SAScii)

# Instalar o pacote para empilhar bases de dados com colunas diferentes

install.packages("plyr")

#Carregar o pacote que vc acabou de instalar

library(plyr)

#Abrindo o dicion�rio da 1� visita que est� na linguagem SAS 
#Ler a base de dados utilizando as informa��es de tamanho e nome das colunas do dicion�rio, e atribuindo as colunas o formato character

dicPnad2018V1=parse.SAScii("Input_PNADC_1_visita_2018.txt")

Pnad2018V1<-read_fwf(file="PNADC_2018_visita1.txt", col_positions=fwf_widths(dicPnad2018V1$width,col_names=dicPnad2018V1$varname),col_types = cols(.default = "c"), progress = T)

#Selecionando algumas vari�veis dessa base de dados

visita1<- c("ANO", "TRIMESTRE", "UF", "UPA","ESTRATO", "V1032", "V2007","V2009", "VD4001", "VD4002", "VD4004A", "VD4005", "VD4009", "VD4016", "V4032")

Pnadc2018V1_REDUZ <- Pnad2018V1[visita1]


#Abrindo o dicion�rio da 5� visita que est� na linguagem SAS 
#Ler a base de dados utilizando as informa��es de tamanho e nome das colunas do dicion�rio, e atribuindo as colunas o formato character

dicPnad2018V5=parse.SAScii("Input_PNADC_5_visita_2018_20190516.txt")

Pnad2018V5<-read_fwf(file="PNADC_2018_visita5_ 20190516.txt", col_positions=fwf_widths(dicPnad2018V5$width,col_names=dicPnad2018V5$varname),col_types = cols(.default = "c"), progress = T)


#Selecionando algumas vari�veis dessa base de dados

visita5<- c("ANO", "TRIMESTRE", "UF", "UPA","ESTRATO", "V4111", "V4120", "V4117A")

Pnadc2018V5_REDUZ <- Pnad2018V5[visita5]

## Empilhando os bancos de dados das visitas 1 e 5 da PNAD 2018 ###

PNADC2018_15 <- rbind.fill(Pnadc2018V1_REDUZ,Pnadc2018V5_REDUZ)

#Transformar a vari�vel fator de expans�o pra num�rica

PNADC2018_15$peso <- as.numeric(PNADC2018_15$V1032)

#Colocando os r�tulos da vari�vel condi��o de atividade

table(PNADC2018_15$VD4001)

PNADC2018_15$condat<-factor(PNADC2018_15$VD4001,labels=c('Pessoas na for�a de trabalho', 'Pessoas fora da for�a de trabalho'))

100*prop.table(table(PNADC2018_15$condat))


#Colocando os r�tulos da vari�vel condi��o de ocupa��o

table(PNADC2018_15$VD4002)

PNADC2018_15$condocup<-factor(PNADC2018_15$VD4002,labels=c('Ocupadas', 'Desocupadas'))

100*prop.table(table(PNADC2018_15$condocup))

#Agora fa�a o mesmo para as demais vari�veis
#V2007 - sexo
#VD4004A - Subocupa��o por insufici�ncia de horas habitualmente trabalhadas em todos os trabalhos
#VD4005 - Pessoas desalentadas na semana de refer�ncia
#VD4009 - Posi��o na ocupa��o e categoria do emprego do trabalho principal da semana de refer�ncia para pessoas de 14 anos ou mais de idade
#V4032 - Era contribuinte de instituto de previd�ncia por esse trabalho 
#V4111 - Se na semana de refer�ncia trabalhou, durante pelo menos uma hora, voluntariamente e sem remunera��o
#V4120 - Se na semana de refer�ncia fez tarefas dom�sticas para o pr�prio domic�lio
#V4117A - Se na semana de refer�ncia realizou tarefas de cuidados a moradores deste domicilio que eram crian�as, idosos, enfermos ou pessoas com necessidades especiais


