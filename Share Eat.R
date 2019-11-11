
#Share Eat

###############################
#SETUP
###############################

#instala bibliotecas 
packages = c("dplyr","janitor","tm","DataCombine","stringr","tidyverse","tidyr","caret","DataExplorer","e1071")
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))  
}

#carrega os pacotes instalados anteriormente
lapply(packages, library, character.only = TRUE)


#importa basese de dados

acts = read.csv("activities_teste_cientista_dadosSE.csv", sep = ";") # sem encoding, fileEncoding = "UTF-8")
vars = read.csv("variables_teste_cientista_dadosSE.csv", sep = ";", fileEncoding = "UTF-8")

################################
#PARTE DA ESTATÍSTICA DESCRITIVA 
################################

#sumário dos dados das variáveis
summary(vars)

#separar por região
table(vars$regiao_ddd)

#SEPARA POR PERFIL

#junta as 3 colunas de quiz
quiz = rbind(as.matrix(vars$quiz_onboarding_1),as.matrix(vars$quiz_onboarding_2),as.matrix(vars$quiz_onboarding_3))

#remove as linhas em branco
quiz =  as.data.frame(quiz[which(quiz != "")])
table(quiz)

#renomeia a coluna pra ficar mais fácil de trabalhar
colnames(quiz)[1] = "grupo"

#arruma os typos ("Exigente" sem o "s" no final)
quiz$grupo = str_replace(quiz$grupo,"Exigentes","Exigente")
quiz$grupo = str_replace(quiz$grupo,"Exigente","Exigentes")


table(quiz$grupo)

#SEPARA POR FAIXA DE PREÇO
table(vars$X_campanha_buzz_vinhos__faixa_de_preco)


#AVALIA CORRELAÇÃO


#separa o tipo de vinho (Branco, tinto, rosé e espumante)

a = acts

a = separate(acts,resultado,sep=",", into=c("name","sku","ecommerce"))

#limpa a coluna "name"
a$name = str_replace(a$name,"[:punct:]","")
a$name = str_replace(a$name,"'name': '","")
a$name = str_replace(a$name,"'","")

#limpa a coluna "sku"
a$sku = str_replace(a$sku,"'sku': '","")
a$name = str_replace(a$name,"'","")

#limpa a coluna "ecommerce"
a$ecommerce = str_replace(a$ecommerce,"'ecommerce': '","")
a$ecommerce = str_replace(a$ecommerce,"'","")
a$ecommerce = str_replace(a$ecommerce,"\\}","")

#remove as linhas sem nenhum vinho
a = drop_na(a,sku)

#insere coluna para classificar o tipo de vinho
a$tipo_vinho = ""

#Classifica o tipo de vinho pelo tipo de uva
a$tipo_vinho = ifelse(str_detect(a$UVA, "Branco") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Sauvignon") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Chardonnay") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Malbec") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Pinot Noir") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Merlot") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "RosÃ©") , "Rose", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Gewurztraminer") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Gamay") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Tannat") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Teroldego") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Riesling") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Barbera") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Moscato") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Tempranillo") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Marselan") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Pinot Noir") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Syrah") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Sangiovese") , "Tinto", a$tipo_vinho)

#Classifica o tipo de vinho pelo nome do vinho
a$tipo_vinho = ifelse(str_detect(a$name, "Sangiovese") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Branco") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Tempranillo") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Merlot") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Barbera") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Pinot Noir") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Sauvignon") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Chardonnay") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Moscato") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Malbec") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "RosÃ©") , "Rose", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Espumante") , "Espumante", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Cabernet") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Tinto") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "tinto") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Alicante Bouschet") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Tannat") , "Tinto", a$tipo_vinho)


table(a$tipo_vinho)

#remove os vinhos sem classificação
a = a[a$tipo_vinho != "",]

#remove os sem perfil
a = a[a$Perfil != 0,]



#observa a correlação com o perfil

table(a$tipo_vinho,a$Perfil)

chisq.test(a$tipo_vinho,a$Perfil, correct=FALSE)



##################################
#PARTE DO MODELO
##################################

#objetivo do modelo: sugerir um vinho baseado em características do usuário.
#o tipo de vinho é composto pelas seguintes características: faixa de preço e tipo de uva

#importa bases de dados (caso não tenha importado na primeira parte)
acts = read.csv("activities_teste_cientista_dadosSE.csv", sep = ";") # sem encoding, fileEncoding = "UTF-8")
vars = read.csv("variables_teste_cientista_dadosSE.csv", sep = ";", fileEncoding = "UTF-8")
cons = read_delim("Consolidado.csv", ";", escape_double = FALSE, trim_ws = TRUE) #base consolidada com atividades e variáveis


#arruma typo na base
cons$Quiz_1 = str_replace(cons$Quiz_1,"Exigentes","Exigente")
cons$Quiz_1 = str_replace(cons$Quiz_1,"Exigente","Exigentes")




#classifica os vinhos e remove os sem classificação (repetindo parte do código do exercício anterior para o caso de ser executada só essa parte)
a = cons

a = separate(cons,resultado,sep=",", into=c("name","sku","ecommerce"))

#limpa a coluna "name"
a$name = str_replace(a$name,"[:punct:]","")
a$name = str_replace(a$name,"'name': '","")
a$name = str_replace(a$name,"'","")

#limpa a coluna "sku"
a$sku = str_replace(a$sku,"'sku': '","")
a$name = str_replace(a$name,"'","")

#limpa a coluna "ecommerce"
a$ecommerce = str_replace(a$ecommerce,"'ecommerce': '","")
a$ecommerce = str_replace(a$ecommerce,"'","")
a$ecommerce = str_replace(a$ecommerce,"\\}","")

#remove as linhas sem nenhum vinho
a = drop_na(a,sku)

#cria uma coluna para armazenar o tipo de vinho
a$tipo_vinho = ""


#Classifica o tipo de vinho pelo tipo de uva
a$tipo_vinho = ifelse(str_detect(a$UVA, "Branco") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Sauvignon") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Chardonnay") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Malbec") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Pinot Noir") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Merlot") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "RosÃ©") , "Rose", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Gewurztraminer") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Gamay") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Tannat") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Teroldego") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Riesling") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Barbera") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Moscato") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Tempranillo") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Marselan") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Pinot Noir") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Syrah") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$UVA, "Sangiovese") , "Tinto", a$tipo_vinho)

#Classifica o tipo de vinho pelo nome do vinho
a$tipo_vinho = ifelse(str_detect(a$name, "Sangiovese") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Branco") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Tempranillo") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Merlot") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Barbera") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Pinot Noir") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Sauvignon") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Chardonnay") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Moscato") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Malbec") , "Branco", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "RosÃ©") , "Rose", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Espumante") , "Espumante", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Cabernet") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Tinto") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "tinto") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Alicante Bouschet") , "Tinto", a$tipo_vinho)
a$tipo_vinho = ifelse(str_detect(a$name, "Tannat") , "Tinto", a$tipo_vinho)

#verifica a distribuição dos tipos de vinho
table(a$tipo_vinho)

#remove as linhas sem tipo de vinho
b = drop_na(a,tipo_vinho)
b = b[!(b$tipo_vinho==""),]

#remove as linhas sem preço
b = drop_na(b,`Preo`)


#Remove linhas que tenham poucas (n<=2) repetições
#Africa do sul
#França
#Brasil
#Portugal
#Espanha

b = b[!(b$Nac_do_vinho=="FranÃ§a"),]
b = b[!(b$Nac_do_vinho=="Africa do Sul"),]
b = b[!(b$Nac_do_vinho=="Brasil"),]
b = b[!(b$Nac_do_vinho=="Portugal"),]
b = b[!(b$Nac_do_vinho=="Espanha"),]


#remove as colunas que não vamos usar (sem significância estatística pré-definida)
data = b[,-c(1,8:14,16:21)]

#seta a semente aleatória para garantir os mesmos resultados apresentados no PPT
set.seed(400)

#CRIA BANCO DE DADOS PARA TESTE
inTrain = createDataPartition(y = data$tipo_vinho, 
                              p = 0.8, #proporção de treino
                              list = FALSE)

TrainingSet = data[inTrain, ] #coloca no TrainingSet tudo que está no data E no inTrain
TestSet = data[-inTrain, ] #coloca no TrainingSet tudo que está no data MENOS o que está no inTrain


mod = train(tipo_vinho ~ ., #regressão do y em cima de todas as variáveis
            data = TrainingSet,
            method = "pls", #Partial Least Squares
            )

#verifica o modelo criado
mod 

#verifica accuracy do modelo
max(mod$results$Accuracy)

#VERIFICA O MODELO NA BASE DE TESTE
ptest = predict(mod, newdata = TestSet)
ptrain = predict(mod, newdata = TrainingSetB)

#quantos previstos são iguais aos reais
mean(ptrain == TrainingSetB$tipo_vinho)
mean(ptest == TestSet$tipo_vinho)


#fazer base de dados de vinhos curtidos separados por tipo
curtidos = b[!(b$avaliacao_do_resultado=="NÃ£o Avaliou"),]
curtidos = curtidos[!(curtidos$avaliacao_do_resultado=="NÃ£o Curtiu"),]

#abre a base de dados de clientes que buscam recomendação
recom <- read_delim("recomendacoes.csv",";", escape_double = FALSE, trim_ws = TRUE)

#remove as colunas que não serão utilizadas
recom2 = recom[,-c(1,8:14,16:20)]

#coloca como 'curtiu'
recom2$avaliacao_do_resultado = "Curtiu"

#remove as nacionalidades de vinho que não foram utilizadas no modelo
recom2$Nac_do_vinho = str_replace(recom2$Nac_do_vinho,"Africa do Sul","0")
recom2$Nac_do_vinho = str_replace(recom2$Nac_do_vinho,"Espanha","0")
recom2$Nac_do_vinho = str_replace(recom2$Nac_do_vinho,"Portugal","0")

#insere as abas preço e tipo de vinho
recom2$`Preo` = recom2$Faixa_Preco
recom2$tipo_vinho = ""
recom2$tipo_vinho = NULL

mod

#PREENCHE A COLUNA DE PREÇO COM FAIXAS ALEATÓRIAS PARA QUEM NÃO PREENCHEU

#vetor com diferentes faixas de preço
lista_precos = c("AtŽ R$30","Entre R$30 e R$50","Entre R$50 e R$100","Entre R$100 e R$200","Acima de R$200")

recom3 = recom2

#troca os "tanto faz" por uma faixa aleatória
recom3$`Preo` = ifelse(str_detect(recom3$`Preo`, "Tanto faz") , sample(lista_precos, 1), recom3$`Preo`)

#troca os que não preencheram (0) por uma faixa aleatória
#tive que fazer essa rotina em loop caso contrário acabava gerando sempre a mesma faixa de preço para todos
for(i in 1:nrow(recom3)) {
 if (recom3[i,8] == 0) {
   recom3[i,8] = sample(lista_precos, 1, replace=TRUE)
 }
}


recom3$tipo_vinho = ""

recom3$tipo_vinho = as.factor(recom3$tipo_vinho)

set.seed(100)

#roda o modelo para esses dados
recomendacoes = predict(mod, newdata = recom3)

#coloca na base de dados as recomendações de tipos de vinho
recom3$tipo_vinho = recomendacoes

#com base na recomendação de tipo e de faixa de preço, escolhe um vinho da base de curtidos

set.seed(100)
for(i in 1:nrow(recom3)) {
  curtidos_preco = curtidos[(curtidos$`Preo`==recom3[i,8]),]
  preco = recom3[1,8]
  if (preco == "AtŽ R$30"){
    curtidos_preco = curtidos[(curtidos$`Preo`=="AtŽ R$30"),]
  }
  if (preco == "Entre R$30 e R$50"){
    curtidos_preco = curtidos[(curtidos$`Preo`=="Entre R$30 e R$50"),]
  }
  if (preco == "Entre R$50 e R$100"){
    curtidos_preco = curtidos[(curtidos$`Preo`=="Entre R$50 e R$100"),]
  }
  if (preco == "Entre R$100 e R$200"){
    curtidos_preco = curtidos[(curtidos$`Preo`=="Entre R$100 e R$200"),]
  }
  if (preco == "Acima de R$200"){
    curtidos_preco = curtidos[(curtidos$`Preo`=="Acima de R$200"),]
  }
  
  tipo = (recom3[i,9])
  if (tipo == "Tinto"){
    recom3[i,10] = sample(curtidos_preco[(curtidos_preco$tipo_vinho=="Tinto"),]$name, size = 1, replace = FALSE)  
  }
  if (tipo == "Rose"){
    recom3[i,10] = sample(curtidos_preco[(curtidos_preco$tipo_vinho=="Rose"),]$name, size = 1, replace = FALSE)  
  }
  if (tipo == "Espumante"){
    recom3[i,10] = sample(curtidos_preco[(curtidos_preco$tipo_vinho=="Espumante"),]$name, size = 1, replace = FALSE)  
  }
  
}


recom3$V10
