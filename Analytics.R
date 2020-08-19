install.packages("wooldridge") #Banco de dados
install.packages("tidyverse") #Tidyverse
install.packages("testthat") #Testes unitários
library(wooldridge)
library(tidyverse)
library(testthat)
#####################################################

data('ceosal1')
ceosal1 = as_tibble(ceosal1) # a visualizãção do tiblbe é mais elegante
ceosal1
?ceosal1


#investigando a relação entre a variável ROE (Retorno do Patrimônio)
# e o salário anual do CEO (Salary) em milhares de dólares.

summary(ceosal1$roe)
summary(ceosal1$salary)


# plotando os gráficos

plot(ceosal1$roe, ceosal1$salary)

#plotar cortando na linha de 4000

#estabelecendo os limites do y com ylim (até 4000). veja que o gráfico
#muda

plot(ceosal1$roe, ceosal1$salary, ylim = c(0,4000))

#fazendo a regressão na mão

#       salario = b0 + b1roe + u

# b0 = média(y) - b1.média(x)

# b1 = [somatório(x-mediax)(y - mediay)]/somatório(x-x)^2  -> covariância/

#     (x-mediax)(y - mediay) = covariância

#     b1 = cov(x,y)/var(x)


b1 = cov(ceosal1$roe, ceosal1$salary)/var(ceosal1$roe)
b1
b0 = mean(ceosal1$salary) - b1*mean(ceosal1$roe)
b0
# prevendo o salário de um ceo cujo retorno de patrimonio é 10 por cento
b0 + b1*10
#salário anual esperado de um ceo que tem um retorno de patrimonio de 
# 10 por cento é 1148.203

b0 + b1*5


#  vamos estimar a mesma regressão agora utilizando os comandos do R

fit1=lm(salary ~ roe, ceosal1)
coef(fit1)

b0;b1

# vemos que ambos retornam a mesma coisa

# relação entre salário e escolaridade (predito e preditor, respectivamente)

data(wage1)
wage1 = as_tibble(wage1)
wage1

# salario = b0 + b1educ + u
?wage1
summary(wage1$wage)
summary(wage1$educ)
# plotando ambas

plot(wage1$educ, wage1$wage)


fit2 = lm(wage ~ educ, wage1)
fit2
coef(fit2)

data("vote1")
?vote1
vote1 = as_tibble(vote1)
vote1

# selecionando as duas variáveis
vote1 %>% select(voteA, shareA)

# montando o modelo

fit3 = lm(voteA ~ shareA, vote1)
# coef mostra intercepto e b1
coef(fit3)


# esperança de voto é 26.81 + 0.46gasto

# propriedades da regressão linear -> Esperança do erro = 0

yhat = predict(fit3) 

u = vote1$voteA - yhat
mean(u)

# função que extrai os resíduos 

mean(resid(fit3))


#  a covariância dos resíduos e x = 0 (entre e resíduos e x)
# pressuposto da independencia do regressor x

cov(resid(fit3), vote1$shareA)
# percebe-se que é igual a 0
cov(u, vote1$shareA)

# R-squared é a soma dos quadrados explicados (SQE) sobre a soma dos quadrados totais (SQT)
# é a mesma coisa de 1 - soma dos quadrados dos resíduos (SQR)/soma dos quadrados totais

# preparando os insumos para calcular o ajuste do modelo

# soma dos quadrados totais

SQT = sum((vote1$voteA - mean(vote1$voteA))^2)
yhat = predict(fit3)
SQE = sum((yhat - mean(vote1$voteA))^2)
SQE
SQR = sum(resid(fit3)^2)
SQR


expect_equal(SQT, SQE + SQR)
expect_equal(SQE/SQT, 1-SQR/SQT)

# R2 diz o tamanho da explicação do meu modelo, isto é, a quantidade de dados que ele consegue explicar.

r2 = SQE/SQT
r2
# como o r2 deu .85, temos que o modelo explica 85% dos dados.


# vendo a mesma coisa (o R2), com o summary
resumo = summary(fit3)
resumo
resumo$r.squared
# r2 é o coeficiente de determinação = Soma dos quadrados explicados/Soma dos quadrados totais

summary(fit2)$r.squared
# #  AULA 03

# MQO - Mínimos quadrados ordinários -> É o método da regressão linear
# para minimizar ao máximo a soma dos quadrados dos erros.

# outros métodos de estimação também usados
# para relações lineares: Máxima Verossimilhança (maximum likelihood)

data(ceosal1)
ceosal1 = as_tibble(ceosal1)
ceosal1

fit1 = lm(salary ~ roe, data = ceosal1)
summary(fit1)

# r2 não é medida de ajuste. é, nada mais nada menos, que a capacidade explicativa do modelo.

##################

data(wage1)
# elabore o seguinte modelo:
# y = wage
# x = educ

fit2 = lm(wage ~ educ, data=wage1)
summary(fit2)

##############

# outra regressão
# banco vote1
# y = voteA(perecentual de votos em A)
# x = shareA(% de gasto com campanha do candidato A)
?vote1
fit3 = lm(voteA ~ shareA, vote1)
summary(fit3)

# testando as propriedades do modelo de MQO

# 1) E(u) = 0

options(scipen = 999) # tira a notação científica
e1 = resid(fit1)
e1
mean(e1)
sum(e1)

# pressuposto da independência entre o x e o erro

# y = regressando ou variável dependente ou variável a ser explicada
# x = regressor, variável independente ou variável explicativa

# x deve ser isolado do erro. para testar, calculamos a covariância entre 
# x e o erro.

cov(ceosal1$roe, e1) # percebe-se que deu zero, ou seja, são independentes.

#####################
#### PRESSUPOSTOS ###
#####################
# 1) linearidade
# para testar, vamos plotar x e y
plot(vote1$shareA, vote1$voteA)
abline(fit3, col = 'red', lwd = 2) #plotando a curva de regressão

# pressuposto da aleatoriedade

# Amostra é aleatória (ou probabilística):

# amostra por conveniência
# amostra ordenada
# amostra probabilística -> todos os casos tem igual probabilidade 
# de serem selecionados. Daí posso assumir quem minha variavel tem
# distribuição normal, o que me permite fazer salto inferencial.

# para verificar o pressuposto da aleatoriedade dos dados,
# vamos utilizar simulações montecarlo para construir um modelo conhecido
# e estimá-lo por MQO


# vamos gerar mil numeros aleatórios

x = rnorm(1000)
hist(x)

# vamos fabricar um y a partir de um modelo dado

y = 3 + 1.5*x + rnorm(1000)

# gerando uma amostra de 200 casos

amostra = sample(1000, 200, replace = F) 

# estimando a regressão com a amostra

regteste = lm(y ~ x, subset = amostra)
coef(regteste)

# criando um vetor vazio que vai guardar 1000 betas zero que serão 
# estimados

b0s = vector("numeric", 1000)
b1s = vector("numeric", 1000)

# fazendo uma iteração no r para gerar 1000 beta

for (i in 1:1000) {
  a = sample(1000, 200) #dentro de 1000 eu tiro 200
  reg = lm(y ~ x, subset = a)
  b0s[i] = coef(reg)[1]
  b1s[i] = coef(reg)[2]
}

hist(b0s) # percebe-se que a maioria dos valores gerados
# são próximos de 3. distribuição normal.
hist(b1s) # a grande maioria está próximo de 1.5. distribuição normal

# esse loop foi para demonstrar que a amostra era aleatória probabilis-
# tica

plot(x,y)
abline(mean(b0s), mean(b1s), col = 'red', lwd = 2)

# pressuposto da variação amostral no x:
# o x tem que ter variabilidade
# vamos testar se var(x) > 0
fit3
var(vote1$shareA)

expect_gt(var(vote1$shareA), 0) # se tiver errado, ele gera um erro

# exemplo:
expect_gt(c(1,1,1,1), 0) #veja a mensagem de erro

# TEOREMA DE GAUSS-MARKOV

# MQO -> Cov(x,y) = 0, é linear, aleatório -> BLUE
# Blue -> Best Linear Unbiased Estimator
# MQO é o melhor estimador linear não-enviesado
# MQO é uma ferramenta poderosa

######################

data(meap93)
head(meap93)
?meap93
# y = math10
# x = lnchprg

fit4 = lm(math10 ~ lnchprg, meap93)
summary(fit4)

# o modelo não faz muito sentido. a adição percentual de estudantes
# diminui a nota.

hist(resid(fit4)) #plotando o histograma dos erros (espera-se que 
# seja uma distribuição normal)

# pressuposto da homoscedasticidade -> variância do erro igual em toda a 
# extensão dos x

# se a variância for diferente, o modelo será heteroscedástico.

# como testa esse pressuposto para saber se o modelo é homoscedástico
# ou heteroscedástico
