# Laboratório de Estatística
# UNIDADE 3
# Distribuições contínuas de probabilidade

# Calculando probabilidades de que hajam x peças numa distribuição
# normal com média 100 desv pad 15
pnorm(120, mean=100, sd=15, lower.tail = T)
pnorm(130, mean=100, sd=15, lower.tail = F)
pnorm(80, mean=100, sd=15, lower.tail = T)
pnorm(95, mean=100, sd=15, lower.tail = F)

# Entre 60 e 115 peças
pnorm(115, mean=100, sd=15, lower.tail = T) - 
  pnorm(60, mean=100, sd=15, lower.tail = T)

  
## Exercícios
## 1)

cond1 = c(20.5, 21.5, 21.6, 20.7, 21.3, 22.5, 21.8, 21.6, 20.9, 21.7)
cond2 = c(22.6, 23.7, 24.6, 23.1, 24.8, 25.1, 26.7, 26.7, 25.9, 24.7)

estats = function(x) {
  res = c(mean(x), sd(x), sd(x)/sqrt(length(x)))
  names(res) = c("Mean", "Std Dev", "Std Error")
  return(res)
}
estats(cond1)
estats(cond2)

# RESPOSTA: condição 2

################################
# 2)
comum = c(23.1, 24.2, 23.6, 26.7, 23.5, 26.8, 23.5)
pozo = c(25.3, 25.9, 24.8, 25.8, 24.9, 25.4, 25.7)

t1 = estats(comum)
t2 = estats(pozo)
cbind(t1[1] - t1[3], t1[1] + t1[3])
cbind(t2[1] - t2[3], t2[1] + t2[3])

## Resposta: considerando o "intervalo de confiança da média", pozolânico 
## tem maior resistencia media

###########################################
# 3)

notas = c(65, 70, 80, 50)
pesos = c(0.25, 0.3, 0.15, 0.3)
weighted.mean(notas, pesos)
## Resposta: não conseguiu

#############################################
# 4)

# Calculando probabilidade de ter exatamente 1 sucesso
pbinom(1, size=10, prob = 0.2, lower.tail = T) - 
  pbinom(0, size=10, prob = 0.2, lower.tail = T)

# Tendo 8, opera com 7 ou 8
# Probabilidade de operar com 7 ou 8
pbinom(8, size=8, prob = 0.95, lower.tail = T) - 
  pbinom(6, size=8, prob = 0.95, lower.tail = T)

pbinom(10, size=10, prob = 0.95, lower.tail = T) - 
  pbinom(7, size=10, prob = 0.95, lower.tail = T)

# Resposta: Melhor colocar 10 do que 8

############################################
# 5)

pnorm(600, mean=700, sd=100, lower.tail = T)
# Resposta: 15.87
