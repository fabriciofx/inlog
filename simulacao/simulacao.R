set.seed(123)

# Parâmetro da Exponencial
lambda <- 1

# Esperança e variância populacional
media_pop <- 1 / lambda
var_pop <- 1 / lambda^2
cat("Média populacional:", round(media_pop, 4), "\n")
cat("Variância populacional:", round(var_pop, 4), "\n")

# Função para simular médias amostrais diretamente da distribuição
simular_medias <- function(n, reps = 1000) {
  replicate(reps, mean(rexp(n, rate = lambda)))
}

# Tamanhos amostrais
n_vals <- c(10, 30, 50, 100)
resultados <- data.frame(
  n = n_vals,
  media = NA,
  variancia = NA,
  P_menor_0_5 = NA,
  P_entre_0_5_0_9 = NA
)

# Limite fixo para o eixo x
limites_x <- c(0, 2)

# Loop
for (i in seq_along(n_vals)) {
  n <- n_vals[i]
  medias <- simular_medias(n)
  
  resultados$media[i] <- mean(medias)
  resultados$variancia[i] <- var(medias)
  resultados$P_menor_0_5[i] <- mean(medias < 0.5)
  resultados$P_entre_0_5_0_9[i] <- mean(medias > 0.5 & medias < 0.9)
  
  pdf(paste0("hist_exp_n", n, ".pdf"), width = 5, height = 4)
  hist(medias, probability = TRUE, breaks = 30,
       main = paste("Distribuição da Média - n =", n),
       xlab = "Média amostral", ylab = "Densidade",
       ylim = c(0, 4),
       col = "lightblue", xlim = limites_x)
  abline(v = media_pop, col = "red", lwd = 2)
  dev.off()
}

print(resultados)
