
library(dplyr)

set.seed(1)

datas = seq(from = as.Date("2002/1/1"), to = as.Date("2019/9/1"), by = "month")

Flamengo = c(1500, 1500 + cumsum(rnorm(length(datas)-1, 0, 10)))
Fluminense = c(1500, 1500 + cumsum(rnorm(length(datas)-1, 0, 10)))
Vasco = c(1500, 1500 + cumsum(rnorm(length(datas)-1, 0, 10)))
Botafogo = c(1500, 1500 + cumsum(rnorm(length(datas)-1, 0, 10)))
Boca = c(1500, 1500 + cumsum(rnorm(length(datas)-1, 0, 10)))
River = c(1500, 1500 + cumsum(rnorm(length(datas)-1, 0, 10)))

exemplo = data.frame(Flamengo, Fluminense, Vasco, Botafogo, Boca, River) %>%
  tidyr::gather(Clube, Elo)
exemplo$Data = rep(datas, ncol(exemplo)-1)
exemplo$Pais = c(rep("Brasil", length(datas)*4),rep("Argentina", length(datas)*2))

exemplo$Clube[exemplo$Clube == "Boca"] = "Boca Juniors"
exemplo$Clube[exemplo$Clube == "River"] = "River Plate"

save(exemplo, file = "exemplo.RData")
