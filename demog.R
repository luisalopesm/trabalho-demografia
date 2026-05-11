library(readr)

nascidos_vivos <- read_csv2("nascidos-vivos-curitiba.csv",
                            skip = 5,
                            locale = locale(encoding = "latin1"))

nascidos_vivos <- nascidos_vivos[1:(nrow(nascidos_vivos) - 10), ]


obitos <- read_csv2("mortalidade-curitiba.csv",
                                      skip = 5,
                                      locale = locale(encoding = "latin1"))

obitos <- obitos[1:(nrow(obitos) - 8), ]


### Questão 1. d) 

library(tidyverse)
library(dplyr)

nascidos <- read_csv2("C:/Users/prhel/Downloads/sinasc_cnv_nvpr212311189_6_17_63.csv")
obitos <- read_csv2("C:/Users/prhel/Downloads/sim_cnv_obt10pr212729189_6_17_63.csv")

colnames(nascidos)[1] <- "Raca_Cor"
colnames(obitos)[1] <- "Raca_Cor"

nascidos_long <- nascidos %>%
  pivot_longer(
    cols = c(`2022`, `2023`), # Seleciona as colunas dos anos
    names_to = "Ano", 
    values_to = "Total_Nascidos"
  )

obitos_long <- obitos %>%
  pivot_longer(
    cols = c(`2022`, `2023`), 
    names_to = "Ano", 
    values_to = "Total_Obitos_Menor_1_Ano"
  )

dados_completos <- inner_join(nascidos_long, obitos_long, by = c("Raca_Cor", "Ano"))

dados_finais <- dados_completos %>%
  mutate(
    # Para garantir que os valores são números (as vezes o Tabnet traz "-" em valores zerados)
    Total_Nascidos = as.numeric(Total_Nascidos),
    Total_Obitos_Menor_1_Ano = as.numeric(Total_Obitos_Menor_1_Ano),
    
    # A probabilidade bruta de morrer antes de 1 ano
    Probabilidade = Total_Obitos_Menor_1_Ano / Total_Nascidos,
    
    # É padrão em epidemiologia apresentar esse dado como Taxa (por 1.000 nascidos vivos)
    Taxa_Mortalidade_Infantil_por_1000 = Probabilidade * 1000
  ) %>%
  # Remove os NA caso alguma categoria não tenha dados cruzados
  drop_na()

# 5. Visualizar o resultado
print(dados_finais)

### Questão 2. d) 

dados <- data.frame(
  Escolaridade = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos e mais", "Ignorado"),
  Vaginal = c(3, 17, 393, 3915, 2485, 24),
  Cesario = c(0, 13, 280, 4185, 5518, 22),
  Ignorado_Parto = c(0, 0, 1, 10, 6, 0)
)

print(dados)

install.packages("vcd")

library(vcd)

v_cramer <- assocstats(dados)$cramer

matriz_dados <- as.matrix(dados[, 2:4])

# 2. Adicionar os nomes das linhas usando a coluna de Escolaridade (opcional, mas organiza a saída)
rownames(matriz_dados) <- dados$Escolaridade

# 3. Calcular o V de Cramér usando a nova matriz
library(vcd)
v_cramer <- assocstats(matriz_dados)$cramer

# Visualizar o resultado
print(v_cramer)


dados_idade <- data.frame(
  Escolaridade = c("Nenhuma", "1 a 3 anos", "4 a 7 anos", "8 a 11 anos", "12 anos e mais", "Ignorado"),
  Idade_10_14 = c(0, 0, 4, 15, 0, 0),
  Idade_15_19 = c(0, 2, 71, 764, 34, 1),
  Idade_20_24 = c(1, 6, 146, 2343, 540, 10),
  Idade_25_29 = c(1, 8, 198, 2338, 1738, 11),
  Idade_30_34 = c(0, 8, 162, 1500, 2750, 14),
  Idade_35_39 = c(0, 2, 66, 874, 2276, 7),
  Idade_40_44 = c(1, 4, 27, 260, 621, 3),
  Idade_45_49 = c(0, 0, 0, 16, 46, 0),
  Idade_50_54 = c(0, 0, 0, 0, 3, 0),
  Idade_60_64 = c(0, 0, 0, 0, 1, 0)
)

# Visualizar o dataframe
print(dados_idade)

matriz_idade <- as.matrix(dados_idade[, 2:11])

# 2. Adicionar os nomes das linhas (opcional, para organização)
rownames(matriz_idade) <- dados_idade$Escolaridade

# 3. Calcular as estatísticas de associação
library(vcd)
estatisticas_idade <- assocstats(matriz_idade)

# 4. Extrair o V de Cramér
v_cramer_final <- estatisticas_idade$cramer

# Exibir o resultado
print(v_cramer_final)
