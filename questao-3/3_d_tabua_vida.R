library(microdatasus)
library(tidyverse)
library(lubridate)

# ---------------------------------------------------
# BAIXANDO DADOS DO SIM (MORTALIDADE)
# ---------------------------------------------------

DO2010 <- fetch_datasus(
  year_start = 2010,
  year_end = 2010,
  uf = "PR",
  information_system = "SIM-DO"
)

# ---------------------------------------------------
# PROCESSANDO DADOS
# ---------------------------------------------------

DO2010 <- process_sim(DO2010)

# ---------------------------------------------------
# TRATAMENTO DOS DADOS
# ---------------------------------------------------

df_base <- DO2010 %>%
  
  select(SEXO, DTNASC, DTOBITO) %>%
  
  mutate(
    DTNASC = ymd(DTNASC),
    DTOBITO = ymd(DTOBITO)
  ) %>%
  
  filter(!is.na(DTNASC), !is.na(DTOBITO)) %>%
  
  mutate(
    Idade = round(
      time_length(interval(DTNASC, DTOBITO), "years"),
      2
    )
  ) %>%
  
  mutate(
    faixa_idade = case_when(
      Idade < 1 ~ "0-1",
      Idade >= 1  & Idade < 5  ~ "1-5",
      Idade >= 5  & Idade < 10 ~ "5-10",
      Idade >= 10 & Idade < 15 ~ "10-15",
      Idade >= 15 & Idade < 20 ~ "15-20",
      Idade >= 20 & Idade < 25 ~ "20-25",
      Idade >= 25 & Idade < 30 ~ "25-30",
      Idade >= 30 & Idade < 35 ~ "30-35",
      Idade >= 35 & Idade < 40 ~ "35-40",
      Idade >= 40 & Idade < 45 ~ "40-45",
      Idade >= 45 & Idade < 50 ~ "45-50",
      Idade >= 50 & Idade < 55 ~ "50-55",
      Idade >= 55 & Idade < 60 ~ "55-60",
      Idade >= 60 & Idade < 65 ~ "60-65",
      Idade >= 65 & Idade < 70 ~ "65-70",
      Idade >= 70 & Idade < 75 ~ "70-75",
      Idade >= 75 & Idade < 80 ~ "75-80",
      Idade >= 80 ~ "80+",
      TRUE ~ NA_character_
    )
  ) %>%
  
  mutate(
    faixa_idade = factor(
      faixa_idade,
      levels = c(
        "0-1", "1-5", "5-10", "10-15",
        "15-20", "20-25", "25-30", "30-35",
        "35-40", "40-45", "45-50", "50-55",
        "55-60", "60-65", "65-70", "70-75",
        "75-80", "80+"
      )
    ),
    
    lim_inf = ifelse(
      faixa_idade != "80+",
      as.numeric(str_split_i(faixa_idade, "-", 1)),
      80
    )
  )

# ---------------------------------------------------
# FILTRANDO APENAS MULHERES
# ---------------------------------------------------

df <- df_base %>%
  filter(SEXO == "Feminino")

# ---------------------------------------------------
# CÁLCULO DO kx
# ---------------------------------------------------

kx <- df %>%
  
  filter(
    faixa_idade != "0-1",
    faixa_idade != "1-5"
  ) %>%
  
  group_by(faixa_idade) %>%
  
  summarise(
    k = round(mean(Idade - lim_inf), 2)
  )

# PARA MULHERES

# se TMI >= 0.107, k0 = 0.35
# se TMI < 0.107,  k0 = 0.053 + 2.8 * TMI

# k1-4 = 1.522 - 1.518 * TMI

#meu TMI exemplo será 0.010164

TMI <- 0.010164

k0 <- df %>%
  filter(faixa_idade == "0-1", ) %>%
  group_by(faixa_idade) %>%
  summarise(k = 0.053 + 2.8 * TMI)

k1 <- df %>%
  filter(faixa_idade == "1-5") %>%
  group_by(faixa_idade) %>%
  summarise(k = 1.522 - 1.518 * TMI)

kx <- rbind(k0, k1, kx) %>%
  mutate(k = round(k, 2))




# dx -----------------------------------------------


# Por problema de arredondamento, sua tabela pode ter um l um pouco maior ou menor que 100.000, você pode corrigir manualmente


dx <- df %>%
  group_by(faixa_idade) %>%
  summarise(d = n()) %>%
  mutate(d = round((d / sum(d)) * 100000))

 

# l -----------------------------------------------

soma <- 100000
lx <- 100000
for (i in 1:16) {
  soma <- soma - dx$d[i]
  lx <- c(lx, soma)
}

lx <- c(lx, dx$d[18])


# L -----------------------------------------------

# como os fatores de separação k já foi calculado com especificadade para cada grupo etário, vamos considerar o k para todas as idades menos a 80+

n <- c(1, 4, rep(5, 15))
Lx <- 1 * lx[2] + kx$k[1] * dx$d[1]

for (i in 2:17) {
  Lx <- c(Lx, round(n[i] * lx[i+1] + kx$k[i] * dx$d[i]) )
}

Lx <- c(Lx, kx$k[18] * dx$d[18])


# T -----------------------------------------------

soma <- Lx[18]

Tx <- Lx[18]

for (i in 1:17) {
  soma <- soma + Lx[18 - i]
  Tx <- c(Tx, soma)
}

# Tabua de vida -----------------------------------------------

n <- c("1", "4", rep("5", 15), "+")

tabua_de_vida_feminina <- left_join(kx, dx, by = "faixa_idade") %>%
  mutate(n = n, lx = lx, Lx = Lx, Tx = rev(Tx)) %>%
  mutate(x = c(0, 1, seq(5, 80, by= 5)) ,ex = Tx / lx, qx = d / lx, dx = d, kx = k) %>%
  select(x, n, kx, qx, lx, dx, Lx, Tx, ex)
