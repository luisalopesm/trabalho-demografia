library(tidyverse)
library(microdatasus)
library(lubridate)

# ============================================
# 1. BAIXAR DADOS SIM
# ============================================

anos <- c(2010, 2021, 2024)

sim_lista <- lapply(anos, function(a){
  
  fetch_datasus(
    year_start = a,
    year_end = a,
    uf = "PR",
    information_system = "SIM-DO"
  )
  
})

sim <- bind_rows(sim_lista) %>%
  process_sim()

# ============================================
# 2. PREPARAR VARIÁVEIS
# ============================================

sim <- sim %>%
  
  mutate(
    
    ano = year(ymd(DTOBITO)),
    
    sexo = case_when(
      SEXO == "M" ~ "Masculino",
      SEXO == "F" ~ "Feminino",
      TRUE ~ NA_character_
    ),
    
    idade_num = as.numeric(substr(IDADE, 2, 4)),
    tipo_idade = substr(IDADE, 1, 1),
    
    idade_anos = case_when(
      tipo_idade == "4" ~ idade_num,
      tipo_idade == "3" ~ idade_num / 12,
      tipo_idade %in% c("1","2") ~ 0,
      TRUE ~ NA_real_
    ),
    
    # grupos de idade solicitados
    grupo_idade = case_when(
      idade_anos < 5 ~ "<5",
      idade_anos >= 5  & idade_anos <= 14 ~ "5-14",
      idade_anos >= 15 & idade_anos <= 39 ~ "15-39",
      idade_anos >= 40 & idade_anos <= 59 ~ "40-59",
      idade_anos >= 60 ~ "60+",
      TRUE ~ NA_character_
    ),
    
    # causa básica CID-10
    causa = CAUSABAS
  ) %>%
  
  filter(ano %in% c(2010, 2021, 2024))

# ============================================
# 3. TRATAR CAUSAS (CID-10)
# ============================================

sim_causas <- sim %>%
  
  mutate(
    
    causa_grupo = substr(causa, 1, 3)  # agrupar por 3 caracteres CID-10
    
  ) %>%
  
  filter(!is.na(causa_grupo))

# ============================================
# 4. TOP 20 CAUSAS (TOTAL)
# ============================================

top20 <- sim_causas %>%
  
  count(causa_grupo, sort = TRUE) %>%
  
  slice_max(n, n = 20) %>%
  
  pull(causa_grupo)

sim_top <- sim_causas %>%
  filter(causa_grupo %in% top20)

# ============================================
# 5. TABELA FINAL (SEXO + IDADE + ANO)
# ============================================

mortalidade_causas <- sim_top %>%
  
  group_by(ano, sexo, grupo_idade, causa_grupo) %>%
  
  summarise(
    obitos = n(),
    .groups = "drop"
  )

# ============================================
# 6. DESTACAR COVID-19

# ============================================

sim_top <- sim_top %>%
  
  mutate(
    covid = case_when(
      causa_grupo %in% c("B34", "U07") ~ "COVID-19 / Coronavírus",
      TRUE ~ "Outras causas"
    )
  )

covid_resumo <- sim_top %>%
  
  group_by(ano, sexo, grupo_idade, covid) %>%
  
  summarise(
    obitos = n(),
    .groups = "drop"
  )


###### GRÁFICO

estrutura_mort <- ggplot(mortalidade_causas,
       aes(x = causa_grupo,
           y = obitos,
           fill = factor(ano))) +
  
  geom_col(position = "dodge") +
  
  facet_wrap(~ sexo + grupo_idade, scales = "free_y") +
  
  labs(
    title = "Top 20 causas de morte (CID-10) - Paraná",
    x = "CID-10 (3 primeiros caracteres)",
    y = "Óbitos",
    fill = "Ano"
  ) +
  
  theme_minimal() +
  
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))