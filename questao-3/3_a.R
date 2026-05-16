library(tidyverse)
library(gt)

# ==============================================================================
# 0. EXECUTAR O SCRIPT 3_a.R PARA CARREGAR OS DADOS NA MEMÓRIA
# ==============================================================================
# O source vai rodar o seu código original e disponibilizar os objetos 'nmx' e 'nmx_primeiras_idades'
source("3_a.R", encoding = "UTF-8")

# ==============================================================================
# FUNÇÃO DEMOGRÁFICA PARA CONSTRUÇÃO DA TÁBUA DE VIDA CORRIGIDA
# ==============================================================================
construir_tabua_vida <- function(dados_sub, tmi_item_b) {
  
  # 1. Mapeia os limites inferiores corretos para ordenar a tábua de vida
  tabua <- dados_sub %>%
    mutate(
      lim_inf = case_when(
        faixa_etaria == "<1"   ~ 0,
        faixa_etaria == "1-4"  ~ 1,
        faixa_etaria == "5-9"  ~ 5,
        faixa_etaria == "10-14" ~ 10,
        faixa_etaria == "15-19" ~ 15,
        faixa_etaria == "20-24" ~ 20,
        faixa_etaria == "25-29" ~ 25,
        faixa_etaria == "30-34" ~ 30,
        faixa_etaria == "35-39" ~ 35,
        faixa_etaria == "40-44" ~ 40,
        faixa_etaria == "45-49" ~ 45,
        faixa_etaria == "50-54" ~ 50,
        faixa_etaria == "55-59" ~ 55,
        faixa_etaria == "60-64" ~ 60,
        faixa_etaria == "65-69" ~ 65,
        faixa_etaria == "70-74" ~ 70,
        faixa_etaria == "75-79" ~ 75,
        faixa_etaria == "80+"   ~ 80,
        TRUE                    ~ NA_real_
      ),
      # Amplitude de cada intervalo (n)
      n = case_when(
        faixa_etaria == "<1"  ~ 1,
        faixa_etaria == "1-4" ~ 4,
        faixa_etaria == "80+" ~ 999, # Convenção para intervalo aberto
        TRUE                  ~ 5
      )
    ) %>%
    filter(!is.na(lim_inf)) %>%
    arrange(lim_inf)
  
  # 2. Definir os Fatores de Separação (kx)
  # Usando as fórmulas lineares do seu enunciado para as idades 0 e 1-4
  tabua <- tabua %>%
    mutate(
      kx = case_when(
        faixa_etaria == "<1"  ~ 0.053 + 2.8 * tmi_item_b,
        faixa_etaria == "1-4" ~ 1.522 - 1.518 * tmi_item_b,
        TRUE                  ~ (n / 2) # Para as demais faixas, assume kx no meio do intervalo
      ),
      kx = round(kx, 2)
    )
  
  # 3. Probabilidade de Morte (qx) convertendo da nMx (dividido por 1000 pois sua taxa está por mil)
  tabua <- tabua %>%
    mutate(
      nMx_base1 = nMx / 1000,
      qx = case_when(
        faixa_etaria == "<1" ~ tmi_item_b,
        faixa_etaria == "80+" ~ 1.000000,
        TRUE                  ~ (n * nMx_base1) / (1 + (n - kx) * nMx_base1)
      )
    )
  
  # 4. Sobreviventes (lx) e Óbitos teóricos da coorte (dx)
  v_qx <- tabua$qx
  v_lx <- numeric(18)
  v_dx <- numeric(18)
  
  v_lx[1] <- 100000 # Raiz padrão da tábua
  
  for(i in 1:17) {
    v_dx[i] <- round(v_lx[i] * v_qx[i])
    v_lx[i+1] <- v_lx[i] - v_dx[i]
  }
  v_dx[18] <- v_lx[18] 
  
  tabua$lx <- v_lx
  tabua$dx <- v_dx
  
  # 5. Pessoas-ano vividas (Lx)
  v_Lx <- numeric(18)
  v_Lx[1] <- 1 * v_lx[2] + tabua$kx[1] * v_dx[1]
  
  for(i in 2:17) {
    v_Lx[i] <- tabua$n[i] * v_lx[i+1] + tabua$kx[i] * v_dx[i]
  }
  # Grupo Aberto 80+: Lx = lx / nMx (em base 1)
  v_Lx[18] <- tabua$lx[18] / tabua$nMx_base1[18]
  tabua$Lx <- round(v_Lx)
  
  # 6. Tx e Esperança de Vida (ex)
  v_Tx <- rev(cumsum(rev(tabua$Lx)))
  tabua$Tx <- v_Tx
  
  tabua <- tabua %>%
    mutate(
      ex = round(Tx / lx, 2),
      n_str = case_when(
        faixa_etaria == "<1"  ~ "1",
        faixa_etaria == "1-4" ~ "4",
        faixa_etaria == "80+" ~ "+",
        TRUE                  ~ "5"
      )
    ) %>%
    select(x = lim_inf, n = n_str, nMx, kx, qx, lx, dx, Lx, Tx, ex)
  
  return(tabua)
}

# ==============================================================================
# PREPARAÇÃO DOS BANCOS DESAGREGADOS (Combinando as idades iniciais com as adultas)
# ==============================================================================

# Unificando o banco bruto completo com faixas separadas (<1 e 1-4 separadas)
df_bruto_completo <- bind_rows(
  nmx_primeiras_idades,
  nmx %>% filter(!faixa_etaria %in% c("<1", "1-4"))
)

# Definição das TMIs reais calculadas (exemplo de Curitiba na base 1)
tmi_fem  <- 0.007119
tmi_masc <- 0.007120

# ==============================================================================
# GERAÇÃO DAS TÁBUAS DEFINITIVAS
# ==============================================================================
tv_masc_2010 <- df_bruto_completo %>% filter(ano == 2010, SEXO == "Masculino") %>% construir_tabua_vida(tmi_masc)
tv_fem_2010  <- df_bruto_completo %>% filter(ano == 2010, SEXO == "Feminino")  %>% construir_tabua_vida(tmi_fem)

tv_masc_2024 <- df_bruto_completo %>% filter(ano == 2024, SEXO == "Masculino") %>% construir_tabua_vida(tmi_masc)
tv_fem_2024  <- df_bruto_completo %>% filter(ano == 2024, SEXO == "Feminino")  %>% construir_tabua_vida(tmi_fem)

# ==============================================================================
# CONSOLE LOG - COMPARAÇÃO RÁPIDA E IMPRESSÃO DOS RESULTADOS REAIS
# ==============================================================================
print("--- TÁBUAS DE VIDA CALCULADAS COM SUCESSO ---")
print(paste("e0 Masculino 2010:", tv_masc_2010$ex[1], "anos"))
print(paste("e0 Feminino 2010:",  tv_fem_2010$ex[1],  "anos"))
print(paste("e0 Masculino 2024:", tv_masc_2024$ex[1], "anos"))
print(paste("e0 Feminino 2024:",  tv_fem_2024$ex[1],  "anos"))