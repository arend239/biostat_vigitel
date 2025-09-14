library(purrr)
library(stringr)
library(survey)
library(dplyr)
library(broom)
library(readr)
library(table1)
#remove.packages("rlang")
#install.packages("rlang", dependencies = TRUE)
library(rlang)
library(readr)

# ============================================================
# 1) Importar e preparar os dados
# ============================================================
Vigitel <- read_csv("Vigitel-2023-peso-rake.csv")

Vigitel_mut <- Vigitel %>%
  mutate(
    # Confundidores
    idade_cat = factor(cut(q6, breaks = c(18, 35, 60, Inf), right = FALSE,
                           labels = c("18-34","35-59","60+"))),
    sexo = factor(q7, labels = c("Homem", "Mulher")),
    esc_cat   = factor(fesc),
    atividade_fisica = factor(q42, labels = c("Sim", "Não")),  # variável nova
    
    # Exposição: ultraprocessados
    upf_cat = factor(score_upp_2cat, labels = c("Não", "Sim")),
    
    # Desfecho: obesidade
    ob = factor(obesid, labels = c("Não", "Sim")),
    
    pesorake = as.numeric(pesorake)
  )

# ============================================================
# 2) Desenho amostral
# ============================================================
vigi_svy <- svydesign(
  id      = ~1,
  weights = ~pesorake,
  data    = Vigitel_mut
)

# ============================================================
# 3) Modelos
# ============================================================

# --- Modelo bruto
mbruto <- svyglm(I(ob=="Sim") ~ upf_cat,
                 design=vigi_svy,
                 subset = !is.na(ob) & !is.na(upf_cat),
                 family=quasipoisson)

# --- Ajustado por idade
m1 <- svyglm(I(ob=="Sim") ~ upf_cat + idade_cat,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

# --- Ajustado por sexo
m2 <- svyglm(I(ob=="Sim") ~ upf_cat + sexo,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

# --- Ajustado por escolaridade
m3 <- svyglm(I(ob=="Sim") ~ upf_cat + esc_cat,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

# --- Ajustado por atividade física
m4 <- svyglm(I(ob=="Sim") ~ upf_cat + atividade_fisica,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat) & !is.na(atividade_fisica),
             family=quasipoisson)

# --- Ajustado por idade + escolaridade + atividade física
m5 <- svyglm(I(ob=="Sim") ~ upf_cat + idade_cat + esc_cat + atividade_fisica,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat) & !is.na(atividade_fisica),
             family=quasipoisson)

# ============================================================
# 4) Tabela final com RP, IC95% e p-valor
# ============================================================
tab_final <- bind_rows(
  tidy(mbruto, conf.int=TRUE, exponentiate=TRUE) %>%
    filter(term=="upf_catSim") %>% mutate(Modelo="Bruta"),
  tidy(m1, conf.int=TRUE, exponentiate=TRUE) %>%
    filter(term=="upf_catSim") %>% mutate(Modelo="Ajustada por Idade"),
  tidy(m2, conf.int=TRUE, exponentiate=TRUE) %>%
    filter(term=="upf_catSim") %>% mutate(Modelo="Ajustada por Sexo"),
  tidy(m3, conf.int=TRUE, exponentiate=TRUE) %>%
    filter(term=="upf_catSim") %>% mutate(Modelo="Ajustada por Escolaridade"),
  tidy(m4, conf.int=TRUE, exponentiate=TRUE) %>%
    filter(term=="upf_catSim") %>% mutate(Modelo="Ajustada por Atividade Física"),
  tidy(m5, conf.int=TRUE, exponentiate=TRUE) %>%
    filter(term=="upf_catSim") %>% mutate(Modelo="Ajustada por Idade + Escolaridade + Atividade Física")
) %>%
  transmute(
    Modelo,
    `RP` = estimate,
    `IC95% (inferior)` = conf.low,
    `IC95% (superior)` = conf.high,
    `p-valor` = p.value
  )

# Renderizar tabela
gt_tab <- tab_final %>%
  mutate(
    RP = sprintf("%.2f", RP),
    `IC95% (inferior)` = sprintf("%.2f", `IC95% (inferior)`),
    `IC95% (superior)` = sprintf("%.2f", `IC95% (superior)`),
    `p-valor` = ifelse(`p-valor`<0.001, "<0,001",
                       sub("\\.", ",", sprintf("%.3f", `p-valor`)))
  ) %>%
  gt::gt() %>%
  gt::tab_header(title="Razões de Prevalência (RP) – Obesidade vs Consumo de Ultraprocessados")

gt_tab

# ============================================================
# 5) DESFECHOS SECUNDÁRIOS: Hipertensão e Diabetes
# ============================================================

# Recodificar variáveis (1=Sim, 2=Não) e descartar 777 ("não sei")
Vigitel_mut <- Vigitel_mut %>%
  mutate(
    hipertensao = case_when(
      q75 %in% c(1,2) ~ factor(q75, levels=c(2,1), labels=c("Não","Sim")),
      TRUE ~ NA
    ),
    diabetes = case_when(
      q76 %in% c(1,2) ~ factor(q76, levels=c(2,1), labels=c("Não","Sim")),
      TRUE ~ NA
    )
  )

# Atualizar desenho com as novas variáveis
vigi_svy <- svydesign(
  id      = ~1,
  weights = ~pesorake,
  data    = Vigitel_mut
)

# ------------------------------------------------------------
# Função auxiliar para rodar análises por desfecho
# ------------------------------------------------------------
analise_desfecho <- function(desfecho, label){
  m_bruto <- svyglm(as.formula(paste0("I(", desfecho, "=='Sim') ~ upf_cat")),
                    design=vigi_svy,
                    subset = !is.na(get(desfecho)),
                    family=quasipoisson)
  
  m_ajust <- svyglm(as.formula(paste0("I(", desfecho, "=='Sim') ~ upf_cat + idade_cat + sexo + esc_cat + atividade_fisica")),
                    design=vigi_svy,
                    subset = !is.na(get(desfecho)),
                    family=quasipoisson)
  
  res <- bind_rows(
    tidy(m_bruto, conf.int=TRUE, exponentiate=TRUE) %>%
      filter(term=="upf_catSim") %>% mutate(Modelo="Bruto", Desfecho=label),
    tidy(m_ajust, conf.int=TRUE, exponentiate=TRUE) %>%
      filter(term=="upf_catSim") %>% mutate(Modelo="Ajustado", Desfecho=label)
  ) %>%
    transmute(
      Desfecho,
      Modelo,
      `RP` = estimate,
      `IC95% (inferior)` = conf.low,
      `IC95% (superior)` = conf.high,
      `p-valor` = p.value
    )
  return(res)
}

# ------------------------------------------------------------
# Rodar para cada desfecho secundário
# ------------------------------------------------------------
res_hipert <- analise_desfecho("hipertensao", "Hipertensão")
res_diab   <- analise_desfecho("diabetes", "Diabetes")

# Juntar resultados
tab_secundarios <- bind_rows(res_hipert, res_diab)

# Renderizar tabela
gt_tab2 <- tab_secundarios %>%
  mutate(
    RP = sprintf("%.2f", RP),
    `IC95% (inferior)` = sprintf("%.2f", `IC95% (inferior)`),
    `IC95% (superior)` = sprintf("%.2f", `IC95% (superior)`),
    `p-valor` = ifelse(`p-valor`<0.001, "<0,001",
                       sub("\\.", ",", sprintf("%.3f", `p-valor`)))
  ) %>%
  gt::gt() %>%
  gt::tab_header(title="Razões de Prevalência (RP) – Consumo de Ultraprocessados e Doenças Crônicas")

gt_tab2

# ============================================================
# 6) Comparar com Modelos Logísticos (OR)
# ============================================================

# ------------------------------------------------------------
# Função auxiliar para rodar análises logísticas
# ------------------------------------------------------------
analise_logit <- function(desfecho, label){
  m_bruto <- svyglm(as.formula(paste0("I(", desfecho, "=='Sim') ~ upf_cat")),
                    design=vigi_svy,
                    subset = !is.na(get(desfecho)),
                    family=binomial(link="logit"))
  
  m_ajust <- svyglm(as.formula(paste0("I(", desfecho, "=='Sim') ~ upf_cat + idade_cat + sexo + esc_cat + atividade_fisica")),
                    design=vigi_svy,
                    subset = !is.na(get(desfecho)),
                    family=binomial(link="logit"))
  
  res <- bind_rows(
    tidy(m_bruto, conf.int=TRUE, exponentiate=TRUE) %>%
      filter(term=="upf_catSim") %>% mutate(Modelo="Bruto", Desfecho=label),
    tidy(m_ajust, conf.int=TRUE, exponentiate=TRUE) %>%
      filter(term=="upf_catSim") %>% mutate(Modelo="Ajustado", Desfecho=label)
  ) %>%
    transmute(
      Desfecho,
      Modelo,
      `OR` = estimate,
      `IC95% (inferior)` = conf.low,
      `IC95% (superior)` = conf.high,
      `p-valor` = p.value
    )
  return(res)
}

# ------------------------------------------------------------
# Rodar para obesidade, hipertensão e diabetes
# ------------------------------------------------------------
res_logit_ob   <- analise_logit("ob", "Obesidade")
res_logit_hip  <- analise_logit("hipertensao", "Hipertensão")
res_logit_diab <- analise_logit("diabetes", "Diabetes")

# Juntar resultados
tab_logit <- bind_rows(res_logit_ob, res_logit_hip, res_logit_diab)

# ------------------------------------------------------------
# Renderizar tabela de ORs
# ------------------------------------------------------------
gt_tab_logit <- tab_logit %>%
  mutate(
    OR = sprintf("%.2f", OR),
    `IC95% (inferior)` = sprintf("%.2f", `IC95% (inferior)`),
    `IC95% (superior)` = sprintf("%.2f", `IC95% (superior)`),
    `p-valor` = ifelse(`p-valor`<0.001, "<0,001",
                       sub("\\.", ",", sprintf("%.3f", `p-valor`)))
  ) %>%
  gt::gt() %>%
  gt::tab_header(title="Razões de Chances (OR) – Consumo de Ultraprocessados e Desfechos Crônicos")

gt_tab_logit




