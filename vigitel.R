#install.packages("gt")
library(purrr)
library(stringr)
library(survey)
library(dplyr)
library(broom)
library(gt)
library(readr)
library(table1)
Vigitel <- read_csv("Vigitel-2023-peso-rake.csv")
View(Vigitel)

Vigitel_mut <- Vigitel %>%
  mutate(
    #confundidoras
    idade_cat = factor(cut(q6, breaks = c(18, 35, 60, Inf), right = FALSE,
                    labels = c("18-34","35-59","60+"))),
    sexo = factor(q7, labels = c("Homem", "Mulher")),
    esc_cat   = factor(fesc),
    #anos_estudo = q8_anos,
    #exposição: ultraprocessados
    upf_cat = factor(score_upp_2cat, labels = c("Não", "Sim")),
    #upf_g = score_upp, #CONSUMO EM g/dia
    #desfecho: obesidade
    ob = factor(obesid, labels = c("Não", "Sim"))
  )

#tabela de contingência
tabela1 <- table1(~ factor(Vigitel_mut$ob) | Vigitel_mut$upf_cat, data = Vigitel_mut)
print(tabela1)

# ============================================================
# 2) Desenho amostral do Vigitel
# ============================================================
vigi_svy <- svydesign(
  id      = ~1,
  weights = ~pesorake,
  data    = Vigitel_mut
)

#3
#proporcao de obesidade

#prevalência global de obesidade
svyciprop(~I(ob=="Sim"), vigi_svy)
#aproximadamente 24% dos adultos são obesos (IC95% 22,7–25,3%)

#prevalência de obesidade por grupo de consumo de ultraprocessados
svyby(~I(ob=="Sim"),~upf_cat,design=vigi_svy, svyciprop,vartype="ci")
#obesidade em quem NÃO consome UPF: ~24,5% (IC95% 23,0–26,0%)
#obesidade em quem consome UPF: ~21,7% (IC95% 19,0–24,6%)
#obesidade ligeiramente MENOR entre consumidores de UPF (resultado bruto, ainda sem ajuste)

#prevalência de obesidade por grupo de idade
svyby(~I(ob=="Sim"),~idade_cat,design=vigi_svy, svyciprop,vartype="ci")
#18–34 anos: 19,9% (IC95% 17,7–22,3%)
#35–59 anos: 27,8% (IC95% 25,9–29,8%)
#60+ anos: 23,4% (IC95% 21,1–25,9%)
#obesidade é MAIS comum em 35–59 anos, intermediária em 60+ e MENOR em 18–34 anos

#prevalência de consumo de ultraprocessados por grupo de idade
svyby(~I(upf_cat=="Sim"), ~idade_cat, design=vigi_svy, svyciprop, vartype="ci")
#18–34 anos: 24,3% consomem UPF (IC95% 22,1–26,7%)
#35–59 anos: 15,7% (IC95% 14,1–17,4%)
#60+ anos: 9,4% (IC95% 7,9–11,1%)
#consumo de UPF é MAIS comum em jovens e cai bastante com a idade

#tabela de contingência
tabela1_age <- table1(~ factor(Vigitel_mut$idade_cat) | Vigitel_mut$upf_cat, data = Vigitel_mut)
print(tabela1_age)

# ============================================================
# Remover missings - exposição, desfecho ou confundidores
# ============================================================
vigi_svy2 <- subset(vigi_svy, !is.na(ob) & !is.na(upf_cat))

# Prevalência de obesidade por consumo de UPF
svyby(~I(ob=="Sim"), ~upf_cat, design=vigi_svy2, svyciprop, vartype="ci")

# Prevalência de obesidade por idade
svyby(~I(ob=="Sim"), ~idade_cat, design=vigi_svy2, svyciprop, vartype="ci")

# Prevalência de UPF por idade
svyby(~I(upf_cat=="Sim"), ~idade_cat, design=vigi_svy2, svyciprop, vartype="ci")


# ============================================================
# 3) Modelos
# ============================================================

# --- Modelo bruto (RP entre UPF e obesidade)
mbruto <- svyglm(I(ob=="Sim") ~ upf_cat,
                 design=vigi_svy,
                 subset = !is.na(ob) & !is.na(upf_cat),
                 family=quasipoisson)

exp(mbruto$coefficients)
exp(confint(mbruto))  # IC95% da RP


# --- Associação obesidade ~ idade
m_Idade <- svyglm(I(ob=="Sim") ~ idade_cat,
                  design=vigi_svy,
                  subset = !is.na(ob) & !is.na(upf_cat),
                  family=quasipoisson)
summary(m_Idade)


# --- Ajustado por idade
m1 <- svyglm(I(ob=="Sim") ~ upf_cat + idade_cat,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

exp(m1$coefficients)
exp(confint(m1))  # IC95% da RP


# --- Ajustado por sexo
m2 <- svyglm(I(ob=="Sim") ~ upf_cat + sexo,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

exp(m2$coefficients)
exp(confint(m2))


# --- Ajustado por escolaridade
m3 <- svyglm(I(ob=="Sim") ~ upf_cat + esc_cat,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

exp(m3$coefficients)
exp(confint(m3))


# --- Modelo com idade + escolaridade
m4 <- svyglm(I(ob=="Sim") ~ upf_cat + idade_cat + esc_cat,
             design=vigi_svy,
             subset = !is.na(ob) & !is.na(upf_cat),
             family=quasipoisson)

exp(m4$coefficients)
exp(confint(m4))


# ============================================================
# Interação: testar se o efeito de UPF varia por idade
# ============================================================
model_int <- svyglm(I(ob=="Sim") ~ upf_cat*idade_cat,
                    design=vigi_svy,
                    subset = !is.na(ob) & !is.na(upf_cat),
                    family=quasipoisson)

summary(model_int)


# ============================================================
# Tabela final com RP, IC95% e p-valor
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
    filter(term=="upf_catSim") %>% mutate(Modelo="Ajustada por Idade + Escolaridade")
) %>%
  transmute(
    Modelo,
    `RP` = estimate,
    `IC95% (inferior)` = conf.low,
    `IC95% (superior)` = conf.high,
    `p-valor` = p.value
  )

# Renderizar tabela bonita
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


#pesorake --> usando regressão de poisson tem que usar o exemplo dela

#fazer intervalo de confiança pra razão