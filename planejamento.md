# Planejamento Atualizado — Trabalho Vigitel 2023

## 1. Definição da Pergunta

- **Hipótese principal:** O consumo frequente de ultraprocessados está associado a maior prevalência de **obesidade** e/ou **hipertensão** em adultos brasileiros.
- **Justificativa:** Ultraprocessados são reconhecidos pela literatura como fator de risco para doenças crônicas (densidade energética, sódio, gorduras trans/saturadas, aditivos).

---

## 2. Variáveis Principais

- **Exposição:**
    - Consumo de ultraprocessados (definir corte — ex.: ≥5 vezes por semana = exposição).
    - Variáveis candidatas no Vigitel: consumo de refrigerante, embutidos, biscoitos recheados, fast food (dependendo do que o banco traz em 2023).
    - Pode-se criar um **índice de ultraprocessados** combinando itens.
- **Desfechos:**
    - **Obesidade**: IMC ≥ 30 (calculado de peso/altura).
    - **Hipertensão**: diagnóstico médico autorreferido.
    - (Opcional: usar ambos e comparar resultados).
- **Confundidores candidatos:**
    - Sociodemográficos: **idade**, **sexo**, **escolaridade**, **renda**, **região/capital**.
    - Comportamentais: **atividade física**, **tabagismo**, **consumo de álcool**.
    - Atenção: **IMC** pode ser **mediador** no caminho ultraprocessados → hipertensão. Então:
        - Se objetivo = **efeito total**, **não ajustar por IMC** no modelo de hipertensão.
        - Se objetivo = **efeito direto independente do peso**, aí sim ajusta por IMC (e discute).

---

## 3. Preparação dos Dados

```r
Vigitel <- Vigitel %>%
  mutate(
    idade_cat = cut(idade, breaks=c(18,35,60,Inf), labels=c("18-34","35-59","60+"), right=FALSE),
    sexo = factor(sexo, levels=c(1,2), labels=c("Homem","Mulher")),
    esc_cat = factor(escolaridade),
    renda_cat = factor(renda),
    # exposição: definir variável ultraprocessados
    upf = ifelse(freq_upf >= 5, 1, 0),
    # desfechos
    obesid = ifelse(imc >= 30, 1, 0),
    hipert = ifelse(hipertensao == 1, 1, 0)
  )

```

---

## 4. Desenho Amostral

```r
library(survey)
vigi_svy <- svydesign(id=~1, weights=~pesorake, data=Vigitel)

```

---

## 5. Análise Descritiva

- Prevalência global dos desfechos (obesidade, hipertensão).
- Prevalência por exposição (consumo alto vs baixo de UPF).
- Distribuição de confundidores por exposição.

Exemplo:

```r
# Prevalência de obesidade por UPF
svyby(~I(obesid==1), ~upf, design=vigi_svy, svyciprop, vartype="ci")

```

---

## 6. Identificação de Confundidores

- Ver se cada candidato está associado à exposição **e** ao desfecho.
- Só entram no modelo final os que forem plausíveis como confundidores.
- Pode usar um **DAG no DAGitty** para justificar.

---

## 7. Modelagem Estatística

- **Bruto:**
    
    ```r
    m_bruto <- svyglm(obesid ~ upf, design=vigi_svy, family=quasipoisson(link="log"))
    
    ```
    
- **Ajustado (modelo principal):**
    
    ```r
    m_adj <- svyglm(obesid ~ upf + idade_cat + sexo + esc_cat + renda_cat,
                    design=vigi_svy, family=quasipoisson(link="log"))
    
    ```
    
- **Hipertensão:** repetir lógica, mas decidir se ajusta por IMC (efeito direto vs total).
- Interpretar `exp(coef)` como **Razão de Prevalências (RP)**.

---

## 8. Análises Adicionais

- **Interações/modificação de efeito**: testar se a associação varia por sexo ou faixa etária.
- **Sensibilidade:** rodar modelo sem ponderação para mostrar diferença → reforça importância do peso.
- **Alternativo:** OR via regressão logística (só para comparação, mas RP é preferida).

---

## 9. Resultados

- Tabelas com prevalência e RP bruta/ajustada.
- Gráficos simples (forest plot ou barras comparando prevalências).

---

## 10. Conclusões

- Responder à pergunta: *“Consumir ultraprocessados está associado a maior obesidade/hipertensão em adultos das capitais brasileiras?”*
- Discutir confundidores: idade provavelmente importante; sexo/escolaridade/renda dependem do resultado.
- Limitações: transversal → não estabelece temporalidade (causalidade reversa possível); viés de informação (autorreporte).
- Relevância: saúde pública (políticas de alimentação saudável).

---

## 11. Entregáveis

- **Código em R** (bem comentado).
- **Relatório** (objetivo, métodos, resultados, conclusão).
- **Apresentação em slides** (fluxo simples → pergunta → método → resultados principais → conclusão).

---

👉 Orientação final:

- **Parcimônia é chave**: não coloquem todas as variáveis no modelo; usem o DAG + checagem prática de confundimento.
- Se forem comparar dois desfechos (obesidade e hipertensão), sigam o mesmo roteiro em paralelo.
- Justifiquem sempre cada escolha no relatório: por que tal variável é confundidor, por que não incluíram mediadores.
