### parte da apresentação que explica a utilzação dos modelos da análise (não precisa colocar tudo que tá escrito, pois é para embasar o que podemos FALAR na apresentação, não necessariamente DEIXAR NOS SLIDES)

### CABE NA APRESENTAÇÃO:

Usamos regressão de Poisson com variância robusta (quasipoisson) porque:

- Queremos Razões de Prevalência, que são mais adequadas em inquéritos transversais.

- O modelo log-binomial, apesar de teoricamente ideal, pode falhar em convergir.

- O modelo logístico foi incluído apenas como análise comparativa, mas não é a principal medida de interesse.

### OUTRAS DISCUSSÕES QUE PODEM SER INTERESSANTES:

🔹 Por que usamos modelos de regressão neste estudo?

- Nosso objetivo é avaliar a associação entre consumo de ultraprocessados (UPF) e doenças crônicas (obesidade como desfecho primário e hipertensão e diabetes como secundários).

- Todas essas variáveis de desfecho são binárias (0/1: Sim/Não).

- Para esse tipo de dado, precisamos de modelos de regressão que estimem a medida de associação ajustada pelos confundidores (idade, sexo, escolaridade e atividade física).

🔹 Modelos possíveis
1. Regressão logística (binomial com link logit)

- É o modelo mais tradicional para desfechos binários.

- Estima odds ratios (OR).

- O problema: em estudos transversais com desfechos não raros (como obesidade ≈ 24% na nossa amostra), o OR não é uma boa aproximação da RP → ele tende a superestimar o efeito.

- Exemplo na nossa análise: OR = 0,61 pode corresponder a uma RP de 0,69.

2. Regressão log-binomial (binomial com link log)

- Modelo diretamente especificado para estimar Razões de Prevalência (RP).

- Interpretação: "A prevalência de obesidade entre consumidores de UPF é X vezes a prevalência entre não consumidores".

- É a medida que faz mais sentido em estudos transversais de inquéritos populacionais.

- Limitação: o modelo pode ter problemas de convergência numérica (não encontrar solução) quando a prevalência é alta ou quando há muitas variáveis no ajuste.

3. Regressão de Poisson com variância robusta (ou quasipoisson)

- Alternativa muito usada quando o log-binomial não converge.

- Ajusta um modelo de Poisson para dados binários (mesmo que Poisson seja "naturalmente" para contagens).

- Ao aplicar correção de variância robusta (ou família = quasipoisson), as estimativas dos coeficientes são válidas para calcular RP.

- Interpretação é a mesma do log-binomial: RP direta.

- É considerado o método padrão ouro em estudos transversais e coortes quando queremos RP ajustada.

🔹 Diferença entre OR e RP

RP (Razão de Prevalência):

- Interpretação direta: "consumidores de UPF têm x% mais/menos prevalência de obesidade comparados aos não consumidores".

- É intuitivo para saúde pública, porque trabalhamos com prevalência e risco absoluto.

OR (Odds Ratio):

- Mede a razão das odds (chances) da doença, não a prevalência.

- Só é próximo da RP quando a prevalência do desfecho é <10% (evento raro).

- Em prevalências comuns (20–30%, como obesidade), o OR exagera o efeito e pode induzir interpretações erradas.
