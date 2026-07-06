# Análise do Projeto: IBrCRM_index

## 1. Visão Geral

O projeto `IBrCRM_index` é um pacote R chamado `IBrCRMindex`, cujo objetivo é calcular o **Índice Brasileiro de Competitividade Regional Municipal (IBrCRM)**. O índice seleciona as variáveis mais relevantes de um conjunto candidato, atribui pesos a cada uma e normaliza os dados para permitir comparações justas entre municípios e ao longo do tempo.

O pacote foi desenvolvido por pesquisadores da **Universidade Federal da Paraíba (UFPB)** e do **Laboratório de Economia e Avaliação de Políticas Públicas (LEAP)**. A UFPB consta no `DESCRIPTION` como detentora dos direitos (`cph`) e financiadora (`fnd`).

## 2. Estrutura do Projeto

O projeto segue a estrutura padrão de um pacote R:

| Caminho | Descrição |
| ------- | --------- |
| `R/IBrCRM_index.R` | Código-fonte da função principal `IBrCRMindex()` e da auxiliar `plot_boruta_results()`. |
| `man/IBrCRMindex.Rd` | Documentação da função no formato R documentation (Rd), gerada via roxygen2. |
| `DESCRIPTION` | Metadados do pacote: nome, versão, autores, descrição, licença e dependências. |
| `NAMESPACE` | Funções exportadas e importadas de outros pacotes. |
| `test/` | Script de exemplo (`example_pedro.R`) e dados de teste. |
| `LICENSE` | Licença do projeto (MIT). |
| `README.md` | Introdução ao projeto. |
| `_pkgdown.yml` | Configuração do site de documentação gerado com `pkgdown`. |

## 3. Funcionalidade Principal: `IBrCRMindex()`

A função `IBrCRMindex()` é o núcleo do pacote. A versão atual do código implementa a **"Versão Boruta"**: seleciona variáveis via **Boruta** e calcula os pesos via **Análise Fatorial Confirmatória (CFA)** com `lavaan`. O processo tem as seguintes etapas:

1. **Preparação e limpeza dos dados.** Mantém `code_muni`, `ano` e as variáveis candidatas presentes na base; descarta variáveis com mais de 80% de valores ausentes e variáveis com variância próxima de zero.
2. **Seleção de variáveis (Boruta).** Aplica imputação simples pela mediana apenas para rodar o algoritmo. Por padrão, o alvo (`target`) é o primeiro componente principal (PC1) das candidatas, obtido por `prcomp`; alternativamente, aceita um `target_variable` informado ou usa a média por linha como fallback. Após o Boruta, aplica `TentativeRoughFix` e extrai os atributos confirmados, com um teto de `max(5, floor(0.5 * p))` variáveis selecionadas.
3. **Geração de pesos (CFA).** Ajusta um modelo fatorial de fator único (`fator =~ v1 + v2 + ...`) sobre as variáveis selecionadas e padronizadas, usando `lavaan::cfa` com o estimador definido em `cfa_estimator`. As cargas fatoriais padronizadas viram os pesos. O ajuste ocorre quando há pelo menos 50 observações e 2 variáveis selecionadas.
4. **Normalização.** Normaliza as variáveis para uma escala comum dentro de cada grupo de `ano × group_by`, com ajuste de outliers baseado no IQR (multiplicador definido por `param_outlier_adjust`, padrão 3). Variáveis listadas em `inverse_variables` são invertidas antes da agregação (casos em que valores menores indicam melhor desempenho, como taxa de mortalidade).
5. **Cálculo e padronização final.** Agrega as variáveis normalizadas e ponderadas no IBrCRM por município e ano. O índice final pode ser padronizado (`mean`, `min-max`, `discrete`, `none`) e categorizado em níveis (Muito Baixo, Baixo, Médio, Alto, Muito Alto).

O retorno é um `data.frame` com o IBrCRM e atributos úteis para auditoria: `selected_variables`, `weights`, `boruta_result` e `selection_report`. A função auxiliar `plot_boruta_results()` gera o gráfico de importância do Boruta.

### Parâmetros da Função

Assinatura atual (`R/IBrCRM_index.R`):

- `df`: `data.frame` de entrada; exige as colunas `code_muni` e `ano`.
- `variables`: vetor com os nomes das variáveis candidatas.
- `inverse_variables`: variáveis a inverter (quanto menor, melhor).
- `group_by`: coluna de agrupamento para a normalização (ex.: `"name_biome_region_area"`). Se `NULL`, não agrupa.
- `adjust_outliers`: `TRUE`/`FALSE` para o ajuste de outliers.
- `param_outlier_adjust`: multiplicador do IQR no ajuste de outliers (padrão 3).
- `standardization_method`: padronização do índice final (`"mean"`, `"discrete"`, `"none"`, `"min-max"`).
- `boruta_maxRuns`: número máximo de iterações do Boruta.
- `boruta_pValue`: p-valor do Boruta.
- `cfa_estimator`: estimador do `lavaan` (ex.: `"ML"`).
- `target_variable`: alvo opcional do Boruta; se `NULL`, usa o PC1 das candidatas.
- `verbose`, `log_fun`, `log_prefix`: controle do log da seleção.

## 4. Dependências

Pelo código-fonte, a função exige em tempo de execução (`requireNamespace`): `dplyr`, `tidyr`, `Boruta`, `lavaan`, `scales` e `tibble`, além de `stats` (`prcomp`, `var`).

> **Observação:** o arquivo `DESCRIPTION` está desatualizado em relação ao código. Ele ainda importa `glmnet` (resquício da versão antiga com Elastic Net + PCA) e não declara `Boruta`, `lavaan` nem `tibble`. Vale alinhar o campo `Imports` do `DESCRIPTION` com as dependências reais da Versão Boruta.

## 5. Dados de Teste

O diretório `test/` contém o arquivo de exemplo `df_agregado_bases_inputado.rds` (15 MB), um `data.frame` salvo em formato R. O script `example_pedro.R` demonstra como carregar esses dados e usar `IBrCRMindex()` para calcular um subíndice, agrupando por bioma e região.
