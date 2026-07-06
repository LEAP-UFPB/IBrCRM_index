# Análise do Projeto: IBrCRM_index

## 1. Visão Geral

O `IBrCRM_index` é um pacote R chamado `IBrCRMindex` que calcula o **Índice Brasileiro de Competitividade Regional Municipal (IBrCRM)**. O índice seleciona as variáveis mais relevantes de um conjunto candidato, atribui pesos a cada uma e normaliza os dados para permitir comparações entre municípios e ao longo do tempo. Foi desenvolvido por pesquisadores da **Universidade Federal da Paraíba (UFPB)** e do **Laboratório de Economia e Avaliação de Políticas Públicas (LEAP)**.

## 2. Estrutura do Projeto

O projeto segue a estrutura padrão de um pacote R:

| Caminho | Descrição |
| ------- | --------- |
| `R/IBrCRM_index.R` | Código-fonte da função principal `IBrCRMindex()` e das auxiliares `plot_boruta_results()`, `get_index_info()` e `print_selection_report()`. |
| `man/IBrCRMindex.Rd` | Documentação da função no formato R documentation (Rd), gerada via roxygen2. |
| `DESCRIPTION` | Metadados do pacote: nome, versão, autores, descrição, licença e dependências. |
| `NAMESPACE` | Funções exportadas e importadas de outros pacotes. |
| `test/` | Script de exemplo (`example_pedro.R`) e dados de teste. |
| `LICENSE` | Licença do projeto (MIT). |
| `README.md` | Introdução ao projeto. |
| `_pkgdown.yml` | Configuração do site de documentação gerado com `pkgdown`. |

## 3. Funcionalidade Principal: `IBrCRMindex()`

A função `IBrCRMindex()` é o núcleo do pacote (Versão Boruta): seleciona variáveis via **Boruta** e calcula os pesos via **Análise Fatorial Confirmatória (CFA)** com `lavaan`. O processo tem as seguintes etapas:

1. **Preparação e limpeza dos dados.** Exige as colunas `code_muni` e `ano`. Mantém as variáveis candidatas presentes na base e descarta as que têm mais de 80% de valores ausentes e as com variância próxima de zero.
2. **Seleção de variáveis (Boruta).** Aplica imputação pela mediana apenas para rodar o algoritmo. Por padrão, o alvo é o primeiro componente principal (PC1) das candidatas, obtido por `prcomp`; alternativamente, aceita um `target_variable` informado ou usa a média por linha como fallback. Com `set.seed(42)`, roda o Boruta, aplica `TentativeRoughFix` e extrai os atributos confirmados — recorrendo aos tentativos, e depois às primeiras variáveis, caso nada seja confirmado. Aplica um teto de `max(5, floor(0.5 * p))` variáveis, ordenando pela importância média (`meanImp`).
3. **Geração de pesos (CFA).** Usa pesos uniformes (`1/k`) por padrão. Quando há pelo menos 50 observações e 2 variáveis selecionadas, ajusta um modelo fatorial de fator único (`fator =~ v1 + v2 + ...`) sobre as variáveis padronizadas, com `lavaan::cfa` (`std.lv = TRUE`, `missing = "fiml"`, estimador definido por `cfa_estimator`). Os pesos são as cargas fatoriais padronizadas, tomadas em módulo e normalizadas para somar 1.
4. **Normalização.** Para cada grupo de `group_by × ano × variável`, inverte o sinal das variáveis listadas em `inverse_variables`, aplica o ajuste de outliers por IQR (limites em `q1 - k·IQR` e `q3 + k·IQR`, com `k = param_outlier_adjust`) quando `adjust_outliers = TRUE`, e reescala os valores para o intervalo de 0 a 1.
5. **Cálculo do índice.** Agrega as variáveis como soma ponderada dos valores normalizados por município, grupo e ano.
6. **Padronização final.** Conforme `standardization_method`: `mean` (desvio relativo à média do grupo), `min-max` (reescala de 0 a 1), `discrete` (reescala de 0 a 1 e categorização em Muito baixo, Baixo, Médio, Alto e Muito alto) ou `none`.

O retorno é um `data.frame` com o IBrCRM e os atributos `selected_variables`, `weights`, `boruta_result` e `selection_report`. O `selection_report` registra as contagens e percentuais de seleção, as variáveis selecionadas, as descartadas (por NA e por variância) e um log em texto.

### Parâmetros da Função

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

Em tempo de execução, a função exige `dplyr`, `tidyr`, `Boruta`, `lavaan`, `scales` e `tibble`, além de `stats` (`prcomp`, `var`, `quantile`).

## 5. Funções Auxiliares

- `plot_boruta_results()`: gera o gráfico de importância das variáveis do Boruta.
- `get_index_info()`: retorna as variáveis selecionadas, os pesos, o número de variáveis e o `selection_report`.
- `print_selection_report()`: imprime as contagens de seleção e a lista de variáveis selecionadas.

## 6. Dados de Teste

O diretório `test/` contém o arquivo de exemplo `df_agregado_bases_inputado.rds` (15 MB), um `data.frame` salvo em formato R. O script `example_pedro.R` demonstra como carregar esses dados e usar `IBrCRMindex()` para calcular um subíndice, agrupando por bioma e região.
