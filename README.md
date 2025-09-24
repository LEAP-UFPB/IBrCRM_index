# Análise do Projeto: IBrCRM_index

## 1. Visão Geral

O projeto `IBrCRM_index` consiste em um pacote R chamado `IBrCRMindex`. O objetivo principal deste pacote é calcular o **Índice Brasileiro de Competitividade Regional Municipal (IBrCRM)**, um índice de desigualdade que seleciona variáveis importantes, atribui pesos a elas e normaliza os dados para permitir comparações justas.

O pacote foi desenvolvido por uma equipe de pesquisadores associados à **Universidade Federal da Paraíba (UFPB)** e ao **Instituto de Pesquisa Econômica Aplicada (Ipea)**, conforme indicado no arquivo `DESCRIPTION`.

## 2. Estrutura do Projeto

O projeto segue a estrutura padrão de um pacote R, com os seguintes diretórios e arquivos principais:

| Caminho                               | Descrição                                                                                             |
| ------------------------------------- | ----------------------------------------------------------------------------------------------------- |
| `R/IBrCRM_index.R`                    | Contém o código-fonte da função principal `IBrCRMindex()`, que implementa toda a lógica do cálculo.   |
| `man/IBrCRMindex.Rd`                  | Arquivo de documentação oficial da função `IBrCRMindex()`, no formato R documentation (Rd).           |
| `DESCRIPTION`                         | Metadados do pacote, incluindo nome, versão, autores, descrição, licença e dependências.              |
| `NAMESPACE`                           | Declara as funções que o pacote exporta e as funções que importa de outros pacotes.                   |
| `test/`                               | Contém arquivos para teste, incluindo um script de exemplo (`example_pedro.R`) e dados de teste.      |
| `LICENSE`                             | Arquivo de licença do projeto (MIT).                                                                  |
| `README.md`                           | Arquivo de introdução ao projeto.                                                                     |
| `_pkgdown.yml`                        | Arquivo de configuração para gerar um site de documentação com o pacote `pkgdown`.                    |

## 3. Funcionalidade Principal: `IBrCRMindex()`

A função `IBrCRMindex()` é o coração do pacote. Ela executa um processo de várias etapas para calcular o índice:

1.  **Seleção de Variáveis:** Utiliza um modelo de regressão **Elastic Net** para identificar as variáveis mais relevantes para o índice, com base em um conjunto de variáveis de referência.
2.  **Geração de Pesos:** Emprega a **Análise de Componentes Principais (PCA)** para calcular os pesos de cada variável selecionada. Alternativamente, pode atribuir pesos uniformes.
3.  **Normalização das Variáveis:** Normaliza os valores das variáveis para uma escala comum (0 a 1), tratando outliers e invertendo variáveis onde valores menores são melhores (ex: taxa de mortalidade).
4.  **Cálculo do Índice:** Calcula o IBrCRM como uma soma ponderada das variáveis normalizadas para cada município e ano.
5.  **Padronização Final:** O índice final pode ser padronizado de diferentes maneiras (média, min-max) ou categorizado em níveis (Muito Baixo, Baixo, Médio, Alto, Muito Alto).

### Parâmetros da Função

A função aceita vários parâmetros para customizar o cálculo:

-   `df`: O `data.frame` de entrada contendo os dados.
-   `variables`: Vetor com os nomes das variáveis a serem consideradas.
-   `reference_variables`: Variáveis de referência para o modelo de seleção.
-   `inverse_variables`: Variáveis que devem ser invertidas (quanto menos, melhor).
-   `group_by`: Variável para agrupar os dados antes da normalização (ex: por região).
-   `adjust_outliers`: Booleano para ativar ou desativar o ajuste de outliers.
-   `include_weight`: Booleano para usar pesos do PCA ou pesos uniformes.
-   `standardization_method`: Método de padronização do índice final (`mean`, `min-max`, `discrete`, `none`).

## 4. Dependências

O pacote depende de várias bibliotecas R para análise de dados e modelagem estatística:

-   `dplyr` e `tidyr`: Para manipulação e organização de dados.
-   `glmnet`: Para o modelo de seleção de variáveis (Elastic Net).
-   `stats`: Para funções estatísticas, incluindo PCA (`princomp`).
-   `scales`: Para normalização de dados (`rescale`).
-   `ggplot2`: Sugerido, provavelmente para visualização dos resultados.

## 5. Dados de Teste

O diretório `test/` contém um arquivo de dados de exemplo, `df_agregado_bases_inputado.rds` (15MB), que é um `data.frame` R salvo. O script `example_pedro.R` demonstra como carregar esses dados e utilizar a função `IBrCRMindex` para calcular um subíndice econômico, agrupando por bioma e região.
