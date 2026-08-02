\name{IBrCRMindex}
\alias{IBrCRMindex}
\title{Índice Brasileiro de Competitividade Regional Municipal}
\usage{
IBrCRMindex(
  df,
  variables,
  inverse_variables = NULL,
  group_by = NULL,
  adjust_outliers = TRUE,
  param_outlier_adjust = 3,
  standardization_method = c("mean", "discrete", "none", "min-max"),
  boruta_maxRuns = 100,
  boruta_pValue = 0.01,
  boruta_max_selected = NULL,
  cfa_estimator = "ML",
  target_variable = NULL,
  verbose = TRUE,
  log_fun = message,
  log_prefix = "IBrCRMindex",
  cfa_fallback = c("error", "uniform"),
  audit_log_path = NULL,
  subindex_name = NULL
)
}
\arguments{
\item{df}{Data frame municipal contendo, no mínimo, \code{code_muni} e
\code{ano}.}
\item{variables}{Vetor de nomes das variáveis candidatas.}
\item{inverse_variables}{Variáveis cuja interpretação é inversa.}
\item{group_by}{Nome da coluna usada para formar grupos comparáveis.}
\item{adjust_outliers}{Se \code{TRUE}, limita outliers pelo intervalo
interquartil.}
\item{param_outlier_adjust}{Multiplicador do intervalo interquartil.}
\item{standardization_method}{Método final de padronização.}
\item{boruta_maxRuns}{Número máximo de execuções do Boruta.}
\item{boruta_pValue}{Nível de significância do Boruta.}
\item{boruta_max_selected}{Limite de variaveis confirmadas pelo Boruta que
seguem para a CFA. Mantem as maiores importancias medias, com desempate pelo
nome. Use \code{NULL} para nao aplicar limite.}
\item{cfa_estimator}{Estimador passado para \code{lavaan::cfa()}.}
\item{target_variable}{Target opcional do Boruta. Se \code{NULL}, usa PC1.}
\item{verbose}{Se \code{TRUE}, imprime mensagens de progresso.}
\item{log_fun}{Função usada para emitir mensagens.}
\item{log_prefix}{Prefixo das mensagens.}
\item{cfa_fallback}{Comportamento quando a CFA falha. \code{"error"} interrompe;
\code{"uniform"} permite pesos iguais explicitamente.}
\item{audit_log_path}{Caminho opcional do CSV persistente com variáveis
selecionadas e pesos.}
\item{subindex_name}{Nome do subíndice gravado no log.}
}
\value{
Data frame com o índice e atributos de auditoria, incluindo variáveis
selecionadas, pesos, resultado do Boruta, relatório de seleção e estado da CFA.
}
\description{
Seleciona variáveis com Boruta, estima pesos por CFA, normaliza indicadores,
calcula o IBrCRM e opcionalmente atualiza um log persistente de auditoria.
}
