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
  cfa_estimator = "ML",
  target_variable = NULL,
  cfa_fallback = c("error", "uniform")
)
}
\arguments{
\item{df}{Data frame municipal contendo, no mínimo, \code{code_muni} e
\code{ano}.}

\item{variables}{Vetor de nomes das variáveis candidatas.}

\item{inverse_variables}{Variáveis cuja interpretação é inversa: valores
menores representam melhor desempenho.}

\item{group_by}{Nome da coluna usada para formar grupos comparáveis durante a
normalização.}

\item{adjust_outliers}{Se \code{TRUE}, limita outliers pelo intervalo
interquartil.}

\item{param_outlier_adjust}{Multiplicador do intervalo interquartil usado nos
limites de outliers.}

\item{standardization_method}{Método final: \code{"mean"},
\code{"discrete"}, \code{"none"} ou \code{"min-max"}.}

\item{boruta_maxRuns}{Número máximo de execuções do Boruta. Deve ser maior que
10.}

\item{boruta_pValue}{Nível de significância do Boruta.}

\item{cfa_estimator}{Estimador passado para \code{lavaan::cfa()}.}

\item{target_variable}{Target opcional do Boruta. Se \code{NULL}, usa a
primeira componente principal das candidatas.}

\item{cfa_fallback}{Comportamento quando a CFA não produz pesos válidos.
\code{"error"} interrompe a execução; \code{"uniform"} permite pesos iguais
com aviso explícito.}
}
\value{
Data frame com o índice por município, grupo e ano. O resultado contém os
atributos \code{selected_variables}, \code{weights}, \code{boruta_result} e
\code{cfa_status}.
}
\description{
Seleciona variáveis com Boruta, estima pesos por análise fatorial confirmatória,
normaliza indicadores e calcula o IBrCRM.
}
