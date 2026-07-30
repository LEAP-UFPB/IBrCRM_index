#' Índice Brasileiro de Competitividade Regional Municipal (IBrCRM) - Versão Boruta
#'
#' @description Criação de um índice de desigualdade capaz de selecionar variáveis importantes
#' usando a metodologia Boruta, calcular pesos através de Análise Fatorial Confirmatória (CFA)
#' e normalizar as variáveis selecionadas, eliminando a necessidade de variáveis target.
#'
#' @param df Data frame contendo os dados municipais
#' @param variables Vetor de caracteres especificando as variáveis candidatas para o índice
#' @param inverse_variables Vetor de caracteres especificando variáveis cuja interpretação é inversa (quanto menor, melhor)
#' @param group_by Nome da coluna para agrupamento (opcional)
#' @param adjust_outliers Valor lógico indicando se deve ajustar outliers
#' @param param_outlier_adjust Parâmetro numérico para ajuste de outliers (padrão: 3)
#' @param standardization_method Método de padronização: 'mean', 'discrete', 'none', 'min-max'
#' @param boruta_maxRuns Número máximo de execuções do algoritmo Boruta (padrão: 100)
#' @param boruta_pValue Valor p para o teste de significância do Boruta (padrão: 0.01)
#' @param cfa_estimator Estimador para CFA: 'ML', 'WLSMV', 'ULS' (padrão: 'ML')
#' @param target_variable Variável target para o Boruta (se NULL, usa primeira componente principal)
#'
#' @return Um data.frame com o índice calculado
#' @import dplyr tidyr Boruta lavaan stats scales
#' @export
#'
#' @examples
#' library(dplyr)
#' variables <- c('mpg','cyl','hp','drat','wt','qsec','vs','am','gear')
#' inverse_variables <- c('wt')
#' 
#' IBrCRM <- IBrCRMindex_boruta(mtcars, variables = variables, 
#'                              inverse_variables = inverse_variables,
#'                              adjust_outliers = TRUE,
#'                              standardization_method = 'mean')

IBrCRMindex <- function(df, variables, inverse_variables = NULL,
                        group_by = NULL, adjust_outliers = TRUE, 
                        param_outlier_adjust = 3,
                        standardization_method = c('mean','discrete','none','min-max'),
                        boruta_maxRuns = 100, boruta_pValue = 0.01,
                        cfa_estimator = 'ML', target_variable = NULL,
                        cfa_fallback = c("error", "uniform")) {
  cfa_fallback <- match.arg(cfa_fallback)
  
  # Carrega pacotes necessários
  if (!requireNamespace("Boruta", quietly = TRUE)) {
    stop("Pacote 'Boruta' não encontrado. Instale com: install.packages('Boruta')")
  }
  if (!requireNamespace("lavaan", quietly = TRUE)) {
    stop("Pacote 'lavaan' não encontrado. Instale com: install.packages('lavaan')")
  }
  
  # Desativa mensagens informativas do dplyr
  options(dplyr.summarise.inform = FALSE)
  
  cat("=== INICIANDO PROCESSO IBrCRM COM BORUTA ===\n")
  
  # -------------------------------
  # ETAPA 1: PREPARAÇÃO DOS DADOS
  # -------------------------------
  
  cat("Etapa 1: Preparando dados...\n")
  
  # Verifica se as colunas obrigatórias existem
  required_cols <- c("code_muni", "ano")
  missing_cols <- setdiff(required_cols, colnames(df))
  if (length(missing_cols) > 0) {
    stop(paste("Colunas obrigatórias não encontradas:", paste(missing_cols, collapse = ", ")))
  }
  
  # Seleciona apenas as variáveis de interesse
  available_vars <- intersect(variables, colnames(df))
  if (length(available_vars) == 0) {
    stop("Nenhuma das variáveis especificadas foi encontrada no dataframe.")
  }
  
  # Prepara dados para análise
  df_analysis <- df %>%
    dplyr::select(all_of(c("code_muni", "ano", available_vars)))
  
  # Remove variáveis com muitos valores ausentes (>80%)
  missing_prop <- df_analysis %>%
    dplyr::select(all_of(available_vars)) %>%
    summarise_all(~ mean(is.na(.))) %>%
    tidyr::pivot_longer(everything(), names_to = "var", values_to = "missing_prop")
  
  high_missing_vars <- missing_prop %>%
    dplyr::filter(missing_prop > 0.8) %>%
    dplyr::pull(var)
  
  if (length(high_missing_vars) > 0) {
    cat("  - Removendo variáveis com >80% de valores ausentes:", paste(high_missing_vars, collapse = ", "), "\n")
    available_vars <- setdiff(available_vars, high_missing_vars)
    df_analysis <- df_analysis %>% dplyr::select(all_of(c("code_muni", "ano", available_vars)))
  }
  
  # Remove linhas com muitos valores ausentes
  df_analysis <- df_analysis %>%
    tidyr::drop_na()
  
  if (nrow(df_analysis) == 0) {
    stop("Não há dados suficientes após remoção de valores ausentes.")
  }
  
  # -------------------------------
  # ETAPA 2: SELEÇÃO DE VARIÁVEIS COM BORUTA
  # -------------------------------
  
  cat("Etapa 2: Executando seleção de variáveis com Boruta...\n")
  
  # Prepara dados para Boruta (média por ano para reduzir dimensionalidade)
  # Mantem as observacoes municipais. Agregar previamente por ano reduz o
  # painel inteiro a poucas linhas e pode alterar radicalmente a selecao.
  df_boruta <- df_analysis %>%
    dplyr::select(all_of(available_vars)) %>%
    tidyr::drop_na()
  
  # Remove variáveis com variância zero ou muito baixa
  # A escala da variavel nao deve determinar se ela possui variancia. O limite
  # absoluto 1e-10 eliminava proporcoes validas antes da padronizacao.
  var_check <- vapply(df_boruta, function(x) stats::var(x, na.rm = TRUE), numeric(1))
  zero_var_cols <- names(var_check[!is.finite(var_check) | var_check <= 0])
  
  if (length(zero_var_cols) > 0) {
    cat("  - Removendo variáveis com variância zero:", paste(zero_var_cols, collapse = ", "), "\n")
    df_boruta <- df_boruta %>% dplyr::select(-all_of(zero_var_cols))
    available_vars <- setdiff(available_vars, zero_var_cols)
  }
  
  if (ncol(df_boruta) == 0) {
    stop("Todas as variáveis foram removidas devido à variância zero. Verifique os dados.")
  }
  
  # Define variável target para Boruta
  if (is.null(target_variable)) {
    # Usa primeira componente principal como proxy de target
    cat("  - Criando variável target baseada na primeira componente principal...\n")
    
    tryCatch({
      pca_result <- prcomp(df_boruta, scale. = TRUE, center = TRUE)
      target_var <- pca_result$x[, 1]
      df_boruta$target_pca <- target_var
      target_col <- "target_pca"
    }, error = function(e) {
      # Se PCA falhar, usa média simples como target
      cat("  - PCA falhou, usando média simples como target...\n")
      df_boruta$target_pca <- rowMeans(df_boruta, na.rm = TRUE)
      target_col <- "target_pca"
    })
  } else {
    if (!target_variable %in% colnames(df_boruta)) {
      stop(paste("Variável target especificada não encontrada:", target_variable))
    }
    target_col <- target_variable
  }
  
  # Executa Boruta
  set.seed(42)  # Para reprodutibilidade
  cat("  - Executando algoritmo Boruta...\n")
  
  formula_boruta <- as.formula(paste(target_col, "~ ."))
  boruta_result <- Boruta::Boruta(formula_boruta, 
                                  data = df_boruta, 
                                  doTrace = 0,  # Silencioso
                                  maxRuns = boruta_maxRuns,
                                  pValue = boruta_pValue)
  
  # Extrai variáveis selecionadas
  selected_vars <- Boruta::getSelectedAttributes(boruta_result, withTentative = FALSE)
  
  # Remove target_pca se foi criada artificialmente
  if (is.null(target_variable)) {
    selected_vars <- setdiff(selected_vars, "target_pca")
  }
  
  # Se nenhuma variável foi selecionada, usa as variáveis com maior importância
  if (length(selected_vars) == 0) {
    cat("  - Nenhuma variável confirmada pelo Boruta. Usando variáveis tentativas...\n")
    selected_vars <- Boruta::getSelectedAttributes(boruta_result, withTentative = TRUE)
    selected_vars <- setdiff(selected_vars, "target_pca")
    
    # Se ainda não há variáveis, usa as top 3-5 por importância
    if (length(selected_vars) == 0) {
      cat("  - Usando variáveis com maior importância...\n")
      importances <- boruta_result$ImpHistory
      if (!is.null(importances)) {
        # Pega médias de importância e seleciona top variáveis
        mean_imp <- apply(importances, 2, function(x) mean(x, na.rm = TRUE))
        mean_imp <- mean_imp[!grepl("shadow|target_pca", names(mean_imp))]
        selected_vars <- names(sort(mean_imp, decreasing = TRUE))[1:min(5, length(mean_imp))]
      } else {
        # Fallback: usa primeiras variáveis disponíveis
        selected_vars <- available_vars[1:min(3, length(available_vars))]
      }
    }
  }
  
  cat(paste("  - Variáveis selecionadas pelo Boruta:", length(selected_vars), "\n"))
  cat(paste("  - Variáveis:", paste(selected_vars, collapse = ", "), "\n"))
  
  # -------------------------------
  # ETAPA 3: CÁLCULO DE PESOS COM CFA
  # -------------------------------
  
  cat("Etapa 3: Calculando pesos com Análise Fatorial Confirmatória...\n")
  
  # Prepara dados para CFA
  df_cfa <- df_analysis %>%
    dplyr::select(all_of(selected_vars)) %>%
    tidyr::drop_na()

  cfa_completed <- FALSE
  
  if (nrow(df_cfa) < 50) {
    warning("Poucos dados para CFA. Usando distribuição uniforme de pesos.")
    pesos_normalizados <- data.frame(
      variavel = selected_vars,
      peso = 1 / length(selected_vars)
    )
  } else {
    # Padroniza variáveis para CFA (resolve problemas de escala)
    df_cfa_scaled <- as.data.frame(scale(df_cfa))
    colnames(df_cfa_scaled) <- selected_vars
    
    # Modelo CFA com um fator latente
    modelo_cfa <- paste0("fator =~ ", paste(selected_vars, collapse = " + "))
    
    tryCatch({
      # Ajusta modelo CFA com dados padronizados
      ajuste_cfa <- lavaan::cfa(modelo_cfa, data = df_cfa_scaled, 
                                estimator = cfa_estimator,
                                std.lv = TRUE,  # Padroniza fator latente
                                missing = "fiml")  # Trata valores ausentes
      
      # Verifica se o modelo convergiu
      if (lavaan::lavInspect(ajuste_cfa, "converged")) {
        # Extrai cargas fatoriais padronizadas
        pesos_cfa <- lavaan::parameterEstimates(ajuste_cfa, standardized = TRUE) %>%
          dplyr::filter(op == "=~") %>%
          dplyr::select(rhs, std.all)
        
        # Normaliza pesos para somarem 1
        pesos_normalizados <- pesos_cfa %>%
          dplyr::mutate(peso = abs(std.all) / sum(abs(std.all))) %>%
          dplyr::select(variavel = rhs, peso)
        cfa_completed <- TRUE
        
        cat("  - Pesos calculados com sucesso via CFA\n")
      } else {
        warning("CFA não convergiu. Usando distribuição uniforme de pesos.")
        pesos_normalizados <- data.frame(
          variavel = selected_vars,
          peso = 1 / length(selected_vars)
        )
      }
      
    }, error = function(e) {
      warning(paste("Erro no CFA:", e$message, ". Usando distribuição uniforme."))
      pesos_normalizados <- data.frame(
        variavel = selected_vars,
        peso = 1 / length(selected_vars)
      )
    })
  }
  
  # -------------------------------
  # ETAPA 4: NORMALIZAÇÃO DAS VARIÁVEIS
  # -------------------------------
  
  cat("Etapa 4: Normalizando variáveis...\n")
  
  # Se não houver agrupamento, define grupo fictício
  if (!cfa_completed && identical(cfa_fallback, "error")) {
    stop(
      "A CFA nao produziu pesos validos. A execucao foi interrompida para evitar pesos uniformes silenciosos. Use cfa_fallback = 'uniform' apenas se esse comportamento for intencional.",
      call. = FALSE
    )
  }

  if (is.null(group_by)) {
    df_input <- df %>%
      dplyr::mutate(group_variable = 'NONE')
  } else {
    if (!group_by %in% colnames(df)) {
      stop(paste("Coluna de agrupamento não encontrada:", group_by))
    }
    df_input <- df %>% 
      dplyr::rename(group_variable = all_of(group_by))
  }
  
  # Inverte variáveis cuja interpretação é "quanto menos, melhor"
  if (!is.null(inverse_variables)) {
    inverse_vars_available <- intersect(inverse_variables, selected_vars)
    if (length(inverse_vars_available) > 0) {
      df_input <- df_input %>%
        dplyr::mutate(across(all_of(inverse_vars_available), ~ 1 / (. + 1))) %>%
        dplyr::mutate(across(all_of(inverse_vars_available), ~ ifelse(is.infinite(.), 1, .)))
    }
  }
  
  # Transforma para formato longo
  df_input <- df_input %>%
    tidyr::pivot_longer(cols = all_of(selected_vars), 
                        names_to = 'variavel', 
                        values_to = 'valor') %>%
    dplyr::inner_join(pesos_normalizados, by = 'variavel')
  
  # Função auxiliar para normalizar cada variável
  normalizar_indicador <- function(x) {
    temp <- subset(df_input, variavel == x)
    
    norm_region_function <- function(i) {
      temp <- subset(temp, group_variable == i)
      
      norm_time_function <- function(t) {
        temp <- subset(temp, ano == t)
        
        # Cálculo de outliers via IQR
        Q1 <- quantile(temp$valor, 0.25, na.rm = TRUE)
        Q3 <- quantile(temp$valor, 0.75, na.rm = TRUE)
        IQR <- Q3 - Q1
        limite_inferior <- Q1 - param_outlier_adjust * IQR
        limite_superior <- Q3 + param_outlier_adjust * IQR
        
        # Ajusta outliers
        if (isTRUE(adjust_outliers)) {
          temp <- temp %>%
            dplyr::group_by(group_variable, ano, variavel) %>%
            dplyr::mutate(valor = ifelse(valor < limite_inferior, limite_inferior,
                                         ifelse(valor > limite_superior, limite_superior, valor)))
        }
        
        # Normalização min-max
        temp <- temp %>%
          dplyr::group_by(group_variable, ano, variavel) %>%
          dplyr::mutate(valor_norm = scales::rescale(valor, to = c(0, 1))) %>%
          dplyr::select(code_muni, group_variable, ano, variavel, valor, valor_norm, peso)
        return(temp)
      }
      
      # Aplica a normalização ano a ano
      temp <- lapply(min(df_input$ano):max(df_input$ano), norm_time_function) %>% 
        dplyr::bind_rows()
    }
    
    # Aplica a normalização grupo a grupo
    temp <- lapply(unique(df_input$group_variable), norm_region_function) %>% 
      dplyr::bind_rows()
  }
  
  # Aplica a normalização para todas as variáveis
  vars <- lapply(unique(df_input$variavel), normalizar_indicador) %>% 
    dplyr::bind_rows()
  
  # -------------------------------
  # ETAPA 5: CÁLCULO DO ÍNDICE
  # -------------------------------
  
  cat("Etapa 5: Calculando índice final...\n")
  
  # Soma ponderada das variáveis normalizadas
  IBrCRM <- vars %>%
    dplyr::arrange(ano) %>%
    dplyr::group_by(code_muni, group_variable, ano) %>%
    dplyr::summarise(IBrCRM = sum(valor_norm * peso, na.rm = TRUE), .groups = 'drop')
  
  # -------------------------------
  # ETAPA 6: PADRONIZAÇÃO DO ÍNDICE FINAL
  # -------------------------------
  
  standardization_method <- ifelse(is.null(standardization_method), 'none', standardization_method[1])
  
  if (standardization_method == 'mean') {
    IBrCRM <- IBrCRM %>%
      dplyr::group_by(ano, group_variable) %>%
      dplyr::mutate(IBrCRM = (IBrCRM - mean(IBrCRM, na.rm = TRUE)) / mean(IBrCRM, na.rm = TRUE))
  } else if (standardization_method == 'min-max') {
    IBrCRM <- IBrCRM %>%
      dplyr::group_by(ano, group_variable) %>%
      dplyr::mutate(IBrCRM = scales::rescale(IBrCRM, to = c(0, 1)))
  } else if (standardization_method == 'discrete') {
    IBrCRM <- IBrCRM %>%
      dplyr::group_by(ano, group_variable) %>%
      dplyr::mutate(IBrCRM = scales::rescale(IBrCRM, to = c(0, 1)),
                    IBrCRM_index = ifelse(IBrCRM < 0.2, 'Muito baixo',
                                          ifelse(dplyr::between(IBrCRM, 0.2, 0.4), 'Baixo',
                                                 ifelse(dplyr::between(IBrCRM, 0.4, 0.6), 'Médio',
                                                        ifelse(dplyr::between(IBrCRM, 0.6, 0.8), 'Alto',
                                                               ifelse(IBrCRM > 0.8, 'Muito alto', NA))))))
  }
  
  # Remove grupo fictício caso não tenha agrupamento original
  if (is.null(group_by)) {
    IBrCRM <- IBrCRM %>% 
      dplyr::ungroup() %>% 
      dplyr::select(-group_variable)
  }
  
  # Restaura opções do dplyr
  options(dplyr.summarise.inform = TRUE)
  
  # Adiciona atributos informativos ao resultado
  attr(IBrCRM, "selected_variables") <- selected_vars
  attr(IBrCRM, "weights") <- pesos_normalizados
  attr(IBrCRM, "boruta_result") <- boruta_result
  attr(IBrCRM, "cfa_status") <- if (cfa_completed) "success" else "fallback_uniform"
  
  cat("=== PROCESSO CONCLUÍDO COM SUCESSO ===\n")
  cat(paste("Variáveis finais utilizadas:", length(selected_vars), "\n"))
  
  # Retorna índice final
  return(IBrCRM)
}

# Função auxiliar para visualizar resultados do Boruta
plot_boruta_results <- function(boruta_result) {
  if (!requireNamespace("Boruta", quietly = TRUE)) {
    stop("Pacote 'Boruta' necessário para plotar resultados.")
  }
  plot(boruta_result, las = 2, cex.axis = 0.7, 
       main = "Importância das Variáveis - Algoritmo Boruta")
}

# Função auxiliar para extrair informações do resultado
get_index_info <- function(index_result) {
  list(
    selected_variables = attr(index_result, "selected_variables"),
    weights = attr(index_result, "weights"),
    cfa_status = attr(index_result, "cfa_status"),
    n_variables = length(attr(index_result, "selected_variables"))
  )
}
