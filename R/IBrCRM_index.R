#' Índice Brasileiro de Competitividade Regional Municipal (IBrCRM)
#'
#' @description Creation of an inequality index capable of selecting important variables,
#' weights for each variable and normalizing the selected variables.
#'
#' @param variables A character vector specifying the axis style. Valid options are
#'        `"none"` (no axis lines), `"full"` (full-length axis lines), and
#'        `"half"` (half-length axis lines), the default.
#' @param reference_variables Logical value indicating whether to show text elements. If `TRUE`,
#'        axis text will be displayed in black; otherwise, they will
#'        be hidden.
#' @param inverse_variables A character vector specifying the position of the
#'        legend. Valid options are `"right"` (default), `"left"`, `"top"`, and
#'        `"bottom"`.
#' @param dplyr::group_by Defines whether the grid lines should be `"horizontal"`
#'       (default) or `"vertical"`.
#' @param adjust_outliers Numeric. The number of breaks on the x-axis
#' @param include_weight Numeric. The number of breaks on the y-axis
#' @param standardization_method Logical value that indicates whether the x-axis
#'        boundary should be expanded. If `TRUE`, the x-axis limits will be
#'        expanded; otherwise there will be no change
#'
#' @return A `data.frame` output.
#' @import dplyr tidyr glmnet stats scales
#' @export
#' @family IBrCRMindex output
#'
#' @examples
#' library(dplyr)
#' variables <- c('mpg','cyl','hp','drat','wt','qsec','vs','am','gear')
#' reference_variables <- c('cyl','mpg')
#' dplyr::group_by <- c('carb')
#' inverse_variables <- c('wt')
#' 
#' IBrCRM <- IBrCRMindex(mtcars,variables = variables, reference_variables = reference_variables,
#'                        inverse_variables = inverse_variables,
#'                        dplyr::group_by = dplyr::group_by,
#'                        adjust_outliers =TRUE, include_weight = TRUE,
#'                        standardization_method = c('mean'))


IBrCRMindex <- function(df,variables,reference_variables,inverse_variables,
                        group_by = NULL,adjust_outliers =TRUE, include_weight = TRUE,
                        standardization_method = c('mean','discrete','none')) {
  
  options(dplyr.summarise.inform = FALSE)
  ## Primeira etapa: Seleção de variáveis ----------------
  
  # Criando base para elastic net
  wb <- df %>% 
    dplyr::group_by(ano) %>% 
    dplyr::summarise(across(everything(), list(mean = ~mean(., na.rm = T)))) %>% 
    dplyr::mutate(across(everything(), ~ ifelse(. == 0 | is.nan(.),NA,.))) %>% 
    ungroup() %>% ungroup() %>% 
    dplyr::select(c(paste0(variables,"_mean")))
  
  # Garantindo preenchimento de tudo
  
  for(i in 1:nrow(wb)){
    wb <- wb %>% 
      dplyr::mutate(across(c(everything()), ~ ifelse(is.na(.),lead(.) - mean(((. - lag(.))/lag(.)), na.rm = T)*lead(.),.)))
    
  }
  
  
  wb <- wb %>% 
    tidyr::drop_na()
  


  
  # Verificar se a coluna de resposta existe no dataframe original
  results_append <- NULL
  if (length(reference_variables %in% colnames(wb)) > 0) {

    for(i in paste0(reference_variables,"_mean")){
      
      # Excluir coluna de resposta das vari?veis preditoras
      X <- wb %>% dplyr::select(-c(i)) %>% as.matrix()
      Y <- as.numeric(wb[[i]])

      
      # Ajustar o modelo Elastic Net
      alpha <- 0.65  # Par?metro de mistura entre Lasso (alpha=1) e Ridge (alpha=0)
      lambda <- 0.1  # Par?metro de penaliza??o
      fit <- glmnet::glmnet(X, Y, alpha = alpha, lambda = lambda)
      
      # Visualizar os coeficientes estimados pelo modelo
      results <- coef(fit)
      results <- as.data.frame(as.matrix(results))
      results <- data.frame(variable = rownames(results),ref = i, coef = results$s0)
      results <- results %>% dplyr::filter(!grepl('Intercept',variable))
      results_append <- rbind(results_append,results)
      
    }
    
    
  } else {
      stop("A coluna de resposta especificada não existe no dataframe.")
  }
  

  
  results_append <- subset(results_append, abs(coef) > 0.005)
  results_append <- results_append %>% 
    dplyr::mutate(variable = gsub("_mean",'',variable),
           ref      = gsub("_mean",'',ref))
  variables_selected <- results_append %>% 
    dplyr::group_by(variable) %>% 
    dplyr::summarise(count = n()) %>% 
    dplyr::filter((variable %in% reference_variables & count >= 1) | 
           (variable %in% variables & count >= 2)) %>% 
    ungroup() %>% 
    distinct(variable)
  
  ## Segunda etapa: Gerar pesos ----------------
  
  if(isTRUE(include_weight)){
  
  ## Criando base para calculo dos pesos
  wb <- df %>% 
    ungroup() %>% 
    dplyr::select(c(variables_selected[[1]])) %>% 
    tidyr::drop_na()
  
  # Calcular a correla??o
  correlacao <- cor(wb)
  
  # Executar a an?lise de componentes principais (PCA)
  resultado_pca <- princomp(wb)
  
  # Obter os dois primeiros componentes principais
  dois_primeiros_componentes <- resultado_pca$loadings[, 1:2]
  
  # C?lculo dos pesos para os dois primeiros componentes
  pesos_primeiro_componente <- abs(correlacao %*% dois_primeiros_componentes[, 1])
  pesos_segundo_componente <- abs(correlacao %*% dois_primeiros_componentes[, 2])
  
  # M?dia ponderada dos pesos dos dois primeiros componentes
  pesos_total <- (pesos_primeiro_componente + pesos_segundo_componente) / 2
  
  # Normaliza??o dos pesos
  pesos_normalizados <- pesos_total / sum(pesos_total)
  pesos_normalizados <- data.frame(variavel = rownames(pesos_normalizados),peso = pesos_normalizados)
  rownames(pesos_normalizados) <- 1:nrow(pesos_normalizados)
  
  } else {
    pesos_normalizados <- data.frame(variables_selected) %>% 
      rename(variavel = variable) %>% 
      dplyr::mutate(peso = 1/nrow(variables_selected))
  }
  
  ## Terceira etapa: normalizacao de pesos ----------------
  
  #group_by = 'name_biome_region_area'
  
  # Verificar se a variável 'dplyr::group_by' é NULL
  if(is.null(group_by)) {
    df_input <- df %>% 
      dplyr::mutate(group_variable = 'NONE')
  } else {
    df_input <- df %>% 
      rename(group_variable = group_by)
  }
  
  ## Transformando variáveis no seu inverso
  # Indicadores cuja lógica é contrária (quanto menos, melhor)
  df_input <- df_input %>% 
    dplyr::mutate(across(c(inverse_variables), ~1/(.+1))) %>% 
    dplyr::mutate(across(c(inverse_variables), ~ ifelse(is.infinite(.),1,.))) %>% 
    tidyr::pivot_longer(!c(code_muni,ano,group_variable), names_to = 'variavel', values_to = 'valor') %>%
    inner_join(pesos_normalizados, by = 'variavel')
  

    
  normalizar_indicador <- function(x){
      
      temp <- subset(df_input, variavel == x) 
      
      norm_region_function <- function(i){
        
        temp <- subset(temp, group_variable == i) 
        
        norm_time_function <- function(t){

          temp <- subset(temp, ano == t) 
          
          # Calcular o IQR
          Q1 <- quantile(temp$valor, 0.25, na.rm = T)
          Q3 <- quantile(temp$valor, 0.75, na.rm = T)
          IQR <- Q3 - Q1
          
          # Definir limites para identificar outliers
          limite_inferior <- Q1 - 3 * IQR
          limite_superior <- Q3 + 3 * IQR
          
          if(isTRUE(adjust_outliers)){
            
            temp <- temp %>% 
              dplyr::group_by(group_variable,ano,variavel) %>% 
              dplyr::mutate(
                # Substituir outliers pelos limites
                valor = ifelse(valor < limite_inferior, limite_inferior,
                        ifelse(valor > limite_superior, limite_superior, valor)))
            
          } else {
            
          }
          
          temp <- temp %>% 
            dplyr::group_by(group_variable,ano,variavel) %>% 
            dplyr::mutate(
              # Normalizar dados usando min-max
              valor_norm = scales::rescale(valor, to=c(0,1))) %>% 
            dplyr::select(code_muni,group_variable,ano,variavel, valor,valor_norm, peso)
          return(temp)
        }
        
        temp <- lapply(min(df_input$ano):max(df_input$ano),norm_time_function) %>% bind_rows()
        
      }
      temp <- lapply(unique(df_input$group_variable),norm_region_function) %>% bind_rows()
  }
  # Tratar outliers que podem prejudicar a normalização do indicador. Também fazer a normalização
  vars <- lapply(unique(df_input$variavel),normalizar_indicador) %>% bind_rows()

  
  IBrCRM <- vars %>% 
    dplyr::arrange(ano) %>% 
    dplyr::group_by(code_muni,group_variable,ano) %>% 
    dplyr::summarise(IBrCRM = sum(valor_norm*peso, na.rm = T))
  
  
  standardization_method <- ifelse(is.null(standardization_method),'none',standardization_method)
  if(standardization_method == 'mean'){
    # Melhorando indicador
    IBrCRM <- IBrCRM %>% 
      dplyr::group_by(ano,group_variable) %>% 
      dplyr::mutate(IBrCRM = (IBrCRM - mean(IBrCRM, na.rm = T))/mean(IBrCRM, na.rm = T))
  } else if(standardization_method == 'discrete'){
    # Melhorando indicador
    IBrCRM <- IBrCRM %>% 
      dplyr::group_by(ano,group_variable) %>% 
      dplyr::mutate(IBrCRM_index = ifelse(IBrCRM < 0.2,'Muito baixo',
                                   ifelse(between(IBrCRM,0.2,0.4),'Baixo',
                                   ifelse(between(IBrCRM,0.4,0.6),'Médio',
                                   ifelse(between(IBrCRM,0.6,0.8),'Alto',
                                   ifelse(IBrCRM > 0.8,'Muito alto',NA))))))
  } else {

  }
  
  if(is.null(dplyr::group_by)) {
    IBrCRM <- IBrCRM %>% ungroup() %>% 
      dplyr::select(-c(group_variable))
  } 
  
  options(dplyr.summarise.inform = TRUE)
  return(IBrCRM)
}


