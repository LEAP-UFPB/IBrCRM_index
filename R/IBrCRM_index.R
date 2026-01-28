# helpers (curtos, sem firula)
`%||%` <- function(a, b) if (is.null(a)) b else a

#' Índice Brasileiro de Competitividade Regional Municipal (IBrCRM) - Versão Boruta
#'
#' Seleciona variáveis via Boruta, calcula pesos via CFA (lavaan),
#' normaliza por (ano x grupo) com ajuste de outliers (IQR) e agrega índice.
#'
#' @param df data.frame com colunas code_muni, ano e variáveis
#' @param variables vetor de variáveis candidatas
#' @param inverse_variables variáveis onde menor é melhor (serão invertidas)
#' @param group_by coluna de agrupamento (ex.: "name_biome_region_area"). Se NULL, não agrupa.
#' @param adjust_outliers TRUE/FALSE
#' @param param_outlier_adjust multiplicador do IQR (padrão 3)
#' @param standardization_method 'mean','discrete','none','min-max'
#' @param boruta_maxRuns máximo de iterações do Boruta
#' @param boruta_pValue pValue do Boruta
#' @param cfa_estimator estimador do lavaan (ex.: 'ML')
#' @param target_variable (opcional) target para Boruta. Se NULL, usa PC1 das candidatas.
#' @param verbose se TRUE, imprime log da seleção
#' @param log_fun função de log (ex.: message, cat, function(x) writeLines(x))
#' @param log_prefix prefixo do log
#'
#' @return data.frame com IBrCRM e atributos (selected_variables, weights, boruta_result, selection_report)
#' @export
IBrCRMindex <- function(df, variables, inverse_variables = NULL,
                        group_by = NULL, adjust_outliers = TRUE,
                        param_outlier_adjust = 3,
                        standardization_method = c("mean","discrete","none","min-max"),
                        boruta_maxRuns = 100, boruta_pValue = 0.01,
                        cfa_estimator = "ML", target_variable = NULL,
                        verbose = TRUE, log_fun = message, log_prefix = "IBrCRMindex") {

  stopifnot(requireNamespace("dplyr", quietly = TRUE))
  stopifnot(requireNamespace("tidyr", quietly = TRUE))
  stopifnot(requireNamespace("Boruta", quietly = TRUE))
  stopifnot(requireNamespace("lavaan", quietly = TRUE))
  stopifnot(requireNamespace("scales", quietly = TRUE))
  stopifnot(requireNamespace("tibble", quietly = TRUE))

  standardization_method <- standardization_method[1]

  # logger interno
  .log <- function(...) {
    if (isTRUE(verbose)) {
      log_fun(paste0(log_prefix, " | ", sprintf(...)))
    }
  }

  # -------------------------------
  # ETAPA 1: PREPARAÇÃO DOS DADOS
  # -------------------------------
  req <- c("code_muni", "ano")
  miss <- setdiff(req, names(df))
  if (length(miss) > 0) stop(paste("Faltando colunas obrigatórias:", paste(miss, collapse = ", ")))

  available_vars <- intersect(variables, names(df))
  if (length(available_vars) < 2) stop("Poucas variáveis candidatas disponíveis no df (>=2).")

  df_analysis <- df |>
    dplyr::select(dplyr::all_of(c("code_muni", "ano", available_vars)))

  # remove variáveis com >80% NA
  miss_rate <- df_analysis |>
    dplyr::summarise(dplyr::across(dplyr::all_of(available_vars), ~ mean(is.na(.x)))) |>
    tidyr::pivot_longer(dplyr::everything(), names_to = "var", values_to = "miss")

  drop_hi_na <- miss_rate |>
    dplyr::filter(.data$miss > 0.8) |>
    dplyr::pull(.data$var)

  available_vars <- setdiff(available_vars, drop_hi_na)
  if (length(available_vars) < 2) stop("Após remover >80% NA, sobraram <2 variáveis.")

  df_analysis <- df_analysis |>
    dplyr::select(dplyr::all_of(c("code_muni", "ano", available_vars)))

  # remove variáveis com variância ~0 (usando tudo disponível)
  var_ok <- vapply(df_analysis[available_vars], function(x) stats::var(x, na.rm = TRUE), numeric(1))
  drop_zero <- names(var_ok)[is.na(var_ok) | var_ok < 1e-10]
  available_vars <- setdiff(available_vars, drop_zero)
  if (length(available_vars) < 2) stop("Após remover variância ~0, sobraram <2 variáveis.")

  df_analysis <- df_analysis |>
    dplyr::select(dplyr::all_of(c("code_muni", "ano", available_vars)))

  # -------------------------------
  # ETAPA 2: BORUTA (com target PC1)
  # -------------------------------
  # (imputação simples por mediana só para rodar o Boruta)
  X <- df_analysis |>
    dplyr::select(dplyr::all_of(available_vars)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x))))

  med <- vapply(X, function(x) stats::median(x, na.rm = TRUE), numeric(1))
  for (nm in names(X)) X[[nm]][is.na(X[[nm]])] <- med[[nm]]

  df_boruta <- as.data.frame(X)

  if (is.null(target_variable)) {
    target <- tryCatch({
      pc1 <- stats::prcomp(df_boruta, center = TRUE, scale. = TRUE)$x[, 1]
      as.numeric(pc1)
    }, error = function(e) {
      as.numeric(rowMeans(df_boruta))
    })
    df_boruta$target_pca <- target
    target_col <- "target_pca"
  } else {
    if (!target_variable %in% names(df_boruta)) stop("target_variable não está nas variáveis do Boruta.")
    target_col <- target_variable
  }

  set.seed(42)
  boruta_result <- Boruta::Boruta(
    stats::as.formula(paste0(target_col, " ~ .")),
    data = df_boruta,
    doTrace = 0,
    maxRuns = boruta_maxRuns,
    pValue = boruta_pValue
  )

  boruta_result <- Boruta::TentativeRoughFix(boruta_result)

  selected_vars <- Boruta::getSelectedAttributes(boruta_result, withTentative = FALSE)
  selected_vars <- setdiff(selected_vars, "target_pca")

  # ajuste p/ não “selecionar tudo” quando as candidatas são muito colineares
  if (length(selected_vars) == 0) {
    selected_vars <- Boruta::getSelectedAttributes(boruta_result, withTentative = TRUE)
    selected_vars <- setdiff(selected_vars, "target_pca")
  }
  if (length(selected_vars) == 0) selected_vars <- available_vars[1:min(5, length(available_vars))]

  p <- length(available_vars)
  max_keep <- max(5, floor(0.5 * p))  # <= ajuste do boruta (cap)
  if (length(selected_vars) > max_keep) {
    imp <- Boruta::attStats(boruta_result)
    imp <- imp[intersect(rownames(imp), selected_vars), , drop = FALSE]
    selected_vars <- rownames(imp)[order(imp$meanImp, decreasing = TRUE)][1:max_keep]
  }

  # -------------------------------
  # RELATÓRIO: SELEÇÃO DE VARIÁVEIS (n, quais, %)
  # -------------------------------
  cand_in <- unique(variables)
  cand_in_df <- intersect(cand_in, names(df))
  cand_valid <- available_vars  # pós filtros (NA e variância)

  selection_report <- list(
    counts = tibble::tibble(
      universe = c("input_candidates", "candidates_in_df", "valid_after_filters"),
      n_total = c(length(cand_in), length(cand_in_df), length(cand_valid)),
      n_selected = c(length(selected_vars), length(selected_vars), length(selected_vars)),
      pct_selected = c(
        ifelse(length(cand_in) == 0, NA_real_, 100 * length(selected_vars) / length(cand_in)),
        ifelse(length(cand_in_df) == 0, NA_real_, 100 * length(selected_vars) / length(cand_in_df)),
        ifelse(length(cand_valid) == 0, NA_real_, 100 * length(selected_vars) / length(cand_valid))
      )
    ),
    selected_variables = selected_vars,
    not_selected_valid = setdiff(cand_valid, selected_vars),
    dropped_hi_na = drop_hi_na,
    dropped_zero_variance = drop_zero
  )

  # --- LOG (n, %, quais) ---
  pct_valid <- selection_report$counts$pct_selected[
    selection_report$counts$universe == "valid_after_filters"
  ]
  if (length(pct_valid) == 0) pct_valid <- NA_real_

  .log("Selecionadas %d/%d (%.1f%%) após filtros.",
       length(selected_vars), length(cand_valid), pct_valid)

  .log("Universos: input=%d | in_df=%d | valid=%d",
       length(cand_in), length(cand_in_df), length(cand_valid))

  .log("Variáveis selecionadas: %s", paste(selected_vars, collapse = ", "))

  if (length(drop_hi_na) > 0)
    .log("Descartadas (>80%% NA): %s", paste(drop_hi_na, collapse = ", "))

  if (length(drop_zero) > 0)
    .log("Descartadas (variância ~0): %s", paste(drop_zero, collapse = ", "))

  # opcional: guardar log text no report (útil pra salvar em arquivo depois)
  selection_report$log_text <- paste(
    paste0("Selecionadas ", length(selected_vars), "/", length(cand_valid),
           " (", sprintf("%.1f", pct_valid), "%) após filtros."),
    paste0("Universos: input=", length(cand_in),
           " | in_df=", length(cand_in_df),
           " | valid=", length(cand_valid)),
    paste0("Selecionadas: ", paste(selected_vars, collapse = ", ")),
    if (length(drop_hi_na) > 0) paste0("Drop >80% NA: ", paste(drop_hi_na, collapse = ", ")) else NULL,
    if (length(drop_zero) > 0) paste0("Drop var~0: ", paste(drop_zero, collapse = ", ")) else NULL,
    sep = "\n"
  )

  # -------------------------------
  # ETAPA 3: PESOS VIA CFA
  # -------------------------------
  df_cfa <- df_analysis |>
    dplyr::select(dplyr::all_of(selected_vars)) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ suppressWarnings(as.numeric(.x)))) |>
    tidyr::drop_na()

  pesos_normalizados <- data.frame(
    variavel = selected_vars,
    peso = 1 / length(selected_vars)
  )

  if (nrow(df_cfa) >= 50 && length(selected_vars) >= 2) {
    modelo_cfa <- paste0("fator =~ ", paste(selected_vars, collapse = " + "))

    pesos_normalizados <- tryCatch({
      df_cfa_scaled <- as.data.frame(scale(df_cfa))
      fit <- lavaan::cfa(
        modelo_cfa,
        data = df_cfa_scaled,
        estimator = cfa_estimator,
        std.lv = TRUE,
        missing = "fiml"
      )
      if (!isTRUE(lavaan::lavInspect(fit, "converged"))) stop("CFA não convergiu")

      pe <- lavaan::parameterEstimates(fit, standardized = TRUE) |>
        dplyr::filter(.data$op == "=~") |>
        dplyr::select(variavel = .data$rhs, std = .data$std.all) |>
        dplyr::mutate(peso = abs(.data$std) / sum(abs(.data$std))) |>
        dplyr::select(.data$variavel, .data$peso)

      as.data.frame(pe)
    }, error = function(e) {
      pesos_normalizados
    })
  }

  # -------------------------------
  # ETAPA 4: NORMALIZAÇÃO (grupo x ano) + OUTLIERS
  # -------------------------------
  if (is.null(group_by)) {
    df_input <- df |>
      dplyr::mutate(group_variable = "NONE")
  } else {
    if (!group_by %in% names(df)) stop(paste("group_by não existe no df:", group_by))
    df_input <- df |>
      dplyr::rename(group_variable = dplyr::all_of(group_by))
  }

  inv <- intersect(selected_vars, inverse_variables %||% character(0))

  long <- df_input |>
    dplyr::select(.data$code_muni, .data$ano, .data$group_variable, dplyr::all_of(selected_vars)) |>
    tidyr::pivot_longer(dplyr::all_of(selected_vars), names_to = "variavel", values_to = "valor") |>
    dplyr::mutate(valor = suppressWarnings(as.numeric(.data$valor))) |>
    dplyr::mutate(valor = dplyr::if_else(.data$variavel %in% inv, -.data$valor, .data$valor)) |>
    dplyr::inner_join(pesos_normalizados, by = "variavel")

  stats_grp <- long |>
    dplyr::group_by(.data$group_variable, .data$ano, .data$variavel) |>
    dplyr::summarise(
      q1 = stats::quantile(.data$valor, 0.25, na.rm = TRUE),
      q3 = stats::quantile(.data$valor, 0.75, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      iqr = .data$q3 - .data$q1,
      lo = .data$q1 - param_outlier_adjust * .data$iqr,
      hi = .data$q3 + param_outlier_adjust * .data$iqr
    )

  long <- long |>
    dplyr::left_join(stats_grp, by = c("group_variable", "ano", "variavel"))

  if (isTRUE(adjust_outliers)) {
    long <- long |>
      dplyr::mutate(valor = pmin(pmax(.data$valor, .data$lo), .data$hi))
  }

  long <- long |>
    dplyr::group_by(.data$group_variable, .data$ano, .data$variavel) |>
    dplyr::mutate(valor_norm = scales::rescale(.data$valor, to = c(0, 1))) |>
    dplyr::ungroup()

  # -------------------------------
  # ETAPA 5: ÍNDICE (soma ponderada)
  # -------------------------------
  IBrCRM <- long |>
    dplyr::group_by(.data$code_muni, .data$group_variable, .data$ano) |>
    dplyr::summarise(IBrCRM = sum(.data$valor_norm * .data$peso, na.rm = TRUE), .groups = "drop")

  # -------------------------------
  # ETAPA 6: PADRONIZAÇÃO FINAL
  # -------------------------------
  if (standardization_method == "mean") {
    IBrCRM <- IBrCRM |>
      dplyr::group_by(.data$ano, .data$group_variable) |>
      dplyr::mutate(IBrCRM = (.data$IBrCRM - mean(.data$IBrCRM, na.rm = TRUE)) / mean(.data$IBrCRM, na.rm = TRUE)) |>
      dplyr::ungroup()
  }

  if (standardization_method == "min-max") {
    IBrCRM <- IBrCRM |>
      dplyr::group_by(.data$ano, .data$group_variable) |>
      dplyr::mutate(IBrCRM = scales::rescale(.data$IBrCRM, to = c(0, 1))) |>
      dplyr::ungroup()
  }

  if (standardization_method == "discrete") {
    IBrCRM <- IBrCRM |>
      dplyr::group_by(.data$ano, .data$group_variable) |>
      dplyr::mutate(
        IBrCRM = scales::rescale(.data$IBrCRM, to = c(0, 1)),
        IBrCRM_index = dplyr::case_when(
          .data$IBrCRM < 0.2 ~ "Muito baixo",
          .data$IBrCRM < 0.4 ~ "Baixo",
          .data$IBrCRM < 0.6 ~ "Médio",
          .data$IBrCRM < 0.8 ~ "Alto",
          TRUE ~ "Muito alto"
        )
      ) |>
      dplyr::ungroup()
  }

  if (is.null(group_by)) {
    IBrCRM <- IBrCRM |>
      dplyr::select(-.data$group_variable)
  }

  attr(IBrCRM, "selected_variables") <- selected_vars
  attr(IBrCRM, "weights") <- pesos_normalizados
  attr(IBrCRM, "boruta_result") <- boruta_result
  attr(IBrCRM, "selection_report") <- selection_report

  IBrCRM
}

plot_boruta_results <- function(boruta_result) {
  stopifnot(requireNamespace("Boruta", quietly = TRUE))
  plot(boruta_result, las = 2, cex.axis = 0.7)
}

get_index_info <- function(index_result) {
  list(
    selected_variables = attr(index_result, "selected_variables"),
    weights = attr(index_result, "weights"),
    n_variables = length(attr(index_result, "selected_variables")),
    selection_report = attr(index_result, "selection_report")
  )
}

# opcional: imprime resumo bonitinho
print_selection_report <- function(index_result) {
  rep <- attr(index_result, "selection_report")
  if (is.null(rep)) stop("selection_report não encontrado nos atributos.")
  print(rep$counts)
  cat("\nSelected variables:\n")
  print(rep$selected_variables)
  invisible(rep)
}
