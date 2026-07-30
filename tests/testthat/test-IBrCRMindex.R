test_that("Boruta is not capped at half of the valid variables", {
  set.seed(123)
  n <- 500
  latent <- rnorm(n)
  variable_names <- paste0("v", seq_len(8))
  values <- replicate(8, latent + rnorm(n, sd = 0.08))
  colnames(values) <- variable_names

  input <- data.frame(
    code_muni = seq_len(n),
    ano = rep(2020:2024, length.out = n),
    values,
    check.names = FALSE
  )

  result <- IBrCRMindex(
    df = input,
    variables = variable_names,
    adjust_outliers = FALSE,
    standardization_method = "none",
    boruta_maxRuns = 30,
    boruta_pValue = 0.05,
    cfa_fallback = "uniform"
  )

  selected <- attr(result, "selected_variables")
  expect_gt(length(selected), floor(length(variable_names) / 2))
  expect_setequal(names(attr(result, "weights")), c("variavel", "peso"))
  expect_equal(sum(attr(result, "weights")$peso), 1, tolerance = 1e-10)
  expect_true(attr(result, "cfa_status") %in% c("success", "fallback_uniform"))
})

test_that("CFA failure is not silently replaced by uniform weights", {
  input <- data.frame(
    code_muni = 1:20,
    ano = rep(2020:2021, each = 10),
    x = rnorm(20),
    y = rnorm(20)
  )

  expect_error(
    suppressWarnings(
      IBrCRMindex(
        df = input,
        variables = c("x", "y"),
        standardization_method = "none",
        boruta_maxRuns = 11,
        cfa_fallback = "error"
      )
    ),
    "CFA nao produziu pesos validos"
  )
})
