
# Update documentation
devtools::document(pkg = ".")
remotes::install_github("PedroJorge7/IBrCRM_index")

## EXEMPLO 
df <- readRDS("./test/df_agregado_bases_inputado.rds")

# Vetor de nomes de variáveis e padrões
inverse_variables <- c('indice_aridez', 'taxa_morte_infantil', 'homicidios', 'razao_dep', 'grau_dependencia',
                       'tdi_fun', 'tdi_med', 'total_desastre', 'obitos_desastre', 'feridos_desastre', 
                       'prejuizos_totais_desastre', 'emissao_co2_t', 'obitos_infantis', 'homicidios', 
                       'homicidios_arma_fogo', 'violencia_causa_intermedinada', 'obitos_causas_externas', 
                       'suicidos', 'suicidos_armas_fogos', 'doencas_transmissao_feco_oral', 'diarreias', 
                       'febres_entericas', 'hepatite_a', 'doencas_transmitidas_insetos', 'dengue', 
                       'febre_amarela', 'leishmanioses', 'filariose_linfatica', 'malaria', 'chagas', 
                       'doencas_contato_agua', 'esquistossomose', 'leptospirose', 'doencas_relacionadas_higiene', 
                       'tracoma', 'conjuntivite', 'micose', 'geohelmintos_teniases', 'teniases', 
                       'cobertura_minerio', 'extensao', 'area_urbanizada', 'total_trabalhadores_setor_publico', 
                       'grau_gasto_transferencias', 'evotranspiracao', 'homicidios', 
                       'morte_causa_evitaveis_0a5','morte_causa_evitaveis_5a74')

## Economico
variables <- c('operacoes_credito','estabelecimentos','empregados',
               'pib','massa_salarial','balanca_comercial')

reference_variables <- c('pib','massa_salarial','balanca_comercial')


IBrCRM_economico <- IBrCRM_index(df,variables = variables, reference_variables = reference_variables,
                                 inverse_variables = inverse_variables,
                                 group_by = "name_biome_region_area",
                                 adjust_outliers =TRUE, include_weight = TRUE,
                                 standardization_method = c('mean'))
IBrCRM_economico$subindicador <- 'economico'


