

metabolites_adj <- phthte_creatinine %>%
  pivot_wider(id_cols = seqn,
              names_from = analyte_code,
              values_from = adj_conc) %>% 
  select(-seqn)

