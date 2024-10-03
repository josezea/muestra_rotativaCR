redistribution_PSU_sample <-
  function(sample_size_PSU_stratum,
           Num_panels_period) {
    Num_PSU_normal_panels <-
      floor((sample_size_PSU_stratum / Num_panels_period))
    num_large_groups <-
      sample_size_PSU_stratum - (Num_PSU_normal_panels * Num_panels_period)
    
    resultado <-
      c(Num_panels_period,
        num_large_groups,
        Num_panels_period - num_large_groups,
        Num_PSU_normal_panels + 1,
        Num_PSU_normal_panels
      )
    names(resultado) <-
      c("Num_panels",
        "Num_large_panels",
        "Num_normal_panels",
        "Num_PSU_large_panels",
        "Num_PSU_normal_panels"
      )
    resultado
  }
# Redistribution_PSU_sample(89, 15)
