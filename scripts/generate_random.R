generate_random <-
  function(N,
           seed,
           xk = NULL,
           Pareto = FALSE,
           n = NULL) {
    set.seed(seed)
    Xi_Perman <- runif(N)
    
    set.seed(seed)
    epsilon <-  runif(1)
    
    Xi_Coloc <- (rank(Xi_Perman) - epsilon) / N
    
    salida <- list(Xi_Perman, Xi_Coloc)
    names(salida) <- c("Xi_Perman", "Xi_Coloc")
    
    if (isTRUE(Pareto) &
        (!is.null(xk) &
         is.null(n)) |
        (is.null(xk) & !is.null(n)))
      stop("Enter n and the vector xk")
    
    if (!is.null(xk)) {
      if (length(xk) != N)
        stop("Enter a vector xk of the same length as N")
      
      
      pk = xk / sum(xk)
      Xi_ppt <-  Xi_Perman / (N * pk)
      
      if (isTRUE(Pareto)) {
        pi_k <- TeachingSampling::PikPPS(n, x = xk)
        Xi_Pareto <-
          (Xi_Perman / (1 - Xi_Perman)) / (pi_k / (1 - pi_k))
        salida <- list(Xi_Perman, Xi_Coloc, Xi_Pareto)
        names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_Pareto")
        
      } else {
        salida <- list(Xi_Perman, Xi_Coloc, Xi_ppt)
        names(salida) <- c("Xi_Perman", "Xi_Coloc", "Xi_pipt")
      }
    }
    
    return(salida)
  }
# xi_pareto <-
#     Generate_random(N = N,
#                 seed = seed,
#                 xk,
#                 Pareto = TRUE,
#                 n)$Xi_Pareto
