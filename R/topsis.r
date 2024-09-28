################################################################################
## Clean memory
################################################################################

rm(list = ls())
gc(full = TRUE)

################################################################################
## Libraries
################################################################################

library(readxl    )  ## Read Excel files
library(writexl   )  ## For writing Excel files
library(entropy   )  ## Information entropy
library(ggplot2   )  ## Fancy plots
library(ggridges  )  ## Ridge plot
library(tcltk     )  ## Progress bar
library(doSNOW    )  ## Parallel processing
library(DEoptim   )  ## Genetic Optimization
library(igraph    )  ## Network graph
library(ggraph    )  ## Graphs
library(tidyverse )  ## Data manipulation
library(AER       )  ## Tobit regression
library(betareg   )  ## Beta regression
library(simplexreg)  ## Simplex regression
library(MLmetrics )  ## Machine Learning Metrics

################################################################################
## Paths, seeds and options
################################################################################

## Library for automatic path
library(this.path)

## paths
path         <- this.dir()
path         <- paste(rev(rev(strsplit(path, "/")[[1]])[-1]), collapse = "/")
path_base    <- paste0(path, "/Bases/")
path_results <- paste0(path, "/Results/")
path_source  <- paste0(path, "/R/")

## Seed
set.seed(100) 

################################################################################
## Load Dataset
################################################################################

## Set working directory
setwd(path_base)

## Load the main dataset
df <- read_xlsx("Europe Banks Dataset.xlsx")

## Load the variables used
vars <- read_xlsx("Variables Used.xlsx")

################################################################################
## Data Manipulation
################################################################################

## Filter variables by type
vars_contextual       <- filter(vars, Type == "Contextual"               )
vars_utility_citizens <- filter(vars, Type == "Utility to Citizens"      )
vars_utility_firms    <- filter(vars, Type == "Utility to Firms"         )
vars_bank_leverage    <- filter(vars, Type == "Banking Economic Leverage")
vars_bank_health      <- filter(vars, Type == "Banking Financial Health" )

## Select variables from the main dataset
df_contextual       <- df %>% select(all_of(vars_contextual      $Variable))
df_utility_citizens <- df %>% select(all_of(vars_utility_citizens$Variable))
df_utility_firms    <- df %>% select(all_of(vars_utility_firms   $Variable))
df_bank_leverage    <- df %>% select(all_of(vars_bank_leverage   $Variable))
df_bank_health      <- df %>% select(all_of(vars_bank_health     $Variable))

################################################################################
## Information Entropy Weights
################################################################################

## Information entropy function
fn_ie <- function(x) entropy(discretize(x, numBins = 10))

## Calculate entropy weights for each group of variables
w_utility_citizens <- map_dbl(df_utility_citizens, fn_ie)
w_utility_firms    <- map_dbl(df_utility_firms   , fn_ie)
w_bank_leverage    <- map_dbl(df_bank_leverage   , fn_ie)
w_bank_health      <- map_dbl(df_bank_health     , fn_ie)

## Normalize weights so that they sum to 1
w_utility_citizens <- w_utility_citizens / sum(w_utility_citizens)
w_utility_firms    <- w_utility_firms    / sum(w_utility_firms   )
w_bank_leverage    <- w_bank_leverage    / sum(w_bank_leverage   )
w_bank_health      <- w_bank_health      / sum(w_bank_health     )

################################################################################
## TOPSIS Analysis
################################################################################

## TOPSIS Function
fn_topsis <- function(decision = NULL, weights = NULL, impacts = NULL) 
{
  ## Normaliza os pesos para somar 1
  weights <- weights/sum(weights)
  
  ## Normalizacao da matriz pela sua norma
  N <- apply(decision, 2, \(x) x/sqrt(sum(x^2)))
  
  ## Matriz de decisao normalizada pelos pesos
  V <- N %*% diag(weights)
  
  ## Melhor alternativa possivel
  u <- 
    as.integer(impacts == "+") * apply(V, 2, max) +
    as.integer(impacts == "-") * apply(V, 2, min)
  
  ## Pior alternativa possivel
  l <- 
    as.integer(impacts == "-") * apply(V, 2, max) +
    as.integer(impacts == "+") * apply(V, 2, min)
  
  ## Distancia para o melhor
  du <- apply(V, 1, \(x) sqrt(sum((x - u)^2)))
  
  ## Distancia para o pior
  dl <- apply(V, 1, \(x) sqrt(sum((x - l)^2)))
  
  ## Score
  score <- dl/(dl + du)
  
  ## Retorno
  return(data.frame(alt.row = 1:nrow(decision),
                    score   = score           , 
                    rank    = rank(-score)    ))
}

## Impacts for topsis (+ for benefit, - for cost)
imp_utility_citizens <- filter(vars, Type == "Utility to Citizens"      )$Impact
imp_utility_firms    <- filter(vars, Type == "Utility to Firms"         )$Impact
imp_bank_leverage    <- filter(vars, Type == "Banking Economic Leverage")$Impact
imp_bank_health      <- filter(vars, Type == "Banking Financial Health" )$Impact

## TOPSIS
eval_time_utility_citizens <- system.time(topsis_utility_citizens <- fn_topsis(as.matrix(df_utility_citizens), w_utility_citizens, imp_utility_citizens))
eval_time_utility_firms    <- system.time(topsis_utility_firms    <- fn_topsis(as.matrix(df_utility_firms   ), w_utility_firms   , imp_utility_firms   ))
eval_time_bank_leverage    <- system.time(topsis_bank_leverage    <- fn_topsis(as.matrix(df_bank_leverage   ), w_bank_leverage   , imp_bank_leverage   ))
eval_time_bank_health      <- system.time(topsis_bank_health      <- fn_topsis(as.matrix(df_bank_health     ), w_bank_health     , imp_bank_health     ))

## Check evaluation times
eval_time_utility_citizens
eval_time_utility_firms   
eval_time_bank_leverage   
eval_time_bank_health     

## Data frame with results
df_topsis <- data.frame(
  `Company Common Name`     = df$`Company Common Name`     ,
  Year                      = df$Year                      ,
  `Country of Headquarters` = df$`Country of Headquarters` ,
  `TRBC Industry Name`      = df$`TRBC Industry Name`      ,
  `TOPSIS Utility Citizens` = topsis_utility_citizens$score,
  `TOPSIS Utility Firms`    = topsis_utility_firms   $score,
  `TOPSIS Bank Leverage`    = topsis_bank_leverage   $score,
  `TOPSIS Bank Health`      = topsis_bank_health     $score,
  check.names               = FALSE                         
)

################################################################################
## Plot TOPSIS results
################################################################################

## Data frame for plot
df_plot <- pivot_longer(data      = df_topsis                   ,
                        names_to  = "Type"                      ,
                        values_to = "TOPSIS"                    ,
                        cols      = c(`TOPSIS Utility Citizens`,
                                      `TOPSIS Utility Firms`   ,
                                      `TOPSIS Bank Leverage`   ,
                                      `TOPSIS Bank Health`     ))

## Ridge plot per type
ggplot(df_plot) + 
  geom_density_ridges(aes(x = TOPSIS, y = Type, fill = Type), alpha = 0.4) + 
  xlab("TOPSIS") + 
  ylab("") + 
  theme_minimal() + 
  theme(legend.position = "none")

## Ridge plot per country and type
ggplot(df_plot) + 
  facet_wrap(~Type, nrow = 1) +
  geom_density_ridges(aes(x = TOPSIS, y = `Country of Headquarters`)) + 
  xlab("TOPSIS") + 
  ylab("") + 
  theme_minimal() + 
  theme(legend.position = "none")

## Ridge plot per year and type
ggplot(df_plot) + 
  facet_wrap(~Type, nrow = 1) +
  geom_density_ridges(aes(x = TOPSIS, y = factor(Year))) + 
  xlab("TOPSIS") + 
  ylab("") + 
  theme_minimal() + 
  theme(legend.position = "none")

## Ridge plot per TRBC and type
ggplot(df_plot) + 
  facet_wrap(~Type, nrow = 1) +
  geom_density_ridges(aes(x = TOPSIS, y = `TRBC Industry Name`)) + 
  xlab("TOPSIS") + 
  ylab("") + 
  theme_minimal() + 
  theme(legend.position = "none")

################################################################################
## Bootstraped TOPSIS Analysis
################################################################################

## Number of bootstraps
n_boot <- 100

## Number of cores
n_cores <- 8

## Evaluation time
eval_time_boot <- system.time({
  
  ## Progress Bar
  PB <- tkProgressBar(
    title   = "TOPSIS bootstrap",
    label   = "bootstrap"       ,
    min     = 0                 , 
    max     = n_boot            , 
    initial = 0                 , 
    width   = 300                
  )
  
  ## Progress bar functions
  PB_snow <- \(n) setTkProgressBar(PB, n, label = sprintf("%d / %d", n, n_boot))
  PB_snow <- list(progress = PB_snow)
  
  ## Cluster for parallel execution
  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)
  
  ## Parallel loop
  res_boot <- foreach(
    j             = 1:n_boot   ,
    .combine      = rbind      ,
    .inorder      = FALSE      ,
    .packages     = c("topsis"), 
    .options.snow = PB_snow     
  ) %dopar% {
    
    ## Bootstrap sample indices
    idx_boot <- sample(1:nrow(df_bank_health), nrow(df_bank_health), replace = TRUE)
    
    ## TOPSIS
    return(list(
      "Utility Citizens"  = fn_topsis(as.matrix(df_utility_citizens[idx_boot, ]), w_utility_citizens, imp_utility_citizens)$score,
      "Utility Firms"     = fn_topsis(as.matrix(df_utility_firms   [idx_boot, ]), w_utility_firms   , imp_utility_firms   )$score,
      "Bank Leverage"     = fn_topsis(as.matrix(df_bank_leverage   [idx_boot, ]), w_bank_leverage   , imp_bank_leverage   )$score,
      "Bank Health"       = fn_topsis(as.matrix(df_bank_health     [idx_boot, ]), w_bank_health     , imp_bank_health     )$score 
    ))

  }
  
  ## End of parallel execution
  stopCluster(cl)
  
})

## bootstrap TOPSIS results
df_boot_topsis_utility_citizens <- do.call(cbind, res_boot[,1])
df_boot_topsis_utility_firms    <- do.call(cbind, res_boot[,2])
df_boot_topsis_bank_leverage    <- do.call(cbind, res_boot[,3])
df_boot_topsis_bank_health      <- do.call(cbind, res_boot[,4])

################################################################################
## Endogeneity Analysis - TOPSIS Optimization
################################################################################

## Function to evaluate mutual information of unconditional and conditional distributions
endogeneity_entropy_opt <- function(w, df){
  
  ## Combinations
  comb <- t(combn(1:ncol(df), 2))
  
  ## vector with mutual information for combinations
  mi_comb <- c()
  
  ## Loop in combinations
  for(i in 1:nrow(comb)){
    
    ## Variables used in combination
    x <- df[, comb[i, 1]]
    y <- df[, comb[i, 2]]
    
    ## Weights used in combination
    w1 <- w[comb[i, 1]]
    w2 <- w[comb[i, 2]]
    
    ## Mean weight
    w_bar <- sqrt(w1 * w2)
    
    ## Number of discretizations
    n_bins_x <- round(sqrt(length(x)))
    n_bins_y <- round(sqrt(length(y)))
    
    ## Change nnumber of discretizations
    n_bins_x <- ifelse(n_bins_x > 100, 100, ifelse(n_bins_x < 10, 10, n_bins_x))
    n_bins_y <- ifelse(n_bins_y > 100, 100, ifelse(n_bins_y < 10, 10, n_bins_y))
    
    ## Add mutual information
    mi_comb <- c(mi_comb, w_bar * mi.empirical(discretize2d(x, y, n_bins_x, n_bins_y)))
    
  }
  
  ## Return
  return(-sum(mi_comb))
  
}

## Function to endogeneity with unconditional and conditional distributions
endogeneity_entropy <- function(df_w, list_df, thresholds){
  
  ## Variables names
  vars_names <- colnames(df_w)
  
  ## Combinations
  comb <- t(combn(1:length(list_df), 2))
  
  ## Data frame with endogeneity analysis
  df_endo <- NULL
  
  ## Loop in bootstraps
  for(boot in 1:nrow(df_w)){
    
    ## Data frame
    df <- do.call(cbind, lapply(list_df, \(x) x[,boot]))
    
    ## Weights
    w <- as.numeric(df_w[boot,])
    
    ## Loop in combinations
    for(i in 1:nrow(comb)){
      
      ## Variables used in combination
      x <- df[, comb[i, 1]]
      y <- df[, comb[i, 2]]
      
      ## Weights used in combination
      w1 <- as.numeric(w[comb[i, 1]])
      w2 <- as.numeric(w[comb[i, 2]])
      
      ## Mean weight
      w_bar <- sqrt(w1 * w2)
      
      ## Number of discretizations
      n_bins_x <- round(sqrt(length(x)))
      n_bins_y <- round(sqrt(length(y)))
      
      ## Change nnumber of discretizations
      n_bins_x <- ifelse(n_bins_x > 100, 100, ifelse(n_bins_x < 10, 10, n_bins_x))
      n_bins_y <- ifelse(n_bins_y > 100, 100, ifelse(n_bins_y < 10, 10, n_bins_y))
      
      ## Entropies
      w_ie_x  <- w1    * entropy(discretize(x, n_bins_x))
      w_ie_y  <- w2    * entropy(discretize(y, n_bins_y))
      w_mi_xy <- w_bar * mi.empirical(discretize2d(x, y, n_bins_x, n_bins_y))
      
      ## Conditional entropies
      cond_ie_xy <- w_ie_x - w_mi_xy
      cond_ie_yx <- w_ie_y - w_mi_xy
      
      ## Data frame for combination
      df_comb_i <- data.frame("Bootstrap"  = boot                           ,
                              "var_x"      = vars_names[comb[i, 1]]         ,
                              "var_y"      = vars_names[comb[i, 2]]         ,
                              "w1"         = w1                             ,
                              "w2"         = w2                             ,
                              "w_ie_x"     = w_ie_x                         ,
                              "w_ie_x"     = w_ie_y                         ,
                              "w_mi_xy"    = w_mi_xy                        ,
                              "cond_ie_xy" = cond_ie_xy                     ,
                              "cond_ie_yx" = cond_ie_yx                     ,
                              "norm_mi_xy" = 2 * w_mi_xy / (w_ie_x + w_ie_y))
      
      ## Save combination result
      df_endo <- rbind(df_endo , df_comb_i)
      
    }
    
  }
  
  ## Thresholds endogeneity
  df_endogeneity <- do.call(rbind, lapply(
    thresholds,
    \(thres) df_endo %>% group_by(var_x, var_y) %>% summarize(
      Threshold    = thres                                                                        ,
      w1           = mean(w1)                                                                     ,
      w2           = mean(w2)                                                                     ,
      p_value_mi   = wilcox.test(x = norm_mi_xy, mu = thres     , alternative = "greater"  )$p.value,
      p_value_both = wilcox.test(x = cond_ie_xy, y  = cond_ie_yx, alternative = "two.sided")$p.value,
      p_value_xy   = wilcox.test(x = cond_ie_xy, y  = cond_ie_yx, alternative = "greater"  )$p.value,
      p_value_yx   = wilcox.test(x = cond_ie_yx, y  = cond_ie_xy, alternative = "greater"  )$p.value
    ))
  )
  
  ## Relationship index
  idx_no_rel <- which(df_endogeneity$p_value_mi >  0.05)
  idx_both   <- which(df_endogeneity$p_value_mi <= 0.05 & df_endogeneity$p_value_both >  0.05)
  idx_xy     <- which(df_endogeneity$p_value_mi <= 0.05 & df_endogeneity$p_value_xy   <= 0.05)
  idx_yx     <- which(df_endogeneity$p_value_mi <= 0.05 & df_endogeneity$p_value_yx   <= 0.05)
  
  ## Relationship vector
  rel_vec             <- rep(NA, nrow(df_endogeneity))
  rel_vec[idx_no_rel] <- "-"
  rel_vec[idx_both  ] <- "x <-> y"
  rel_vec[idx_xy    ] <- "x -> y"
  rel_vec[idx_yx    ] <- "x <- y"
  
  ## Ungroup data
  df_endogeneity <- ungroup(df_endogeneity)
  
  ## Add weights signal and relationships
  df_endogeneity <- df_endogeneity %>% mutate(relationship = rel_vec, signal = w1 * w2)
  
  ## Return
  return(df_endogeneity)
  
}

## Function to plot endogeneity
fn_plot_endo <- function(df_endo){
  
  ## Identify thresholds
  thresholds <- sort(unique(df_endo$Threshold))
  
  ## List with results
  list_plots <- list()
  
  ## Loop in thresholds
  for(i in 1:length(thresholds)){
    
    ## Threshold analysed
    thres <- thresholds[i]
    
    ## Data frame for plot
    df_plot <- 
      df_endo                                                %>%
      filter(as.character(Threshold) == as.character(thres)) %>% 
      select(var_x, var_y, relationship, signal)             %>%
      setNames(c("from", "to", "relation", "signal"))           
    
    ## Remove empty relationships
    df_plot <- df_plot %>% filter(relation != "-")
    
    ## If its empty, return nothing
    if(nrow(df_plot) == 0){
      list_plots[[i]] <- "No relationships"
      next
    } 
    
    ## Data frame with directions
    df_plot_xy   <- df_plot %>% filter(relation == "x -> y" )
    df_plot_yx   <- df_plot %>% filter(relation == "x <- y" )
    df_plot_both <- df_plot %>% filter(relation == "x <-> y")
    
    ## Correct diretions x <- y
    df_plot_yx <- 
      df_plot_yx                                      %>%
      relocate(to)                                    %>%
      setNames(c("from", "to", "relation", "signal")) 
    
    ## Correct diretions in data frame biderectional
    df_plot_both <- 
      df_plot_both                                    %>% 
      relocate(to)                                    %>%
      mutate(signal = NA)                             %>%
      setNames(c("from", "to", "relation", "signal")) %>%
      rbind(df_plot_both)
    
    ## Combine data frames
    df_plot <- rbind(df_plot_xy, df_plot_yx, df_plot_both)
    
    ## Remove unused variables
    df_plot$relation <- NULL
    
    ## Convert to a graph data structure
    g <- graph_from_data_frame(df_plot, directed = TRUE)
    
    ## Create the plot with circle layout
    p_endo <- ggraph(g, layout = 'circle')
    
    ## Add edges and links geom
    p_endo <- p_endo + geom_edge_link(
      mapping     = aes(label = ifelse(is.na(signal), "", round(signal, digits = 3)), width = signal, edge_alpha = 0.5), 
      arrow       = arrow(length = unit(2, 'mm'), type = "closed")                                                     , 
      end_cap     = circle(4, 'mm')                                                                                    ,
      edge_colour = "grey70"                                                                                           ,
      check_overlap = TRUE
    )
    
    ## Add edges 
    p_endo <- p_endo + geom_node_point(color = '#53E1FA', size = 10)
    
    ## Add labels
    p_endo <- p_endo + geom_node_text(aes(label = name), size = 4, repel = TRUE)
    
    ## Scale 
    p_endo <- p_endo + scale_edge_width(range = c(0.5, 1.5))
    
    ## Theme
    p_endo <- p_endo + theme_void()
    
    ## Remove legend
    p_endo <- p_endo + theme(legend.position = "none")
    
    ## Save plot
    list_plots[[i]] <- p_endo
    
  }
  
  ## Change names
  names(list_plots) <- paste0("Threshold_", thresholds)
  
  ## Return
  return(list_plots)
  
}

## Get evaluation time
eval_time <- system.time({
  
  ## Progress bar windows
  PB <- tkProgressBar(title   = "Endogeneity Optimization",
                      label   = "Bootstrap"               ,
                      min     = 0                         , 
                      max     = n_boot                    , 
                      initial = 0                         , 
                      width   = 300                       )
  
  ## Function to update the progress bar
  fn_PB <- \(n) setTkProgressBar(
    pb    = PB                          ,
    value = n                           ,
    label = sprintf("%d / %d", n, n_boot)
  )
  
  ## Function to SNOW update
  PB_snow <- list(progress = fn_PB)
  
  ## Register parallel cluster
  cl <- makeCluster(n_cores)
  registerDoSNOW(cl)
  
  ## Loop in bootstraps
  W <- foreach(
    j             = 1:n_boot              ,
    .combine      = rbind                 ,
    .inorder      = FALSE                 , 
    .packages     = c("Rsolnp", "entropy"),
    .options.snow = PB_snow                
  ) %dopar% return(list(
    boot = j                                                             ,
    w    = solnp(pars    = rep(1/4, 4)                                   ,
                 fun     = endogeneity_entropy_opt                       ,
                 eqfun   = \(w, df) sum(w)                               ,
                 eqB     = 1                                             ,
                 LB      = rep(0, 4)                                     ,
                 UB      = rep(1, 4)                                     ,
                 control = list(trace = 1)                               ,
                 df      = data.frame(df_boot_topsis_utility_citizens[,j],
                                      df_boot_topsis_utility_firms   [,j],
                                      df_boot_topsis_bank_leverage   [,j],
                                      df_boot_topsis_bank_health     [,j]))$pars
  ))
  
  ## Close parallel cluster
  stopCluster(cl)
  
})

## Get Weights
df_weights <- do.call(rbind, W[,2])

## Coerce to data frame
df_weights <- data.frame(df_weights)

## Order results
df_weights <- df_weights[order(do.call(c, W[,1])), ]

## change names
colnames(df_weights) <- c("Utility citizens",
                          "Utility firms"   ,   
                          "Bank Leverage"   ,
                          "Bank Health"     )

## List with variables
list_vars_endo <- list(df_boot_topsis_utility_citizens,
                       df_boot_topsis_utility_firms   ,
                       df_boot_topsis_bank_leverage   ,
                       df_boot_topsis_bank_health     )

## Data frame with endogeneity results
df_endo_res <- endogeneity_entropy(
  df_w       = df_weights                            ,
  list_df    = list_vars_endo                        ,
  thresholds = seq(from  = 0.05, to = 0.4, by = 0.05) 
)

################################################################################
## Plots for endogeneity analysis
################################################################################

## Plot network
list_plots_endogeneity <- fn_plot_endo(df_endo_res)

## Boxplot weights
p_weights <- df_weights %>% pivot_longer(cols = everything()) %>% ggplot() + 
  geom_density_ridges(aes(x = value, y = name), fill = "#53E1FA", alpha = 0.5) + 
  xlab("Endogeneity Weights") + 
  ylab("") +
  theme_minimal()

################################################################################
## Prepare Data for Regression Analysis
################################################################################

## Create a dataframe for the TOPSIS scores
df_topsis_scores <- data.frame(
  Utility_Citizens = topsis_utility_citizens$score,
  Utility_Firms    = topsis_utility_firms   $score,
  Bank_Leverage    = topsis_bank_leverage   $score,
  Bank_Health      = topsis_bank_health     $score
)

## Combine the TOPSIS scores and contextual variables into a single dataframe
df_regression <- cbind(df_topsis_scores, df_contextual)

## Remove unused variables
df_regression$`Company Common Name` <- NULL

################################################################################
## Regression Analysis with Bootstrap Sampling
################################################################################

## Coefficients list
coefficients_tobit   <- list()
coefficients_beta    <- list()
coefficients_simplex <- list()

## Residuals lists
residuals_tobit   <- list()
residuals_beta    <- list()
residuals_simplex <- list()

## Metrics lists
df_metrics <- list()

## Vector with dependent variables (TOPSIS scores)
vars_y <- c("Utility_Citizens", "Utility_Firms", "Bank_Leverage", "Bank_Health")

## List with bootstrap indexes without aliased coefficients
list_idx_boot <- list()

## Initial bootstrap value
b <- 1

## Loop to find bootstrap indexes without aliased coefs
while(b <= n_boot){
  
  ## Bootstrap indexes
  idx_boot <- sample(1:nrow(df_regression), nrow(df_regression), replace = TRUE)
  
  ## Try linear regression
  error <- try(summary(lm(formula = Utility_Citizens~., data = df_regression[idx_boot,])))
  
  ## Check error
  if(is(error, "try-error")) next
  
  ## Print evaluation
  print(b)
  
  ## Save indexes
  list_idx_boot[[b]] <- idx_boot
  
  ## Update bootstrap
  b <- b + 1
  
}

var_dep <- "Utility_Citizens"
## Loop over each dependent variable
for (var_dep in vars_y) {
  
  ## Formula
  frl <- as.formula(paste0(var_dep, "~. -", paste(setdiff(vars_y, var_dep), collapse = " -")))
  
  ## Initialize data frames to store results
  df_coef_tobit    <- NULL
  df_coef_beta     <- NULL
  df_coef_simplex  <- NULL
  df_resid_tobit   <- NULL
  df_resid_beta    <- NULL
  df_resid_simplex <- NULL
  df_metrics_dep   <- NULL

  ## Bootstrap sampling
  for (i in 1:n_boot) {
    
    ## Evaluation time
    eval_time <- system.time({
      
      ## Data frame with original observations
      df_orig <- df_regression
      
      ## Ensure the dependent variable is within (0,1) for beta and simplex regression
      df_orig[,var_dep] <- ifelse(df_orig[,var_dep] >= 0.9999, 0.9999, df_orig[,var_dep])
      df_orig[,var_dep] <- ifelse(df_orig[,var_dep] <= 0.0001, 0.0001, df_orig[,var_dep])
      
      ## Bootstrap sample indices
      idx_boot <- list_idx_boot[[i]]
      
      ## Out-of-bag sample indices
      idx_oob <- setdiff(1:nrow(df_regression), idx_boot)
      
      ## Create bootstrap and oob datasets
      df_boot <- df_orig[idx_boot, ]
      df_oob  <- df_orig[idx_oob , ]
      
      ## Fit tobit regression
      reg_tobit <- tobit(formula = frl, data = df_boot, left = 0.0001, right = 0.9999)
      
      ## Fit beta regression
      reg_beta <- betareg(formula = frl, data = df_boot)
      
      ## Fit simplex regression
      reg_simplex <- simplexreg(formula = frl, data = df_boot)
      
      ## Predict on oob data
      pred_oob_tobit   <- predict(reg_tobit  , newdata = df_oob)
      pred_oob_beta    <- predict(reg_beta   , newdata = df_oob)
      pred_oob_simplex <- predict(reg_simplex, newdata = df_oob)
      
      ## Predict on the full data
      pred_full_tobit   <- predict(reg_tobit  , newdata = df_orig)
      pred_full_beta    <- predict(reg_beta   , newdata = df_orig)
      pred_full_simplex <- predict(reg_simplex, newdata = df_orig)
      
      ## Compute performance metrics for oob sample
      df_metrics_i <- rbind(
        data.frame(Bootstrap = i                                         ,
                   Model     = "Tobit"                                   ,
                   RMSE      = RMSE    (pred_oob_tobit, df_oob[,var_dep]),
                   MAE       = MAE     (pred_oob_tobit, df_oob[,var_dep]),
                   MAPE      = MAPE    (pred_oob_tobit, df_oob[,var_dep]),
                   R2_Score  = R2_Score(pred_oob_tobit, df_oob[,var_dep])),
        
        data.frame(Bootstrap = i                                        ,
                   Model     = "Beta"                                   ,
                   RMSE      = RMSE    (pred_oob_beta, df_oob[,var_dep]),
                   MAE       = MAE     (pred_oob_beta, df_oob[,var_dep]),
                   MAPE      = MAPE    (pred_oob_beta, df_oob[,var_dep]),
                   R2_Score  = R2_Score(pred_oob_beta, df_oob[,var_dep])),
        
        data.frame(Bootstrap = i                                           ,
                   Model     = "Simplex"                                   ,
                   RMSE      = RMSE    (pred_oob_simplex, df_oob[,var_dep]),
                   MAE       = MAE     (pred_oob_simplex, df_oob[,var_dep]),
                   MAPE      = MAPE    (pred_oob_simplex, df_oob[,var_dep]),
                   R2_Score  = R2_Score(pred_oob_simplex, df_oob[,var_dep]))
      )
      
      ## Add in data frame with metrics
      df_metrics_dep <- rbind(df_metrics_dep, df_metrics_i)
      
      ## Save coefficients
      df_coef_tobit   <- rbind(df_coef_tobit  , c(Bootstrap = i, coef(reg_tobit  )))
      df_coef_beta    <- rbind(df_coef_beta   , c(Bootstrap = i, coef(reg_beta   )))
      df_coef_simplex <- rbind(df_coef_simplex, c(Bootstrap = i, coef(reg_simplex)))
      
      ## Save residuals
      df_resid_tobit   <- cbind(df_resid_tobit  , df_regression[, var_dep] - pred_full_tobit  )
      df_resid_beta    <- cbind(df_resid_beta   , df_regression[, var_dep] - pred_full_beta   )
      df_resid_simplex <- cbind(df_resid_simplex, df_regression[, var_dep] - pred_full_simplex)
      
    })
    
    ## Print progress
    print(sprintf("Bootstrap %d/%d; Model: %s; Time: %.2f", i, n_boot, var_dep, eval_time[3]))

  }

  ## Store results in lists
  coefficients_tobit  [[var_dep]] <- df_coef_tobit
  coefficients_beta   [[var_dep]] <- df_coef_beta
  coefficients_simplex[[var_dep]] <- df_coef_simplex
  residuals_tobit     [[var_dep]] <- df_resid_tobit
  residuals_beta      [[var_dep]] <- df_resid_beta
  residuals_simplex   [[var_dep]] <- df_resid_simplex
  df_metrics          [[var_dep]] <- df_metrics_dep
  
}
 
################################################################################
## Optimization to Combine Models
################################################################################

## Function to optimize weighted variance-covariance of residuals
weighted_var_covar <- function(w, j, resids_tobit, resids_beta, resids_simplex){
  
  ## Residuals for each model and obs j
  resids_obs <- as.matrix(cbind(resids_tobit[,j], resids_beta[,j], resids_simplex[,j]))
  
  ## Weighted resid
  weighted_resid <- resids_obs %*% w
  
  ## Variance
  var_resids <- as.numeric(var(weighted_resid))
  
  ## Weights Matrix 
  weighted_mat <- resids_obs %*% diag(w)
  
  ## Create combinations (i != j)
  comb <- t(combn(1:ncol(weighted_mat), 2))
  
  ## Create the covar matrix
  covar_mat <- apply(comb, 1, \(x) weighted_mat[,x[1]] * weighted_mat[,x[2]])
  
  ## Covariance
  covar_resids <- sum(cov(covar_mat))
  
  ## Return
  return(var_resids + 2*covar_resids)
  
}

## Initialize a list to store optimized weights
list_reg_weights <- list()

## For each dependent variable, perform optimization
for (var_dep in vars_y) {
  
  ## Get residuals from models
  resids_tobit   <- residuals_tobit  [[var_dep]]
  resids_beta    <- residuals_beta   [[var_dep]]
  resids_simplex <- residuals_simplex[[var_dep]]
  
  ## Evaluation time
  eval_time_opt <- system.time({
    
    ## Set up parallel processing
    cl <- makeCluster(n_cores)
    registerDoSNOW(cl)
    
    ## Progress Bar
    PB <- tkProgressBar(
      title   = paste0("Combine bootstrap regressions - ", var_dep),
      label   = "bootstrap"                                        ,
      min     = 0                                                  , 
      max     = n_boot                                             , 
      initial = 0                                                  , 
      width   = 300                                                 
    )
    
    ## Funcoes para a barra de progresso
    PB_snow <- \(n) setTkProgressBar(PB, n, label = sprintf("%d / %d", n, n_boot))
    PB_snow <- list(progress = PB_snow)
    
    ## Optimization control
    list_ctrl <- list(itermax = 300, trace = FALSE, reltol = 1e-4)
    
    ## Perform optimization in parallel
    w_boot <- foreach(
      j             = 1:n_boot    ,
      .combine      = rbind       ,
      .packages     = c('DEoptim'),
      .options.snow = PB_snow      
    ) %dopar% DEoptim(fn             = weighted_var_covar,
                      fnMap          = \(w) w/sum(w)     ,
                      lower          = rep(0, 3)         ,
                      upper          = rep(1, 3)         ,
                      j              = j                 ,
                      resids_tobit   = resids_tobit      , 
                      resids_beta    = resids_beta       , 
                      resids_simplex = resids_simplex    ,  
                      control        = list_ctrl         )$optim$bestmem
    
    ## Close cluster
    stopCluster(cl)
    
  })
  
  ## Store weights
  list_reg_weights[[var_dep]] <- data.frame(Tobit   = w_boot[,1], 
                                            Beta    = w_boot[,2], 
                                            Simplex = w_boot[,3])
  
  ## prnt evaluation
  print(sprintf("Model: %s; Time: %.2f", var_dep, eval_time_opt[3]))
  
}

################################################################################
## Combine Coefficients
################################################################################

## List with combined coefs
coefficients_combined <- list()

## For each dependent variable, combine coefficients using optimized weights
for (var_dep in vars_y) {

  ## Data frames with coefficients and weights
  weights         <- list_reg_weights    [[var_dep]]
  df_coef_tobit   <- coefficients_tobit  [[var_dep]]
  df_coef_beta    <- coefficients_beta   [[var_dep]]
  df_coef_simplex <- coefficients_simplex[[var_dep]]

  ## Remove 'Bootstrap' column from coefficients
  coefs_tobit   <- df_coef_tobit  [,-1]
  coefs_beta    <- df_coef_beta   [,-1]
  coefs_simplex <- df_coef_simplex[,-1]
  
  ## Remove Phi in beta coefs
  coefs_beta <- coefs_beta[,!grepl("(phi)", colnames(coefs_beta), ignore.case = TRUE)]
  
  ## Combine coefficients using weights
  df_coef_comb <- lapply(1:nrow(weights), function(i){
    w_tobit   <- weights$Tobit  [i]
    w_beta    <- weights$Beta   [i]
    w_simplex <- weights$Simplex[i]
    coef_comb <- w_tobit * coefs_tobit[i, ] + w_beta * coefs_beta[i, ] + w_simplex * coefs_simplex[i, ]
    return(coef_comb)
  })
  
  ## Coerce to data frame
  df_coef_comb <- do.call(rbind, df_coef_comb)
  
  ## Save data frame
  coefficients_combined[[var_dep]] <- df_coef_comb
  
  ## Print evaluation
  cat("Combining coefficients for", var_dep, "\n")
  
}

################################################################################
## Analyze Weights 
################################################################################

## Initialize a data frame to collect weights from all categories
df_all_weights <- lapply(
  1:length(list_reg_weights),
  \(i) mutate(list_reg_weights[[i]], Model = names(list_reg_weights)[i])
)

## Coerce to data frame
df_all_weights <- do.call(rbind, df_all_weights)

## Pivot data
df_all_weights <- df_all_weights %>% pivot_longer(cols = c("Tobit", "Beta", "Simplex"),
                                                  names_to = "Regression", 
                                                  values_to = "Weight")


## Combined Weights Density Ridge Plot
p_weights_combined_ridge <- ggplot(df_all_weights) +
  facet_wrap(~Model, scales = "free_y", ncol = 4) + 
  geom_density_ridges(aes(x = Weight, y = Regression), alpha = 0.5) +
  xlab("Weights") + 
  ylab("") + 
  theme_minimal() +
  labs(
    title = "Distribution of Optimized Weights Across All Categories",
    x = "Weights",
    fill = "Regression"
  )

## Combined Weights Boxplot
p_weights_combined_boxplot <- ggplot(df_all_weights) +
  facet_wrap(~Model, scales = "free_y", ncol = 4) + 
  geom_boxplot(aes(x = Regression, y = Weight), alpha = 0.5) +
  xlab("Weights") + 
  ylab("") + 
  theme_minimal() +
  labs(
    title = "Distribution of Optimized Weights Across All Categories",
    x = "Weights",
    fill = "Regression"
  )

################################################################################
## Analyze Coefficients with Log Scaling and Significance Marking
################################################################################


for (var_dep in vars_y) {

  cat("Analyzing results for", var_dep, "\n")
  
  # Coefficients
  df_coef_comb <- coefficients_combined[[var_dep]]

  # Compute median and quantiles for coefficients
  df_coef_quantile <- apply(df_coef_comb, 2, function(x) quantile(x, probs = c(0.5, 0.025, 0.975)))
  df_coef_quantile <- t(df_coef_quantile)
  df_coef_quantile <- data.frame(Variables = rownames(df_coef_quantile), df_coef_quantile, row.names = NULL)

  # Replace '.' or '_' with spaces for better readability in variable names (optional)
  df_coef_quantile$Variables <- gsub("[._]", " ", df_coef_quantile$Variables)

  # Mark significant variables based on confidence intervals (i.e., intervals that do not include zero)
  df_coef_quantile$Sig <- ifelse(df_coef_quantile$X2.5. * df_coef_quantile$X97.5. > 0, "Yes", "No")

  # Transform data to long format for plotting
  df_coef_long <- pivot_longer(as.data.frame(df_coef_comb), cols = everything(), names_to = "Variables", values_to = "Coefficient")

  # Apply log scaling to the coefficients of non-intercept and non-phi variables
  df_coef_long$Coefficient_log <- sign(df_coef_long$Coefficient) * log10(abs(df_coef_long$Coefficient) + 1e-5)

  # Exclude intercept and phi terms from the data
  df_intercept_phi <- df_coef_long %>% filter(Variables == "(Intercept)" | grepl("phi", Variables, ignore.case = TRUE))
  df_others <- df_coef_long %>% filter(!Variables %in% df_intercept_phi$Variables)

  # Merge the significance indicator with the long format data
  df_others <- merge(df_others, df_coef_quantile[, c("Variables", "Sig")], by = "Variables")

  # Beautify the log-scaled boxplot
  # Reorder variables based on median coefficient value for better visualization
  df_others$Variables <- factor(df_others$Variables, levels = df_others %>%
                                  group_by(Variables) %>%
                                  summarize(median_coef = median(Coefficient_log)) %>%
                                  arrange(median_coef) %>%
                                  pull(Variables))

  # Create the ggplot object for log-scaled boxplot of non-intercept and non-phi coefficients
  p_coef_boxplot_log <- ggplot(df_others, aes(x = Variables, y = Coefficient_log, fill = Sig)) +
    geom_boxplot(color = "black", alpha = 0.7, outlier.color = "red", outlier.size = 1.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "darkred", size = 1) +  # Add reference line at zero
    coord_flip() +  # Flip the axes to make the plot horizontal
    theme_minimal() +
    labs(title = paste("Log-Scaled Combined Coefficients for", var_dep),
         x = "", y = "Log-Scaled Coefficient") +
    scale_fill_manual(values = c("No" = "lightgrey", "Yes" = "#1f77b4")) +  # Color significant variables
    theme(axis.text.y = element_text(size = 10),
          axis.title.y = element_blank(),
          plot.title = element_text(hjust = 0.5),
          legend.position = "top") +
    guides(fill = guide_legend(title = "Significant"))

  # Display the log-scaled coefficients boxplot for non-intercept and non-phi variables
  print(p_coef_boxplot_log)

  # Save the log-scaled boxplot to a file
  ggsave(filename = paste0("Log_Scaled_Combined_Coefficients_Boxplot_", var_dep, ".png"),
         plot = p_coef_boxplot_log, width = 10, height = 8, dpi = 300)

}


################################################################################
## Consolidate and Separate Intercepts and Phi Terms for Better Scale Visualization
################################################################################

# Create empty data frames to store intercepts and phi terms from all categories
df_all_intercepts <- data.frame()
#df_all_phi        <- data.frame()

for (var_dep in vars_y) {
  
  cat("Processing intercepts and phi for", var_dep, "\n")

  # Coefficients
  df_coef_comb <- coefficients_combined[[var_dep]]

  # Transform data to long format
  df_coef_long <- pivot_longer(as.data.frame(df_coef_comb), 
                               cols = everything(),
                               names_to = "Variables",
                               values_to = "Coefficient")

  # Filter intercept (Intercept term) and phi (scaling parameter)
  df_intercept <- df_coef_long %>% filter(Variables == "(Intercept)")
  #df_phi <- df_coef_long %>% filter(grepl("phi", Variables, ignore.case = TRUE))

  # Add a column for the dependent variable name (category)
  df_intercept$Category <- var_dep
  #df_phi$Category <- var_dep

  # Append to the consolidated data frames
  df_all_intercepts <- rbind(df_all_intercepts, df_intercept)
  #df_all_phi <- rbind(df_all_phi, df_phi)
}

# Beautify the combined intercepts plot using the same style as the initial TOPSIS plots

## Combined Intercepts Density Ridge Plot
p_intercept_combined_ridge <- ggplot(df_all_intercepts, aes(x = Coefficient, y = Category, fill = Category)) +
  geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Distribution of Intercepts Across All Categories",
       x = "Coefficient")

# Display the consolidated intercept density ridge plot
print(p_intercept_combined_ridge)

# Save the combined intercept density ridge plot to a file
ggsave(filename = "Combined_Intercepts_Ridge_Plot_All_Categories.png", plot = p_intercept_combined_ridge, width = 10, height = 6, dpi = 300)

## Combined Intercepts Boxplot
p_intercept_combined_box <- ggplot(df_all_intercepts, aes(x = Category, y = Coefficient, fill = Category)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Combined Intercepts Across All Categories",
       y = "Coefficient") +
  coord_flip()  # Flip coordinates for better readability

# Display the consolidated intercept boxplot
print(p_intercept_combined_box)

# Save the combined intercept boxplot to a file
ggsave(filename = "Combined_Intercepts_Boxplot_All_Categories.png", plot = p_intercept_combined_box, width = 10, height = 6, dpi = 300)

## ## Combined Phi Terms Density Ridge Plot
## p_phi_combined_ridge <- ggplot(df_all_phi, aes(x = Coefficient, y = Category, fill = Category)) +
##   geom_density_ridges(alpha = 0.7, scale = 1.2, rel_min_height = 0.01) +
##   theme_minimal() +
##   theme(legend.position = "none",
##         axis.title.y = element_blank(),
##         axis.text.y = element_text(size = 10),
##         plot.title = element_text(hjust = 0.5)) +
##   labs(title = "Distribution of Phi (Scaling) Terms Across All Categories",
##        x = "Coefficient")
## 
## # Display the consolidated phi density ridge plot
## print(p_phi_combined_ridge)
## 
## # Save the combined phi density ridge plot to a file
## ggsave(filename = "Combined_Phi_Ridge_Plot_All_Categories.png", plot = p_phi_combined_ridge, width = 10, height = 6, dpi = 300)
## 
## ## Combined Phi Terms Boxplot
## p_phi_combined_box <- ggplot(df_all_phi, aes(x = Category, y = Coefficient, fill = Category)) +
##   geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16, outlier.size = 2) +
##   theme_minimal() +
##   theme(legend.position = "none",
##         axis.title.x = element_blank(),
##         axis.text.x = element_text(size = 10),
##         plot.title = element_text(hjust = 0.5)) +
##   labs(title = "Combined Phi (Scaling) Terms Across All Categories",
##        y = "Coefficient") +
##   coord_flip()  # Flip coordinates for better readability
## 
## # Display the consolidated phi boxplot
## print(p_phi_combined_box)
## 
## # Save the combined phi boxplot to a file
## ggsave(filename = "Combined_Phi_Boxplot_All_Categories.png", plot = p_phi_combined_box, width = 10, height = 6, dpi = 300)


################################################################################
## Save Results
################################################################################

# List of dependent variables (TOPSIS scores)
dep_vars <- c("Utility_Citizens", "Utility_Firms", "Bank_Leverage", "Bank_Health")

# Save the weights and coefficients to Excel files
for (dep_var in dep_vars) {

  # Weights
  weights <- list_reg_weights[[dep_var]]
  write_xlsx(weights, paste0("Optimized_Weights_", dep_var, ".xlsx"))

  # Coefficients
  df_coef_comb <- coefficients_combined[[dep_var]]
  df_coef_comb <- as.data.frame(df_coef_comb)
  write_xlsx(df_coef_comb, paste0("Combined_Coefficients_", dep_var, ".xlsx"))

  # Metrics
  metrics_tobit_df <- metrics_tobit[[dep_var]]
  metrics_beta_df  <- metrics_beta [[dep_var]]

  # Ensure metrics are not NULL
  if (is.null(metrics_tobit_df) || is.null(metrics_beta_df)) {
    cat("Metrics for", dep_var, "are NULL or not found. Skipping...\n")
    next
  }

  # Convert to data frames if necessary
  if (!is.data.frame(metrics_tobit_df)) {
    metrics_tobit_df <- as.data.frame(metrics_tobit_df)
  }

  if (!is.data.frame(metrics_beta_df)) {
    metrics_beta_df <- as.data.frame(metrics_beta_df)
  }

  # Write metrics to Excel
  write_xlsx(list(Tobit = metrics_tobit_df, Beta = metrics_beta_df), paste0("Metrics_", dep_var, ".xlsx"))

}

################################################################################
## Save Results
################################################################################

## Create a folder to save results if it does not exist
if(!dir.exists("Results")) dir.create("Results")

## Set the path to save results
path_results <- "./Results/"

## -----------------------------------------------------------------------------
## Save Entropy Weights
## -----------------------------------------------------------------------------
# Saving entropy weights for each group of variables
write_xlsx(
  list(
    Utility_Citizens = as.data.frame(w_utility_citizens),
    Utility_Firms    = as.data.frame(w_utility_firms),
    Bank_Leverage    = as.data.frame(w_bank_leverage),
    Bank_Health      = as.data.frame(w_bank_health)
  ),
  paste0(path_results, "Entropy_Weights.xlsx")
)

## -----------------------------------------------------------------------------
## Save TOPSIS Scores
## -----------------------------------------------------------------------------
# This includes the TOPSIS scores for all categories
write_xlsx(df_topsis, paste0(path_results, "TOPSIS_Scores.xlsx"))

## -----------------------------------------------------------------------------
## Save Bootstrapped TOPSIS Results
## -----------------------------------------------------------------------------
# Bootstrapped TOPSIS scores for each category
write_xlsx(
  list(
    Utility_Citizens = as.data.frame(df_boot_topsis_utility_citizens),
    Utility_Firms    = as.data.frame(df_boot_topsis_utility_firms),
    Bank_Leverage    = as.data.frame(df_boot_topsis_bank_leverage),
    Bank_Health      = as.data.frame(df_boot_topsis_bank_health)
  ),
  paste0(path_results, "Bootstrapped_TOPSIS_Scores.xlsx")
)

## -----------------------------------------------------------------------------
## Save Endogeneity Analysis Results
## -----------------------------------------------------------------------------
# Data frame with endogeneity analysis results
write_xlsx(df_endo_res, paste0(path_results, "Endogeneity_Analysis_Results.xlsx"))

## -----------------------------------------------------------------------------
## Save Regression Coefficients
## -----------------------------------------------------------------------------
# Coefficients from Tobit, Beta, and Simplex regressions for each dependent variable
for (var_dep in vars_y) {
  # Ensure coefficients are data frames
  coef_tobit   <- as.data.frame(coefficients_tobit[[var_dep]])
  coef_beta    <- as.data.frame(coefficients_beta[[var_dep]])
  coef_simplex <- as.data.frame(coefficients_simplex[[var_dep]])
  
  write_xlsx(
    list(
      Tobit   = coef_tobit,
      Beta    = coef_beta,
      Simplex = coef_simplex
    ),
    paste0(path_results, "Regression_Coefficients_", var_dep, ".xlsx")
  )
}

## -----------------------------------------------------------------------------
## Save Residuals
## -----------------------------------------------------------------------------
# Residuals from Tobit, Beta, and Simplex regressions for each dependent variable
for (var_dep in vars_y) {
  write_xlsx(
    list(
      Tobit   = as.data.frame(residuals_tobit[[var_dep]]),
      Beta    = as.data.frame(residuals_beta[[var_dep]]),
      Simplex = as.data.frame(residuals_simplex[[var_dep]])
    ),
    paste0(path_results, "Residuals_", var_dep, ".xlsx")
  )
}

## -----------------------------------------------------------------------------
## Save Performance Metrics
## -----------------------------------------------------------------------------
# Performance metrics for each regression model and dependent variable
for (var_dep in vars_y) {
  metrics_df <- df_metrics[[var_dep]]
  write_xlsx(metrics_df, paste0(path_results, "Metrics_", var_dep, ".xlsx"))
}

## -----------------------------------------------------------------------------
## Save Optimized Regression Weights
## -----------------------------------------------------------------------------
# Weights obtained from optimizing the combination of regression models
for (var_dep in vars_y) {
  weights <- list_reg_weights[[var_dep]]
  write_xlsx(weights, paste0(path_results, "Optimized_Regression_Weights_", var_dep, ".xlsx"))
}

## -----------------------------------------------------------------------------
## Save Combined Coefficients
## -----------------------------------------------------------------------------
# Combined coefficients for each dependent variable after combining models
for (var_dep in vars_y) {
  df_coef_comb <- as.data.frame(coefficients_combined[[var_dep]])
  write_xlsx(df_coef_comb, paste0(path_results, "Combined_Coefficients_", var_dep, ".xlsx"))
}


