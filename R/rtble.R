#' Tabela estatisticas modelo gml
#' 
#' @param object an object of class "glm", usually, a result of a call to glm
#' 
#'
#'
#'
#' @importFrom DescTools PseudoR2
#' @import margins 

rtble.glmer <- function(object){
  
  tab <- summary(object)$coefficients
  aux <- rownames(tab)
  tab <- as_tibble(tab)
  tab <- cbind(factor=aux, tab)
  tab <- as_tibble(tab)
  
  effects_logit <- margins::margins(object)
  efm <- summary(effects_logit)
  efm <- as_tibble(efm)
  
  tab <- left_join(tab, efm)
  
  tab <- tab[, c(1, 2, 3, 5, 6)]

  # estatisticas extras
  # McFadden's Pseudo R-squared
  
  #pR <- DescTools::PseudoR2(object,  c("McFadden", "Effron", "Nagelkerke"))
  
  
  tble <- data.frame(table(true=object@resp$y, 
                           predicted=round(fitted(object))))
  # Percent correctly predicted
  y1 <- tble %>%
    filter(predicted==1) %>%
    adorn_percentages("col") %>%
    filter(true==1) %>%
    select(Freq)
  
  y0 <- tble %>%
    filter(predicted==0) %>%
    adorn_percentages("col") %>%
    filter(true==0) %>%
    select(Freq)
  
  y <- tble %>%
    adorn_percentages("col") %>%
    filter(true==predicted) %>%
    select(Freq) %>%
    sum()
  
 aux <- cbind(rbind(aic = object@resp$aic, 
         n = sum(object@resp$n),
         predictedy1 = 100*y1,
         predictedy0 = 100*y0,
         predictedy = 100*y), NA, NA, NA)
 
 aux <- cbind(rownames(aux), aux)
 colnames(aux) <- colnames(tab)
 
 tab <- rbind(tab, aux)
 rownames(tab) <- NULL
 return(tab)
}


rtble.glm <- function(object){
  
  tab <- summary(object)$coefficients
  aux <- rownames(tab)
  tab <- as_tibble(tab)
  tab <- cbind(factor=aux, tab)
  tab <- as_tibble(tab)
  
  effects_logit <- margins::margins(object)
  efm <- summary(effects_logit)
  efm <- as_tibble(efm)
  
  tab <- left_join(tab, efm)
  
  tab <- tab[, c(1, 2, 3, 5, 6)]
  
  # estatisticas extras
  # McFadden's Pseudo R-squared
  
  pR <- DescTools::PseudoR2(object,  c("McFadden", "Effron", "Nagelkerke"))
  
  
  tble <- data.frame(table(true=object$y, 
                           predicted=round(fitted(object))))
  # Percent correctly predicted
  y1 <- tble %>%
    filter(predicted==1) %>%
    adorn_percentages("col") %>%
    filter(true==1) %>%
    select(Freq)
  
  y0 <- tble %>%
    filter(predicted==0) %>%
    adorn_percentages("col") %>%
    filter(true==0) %>%
    select(Freq)
  
  y <- tble %>%
    adorn_percentages("col") %>%
    filter(true==predicted) %>%
    select(Freq) %>%
    sum()
  
  aux <- cbind(rbind(aic = object$aic, 
                     n = object$df.null,
                     McFadden = pR["McFadden"],
                     Effron = pR["Effron"],
                     Nagelkerke = pR["Nagelkerke"],
                     predictedy1 = 100*y1,
                     predictedy0 = 100*y0,
                     predictedy = 100*y), NA, NA, NA)
  
  aux <- cbind(rownames(aux), aux)
  colnames(aux) <- colnames(tab)
  
  tab <- rbind(tab, aux)
  rownames(tab) <- NULL
  return(tab)
}
