mode_factor <- function(x) {
  ux <- unique(x)
  tabu <- tabulate(match(x, ux))
  as.character( ux[which(tabu==max(tabu))] )
}