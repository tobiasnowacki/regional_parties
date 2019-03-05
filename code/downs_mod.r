library(ggplot2)
n <- 0.9 # national party's policy position

qunif(n)
F_n <- pbeta(n, 0.5, 0.5)
R <- qbeta(F_n / 3, 0.5, 0.5)
pbeta(R, 0.5, 0.5)
F_n - pbeta(R, 0.5, 0.5)


beta_par <- c(3, 2, 1, 0.9, 0.8, 0.7, 0.6, 0.5)
calc_pos <- function(N, par){
  dens_vec <- dbeta(seq(0, 1, by = 0.01), par, par)
  F_n <- pbeta(N, par, par)
  R <- qbeta(F_n / 3, par, par)
  F_r <- pbeta(R, par, par)
  out <- data.frame(dens = dens_vec, x = seq(0, 1, by = 0.01), N = N, R = R, beta_par = par)
  out$vote <- "N"
  out$vote[out$x < floor((N + R)/2 * 100) / 100] <- "R"
  return(out)
}

beta_tables <- lapply(as.list(beta_par), function(x) calc_pos(0.99, x))
beta_tables <- do.call(rbind, beta_tables)

ggplot(beta_tables, aes(x = x)) +
  geom_area(data = beta_tables[beta_tables$vote == "R", ], aes(x = x, y = dens, alpha = .3), fill = "red") +
  geom_area(data = beta_tables[beta_tables$vote == "N", ], aes(x = x, y = dens, alpha = .3), fill = "blue") +
  geom_vline(aes(xintercept = R)) +
  geom_vline(aes(xintercept = N)) +
  facet_wrap(. ~ beta_par)
