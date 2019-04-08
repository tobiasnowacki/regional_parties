library(ggplot2)

theme_sv <- function(){
  theme_bw(base_size=11) %+replace%
  theme(
    panel.grid.major =  element_line(
      colour = "grey50",
      size = 0.2,
      linetype = "dotted"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey97"),
    plot.margin = unit(c(0.2, 1, 0.2, 1), "cm"),
    legend.margin = margin(0, 0, 0, 0),
    legend.title = element_text(size = 10, face = "bold"),
    strip.background = element_rect(fill= NULL, colour = "white", linetype = NULL),
    strip.text = element_text(colour = 'grey50', size = 9, vjust = 0.5)
  )
}


n <- 0.9 # national party's policy position

qunif(n)
F_n <- pbeta(n, 0.5, 0.5)
R <- qbeta(F_n / 3, 0.5, 0.5)
pbeta(R, 0.5, 0.5)
F_n - pbeta(R, 0.5, 0.5)


beta_par <- c(1, 0.9, 0.8, 0.7, 0.6, 0.5)
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

beta_tables <- lapply(as.list(beta_par), function(x) calc_pos(0.9, x))
beta_tables <- do.call(rbind, beta_tables)

ggplot(beta_tables, aes(x = x)) +
  geom_area(data = beta_tables[beta_tables$vote == "R", ], aes(x = x, y = dens), fill = "red", alpha = .3) +
  geom_area(data = beta_tables[beta_tables$vote == "N", ], aes(x = x, y = dens), fill = "blue", alpha = .3) +
  geom_vline(aes(xintercept = R)) +
  geom_vline(aes(xintercept = N)) +
  facet_wrap(. ~ beta_par) +
  labs(x = "Policy dimension", y = "Density") +
  theme_sv()
ggsave("output/polarisation.pdf", width = 6, height = 4)