
# x <- brps_lh
# refpts <- c("msy", "mey", "f0.1", "spr.30", "fmax", "crash")
# obs <- FALSE

plot_brps2 <- function(x, refpts = c("msy", "mey", "f0.1", "spr.30", "fmax", 
                                    "crash"), obs = FALSE, ...) {
  
  df <- model.frame(metrics(x, list(ssb = ssb, harvest = fbar, 
                                    rec = rec, yield = landings, profit = profit)), drop = FALSE)
  drps <- dimnames(refpts(x))$refpt
  rps <- refpts(x)[drps %in% refpts, ]
  rpf <- !all(is.na(rps))
  if (rpf && "crash" %in% dimnames(rps)$refpt) 
    df <- df[df$harvest <= c(rps["crash", "harvest"]), ]
  panels <- list(P1 = c(x = "harvest", y = "ssb", panel = "Equilibrium SSB v. F"), 
                 P2 = c(x = "ssb", y = "rec", panel = "Equilibrium Recruitment v. SSB"), 
                 P3 = c(x = "harvest", y = "yield", panel = "Equilibrium Yield v. F"), 
                 P4 = c(x = "ssb", y = "yield", panel = "Equilibrium Yield v. SSB"))
  if (!all(is.na(rps[, "profit"]))) {
    panels <- c(panels, list(P5 = c(x = "harvest", y = "profit", 
                                    panel = "Equilibrium Profit v. F"), P6 = c(x = "ssb", 
                                                                               y = "profit", panel = "Equilibrium Profit v. SSB")))
  } else {
    dms <- dimnames(rps)
    rps <- rps[!dms$refpt %in% "mey", !dms$quant %in% c("revenue", 
                                                        "cost", "profit")]
  }
  dat <- lapply(panels, function(p) {
    data.frame(x = df[, p["x"]], y = df[, p["y"]], iter = df[, 
                                                             "iter"], panel = p["panel"], row.names = NULL)
  })
  dat <- do.call(rbind, c(dat, list(make.row.names = FALSE)))
  p <- ggplot(dat, aes_(x = ~x, y = ~y, group = ~iter)) + 
    geom_line() + facet_wrap(~panel, scales = "free", ncol = 2) + 
    xlab("") + ylab("") + scale_x_continuous(labels = human_numbers, 
                                             limits = c(0, NA))
  
  return(p)
  
}