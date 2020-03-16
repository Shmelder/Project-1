

set.seed(103)
my_data <- MASS::mvrnorm(n = 100, mu = rep(0, 2), Sigma = diag(2))
cols <- wesanderson::wes_palettes$Darjeeling1
cols[c(2, 4)] <- cols[c(4, 2)]

plot(my_data[, 1], my_data[, 2], axes = FALSE, asp = 1, 
     bty = 'n', panel.first = grid(), pch = 16)
axis(1, (-4):4, pos = 0, cex.axis = 0.8)
axis(2, (-4):4, pos = 0, cex.axis = 0.8, las = 2)

l_2_cutoff <- quantile(apply(my_data, 1, function(x) sqrt(sum(x ** 2))), 0.95)
l_h_cutoff <- quantile(apply(my_data, 1, function(x) sum(sqrt(abs(x))) ** 2), 0.95)
max_cutoff <- quantile(apply(my_data, 1, function(x) max(abs(x))), 0.95)
max_line <- cbind(c(-max_cutoff, max_cutoff, max_cutoff,-max_cutoff, -max_cutoff),
                 c(-max_cutoff, -max_cutoff, max_cutoff, max_cutoff, -max_cutoff))
half_line_x <- seq(from = -l_h_cutoff, to = l_h_cutoff, length.out = 1000)
half_line_y <- (l_h_cutoff ** (1/2) - half_line_x ** (1/2)) ** 2
half_line_df <- cbind(c(half_line_x, rev(half_line_x), -half_line_x, -rev(half_line_x)), 
                      c(half_line_y, -rev(half_line_y), -half_line_y, rev(half_line_y)))
thetas <- seq(from = 0, to = 2 * pi, length.out = 1000)
l_2_line <- l_2_cutoff * cbind(sin(thetas), cos(thetas))
lines(max_line[, 1], max_line[, 2], col = cols[1], lwd = 4)
lines(l_2_line[, 1], l_2_line[, 2], col = cols[5], lwd = 4)
lines(half_line_df[, 1], half_line_df[, 2], col = cols[2], lwd = 4)
points(my_data[, 1], my_data[, 2], pch = 16)


make_plot <- function(seed){
  set.seed(seed)
  my_data <- MASS::mvrnorm(n = 100, mu = rep(0, 2), Sigma = diag(2))
  
  plot(my_data[, 1], my_data[, 2], axes = FALSE, asp = 1, 
       bty = 'n', panel.first = grid(), pch = 16)
  
  l_2_cutoff <- quantile(apply(my_data, 1, function(x) sqrt(sum(x ** 2))), 0.95)
  l_h_cutoff <- quantile(apply(my_data, 1, function(x) sum(sqrt(abs(x))) ** 2), 0.95)
  max_cutoff <- quantile(apply(my_data, 1, function(x) max(abs(x))), 0.95)
  cut_seq <- seq(from = -max_cutoff, to = max_cutoff, length.out = 250)
  max_line <- cbind(c(-cut_seq, rep(max_cutoff, 250), rev(cut_seq), rep(-max_cutoff, 250)),
                    c(rep(-max_cutoff, 250), cut_seq, rep(max_cutoff, 250), rev(cut_seq)))
  thetas <- seq(from = 0, to = 2 * pi, length.out = 1000)
  l_2_line <- l_2_cutoff * cbind(sin(thetas + 5/4 * pi), cos(thetas + 5/4 * pi))
  theta_cut <- asin(max_cutoff/l_2_cutoff)
  theta_cut_2 <- acos(max_cutoff/l_2_cutoff)
  seq_1 <- seq(from = theta_cut, to = pi - theta_cut, length.out = 1000)
  seq_2 <- seq(from = theta_cut_2, to = -theta_cut_2, length.out = 1000)
  seq_3 <- seq(from = theta_cut, to = theta_cut_2, length.out = 1000)
  max_rej <- l_2_cutoff * cbind(sin(c(seq_1, seq_1[1])), cos(c(seq_1, seq_1[1])))
  max_rej_2 <- l_2_cutoff * cbind(sin(c(seq_2, seq_2[1])), cos(c(seq_2, seq_2[1])))
  l_2_rej_2 <- cbind(c(l_2_cutoff * sin(seq_3), max_cutoff, max_cutoff), 
                     c(l_2_cutoff * cos(seq_3), max_cutoff, l_2_cutoff * cos(seq_3[1])))
  
  polygon(max_rej[, 1], max_rej[, 2], col = cols[4])
  polygon(- 1 * max_rej[, 1], max_rej[, 2], col = cols[4])
  polygon(max_rej_2[, 1], max_rej_2[, 2], col = cols[4])
  polygon(max_rej_2[, 1], -1 * max_rej_2[, 2], col = cols[4])
  polygon(l_2_rej_2[, 1], l_2_rej_2[, 2], col = cols[2])
  polygon(l_2_rej_2[, 1], -l_2_rej_2[, 2], col = cols[2])
  polygon(-l_2_rej_2[, 1], -l_2_rej_2[, 2], col = cols[2])
  polygon(-l_2_rej_2[, 1], l_2_rej_2[, 2], col = cols[2])
  lines(max_line[, 1], max_line[, 2], col = cols[1], lwd = 4)
  lines(l_2_line[, 1], l_2_line[, 2], col = cols[5], lwd = 4)
  
  points(my_data[, 1], my_data[, 2], pch = 16, cex = 1.5)
  axis(1, (-4):4, pos = 0, cex.axis = 0.8)
  axis(2, (-4):4, pos = 0, cex.axis = 0.8, las = 2)
}

make_plot(37)

where_are_you <- function(points, max_cut, l2_cut){
  max_norm <- apply(points, 1, function(x) max(abs(x)))
  l_2_norm <- apply(points, 1, function(x) sqrt(sum(x ** 2)))
  max_ind  <- as.integer(max_norm > max_cut)
  l2_ind   <- as.integer(l_2_norm > l2_cut)
  cmb_ind <- 2 * max_ind + l2_ind + max_ind * l2_ind
  col_ind <- rep(NA, length(cmb_ind))
  col_ind[cmb_ind == 0] <- "black"
  col_ind[cmb_ind == 1] <- cols[2]
  col_ind[cmb_ind == 2] <- cols[4]
  col_ind[cmb_ind == 4] <- cols[3]
  return(col_ind)
}

make_three_plot <- function(seed){
  set.seed(seed)
  par(mar = c(5, 2.5, 0, 0.5))
  my_data <- MASS::mvrnorm(n = 100, mu = rep(0, 2), Sigma = diag(2))
  par(mfrow = c(1, 3))
  p_xlim <- range(my_data[, 1]) * 1.2
  p_ylim <- range(my_data[, 2]) * 1.2
  plot(my_data[, 1], my_data[, 2], axes = FALSE, asp = 1, 
       bty = 'n', panel.first = grid(), xlim = p_xlim, ylim = p_ylim,
       pch = 16, ylab = "", xlab = expression(psi[1]),
       cex.lab = 2)
  mtext("(A)", side = 3, line = -1, adj = 0.1, cex = 1.6, padj = 1)
  mtext(expression(psi[2]), side = 2, las = 2, cex = 1.5, padj = 0.2)
  
  l_2_cutoff <- quantile(apply(my_data, 1, function(x) sqrt(sum(x ** 2))), 0.95)
  l_h_cutoff <- quantile(apply(my_data, 1, function(x) sum(sqrt(abs(x))) ** 2), 0.95)
  max_cutoff <- quantile(apply(my_data, 1, function(x) max(abs(x))), 0.95)
  cut_seq <- seq(from = -max_cutoff, to = max_cutoff, length.out = 250)
  max_line <- cbind(c(-cut_seq, rep(max_cutoff, 250), rev(cut_seq), rep(-max_cutoff, 250)),
                    c(rep(-max_cutoff, 250), cut_seq, rep(max_cutoff, 250), rev(cut_seq)))
  thetas <- seq(from = 0, to = 2 * pi, length.out = 1000)
  thetas <- seq(from = 0, to = 2 * pi, length.out = 1000)
  l_2_line <- l_2_cutoff * cbind(sin(thetas + 5/4 * pi), cos(thetas + 5/4 * pi))
  theta_cut <- asin(max_cutoff/l_2_cutoff)
  theta_cut_2 <- acos(max_cutoff/l_2_cutoff)
  seq_1 <- seq(from = theta_cut, to = pi - theta_cut, length.out = 1000)
  seq_2 <- seq(from = theta_cut_2, to = -theta_cut_2, length.out = 1000)
  seq_3 <- seq(from = theta_cut, to = theta_cut_2, length.out = 1000)
  max_rej <- l_2_cutoff * cbind(sin(c(seq_1, seq_1[1])), cos(c(seq_1, seq_1[1])))
  max_rej_2 <- l_2_cutoff * cbind(sin(c(seq_2, seq_2[1])), cos(c(seq_2, seq_2[1])))
  l_2_rej_2 <- cbind(c(l_2_cutoff * sin(seq_3), max_cutoff, max_cutoff), 
                     c(l_2_cutoff * cos(seq_3), max_cutoff, l_2_cutoff * cos(seq_3[1])))
  
  polygon(max_rej[, 1], max_rej[, 2], col = cols[4])
  polygon(- 1 * max_rej[, 1], max_rej[, 2], col = cols[4])
  polygon(max_rej_2[, 1], max_rej_2[, 2], col = cols[4])
  polygon(max_rej_2[, 1], -1 * max_rej_2[, 2], col = cols[4])
  polygon(l_2_rej_2[, 1], l_2_rej_2[, 2], col = cols[2])
  polygon(l_2_rej_2[, 1], -l_2_rej_2[, 2], col = cols[2])
  polygon(-l_2_rej_2[, 1], -l_2_rej_2[, 2], col = cols[2])
  polygon(-l_2_rej_2[, 1], l_2_rej_2[, 2], col = cols[2])
  
  lines(max_line[, 1], max_line[, 2], col = cols[1], lwd = 4)
  lines(l_2_line[, 1], l_2_line[, 2], col = cols[5], lwd = 4)
  
  points(my_data[, 1], my_data[, 2], pch = 16, cex = 1.5)
  axis(1, (-4):4, pos = 0, cex.axis = 0.8)
  axis(2, (-4):4, pos = 0, cex.axis = 0.8, las = 2)
  plot(NULL, xlim = p_xlim, ylim = p_ylim, axes = FALSE, asp = 1,
       bty = 'n', panel.first = grid(), ylab = "", xlab = expression(psi[1]),
       cex.lab = 2)
  mtext("(B)", side = 3, line = -1, adj = 0.1, cex = 1.6, padj = 1)
  lines(max_line[, 1], max_line[, 2], col = cols[1], lwd = 4)
  lines(l_2_line[, 1], l_2_line[, 2], col = cols[5], lwd = 4)
  my_data_2 <- MASS::mvrnorm(n = 100, mu = c(max_cutoff * 1.2, 0), Sigma = diag(2))

  points(my_data_2[, 1], my_data_2[, 2], pch = 16,
         col = where_are_you(my_data_2, max_cut = max_cutoff,
                             l2_cut =  l_2_cutoff), cex = 1.5)
  axis(1, (-4):4, pos = 0, cex.axis = 0.8)
  axis(2, (-4):4, pos = 0, cex.axis = 0.8, las = 2)
  plot(NULL, xlim = p_xlim, ylim = p_ylim, axes = FALSE, asp = 1,
       bty = 'n', panel.first = grid(), ylab = "", xlab = expression(psi[1]),
       cex.lab = 2)
  mtext("(C)", side = 3, line = -1, adj = 0.1, cex = 1.6, padj = 1)
  lines(max_line[, 1], max_line[, 2], col = cols[1], lwd = 4)
  lines(l_2_line[, 1], l_2_line[, 2], col = cols[5], lwd = 4)
  my_data_3 <- MASS::mvrnorm(n = 100, mu = 1.2 * rep(l_2_cutoff/sqrt(2), 2), Sigma = diag(2))
  
  points(my_data_3[, 1], my_data_3[, 2], pch = 16,
         col = where_are_you(my_data_3, max_cut = max_cutoff,
                             l2_cut =  l_2_cutoff), cex = 1.5)
  axis(1, (-4):4, pos = 0, cex.axis = 0.8, cex.lab = 2)
  axis(2, (-4):4, pos = 0, cex.axis = 0.8, las = 2)
}

cols <- pal_freiburg_info <- c("#2a6ebb", "#a7c1e3", "#739600",
                               "#f52c2c", "#a10500",  "#92d400")

make_three_plot(201)
