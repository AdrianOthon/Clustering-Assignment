#Libraries needed for Task1 and 2 
library(cluster)
library(ggplot2)
library(plotly)
library(stats)

## Set seed for Task1 and 2 (611 for the class number)
set.seed(611)

## Clustering 
clusters <- function(n, k, side_length, noise_sd = 1) {
  centers <- diag(rep(side_length, n))              # n x n
  do.call(rbind, lapply(1:n, function(i) {
    noise  <- matrix(rnorm(k * n, sd = noise_sd), nrow = k, ncol = n)
    center <- matrix(rep(centers[i, ], each = k),   nrow = k, ncol = n, byrow = FALSE)
    # sanity check
    if (!all(dim(noise) == dim(center))) stop("shape mismatch: ",
                                              paste(dim(noise), collapse="x"), " vs ",
                                              paste(dim(center), collapse="x"))
    noise + center
  }))
}

x <- clusters(3, 100, 6, 1)

dim(x)
head(x)

plot_ly(x = x[,1], y = x[,2], z = x[,3],
        type = "scatter3d", mode = "markers",marker = list(size = 3))



## Parameters for Task 1 
dims    <- c(6,5,4,3,2)   # dimensions
side_ls <- 10:1           # side length values
k_per   <- 100            # points per cluster
noise   <- 1.0            # cluster spread

## Simulation Loop 
out <- list(); idx <- 1
for (n in dims) {
  for (side_len in side_ls) {
    x <- clusters(n = n, k = k_per, side_length = side_len, noise_sd = noise)
    gap <- clusGap(
      x,
      FUN = function(z, k) kmeans(z, centers = k, nstart = 20, iter.max = 50),
      K.max = n, B = 50
    )
    k_hat <- cluster::maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"], method = "firstSEmax")
    out[[idx]] <- data.frame(n = n, side_length = side_len, est_k = k_hat)
    idx <- idx + 1
  }
}
results <- do.call(rbind, out)

## Plot 
p <- ggplot(results, aes(x = side_length, y = est_k, color = factor(n), group = n)) +
  geom_line() + geom_point() +
  geom_hline(aes(yintercept = n, color = factor(n)), linetype = "dashed", show.legend = FALSE) +
  scale_x_reverse(breaks = 10:1) +
  labs(x = "Side length (L)", y = "Estimated clusters", color = "Dimension n") +
  theme_minimal(base_size = 12)
p

##Failing 
fail <- do.call(rbind, lapply(split(results, results$n), function(df) {
  df <- df[order(-df$side_length), ]
  drop_idx <- which(df$est_k < unique(df$n))
  data.frame(n = unique(df$n),
             L_where_drop = ifelse(length(drop_idx), df$side_length[min(drop_idx)], NA))
}))
fail

## Answers to Task 1 When clusters are far apart (L ≥ 4–10), K-means finds all clusters correctly.As clusters move closer (L ≈ 3 for high-D data), the algorithm begins merging nearby clusters.In very tight or noisy settings (L ≤ 2), all dimensions collapse to fewer clusters.

# Task 2 

## Data Generation 
generate_shell_clusters <- function(n_shells, k_per_shell, max_radius, noise_sd = 0.1,
                                    inner_radius = 0.5) {
  stopifnot(max_radius > inner_radius)
  radii <- seq(inner_radius, max_radius, length.out = n_shells)
  pts <- lapply(radii, function(r) {
    theta <- runif(k_per_shell, 0, 2*pi)
    u <- runif(k_per_shell, -1, 1)  # cos(phi)
    phi <- acos(u)
    rr <- pmax(r + rnorm(k_per_shell, 0, noise_sd), inner_radius)
    x <- rr * sin(phi) * cos(theta)
    y <- rr * sin(phi) * sin(theta)
    z <- rr * cos(phi)
    cbind(x, y, z)
  })
  do.call(rbind, pts)
}

## 3D Check 
Xdemo <- generate_shell_clusters(n_shells = 4, k_per_shell = 100, max_radius = 6, noise_sd = 0.1)
plot_ly(x = Xdemo[,1], y = Xdemo[,2], z = Xdemo[,3],
        type = "scatter3d", mode = "markers", marker = list(size = 2))

## Wraper for clusGap 
spectral_clustering_wrapper <- function(x, k, d_threshold = 1) {
  Dmat <- as.matrix(dist(x))
  A <- (Dmat < d_threshold) * 1
  diag(A) <- 0
  deg <- rowSums(A)
  L <- diag(deg) - A
  invsqrt <- ifelse(deg > 0, 1/sqrt(deg), 0)
  Dm12 <- diag(invsqrt)
  Lsym <- Dm12 %*% L %*% Dm12
  eg <- eigen(Lsym, symmetric = TRUE)
  U <- eg$vectors[, (ncol(Lsym)-k+1):ncol(Lsym), drop = FALSE]
  # row-normalize
  rn <- sqrt(rowSums(U^2)); rn[rn==0] <- 1
  U <- U / rn
  km <- kmeans(U, centers = k, nstart = 20, iter.max = 50)
  list(cluster = km$cluster)
}

## Simulation across shrinking radii
n_shells <- 4
k_per    <- 100
noise    <- 0.1
d_thr    <- 1
Rvals    <- 10:1   # skip 0 to avoid degeneracy

out2 <- list(); j <- 1
for (Rmax in Rvals) {
  X <- generate_shell_clusters(n_shells, k_per, max_radius = Rmax, noise_sd = noise)
  gap <- clusGap(
    X,
    FUN = function(z, k) spectral_clustering_wrapper(z, k, d_threshold = d_thr),
    K.max = n_shells, B = 50
  )
  k_hat <- cluster::maxSE(gap$Tab[,"gap"], gap$Tab[,"SE.sim"], method = "firstSEmax")
  out2[[j]] <- data.frame(max_radius = Rmax, est_k = k_hat)
  j <- j + 1
}
res2 <- do.call(rbind, out2)
res2

## Plot 
p2 <- ggplot(res2, aes(x = max_radius, y = est_k)) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 4, linetype = "dashed") +
  scale_x_reverse(breaks = Rvals) +
  labs(x = "Max radius", y = "Estimated clusters (spectral)") +
  theme_minimal(base_size = 12)
p2

## Fail
drop_idx <- which(res2$est_k < n_shells)
fail_R <- if (length(drop_idx)) res2$max_radius[min(drop_idx)] else NA
fail_R

# Task 1 plot (K-means + Gap Statistic)
dir.create("figures", showWarnings = FALSE)
ggsave("figures/gap_est_k.png", p, width = 8, height = 5, dpi = 150)

# Task 2 plot (Spectral Clustering)
ggsave("figures/spectral_est_k.png", p2, width = 8, height = 5, dpi = 150)