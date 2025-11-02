# Solutions of the first R exam

## TASK 0.
```r
setwd("/home/kmark7/Asztal/PE-MIK-manjaro/ZH/");
getwd();
adatok <- read.csv("R_tasks_part1_datasource.csv", sep = ";", fileEncoding = "latin1")
print(head(adatok))
print(str(adatok))
```

## TASK 1.A.
```r
length(unique(adatok$csapat))
# és:
for (c in unique(adatok$csapat)) { db <- sum(adatok$csapat == c); cat("A", c, ". csapat", db, "db munkát végzett.\n") }
# vagy:
table(adatok$csapat)
```

## TASK 1.B.
```r
adatok$ido <- as.numeric(gsub(",", ".", adatok$ido))
lower_decile <- quantile(adatok$ido, 0.1)
upper_decile <- quantile(adatok$ido, 0.9)
cat("Also decilis (10%-os kvantilis):", lower_decile, "\n")
cat("Felso decilis (90%-os kvantilis):", upper_decile, "\n")
cat("Decilisek különbsége: ", upper_decile - lower_decile, "\n")
```

## TASK 1.C.
```r
breaks_seq <- seq(0, max(adatok$ido) + 0.5, by = 0.5)
hist(adatok$ido,
    breaks = breaks_seq,
    main = "Idők hisztogramja",
    xlab = "Idő (óra)",
    ylab = "Gyakoriság"
)
```

## TASK 1.D.
```r
# counts <- table(adatok$csapat)
# print(counts[2]) # output: 2.osztály => 42db munka
# p_hat <- counts["2"] / sum(counts[names(counts) != "2"]) - LOGIKAI HIBÁS!
n <- length(adatok$minoseg)
k <- sum(adatok$minoseg == 2)
p_hat <- k / n
cat("Másodosztályú munkák aránya:", round(p_hat, 4), "\n")
eredmeny <- prop.test(k, n, conf.level = 0.9) #prop.test() fgv arányokra való Khí-Négyzet próba!
print(eredmeny)
cat("90%-os konfidenciaintervallum: [", round(eredmeny$conf.int[1], 4), ";", round(eredmeny$conf.int[2], 4), "]\n")
# vagy másik megoldási mód (de ez pontatlanabb):
z <- qnorm(0.95) # és neg_z <- qnorm(0.05); z <- abs(neg_z) ugyanaz
konf_int_lower <- p_hat-z*(sqrt((p_hat*(1-p_hat))/n))
konf_int_upper <- p_hat+z*(sqrt((p_hat*(1-p_hat))/n))
cat("90%-os konfidenciaintervallum: [", round(konf_int_lower, 4), ";", round(konf_int_upper, 4), "]\n")
```

## TASK 1.E.
```r
elso_csapat_idok <- adatok$ido[adatok$csapat == 1]
szoras <- sd(elso_csapat_idok) # 1/(n-1)-el, tehát torzítatlanul
chi_upper <- qchisq(0.9, df = n1 - 1) # χ²_{0.9, n-1}
sigma_lower <- sqrt((n1 - 1) * s1^2 / chi_upper)
cat("90%-os alsó konfidenciaintervallum határ: [", round(sigma_lower, 4), "\n")
```

## TASK 2.A.
```r
x <- adatok$ido[adatok$ido >= 1]
n_samples <- length(x)
loglik <- function(n, x) {
  if (n <= 0) return(-Inf)  # kizárjuk a negatív n értékeket
  n_samples * log(n) - (n + 1) * sum(log(x))
}
n_vals <- seq(0.1, 10, by = 0.01)
logL_vals <- sapply(n_vals, loglik, x=x)
n_hat <- n_vals[which.max(logL_vals)]
plot(n_vals, logL_vals, type = "l", lwd = 2, col = "blue",
     main = "Log-likelihood függvény",
     xlab = "n paraméter",
     ylab = "log-likelihood érték")
abline(v = n_hat, col = "red", lty = 2, lwd = 2)
text(n_hat, max(logL_vals), paste("MLE n =", round(n_hat, 3)),
     pos = 4, col = "red")
```

## TASK 2.B.
```r
n_hat <- -n_samples / sum(log(x))
cat("Maximum likelihood becslés (n̂):", round(n_hat, 4), "\n")
```

## TASK 3.A.
(analytical solution)
```r
lambda <- 1/1.5
x99 <- qexp(0.99, rate = lambda)
cat("Ennél az értéknél kisebb 0.99-es val.séggel: ", x99, "\n")
```

## TASK 3.B.
(numerical solution)
```r
set.seed(123)
lambda <- 1/1.5
n <- 1e6
x <- rexp(n, rate = lambda)
x99_sim <- quantile(x, 0.99)
cat("Szimulált 0.99 kvantilis (becsült):", round(x99_sim, 4), "\n")
```
