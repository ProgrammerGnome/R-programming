# Megj.: sd(...) function => SZÓRÁS!!!
# ==============================
# 1. FELADAT:
# Az minta elemszáma n=1000 és 0.95 valószínűséggel adunk jó becslést. A minták bináris
# kísérleti eredményeket tartalmaznak. Adjunk konfidenciaintervallumot a becslésre!
# ==============================

# MINTAADAT
n <- 1000
confidence_level <- 0.95

# Véletlenszerű minta generálása
set.seed(123)
sample_data <- rnorm(n, mean = 50, sd = 10)

# Konfidenciaintervallum számítása
sample_mean <- mean(sample_data)
sample_sd <- sd(sample_data)
standard_error <- sample_sd / sqrt(n)
z_value <- qnorm((1 + confidence_level) / 2)
margin_of_error <- z_value * standard_error

ci_lower <- sample_mean - margin_of_error
ci_upper <- sample_mean + margin_of_error

cat("1. FELADAT: Konfidenciaintervallum\n")
cat("Alsó határ:", ci_lower, "\n")
cat("Felső határ:", ci_upper, "\n")
cat("Szélesség:", ci_upper - ci_lower, "\n\n")

# ==============================
# 2. FELADAT:
# Normális-eloszlású a minta: {100,96,103,99,100}.
# (a) Legalább mekkora a várható értéke 0.95 valószínűséggel?
# (b) Legalább mennyi a konfidenciaintervallum alsó határának becslése?
# (c) Szerkesszük a feladatra kétoldali konfidenciaintervallumot!
# (d) Hagyjuk el az előbb megszerkesztett kétoldali konfidenciaintervallum felső végét!
# (e) Ha az egyoldali intervallum szintje 0.95, akkor mi a kétoldali szintje?
# ==============================

sample_2 <- c(100, 96, 103, 99, 100)
n_2 <- length(sample_2)
confidence_level_2 <- 0.95

# (a) Legalább mekkora a várható értéke 0.95 valószínűséggel?
sample_mean_2 <- mean(sample_2)
sample_sd_2 <- sd(sample_2)
t_value_lower <- qt(confidence_level_2, df = n_2 - 1)
lower_bound_a <- sample_mean_2 - t_value_lower * (sample_sd_2 / sqrt(n_2))

# (b) Legalább mennyi a konfidenciaintervallum alsó határának becslése?
# Ez lényegében ugyanaz, mint (a)
lower_bound_b <- lower_bound_a

# (c) Kétoldali konfidenciaintervallum
t_value_two_sided <- qt((1 + confidence_level_2) / 2, df = n_2 - 1)
margin_error_two <- t_value_two_sided * (sample_sd_2 / sqrt(n_2))
ci_lower_two <- sample_mean_2 - margin_error_two
ci_upper_two <- sample_mean_2 + margin_error_two

# (d) Felső vég elhagyása (egyoldali intervallum)
# Ez megegyezik az (a) rész eredményével

# (e) Egyoldali és kétoldali szintek kapcsolata
# Ha egyoldali szint 0.95, akkor kétoldali szint 0.90, a képlet: 1-2*(1-confidence_level_2)

cat("2. FELADAT:\n")
cat("(a) Legalább mekkora a várható érték:", lower_bound_a, "\n")
cat("(b) Alsó határ becslése:", lower_bound_b, "\n")
cat("(c) Kétoldali konfidenciaintervallum: [", ci_lower_two, ",", ci_upper_two, "]\n")
cat("(d) Egyoldali intervallum (alsó): >", lower_bound_a, "\n")
cat("(e) Ha egyoldali szint 0.95, akkor kétoldali szint: 0.90\n\n")

# ==============================
# 3. FELADAT:
# (a) Adjon 0.9 szintű konfidenciaintervallumot a szórásnégyzetre!
# (b) Mennyi a várható érték 0.9 valószínűséggel?
# ==============================

# MINTAADAT
set.seed(456)
sample_3 <- rnorm(30, mean = 25, sd = 5)
n_3 <- length(sample_3)
confidence_level_3 <- 0.90

# (a) Konfidenciaintervallum a szórásnégyzetre (variancia)
sample_var_3 <- var(sample_3)
df_3 <- n_3 - 1

border_lower <- (1 - confidence_level_3) / 2
border_upper <- (1 + confidence_level_3) / 2
chi_sq_lower <- qchisq(border_lower, df_3)
chi_sq_upper <- qchisq(border_upper, df_3)

ci_var_lower <- (df_3 * sample_var_3) / chi_sq_upper
ci_var_upper <- (df_3 * sample_var_3) / chi_sq_lower

# (b) Várható érték konfidenciaintervalluma
sample_mean_3 <- mean(sample_3)
sample_sd_3 <- sd(sample_3)
border_t <- (1 + confidence_level_3) / 2
t_value_3 <- qt(border_t, df = n_3 - 1)
margin_error_3 <- t_value_3 * (sample_sd_3 / sqrt(n_3))

ci_mean_lower <- sample_mean_3 - margin_error_3
ci_mean_upper <- sample_mean_3 + margin_error_3

cat("3. FELADAT:\n")
cat("(a) Variancia konfidenciaintervallum: [", ci_var_lower, ",", ci_var_upper, "]\n")
cat("(b) Várható érték konfidenciaintervallum: [", ci_mean_lower, ",", ci_mean_upper, "]\n\n")

# ==============================
# 4. FELADAT (Megj.: a 3. feladat adatait használtam.):
# Adjon 0.9 szintű konfidenciaintervallumot a szórásra!
# ==============================

# Konfidenciaintervallum a szórásra
ci_sd_lower <- sqrt(ci_var_lower)
ci_sd_upper <- sqrt(ci_var_upper)

cat("4. FELADAT:\n")
cat("Szórás konfidenciaintervallum: [", ci_sd_lower, ",", ci_sd_upper, "]\n\n")

# ==============================
# 5. FELADAT:
# Adott egy ötös szabadsági fokú Khí négyzet eloszlású valószínűségi változó.
# (a) Mi a valószínűsége, hogy értéke 10-nél kisebb?
# (b) Legalább mennyi a várható értéke 0.9 valószínűséggel?
# ==============================

df_5 <- 5

# (a) Valószínűség, hogy értéke 10-nél kisebb
prob_less_than_10 <- pchisq(10, df_5) # F(10)-eloszlásfüggvény

# (b) Legalább mennyi a várható érték 0.9 valószínűséggel
# Khí-négyzet eloszlás várható értéke = szabadsági fok
expected_value <- df_5
# 0.9 kvantilis
quantile_90 <- qchisq(0.9, df_5) # Hasonló mint a qnorm(...)

cat("5. FELADAT:\n")
cat("(a) P(X < 10) =", prob_less_than_10, "\n")
cat("(b) Várható érték:", expected_value, ", 0.9 kvantilis:", quantile_90, "\n\n")

# ==============================
# 6. FELADAT:
# Konfidenciaintervallumot szeretnénk adni egy normális eloszlású minta várható értékére
# ismert szórás esetén.
# (a) Milyen eloszláshoz tudja kapcsolni a konfidenciaintervallumot?
# (b) Hogyan változik a konfidenciaintervallum szélessége, ha a mintaelemszámot 10-szeresére
# növeljük?
# (c) Hogyan változik a konfidenciaintervallum megbízhatósága, ha a megszerkesztett 0.99
# megbízhatóságú konfidenciaintervallumnak elhagyjuk az alsó végét?
# ==============================

# MINTAADAT
set.seed(789)
sample_6 <- rnorm(50, mean = 100, sd = 15)
n_6 <- length(sample_6)
known_sd <- 15
confidence_level_6 <- 0.99

# (a) Milyen eloszláshoz kapcsolódik?
# Ismert szórás esetén standard normális eloszláshoz

# (b) Mintaelemszám 10-szeresére növelése
sample_mean_6 <- mean(sample_6)
z_value_6 <- qnorm((1 + confidence_level_6) / 2)

# Eredeti intervallum
margin_error_original <- z_value_6 * (known_sd / sqrt(n_6))
width_original <- 2 * margin_error_original

# 10-szeres mintaelemszám
n_large <- 10 * n_6
margin_error_large <- z_value_6 * (known_sd / sqrt(n_large))
width_large <- 2 * margin_error_large

width_ratio <- width_large / width_original

# (c) Megbízhatóság változása
# Ha elhagyjuk az alsó végét, egyoldali intervallumot kapunk
# 0.99 kétoldali -> 0.995 egyoldali, mert: 0.99+((1-0.99)/2)=eredeti+egyik_farok

cat("6. FELADAT:\n")
cat("(a) Standard normális eloszláshoz kapcsolódik\n")
cat("(b) Eredeti szélesség:", width_original, "\n")
cat("    Új szélesség:", width_large, "\n")
cat("    Szélesség aránya:", width_ratio, "(elméletileg 1/sqrt(10) =", 1/sqrt(10), ")\n")
cat("(c) Új megbízhatóság: 0.995\n\n")

# ==============================
# 7. FELADAT:
# Egy helyen a tárgyteljesítési adatok alapján az alábbi mondható: annak a valószínűsége,
# hogy egy hallgató előszörre teljesíti a tárgyat p, hogy másodszorra p/2, hogy harmadszorra
# vagy többedszerre az a maradék rész. Ha egy 100 elemű minta a beolvasott adatfájl 3.
# oszlopában található, akkor:
# (a) Rajzoltassa ki a log-likelihood függvényt a p paraméter függvényében!
# (b) Adja meg a p paraméter maximum likelihood becslését!
# ==============================

# MINTAADAT
set.seed(321)
n_7 <- 100
# Véletlenszerű adatok generálása a feladat logikája szerint
p_true <- 0.6
attempts <- numeric(n_7)
for(i in 1:n_7) {
  rand <- runif(1)
  if(rand < p_true) {
    attempts[i] <- 1
  } else if(rand < p_true + p_true/2) {
    attempts[i] <- 2
  } else {
    attempts[i] <- sample(3:10, 1)  # 3 vagy több próbálkozás
  }
}

# (a) Log-likelihood függvény ábrázolása
log_likelihood <- function(p, data) {
  n <- length(data)
  count_first <- sum(data == 1)
  count_second <- sum(data == 2)
  count_third_or_more <- n - count_first - count_second
  
  # Likelihood függvény logaritmusa
  ll <- count_first * log(p) + 
    count_second * log(p/2) + 
    count_third_or_more * log(1 - p - p/2)
  
  return(ll)
}

# Likelihood ábrázolása
p_values <- seq(0.01, 0.8, length.out = 100)
ll_values <- sapply(p_values, function(p) log_likelihood(p, attempts))

# (b) Maximum likelihood becslés
# Numerikus optimalizálás
optim_result <- optimize(function(p) log_likelihood(p, attempts), 
                         interval = c(0.01, 0.8), maximum = TRUE)
p_mle <- optim_result$maximum

cat("7. FELADAT:\n")
cat("Minta statisztikái:\n")
cat("Elsőre sikeres:", sum(attempts == 1), "\n")
cat("Másodjára sikeres:", sum(attempts == 2), "\n")
cat("Harmadikra vagy többszörre sikeres:", sum(attempts >= 3), "\n")
cat("(b) Maximum likelihood becslés p-re:", p_mle, "\n")

# Ábrázolás
plot(p_values, ll_values, type = "l", lwd = 2, col = "blue",
     xlab = "p", ylab = "Log-likelihood",
     main = "Log-likelihood függvény")
abline(v = p_mle, col = "red", lty = 2, lwd = 2)
legend("bottomleft", legend = c("Log-likelihood", paste("MLE =", round(p_mle, 3))),
       col = c("blue", "red"), lty = c(1, 2), lwd = 2)
