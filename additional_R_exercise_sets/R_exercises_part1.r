# ----------------------------------------------------------------------
# 1. (a) Rajzolja fel a gamma eloszlású v.v. sűrűségfüggvényét, 
#   ha 5 darab független 2 várható értékű exponenciális eloszlású v.v. összegeként kaptuk! 4p
# ----------------------------------------------------------------------
# A gamma eloszlás paraméterei:
shape_param <- 5     # Alakparaméter (5 db exponenciális v.v. összege)
rate_param <- 0.5    # Skálaparaméter (lambda, mert 1/lambda = 2)

# Értékkészlet (x-tengely) meghatározása, amin ábrázolni fogunk
x_ertekek <- seq(0, 30, length.out = 500)

# Sűrűségfüggvény (PDF) értékeinek kiszámítása
y_suruseg <- dgamma(x_ertekek, shape = shape_param, rate = rate_param)

# Ábra kirajzolása
plot(x_ertekek, y_suruseg, type = "l", 
     main = "Gamma eloszlás sűrűségfüggvénye (shape=5, rate=0.5)",
     xlab = "x",
     ylab = "f(x) - Sűrűségfüggvény",
     col = "blue", 
     lwd = 2)
# Hozzáadhatunk egy függőleges vonalat a várható értéknél (E(Y) = shape/rate = 5/0.5 = 10)
abline(v = 10, col = "red", lty = 2)
text(10, max(y_suruseg) * 0.9, "Várható érték (10)", col = "red", pos = 4)

# ----------------------------------------------------------------------
# 1. (b) Minek nagyobb a valószínűsége, 
#   hogy a 10 vagy 15 kis környezetében veszi fel az értékeit? 3p
# ----------------------------------------------------------------------
# Sűrűségfüggvény értéke 10-nél
suruseg_10 <- dgamma(10, shape = shape_param, rate = rate_param)
# Sűrűségfüggvény értéke 15-nél
suruseg_15 <- dgamma(15, shape = shape_param, rate = rate_param)

# Eredmény kiírása
cat(paste("Sűrűségfüggvény értéke 10-nél:", round(suruseg_10, 5), "\n"))
cat(paste("Sűrűségfüggvény értéke 15-nél:", round(suruseg_15, 5), "\n"))

if (suruseg_10 > suruseg_15) {
  valasz_b <- "A 10 kis környezetében nagyobb a valószínűsége, mert itt nagyobb a sűrűségfüggvény értéke."
} else if (suruseg_15 > suruseg_10) {
  valasz_b <- "A 15 kis környezetében nagyobb a valószínűsége, mert itt nagyobb a sűrűségfüggvény értéke."
} else {
  valasz_b <- "A valószínűségek közel azonosak, mert a sűrűségfüggvény értéke a két pontban közel azonos."
}

print(valasz_b)

# ----------------------------------------------------------------------
# 1. (c) Legfeljebb mennyi az értéke 0.9 valószínűséggel? 3p
# ----------------------------------------------------------------------
# A 0.9 valószínűséghez tartozó kvantilis kiszámítása
kvantilis_0_9 <- qgamma(0.9, shape = shape_param, rate = rate_param)

# Eredmény kiírása
cat(paste("Legfeljebb mennyi az értéke 0.9 valószínűséggel (90% kvantilis):", 
          round(kvantilis_0_9, 4), "\n"))

# ----------------------------------------------------
# 2. Olvassa be az "adatfile_zh_nappali.csv" nevű file-t az R-ből.
# ----------------------------------------------------
# Fájl beolvasása
setwd("/home/kmark7/Asztal/PE-MIK-manjaro/ZH/Jegyzet/");
getwd();
adatok <- read.csv("adatfile_zh_nappali.csv", sep = ";", fileEncoding = "latin1")

# A beolvasott adatok szerkezetének ellenőrzése (opcionális, de ajánlott)
print(head(adatok))
print(str(adatok))

# ----------------------------------------------------------------------
# 2. (a) Rajzolja fel a hiba nevű oszlop adatainak 
#   hisztogramját 5 részre osztva az intervallumot! 5p
# ----------------------------------------------------------------------

# Hisztogram rajzolása a 'hiba' oszlopra, 5 szakaszra (breaks = 5) osztva
hist(adatok$hiba, 
     breaks = 5, 
     main = "A 'hiba' oszlop adatinak hisztogramja (5 szakasz)",
     xlab = "Hiba értéke",
     ylab = "Gyakoriság",
     col = "lightblue",
     border = "blue")

# ----------------------------------------------------------------------
# 2. (b) Becsülje meg a hiba nevű oszlopban és a h2 oszlopban levő mennyiségek 
#   korrelációs együtthatóját! 5p
# ----------------------------------------------------------------------

# Korrelációs együttható kiszámítása a cor() függvénnyel a 'hiba' és 'h2' oszlopok között
korrelacio <- cor(adatok$hiba, adatok$h2)

# Eredmény kiírása
cat(paste("A 'hiba' és 'h2' oszlopok korrelációs együtthatója:", korrelacio, "\n"))

# ----------------------------------------------------------------------
# 2. (e) Mennyi a hiba nevű file-ban levő adatok alsó és felső decilise? 5p
# Nevezetes kvantilisek: kvartilis (25%), decilis (10%), percentilis (1%)
# ----------------------------------------------------------------------

# Decilisek kiszámítása a quantile() függvénnyel.
# Alsó decilis: 10% kvantilis (p=0.1)
# Felső decilis: 90% kvantilis (p=0.9)
decilisek <- quantile(adatok$hiba, probs = c(0.1, 0.9))

# Eredmény kiírása
cat("A 'hiba' oszlop alsó (10%) és felső (90%) decilisei:\n")
print(decilisek)


