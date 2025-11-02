# ----------------------------------------------------------------------
# 1. (a) Rajzolja fel egy 5 szabadsági fokú khi-négyzet (chi-squared) eloszlású 
#   v.v. sűrűségfüggvényét! 4p
# ----------------------------------------------------------------------
# Szabadsági fok (degrees of freedom)
df <- 5

# Értékkészlet (x-tengely) meghatározása, amin ábrázolni fogunk
# A chi-squared eloszlás csak nemnegatív értékeket vesz fel
x_ertekek <- seq(0, 20, length.out = 500)

# Sűrűségfüggvény (PDF) értékeinek kiszámítása a dchisq() függvénnyel
y_suruseg <- dchisq(x_ertekek, df = df)

# Ábra kirajzolása
plot(x_ertekek, y_suruseg, type = "l", 
     main = expression(paste("Chi-Squared Eloszlás Sűrűségfüggvénye (", chi^2, ", df=5)")),
     xlab = expression(chi^2),
     ylab = "f(x) - Sűrűségfüggvény",
     col = "darkred", 
     lwd = 2)

# Jelöljük a várható értéket, ami chi-négyzet eloszlásnál df (5)
abline(v = df, col = "blue", lty = 2)
text(df, max(y_suruseg) * 0.9, "Várható érték (5)", col = "blue", pos = 4)

# ----------------------------------------------------------------------
# 1. (b) Mennyi a valószínűsége, hogy egy 5 szabadsági fokú khi-négyzet 
#   eloszlású v.v. 10-nél kisebb értéket vesz fel? 3p
# ----------------------------------------------------------------------
df <- 5 # Szabadsági fok

# Valószínűség (P(X < 10)) kiszámítása az eloszlásfüggvénnyel (pchisq)
valoszinuseg_kisebb_10 <- pchisq(10, df = df)

# Eredmény kiírása
cat(paste("P(X < 10) valószínűség (5 szabadsági fokkal):", round(valoszinuseg_kisebb_10, 4), "\n"))

# ----------------------------------------------------------------------
# 1. (c) Legalább mennyi egy 5 szabadsági fokú 
#   chi-négyzet v.v értéke 0.9 valószínűséggel? 3p
# ----------------------------------------------------------------------
df <- 5 # Szabadsági fok

# A P(X >= x) = 0.9 feltételt átalakítjuk P(X <= x) = 0.1-re
alsó_valoszinuseg <- 1 - 0.9  # 0.1

# Az alsó 10%-os kvantilis (x_0.1) kiszámítása a qchisq() függvénnyel
kvantilis_0_1 <- qchisq(alsó_valoszinuseg, df = df)

# Eredmény kiírása
cat(paste("A 10%-os kvantilis (Legalább 0.9 valószínűséggel vett érték):", round(kvantilis_0_1, 4), "\n"))

# ----------------------------------------------------------------------
# ----------------------------------------------------------------------
# 2. FELADAT
# ----------------------------------------------------------------------
# A szükséges könyvtárak betöltése
# A Levene-teszthez (szórásazonosság) és a Wilcoxon/Kolmogorov-Smirnov teszthez hasznos lehet:
# install.packages("car") 
# library(car) 

# Fájl beolvasása az R-be
setwd("/home/kmark7/Asztal/PE-MIK-manjaro/ZH/Jegyzet/");
getwd();
# A megadott minta (vernyomas_s;vernyomas_d;nem;vercukor) alapján a szeparátor a pontosvessző (semicolon).
# adatok <- read.csv("adat_levelezo_zh2.csv", sep = ";")
# Ha a fenti nem működik:
adatok <- read.csv("adat_levelezo_zh2.csv", sep = ";", fileEncoding = "latin1") 

# A változók átnevezése a feladat szövegének megfelelően, a beolvasott oszlopnevek alapján
names(adatok) <- c("vernyomas_s", "vernyomas_d", "nem", "vercukor")

# A 'nem' és 'vercukor' változókat faktorrá alakítjuk a csoportosításhoz
adatok$nem <- factor(adatok$nem, levels = c(1, 2), labels = c("ferfi", "no"))
adatok$vercukor <- factor(adatok$vercukor, levels = c(0, 1), labels = c("normal", "magas"))

# Gyors ellenőrzés
print(head(adatok))

# ----------------------------------------------------------------------
# 2. (a) Mekkora a systoles vérnyomásértékek felső decilise? 5p
# ----------------------------------------------------------------------
# Felső decilis: a 90% kvantilis (0.9)
decilis_felso <- quantile(adatok$vernyomas_s, probs = 0.9)

cat("--- (a) Felső decilis ---\n")
cat(paste("A systoles vérnyomás (vernyomas_s) felső decilise (90% kvantilis) a(z):", round(decilis_felso, 2), "\n"))

# ----------------------------------------------------------------------
# 2. (b) Tekinthetjük-e függetlennek a systoles és a diastoles vérnyomásadatokat? 5p
# ----------------------------------------------------------------------
# Függetlenség vizsgálata korrelációval (t-teszt nullhipotézise, H0: korreláció = 0)
# A vérnyomás adatok normális eloszlását feltételezzük.
korr_teszt <- cor.test(adatok$vernyomas_s, adatok$vernyomas_d)

cat("\n--- (b) Függetlenségi teszt (Korreláció) ---\n")
cat(paste("Pearson korrelációs együttható:", round(korr_teszt$estimate, 4), "\n"))
cat(paste("A korrelációs teszt p-értéke:", round(korr_teszt$p.value, 4), "\n"))

# H0: A korreláció 0 (azaz függetlenek). Alfa szint: 0.05.
if (korr_teszt$p.value < 0.05) {
  cat("Mivel a p-érték < 0.05, elutasítjuk H0-t. Nem tekinthetők függetlennek.\n")
} else {
  cat("Mivel a p-érték >= 0.05, nincs elegendő bizonyíték H0 elutasítására. Tekinthetők függetlennek.\n")
}

# ----------------------------------------------------------------------
# 2. (c) Adjon 0.9 szintű konfidenciaintervallumot a systoles 
#   vérnyomás adatok szórására! 5p
# ----------------------------------------------------------------------

# Konfidenciaintervallum (KI) a szórásra ($sigma$) egy $chi^2$ eloszláson alapuló képletet használ.
# A $\sigma^2$-re vonatkozó KI-t számítjuk, majd gyököt vonunk.
n <- length(adatok$vernyomas_s) # Minta elemszáma
s_2 <- var(adatok$vernyomas_s)  # Mintaszórás négyzete (variancia)
alfa <- 1 - 0.9                 # Alfa szint (0.1)

# A chi-négyzet értékek (alsó és felső kvantilisek):
# Alsó korlát kvantilise: (1 - alfa/2) = 0.95
chi_2_felso <- qchisq(1 - alfa/2, df = n - 1)
# Felső korlát kvantilise: (alfa/2) = 0.05
chi_2_also <- qchisq(alfa/2, df = n - 1)

# Variancia (sigma^2) konfidenciaintervallumának alsó és felső határa
ki_var_also <- (n - 1) * s_2 / chi_2_felso
ki_var_felso <- (n - 1) * s_2 / chi_2_also

# Szórásra ($\sigma$) vonatkozó konfidenciaintervallum
ki_szoras_also <- sqrt(ki_var_also)
ki_szoras_felso <- sqrt(ki_var_felso)

cat("\n--- (c) Szórás 90%-os Konfidenciaintervallum (KI) ---\n")
cat(paste("A systoles vérnyomás szórásának 90%-os KI-je: [", 
          round(ki_szoras_also, 4), "; ", round(ki_szoras_felso, 4), "]\n", sep = ""))

# ----------------------------------------------------------------------
## (d) Hány férfi és hány nő adata van az adatbázisban? 5p
# ----------------------------------------------------------------------
# Csoportosítás a 'nem' oszlop alapján
nem_eloszlas <- table(adatok$nem)

cat("\n--- (d) Nemek eloszlása ---\n")
print(nem_eloszlas)

# ----------------------------------------------------------------------
## (e) Tekinthetjük-e azonosnak a nők és a férfiak systoles
#   vérnyomásának várható értékét? 5p
# ----------------------------------------------------------------------

# Kétmintás t-próba alkalmazása (feltételezve a normális eloszlást).
# Először a szórások azonosságát kell vizsgálni, pl. Levene-teszttel (feltételezve, hogy a 'car' csomag be van töltve).

# 1. Szórások azonossága (Variancia Homogenitás) Levene-teszttel (H0: szórások azonosak)
# levene_teszt <- leveneTest(vernyomas_s ~ nem, data = adatok)
# levene_p_ertek <- levene_teszt$`Pr(>F)`[1]

# Ha a Levene-teszt nem elérhető, vagy a feladat nem követeli meg, használhatjuk a t.test() függvényben a 'var.equal' paramétert.

# T-teszt a várható értékek azonosságára (H0: E_ferfi = E_no)
# Megjegyzés: A t.test() elvégzi a Welch-tesztet, ha a var.equal=FALSE (alapértelmezett), ami nem igényli a szórások azonosságát.
# Ha feltételezzük, hogy a szórások azonosak (var.equal=TRUE), akkor a standard t-tesztet használjuk.
t_teszt_vernyomas <- t.test(vernyomas_s ~ nem, data = adatok, var.equal = FALSE) # Welch-teszt

cat("\n--- (e) Várható érték azonosság (t-teszt) ---\n")
cat(paste("A t-teszt p-értéke (vernyomas_s ~ nem, Welch-teszt):", round(t_teszt_vernyomas$p.value, 4), "\n"))

# H0: A két csoport várható értéke azonos. Alfa szint: 0.05.
if (t_teszt_vernyomas$p.value < 0.05) {
  cat("Mivel a p-érték < 0.05, elutasítjuk H0-t. Nem tekinthetők azonosnak a várható értékek.\n")
} else {
  cat("Mivel a p-érték >= 0.05, nincs elegendő bizonyíték H0 elutasítására. Tekinthetők azonosnak a várható értékek.\n")
}

# ----------------------------------------------------------------------
## (f) Tekinthetjük-e azonosnak a nők és a férfiak systoles vérnyomásának eloszlását? 5p
# ----------------------------------------------------------------------
# Eloszlásazonosság vizsgálata nem-parametrikus teszttel: Kolmogorov-Smirnov (KS) vagy Wilcoxon.
# Kétmintás Kolmogorov-Smirnov teszt (H0: az eloszlások azonosak)
# Különböző elemszámú adatoknál a Wilcoxon (Mann-Whitney U) teszt is gyakori, de a KS-teszt közvetlenül az eloszlások azonosságát vizsgálja.

ferfi_vernyomas <- adatok$vernyomas_s[adatok$nem == "ferfi"]
no_vernyomas <- adatok$vernyomas_s[adatok$nem == "no"]

ks_teszt <- ks.test(ferfi_vernyomas, no_vernyomas)

cat("\n--- (f) Eloszlás azonosság (Kolmogorov-Smirnov teszt) ---\n")
cat(paste("KS-teszt p-értéke (vernyomas_s ~ nem):", round(ks_teszt$p.value, 4), "\n"))

# H0: Az eloszlások azonosak. Alfa szint: 0.05.
if (ks_teszt$p.value < 0.05) {
  cat("Mivel a p-érték < 0.05, elutasítjuk H0-t. Nem tekinthetők azonosnak az eloszlások.\n")
} else {
  cat("Mivel a p-érték >= 0.05, nincs elegendő bizonyíték H0 elutasítására. Tekinthetők azonosnak az eloszlások.\n")
}

# ----------------------------------------------------------------------
## (g) Tekinthetjük-e függetlennek a nemet és a nem magas/magas 
#   vércukor értékeket? 5p
# ----------------------------------------------------------------------
# Két kategorikus változó (nem és vercukor) függetlenségét $\chi^2$ (Khi-négyzet) teszttel vizsgáljuk.
# H0: A nem és a vércukor függetlenek.

kontingencia_tablazat <- table(adatok$nem, adatok$vercukor)

chi_2_teszt <- chisq.test(kontingencia_tablazat)

cat("\n--- (g) Függetlenségi teszt (Chi-négyzet) ---\n")
print(kontingencia_tablazat)
cat(paste("Chi-négyzet teszt p-értéke:", round(chi_2_teszt$p.value, 4), "\n"))

# H0: A két változó független. Alfa szint: 0.05.
if (chi_2_teszt$p.value < 0.05) {
  cat("Mivel a p-érték < 0.05, elutasítjuk H0-t. Nem tekinthetők függetlennek.\n")
} else {
  cat("Mivel a p-érték >= 0.05, nincs elegendő bizonyíték H0 elutasítására. Tekinthetők függetlennek.\n")
}

# ----------------------------------------------------------------------
## (h) Hol nagyobb a magas vércukor szintűek aránya, a nők vagy férfiak közt? 5p
# ----------------------------------------------------------------------
# A kontingenciatáblázatból (vagy új táblázatból) a feltételes arányokat kell kiszámítani.
# Arányok oszloponkénti normalizálása (vercukor eloszlása nemenként)
arany_tablazat <- prop.table(kontingencia_tablazat, margin = 1) # Normalizálás soronként (nem alapján)

cat("\n--- (h) Magas vércukor aránya (nemenként) ---\n")
print(arany_tablazat)

magas_vercukor_ferfi <- arany_tablazat["ferfi", "magas"]
magas_vercukor_no <- arany_tablazat["no", "magas"]

if (magas_vercukor_ferfi > magas_vercukor_no) {
  cat(paste("\nA magas vércukor szintűek aránya a FÉRFIAK (", round(magas_vercukor_ferfi * 100, 2), "%)", " közt nagyobb.\n", sep = ""))
} else if (magas_vercukor_no > magas_vercukor_ferfi) {
  cat(paste("\nA magas vércukor szintűek aránya a NŐK (", round(magas_vercukor_no * 100, 2), "%)", " közt nagyobb.\n", sep = ""))
} else {
  cat("\nAz arányok megegyeznek.\n")
}


