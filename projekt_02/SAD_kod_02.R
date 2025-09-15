### Jakub Śliwa (335209), Jakub Smela (310900)
### SAD - projekt cz. 2
### 2024L


# set working directory
my_wd = "/Users/jakubsliwa/Desktop/PW/2024L/SAD/projekt_2"
setwd(my_wd)

# load packages
library(ggplot2)
library(tidyverse)
library(rstatix)


#####################
##### Zadanie 1 #####
#####################

# parametry uruchomienia programu
n = 30
deltas = seq(-2, 2, by = 0.05)
sig1 = 1
sig2 = 1
sims = 1000
powersT = list()
powersW = list()
alpha = 0.05
power_threshold = 0.8

# funkcja wykonująca symulacje i zwracająca moce testu t-Studenta dla danych delta
test_norms <- function(delta, sig1, sig2, n,sims, alpha){
  test_results = replicate(sims, {
    nums_1 = rnorm(n, 0, sd=sig1)
    nums_2 = rnorm(n, delta, sd=sig2)
    t.test(nums_1, nums_2)$p.value
  })
  return(mean(test_results < alpha))
}

# funkcja wykonująca symulacje i zwracająca moce testu Wilcoxona dla danych delta
test_results = list()
test_wilcoxon <- function(delta, sig1, sig2, n,sims, alpha){
  test_results = replicate(sims, {
    nums_1 = rnorm(n, 0, sd=sig1)
    nums_2 = rnorm(n, delta, sd=sig2)
    wilcox.test(nums_1, nums_2)$p.value
  })
  return(mean(test_results < alpha))
}


# wykonywawnie symaulacji dla każdej wartości delta
for (i in 1:(length(deltas))){
  powersT <- append(powersT, test_norms(deltas[i], sig1, sig2, n, sims, alpha))
  powersW <- append(powersW, test_wilcoxon(deltas[i], sig1, sig2, n, sims, alpha))
}

# konwersja wyników na data frame
deltas <- as.list(deltas)
resultT = data.frame(delta = as.numeric(deltas), power = as.numeric(powersT))
resultW = data.frame(delta = as.numeric(deltas), power = as.numeric(powersW))

# rysowanie wykresu
ggplot() + 
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") +
  geom_point(data = resultT, aes(x = delta, y = power, color = "t-Student")) +
  geom_point(data = resultW, aes(x = delta, y = power, color = "Wilcoxon")) +
  scale_color_manual(name = "Rozkłady", values = c("t-Student" = "blue", "Wilcoxon" = "red")) +
  labs(title = "Rozkłady takimi samymi odchyleniami standardowymi") 


# wyszuiwanie minimalnej delty dla której moc testu jest większa niż 0.8
min_deltaT <- min(abs(resultT$delta[resultT$power >= power_threshold]))
min_deltaW <- min(abs(resultW$delta[resultW$power >= power_threshold]))


#####################
##### Zadanie 2 #####
#####################

# Wczytanie danych o inflacji studenckiej i wlasnej z poprzedniego projektu
inflations_df = read.csv("inflations.csv", header=TRUE, sep = ",", dec = ".")

# wyodrebnienie danych o studenckiej inflacji (X) oraz inflacji oficjalnej (Y) do wektorow
X = inflations_df$b_inflation_rate[13:31]
Y = inflations_df$inflation_rate[13:31]

# stworzenie nowego df, zawierającego jedynie potrzebne dane
df = data.frame(
  studencka = X,
  oficjalna = Y
)

# zmiana ksztaltu danych na potrzeby wizualizacji
inflations_long_df <- df %>%
  pivot_longer(cols = c(studencka, oficjalna), names_to = "Variable", values_to = "Value")

# (a) narysowanie wykresow pudelkowych
ggplot(inflations_long_df, aes(x = Variable, y = Value, fill = Variable)) +
  coord_cartesian(ylim = c(0, 25)) +
  geom_boxplot() +
  labs(title = "Wykresy pudełkowe inflacji oficjalnej i studenckiej", x = "Typ inflacji", y = "Stopa inflacji [%]") +
  theme_minimal()

# wyznaczenie roznic inflacji studenckiej i oficjalnej: X - Y
df["delta"] = X - Y

# (b) narysowanie wykresu pudelkowego roznic X - Y
ggplot(df, aes(x = "", y = delta)) +
  coord_cartesian(ylim = c(-2.5, 7.5)) +
  geom_boxplot(fill = "orange") +
  labs(title = "Wykres pudełkowy różnic pomiędzy inflacją studencką a oficjalną", x = "", y = "Różnica stóp inflacji [%] - w punktach proc.") +
  theme_minimal()

# narysowanie histogramow X oraz Y
ggplot(inflations_long_df, aes(x = Value, fill = Variable)) +
  coord_cartesian(xlim = c(0, 30), ylim = c(0, 8)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 7, color = "black") +
  labs(title = "Histogramy inflacji oficjalnej i studenckiej", x = "Stopa inflacji [%]", y = "Liczba wystąpień") +
  scale_fill_manual(values = c("studencka" = "blue", "oficjalna" = "red")) +
  theme_minimal()

# narysowanie histogramu X - Y
ggplot(df, aes(x = delta)) +
  coord_cartesian(xlim = c(-5, 10), ylim = c(0, 8)) +
  geom_histogram(binwidth = 1.25, fill = "orange", color = "black") +
  labs(title = "Histogram różnic pomiędzy inflacją studencką a oficjalną", x = "Różnica stóp inflacji [%] - w punktach proc.", y = "Liczba wystąpień") +
  theme_minimal()

# przeprowadzenie testu Wilcoxona (dla par)
wilcox.test(X, Y, paired=TRUE, alternative="greater")


#####################
##### Zadanie 3 #####
#####################

# ziarno dla powtarzalnosci eksperymentow
set.seed(100)

# zdefiniowanie parametrow
number_of_estimations = 1000
sample_sizes = c(10, 1000, 100000)
p = 0.7

# zdefiniowanie funkcji do przeprowadzania pojedynczego eksperymentu
perform_estimation = function(num_est, sample_size, p) {
  est_p = numeric(num_est)
  for (i in 1:num_est) {
    bern_sample = rbinom(sample_size, 1, p)
    est_p[i] = mean(bern_sample)
  }
  return(est_p)
}

# zdefiniowanie funkcji do przeprowadzenia szeregu eksperymentow
run_experiments = function(num_est, sample_sizes, p) {
  res_p = list()
  for (i in 1:length(sample_sizes)) {
    res_exper = perform_estimation(num_est, sample_sizes[[i]], p)
    res_p[[i]] = res_exper
  }
  return(res_p)
}


# wywolanie przeprowadzenia eksperymentow oraz podzielenie wynikow na poszczegolne estymatory
result = run_experiments(number_of_estimations, sample_sizes, p)

# rozklad bernoulliego - normalizacja wartosci
bern_mtx = matrix(result)

bern_1 = bern_mtx[1,][[1]]
bern_2 = bern_mtx[2,][[1]]
bern_3 = bern_mtx[3,][[1]]

fisher_info_p = 1 / (p * (1 - p))

bern_1_trans = sqrt(sample_sizes[1] * fisher_info_p) * (bern_1 - p)
bern_2_trans = sqrt(sample_sizes[2] * fisher_info_p) * (bern_2 - p)
bern_3_trans = sqrt(sample_sizes[3] * fisher_info_p) * (bern_3 - p)

# rozklad bernoulliego - prezentacja wyników
df_bern = data.frame(
  n_1 = bern_1_trans,
  n_2 = bern_2_trans,
  n_3 = bern_3_trans
)

ggplot() +
  geom_density(data = df_bern, aes(x = n_1, color = "n = 10")) +
  geom_density(data = df_bern, aes(x = n_2, color = "n = 1000")) +
  geom_density(data = df_bern, aes(x = n_3, color = "n = 100000"), size=0.75) +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1), color = "black", linetype = "dashed", size=1.25) +
  labs(title = "Asymptotyczna normalność estymatora NW - rozkład Bernoulliego", x = "sqrt(n*I(p))*(est_p - p)", y = "Density", color = "Liczebność próby") +
  scale_color_manual(values = c("n = 10" = "blue", "n = 1000" = "red", "n = 100000" = "green", "Standard Normal" = "black")) +
  theme_minimal() +
  theme(legend.position = "right")
