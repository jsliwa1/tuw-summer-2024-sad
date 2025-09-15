# Jakub Śliwa, 335209
# SAD - projekt cz. 1
# sem. 2024L


# set working directory
setwd("/Users/jakubsliwa/Desktop/PW/2024L/SAD/projekt_1")

# load packages
library(ggplot2)
library(tidyverse)

####################################
### Problem 1 - custom inflation ###
####################################

# load selected data
food_mtx = read.csv("data_food.csv", header=TRUE, sep = ";", dec = ",")
food_df = subset(food_mtx, select = -c(Zmienna, Typ.informacji, Jednostka.terytorialna))
colnames(food_df)[1] = c("name") 

services_mtx = read.csv("data_services.csv", header=TRUE, sep = ";", dec = ",")
services_df = subset(services_mtx, select = -c(Zmienna, Typ.informacji, Jednostka.terytorialna))
colnames(services_df)[1] = c("name") 

inflation_v = read.csv("inflation.csv", header = TRUE, sep = ";", dec = ",")

# choose weights for products (approx. monthly consumption)
food_weights = c(
  10, # chleb
  3, # kurczak
  2, # szynka
  3, # kielbasa
  10, # mleko
  1, # twarog
  20, # jaja
  3, # maslo
  1, # olej
  4, # pomarancze
  2, # marchew
  2, # cebula
  4, # ziemniaki
  5 # sok
)

services_weights = c(
  175, # energia elektryczna (srednie miesieczne zuzycie na przecietnego Polaka: 200)
  1, # pasta do zebow
  1, # szampon do wlosow
  2, # mydlo w kostce
  0.5, # strzyzenie meskie
  1, # bilet do kina
  50, # bilet jednorazowy komunikacja
  8, # bilet odleglosc 11-50 km
  0.5, # wizyta u lekarza
  0.5, # plyn do prania 
  20, # ogrzewanie
  1.25 # ciepla woda
)

data_df = rbind(food_df, services_df)
for (i in 2:length(colnames(data_df))) {
  colnames(data_df)[i] = substr(colnames(data_df)[i], 2, 8)
}
weights = c(food_weights, services_weights)


# calculate total custom basket monthly price: 09.2021 - 03.2024
calculate_basket_price = function(monthly_prices, weights) {
  return (sum(unlist(monthly_prices)*weights))
}

calculate_all_baskets = function(df, weights) {
  len = length(colnames(df)) - 1
  res = vector(mode = "numeric", length = len)
  for (i in 1:len) {
    res[i] = calculate_basket_price(df[colnames(df)[i+1]], weights)
  }
  return (res)
}

basket_prices = calculate_all_baskets(data_df, weights)


# calculate YTY inflation rates based on custom basket: 09.2022 - 03.2024
calculate_basket_inflation = function(prices) {
  len = length(prices) - 12
  inflation_basket = vector(mode = "numeric", length = len)
  for (i in 1:len) {
    inflation_basket[i] = prices[i+12] / prices[i]
  }
  return (inflation_basket)
}

inflation_basket = calculate_basket_inflation(basket_prices)


# visualize obtained custom basket inflation rates and compare with official rates
inflation_df = data.frame(
  month = colnames(inflation_v),
  inflation_rate = unlist(inflation_v) - 100,
  b_inflation_rate = round(inflation_basket * 100 - 100, 1),
  basket_price = round(basket_prices[13:31], 2)
)

inflations_df = data.frame(
  month = colnames(data_df)[2:length(colnames(data_df))],
  inflation_rate = c(vector(mode="numeric", length=12), unlist(inflation_v) - 100),
  b_inflation_rate = c(vector(mode="numeric", length=12), round(inflation_basket * 100 - 100, 1)),
  basket_price = round(basket_prices[1:31], 2)
)

colors = c("Custom inflation" = "red", "Official inflation" = "green")

ggp = ggplot(inflations_df, aes(x=month, y=(basket_price-800)/20)) +
  labs(title= "Custom vs official inflation rates: 09.2022 to 03.2024", x="Month", y="Inflation rate [%]", color="Legend") + 
  coord_cartesian(ylim = c(0, 25)) +
  geom_bar(stat="identity", width=0.75) +
  geom_line(data=inflations_df %>% slice(13:31), aes(x=month, y=inflation_rate, group=1, color="Official inflation"), stat="identity", size=2) +
  geom_line(data=inflations_df %>% slice(13:31), aes(x=month, y=b_inflation_rate, group=1, color="Custom inflation"), stat="identity", size=2) +
  scale_y_continuous(sec.axis = sec_axis(~.*20 + 800, name="Basket total price [PLN]")) +
  scale_color_manual(values = colors) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = c(0.2, 0.8))

ggp


####################################
### Problem 2a - R&D expenditure ###
####################################

# load data
rd_mtx = read.csv("inf_rd.csv", header=TRUE, sep = ";", dec = ",")
rd_df = subset(rd_mtx, select = -c(Typ.informacji, Towary.i.usługi.konsumpcyjne_3, Brak.wymiarów, Jednostka.terytorialna))

for (i in 2:length(colnames(rd_df))) {
  colnames(rd_df)[i] = substr(colnames(rd_df)[i], 2, 8)
}

# calculate YTY growth rates of R&D expenditures
calculate_growth_rate = function(expenditures) {
  len = length(expenditures) - 1
  deltas = vector(mode = "numeric", length = len+1)
  for (i in 1:len) {
    deltas[i+1] = expenditures[i+1] / expenditures[i]
  }
  return (deltas)
}

rd_deltas = round(calculate_growth_rate(unlist(rd_df[2,2:length(colnames(rd_df))])), 3)

# calculate cumulated inflation rate
calculate_cumulated_inflation = function(inflation) {
  len = length(inflation) - 1
  cum_inf = vector(mode = "numeric", length = len+1)
  cum_inf[1] = 1
  for (i in 1:len) {
    cum_inf[i+1] = round((inflation[i+1]/100) * cum_inf[i], 3)
    print(inflation[i+1])
    print(cum_inf[i])
  }
  return (cum_inf)
}

cum_inf = calculate_cumulated_inflation(unlist(rd_df[1,2:length(colnames(rd_df))])[11:24])

# prepare df
rd_exp_df = data.frame(
  month = colnames(rd_df)[2:length(colnames(rd_df))],
  rd_exp = unlist(rd_df[2,2:length(colnames(rd_df))]),
  rd_exp_rate = rd_deltas,
  inf = unlist(rd_df[1,2:length(colnames(rd_df))]) / 100,
  cum_inf = c(vector(mode="numeric", length=10), cum_inf)
)

# visualize R&D total expenditure, inflation rate and deltas
colors = c("R&D growth" = "green", "Inflation" = "red", "Cumulated exp. (2009)" = "yellow")


ggp = ggplot(rd_exp_df, aes(x=month, y=rd_exp/50)) +
  labs(title= "R&D expenditure growth vs inflation", x="Year", y="Inflation/R&D growth rate [%]", color="Legend") + 
  coord_cartesian(ylim = c(-5, 30)) +
  geom_bar(stat="identity", width=0.75) +
  geom_bar(data=rd_exp_df %>% slice(11:24), aes(x=month, y=cum_inf*rd_exp_df[11,2]/50, group=1, color="Cumulated exp. (2009)"), stat="identity", width=0.25, size=0.8) +
  geom_line(data=rd_exp_df %>% slice(12:24), aes(x=month, y=(rd_exp_rate-1)*100, group=1, color="R&D growth"), stat="identity", size=1.5) +
  geom_line(data=rd_exp_df %>% slice(12:24), aes(x=month, y=(inf-1)*100, group=1, color="Inflation"), stat="identity", size=1.5) +
  scale_color_manual(values = colors) +
  scale_y_continuous(sec.axis = sec_axis(~.*50, name="R&D expenditure per person [PLN]")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = c(0.2, 0.8))

ggp


#######################################
### Problem 2b - R&D exp. structure ###
#######################################

# load data
rd_str_mtx = read.csv("sektory.csv", header=TRUE, sep = ";", dec = ",")
rd_str_df = subset(rd_str_mtx, select = -c(Typ.informacji, Zmienna, Sektory.finansujące, Jednostka.terytorialna))
colnames(rd_str_df) = c("sektor", 2020, 2021, 2022)

# prepare data
rd_per_df = data.frame(
  year = c(2020, 2021, 2022),
  BES = unlist(rd_str_df[2, 2:4]) / unlist(rd_str_df[1, 2:4]),
  GOV = unlist(rd_str_df[3, 2:4]) / unlist(rd_str_df[1, 2:4]),
  HES = unlist(rd_str_df[4, 2:4]) / unlist(rd_str_df[1, 2:4]),
  PNP = unlist(rd_str_df[5, 2:4]) / unlist(rd_str_df[1, 2:4])
)

# visualize data
rd_per_df %>% 
  pivot_longer(c("BES", "GOV", "HES", "PNP"), names_to = "Sector") %>% 
  ggplot(aes(y = value, x = year, fill = Sector)) +
  geom_col() +
  labs(title= "Structure of R&D expenditure 2020-2022", x="Year", y="Share [%]")

# another way
colors = c("BES" = "#F8766D", "GOV" = "#7CAE00", "HES" = "#00BFC4", "PNP" = "#C77CFF")

ggp = ggplot(rd_per_df, aes(x=year, y=BES*100, color="BES")) +
  labs(title= "Structure of R&D expenditure 2020-2022", x="Year", y="Share (BES, HES) [%]", color="Legend") + 
  coord_cartesian(ylim = c(0, 100)) +
  geom_line(stat="identity", size=1) +
  geom_line(aes(x=year, y=GOV*4000, color="GOV"), size=1) +
  geom_line(aes(x=year, y=HES*100, color="HES"), size=1) +
  geom_line(aes(x=year, y=PNP*4000, color="PNP"), size=1) +
  scale_color_manual(values = colors) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.025, name="Share (GOV, PNP) [%]")) +
  scale_x_continuous(breaks=seq(2020,2022,by=1)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "top")

ggp


##################################
### Problem 3 - housing market ###
##################################

# load data
house_mtx = read.csv("median_house.csv", header=TRUE, sep = ";", dec = ",")
house_df = subset(house_mtx, select=-c(Zmienna, Typ.informacji, Jednostka.terytorialna))
inflation_mtx = read.csv("inflation_2.csv", header=TRUE, sep = ";", dec = ",")
inflation_df = subset(inflation_mtx, select=-c(Zmienna, Typ.informacji, Jednostka.terytorialna, Towary.i.usługi.konsumpcyjne_3))


for (i in 2:length(colnames(house_df))) {
  colnames(house_df)[i] = substr(colnames(house_df)[i], 2, 8)
}

# calculate house price growth (1 m^2)
# overall and separately for both primary and secondary market
calculate_house_growth = function(df, num_row) {
  len = length(colnames(df))-1
  res = vector(mode = "numeric", length = len)
  for (i in 5:len+1) {
    res[i-1] = df[num_row, i] / df[num_row, i-4]
  }
  return (res)
}

house_price_growth = calculate_house_growth(house_df, 1)
primary_market = calculate_house_growth(house_df, 2)
secondary_market = calculate_house_growth(house_df, 3)

# calculate traditional inflation quarterly
inflation_quarterly = vector(mode="numeric", length=12)
for (i in 1:length(colnames(inflation_df))) {
  if (i %% 3 == 0) {
    inflation_quarterly[i%/%3] = round((inflation_df[i-2] + inflation_df[i-1] + inflation_df[i]) / 3, 2)
  }
}
inflation_quarterly = c(0, 0, 0, 0, unlist(inflation_quarterly))

# prepare data
data_df = data.frame(
  quarter = colnames(house_df)[2:17],
  inflation = inflation_quarterly / 100,
  overall_growth = round(house_price_growth, 4),
  primary_growth = round(primary_market, 4),
  secondary_growth = round(secondary_market, 4),
  overall_price = unlist(house_df[1, 2:17]),
  primary_price = unlist(house_df[2, 2:17]),
  secondary_price = unlist(house_df[3, 2:17])
)

# compare growth rates of median house price (for 1 m^2) vs inflation
colors = c("Inflation" = "red", "Price growth" = "green")
ggp = ggplot(data_df, aes(x=quarter, y=overall_price)) +
  labs(title="Housing price growth vs inflation", x="Quarter", y="Overall median price per 1 m^2 [PLN]", color="Legend") +
  coord_cartesian(ylim = c(4000, 8000)) +
  geom_bar(stat="identity", width=0.75) +
  geom_line(data=data_df %>% slice(5:16), aes(x=quarter, y=(((inflation*100)-100)+20)*200, group=1, color="Inflation"), stat="identity", size=1.5) +
  geom_line(data=data_df %>% slice(5:16), aes(x=quarter, y=(((overall_growth*100)-100)+20)*200, group=1, color="Price growth"), stat="identity", size=1.5) +
  scale_color_manual(values = colors) +
  scale_y_continuous(sec.axis = sec_axis(~./200-20, name="Inflation/price growth rate [%]")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = c(0.2, 0.8))

ggp
  
# compare primary vs secondary market
colors = c("Primary market" = "#00BFC4", "Secondary market" = "#C77CFF", "Primary growth" = "#F8766D", "Secondary growth" = "green")

ggp = ggplot(data_df, aes(x=quarter, y=overall_price)) +
  labs(title="Housing price growth vs inflation", x="Quarter", y="Median price per 1 m^2 [PLN]", color="Legend") +
  coord_cartesian(ylim = c(4000, 8000)) +
  geom_bar(data=data_df, aes(x=quarter, y=primary_price, color="Primary market"), stat="identity", width=0.65, size=1) +
  geom_bar(data=data_df, aes(x=quarter, y=secondary_price, color="Secondary market"), stat="identity", width=0.35, size=1) +
  geom_line(data=data_df %>% slice(5:16), aes(x=quarter, y=(((primary_growth*100)-100)+30)*150, group=1, color="Primary growth"), stat="identity", size=1.5) +
  geom_line(data=data_df %>% slice(5:16), aes(x=quarter, y=(((secondary_growth*100)-100)+30)*150, group=1, color="Secondary growth"), stat="identity", size=1.5) +
  scale_color_manual(values = colors) +
  scale_y_continuous(sec.axis = sec_axis(~./150-30, name="Price growth rate [%]")) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = c(0.2, 0.8))

ggp












