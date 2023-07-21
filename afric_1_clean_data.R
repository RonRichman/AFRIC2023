### AFRIC Data analytics workshop
# Script 1 - clean data

library(data.table)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(keras)
library(tensorflow)
library(stringr)
library(lubridate)
library(ROCit)
library(PRROC)

### read in cleaned data

dat=fread("C:/r/VicRoadFatalData.csv")

dat[, .N, keyby = fatal_cnt]

### feature engineering

dat[, ACCIDENTDATE := dmy(ACCIDENTDATE)]
dat[, ACCIDENT_YEAR := year(ACCIDENTDATE)]
dat[, ACCIDENT_QRTR := quarter(ACCIDENTDATE)]
dat[, ACCIDENT_MONTH := month(ACCIDENTDATE)]
dat[, ACCIDENT_DAY := day(ACCIDENTDATE)]

dat[, ACCIDENTTIME := hour(hms(ACCIDENTTIME))]

dat[, id := 1:.N]

### define categorical versus continuous variables
var_names = dat %>% names()

cat_vars = var_names[c(2,5,6,9,10,11,12,13,14,18,19,20,21,22,23,24,25,26,31,32,33,34)]

cont_vars = var_names[c(3,8)]

### split train and test on policy

set.seed(123)
train = dat %>%  sample_frac(0.8)
test = dat[!id %in% train$id]

train[, set := "train"]
test[, set := "test"]
dat = rbind(train, test)

### check empirical proportions

dat[, .N, keyby = .(set, fatal_cnt)] %>% 
  dcast.data.table(set~fatal_cnt) %>% 
  .[,`1`/`0`]


### scale continuous and convert categorical to integers

min_max = function(col){
  dat = data.table(col_name = col)
  list(min = dat[, min(col_name)], max = dat[, max(col_name)], 
       scaled = dat[, (col_name - min(col_name))/(max(col_name) - min(col_name))])
}

for (col in cont_vars){
  mins_maxs = train[, get(col)] %>% min_max()
  train[, paste0(col, "_input") := mins_maxs$scaled]
  test[, paste0(col, "_input") := (get(col) - mins_maxs$min)/(mins_maxs$max - mins_maxs$min)]
}

cat_to_int = function(col){
  dat = data.table(col_name = col)
  dat = dat[order(col_name)] %>% unique()
  dat[, paste0("col_name", "_input") := as.integer(as.factor(col_name))]
  list(map = dat)
}

for (col in cat_vars){
  maps = (train[, get(col)] %>% cat_to_int())$map
  maps %>% setnames(names(maps), c(col, paste0(col, "_input")))
  train %>% setkeyv(col)
  train = train %>% merge(maps)
  test %>% setkeyv(col)
  test = test %>% merge(maps)
}

### get data in format for keras
get_keras_data = function(dat){
  temp = list()
  for (col in c(cat_vars, cont_vars)) temp[[paste0(col, "_input")]] = dat[, get(paste0(col, "_input"))] %>% as.matrix
  temp
}

train_x = train %>% get_keras_data
test_x = test %>% get_keras_data

all = rbind(train, test)
all_x = all %>% get_keras_data()

train_y = train$fatal_cnt %>% as.matrix
test_y = test$fatal_cnt %>% as.matrix