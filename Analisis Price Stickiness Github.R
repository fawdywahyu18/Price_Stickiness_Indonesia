library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(rlist)
library(lubridate)
library(survival)
library(survminer)

# Frekuensi perubahan hanya mempertimbangkan perubahan regular price secara raw
setwd("")

# Append all the data
data_list <- dir()
data_list_all <- list()
for (d in 1:length(data_list)) {
  data_csv <- read.csv(file = data_list[d])
  data_list_all[[d]] <- data_csv
}
data_raw <- list.rbind(data_list_all)
pasar <- unique(data_raw$PASAR)
pasar

# Freq and duration
data_freq <- data_raw %>%
  mutate(month_name=months(as.Date(TANGGAL))) %>%
  mutate(year_name=format(as.Date(data_raw$TANGGAL), format="%Y")) %>%
  mutate(quarter_name=quarters(as.Date(TANGGAL))) %>%
  mutate(quarter_date=ifelse(quarter_name=="Q1","03-01",
                             ifelse(quarter_name=="Q2", "06-01",
                                    ifelse(quarter_name=="Q3", "09-01", "12-01")))) %>%
  mutate(quarter_com=as.Date(paste(year_name, quarter_date, sep = "-"))) %>%
  mutate(JENIS=replace(JENIS, JENIS=="BAWANG MERAH", "BAWANG")) %>%
  # filter(is.na(PERUB_PERSEN)==FALSE) %>%
  group_by(PASAR, year_name, quarter_com, month_name, JENIS, NAMA) %>%
  mutate(freq_all=n()) %>%
  filter(PERUB_PERSEN!=0) %>%
  summarise(freq_change = n(),
            freq_all =  first(freq_all)) %>%
  mutate(share_change=freq_change/freq_all)

data_freq_ym <- data_freq %>%
  group_by(year_name, quarter_com, month_name, JENIS, NAMA) %>%
  summarise(mean_freq = mean(share_change)) %>%
  mutate(duration=-1/(log(1-mean_freq))) %>%
  mutate(change_persen=mean_freq*100)

data_freq_mean <- data_freq_ym %>%
  group_by(quarter_com, JENIS, NAMA) %>%
  summarise(mean_duration=mean(duration),
            mean_freq=mean(mean_freq))
data_freq_mean

data_freq_ym_med <- data_freq %>%
  group_by(year_name, quarter_com, month_name, JENIS, NAMA) %>%
  summarise(med_freq = median(share_change)) %>%
  mutate(duration=-1/(log(1-med_freq))) %>%
  mutate(change_persen=med_freq*100)

data_freq_med <- data_freq_ym_med %>%
  group_by(quarter_com, JENIS, NAMA) %>%
  summarise(med_duration=median(duration),
            med_freq=median(med_freq))
data_freq_med

duration_diff = data_freq_mean$mean_duration - data_freq_med$med_duration
hist(duration_diff)

freq_diff = data_freq_mean$mean_duration - data_freq_med$med_freq
hist(freq_diff)

# Which good has max diff between mean and median in term of duration?
index_max = which.max(abs(duration_diff))
price_name_max <- data_raw %>%
  filter(NAMA==as.character(data_freq_mean[index_max,"NAMA"])) %>%
  group_by(TANGGAL) %>%
  summarise(price_now=mean(HARGA_SKRG),
            NAME=first(NAMA))
hist(price_name_max$price_now, main=paste("Histogram", as.character(data_freq_mean[index_max,"NAMA"]), "Price",
                                          sep=" "),
     xlab=paste(as.character(data_freq_mean[index_max,"NAMA"]), "Current Price", sep=" "))

price_name_max$TANGGAL = as.Date(price_name_max$TANGGAL)
price_name_max_ts = ts(price_name_max, start = c(2015, as.numeric(format(price_name_max$TANGGAL[1], "%j"))),
                       frequency = 365)

plot.ts(price_name_max_ts[,"price_now"], main=paste("TS Plot", as.character(data_freq_mean[index_max,"NAMA"]), "Price",
                                                    sep=" "),
        ylab=paste(as.character(data_freq_mean[index_max,"NAMA"]), "Current Price", sep=" "))

# Sensitivity analysis
data_freq_l <- data_raw %>%
  mutate(month_name=months(as.Date(TANGGAL))) %>%
  mutate(year_name=format(as.Date(data_raw$TANGGAL), format="%Y")) %>%
  mutate(quarter_name=quarters(as.Date(TANGGAL))) %>%
  mutate(quarter_com=paste(quarter_name,year_name, sep = "-")) %>%
  mutate(JENIS=replace(JENIS, JENIS=="BAWANG MERAH", "BAWANG")) %>%
  # filter(is.na(PERUB_PERSEN)==FALSE) %>%
  group_by(year_name, quarter_com, month_name, JENIS, NAMA) %>%
  mutate(freq_all=n()) %>%
  filter(PERUB_PERSEN!=0) %>%
  summarise(freq_change = n(),
            freq_all =  first(freq_all)) %>%
  mutate(share_change=freq_change/freq_all)

data_freq_ym_l <- data_freq %>%
  group_by(PASAR, year_name, quarter_com, month_name, JENIS, NAMA) %>%
  summarise(mean_freq = mean(share_change)) %>%
  mutate(duration=-1/(log(1-mean_freq))) %>%
  mutate(change_persen=mean_freq*100)

data_freq_mean_l <- data_freq_ym %>%
  group_by(quarter_com, JENIS, NAMA) %>%
  summarise(mean_duration=mean(duration),
            mean_freq=mean(mean_freq))
data_freq_mean_l

data_freq_ym_med_l <- data_freq %>%
  group_by(PASAR, year_name, quarter_com, month_name, JENIS, NAMA) %>%
  summarise(med_freq = median(share_change)) %>%
  mutate(duration=-1/(log(1-med_freq))) %>%
  mutate(change_persen=med_freq*100)

data_freq_med_l <- data_freq_ym_med %>%
  group_by(quarter_com, JENIS, NAMA) %>%
  summarise(med_duration=median(duration),
            med_freq=median(med_freq))
data_freq_med_l

duration_diff_l = data_freq_mean$mean_duration - data_freq_mean_l$mean_duration
unique(duration_diff_l)
max(duration_diff_l)
min(duration_diff_l)


#========================================================Part 2====================================================#


# Plot data : duration per city
data_plot = data_freq_mean %>%
  select(quarter_com, JENIS, NAMA, mean_duration) %>%
  filter(JENIS=="DAGING")

ggplot(data_plot, aes(x = quarter_com, y = mean_duration, group=NAMA, col=NAMA, fill=NAMA)) +
  geom_line(aes(linetype=NAMA), size=1) +
  theme_minimal() +
  theme(legend.position="top") +
  ylab(paste(as.character(first(data_plot$JENIS)), "Duration", sep=" ")) +
  xlab("quarters") +
  ggtitle(paste("TS Plot", as.character(first(data_plot$JENIS)), "Duration", sep=" "))

# Creating panel data
kota = c("surabayakota", "jemberkab", "banyuwangikab", "malangkota", "sumenepkab",
         "madiunkota", "probolinggokota", "kedirikota", "blitarkab", "bojonegorokab", 
         "bondowosokab", "gresikkab", "lamongankab", "madiunkab", "magetankab",
         "jombangkab", "malangkab", "mojokertokab", "nganjukkab", "ngawikab",
         "pasuruankab", "ponorogokab")

# Function to find the duration
durasi = function(nama_kota) {
  wd_monther = ""
  
  setwd(paste(wd_monther, nama_kota, sep = "/"))
  
  # Append all the data
  data_list <- dir()
  data_list_all <- list()
  for (d in 1:length(data_list)) {
    data_csv <- read.csv(file = data_list[d])
    data_list_all[[d]] <- data_csv
  }
  data_raw <- list.rbind(data_list_all)
  pasar <- unique(data_raw$PASAR)
  pasar
  
  # Freq and duration
  data_freq <- data_raw %>%
    mutate(month_name=months(as.Date(TANGGAL))) %>%
    mutate(year_name=format(as.Date(data_raw$TANGGAL), format="%Y")) %>%
    mutate(quarter_name=quarters(as.Date(TANGGAL))) %>%
    mutate(quarter_date=ifelse(quarter_name=="Q1","03-01",
                               ifelse(quarter_name=="Q2", "06-01",
                                      ifelse(quarter_name=="Q3", "09-01", "12-01")))) %>%
    mutate(quarter_com=as.Date(paste(year_name, quarter_date, sep = "-"))) %>%
    mutate(JENIS=replace(JENIS, JENIS=="BAWANG MERAH", "BAWANG")) %>%
    # filter(is.na(PERUB_PERSEN)==FALSE) %>%
    group_by(PASAR, year_name, quarter_com, month_name, JENIS, NAMA) %>%
    mutate(freq_all=n()) %>%
    filter(PERUB_PERSEN!=0) %>%
    summarise(freq_change = n(),
              freq_all =  first(freq_all)) %>%
    mutate(share_change=freq_change/freq_all)
  
  data_freq_ym <- data_freq %>%
    group_by(year_name, quarter_com, month_name, JENIS, NAMA) %>%
    summarise(mean_freq = mean(share_change)) %>%
    mutate(duration=-1/(log(1-mean_freq))) %>%
    mutate(change_persen=mean_freq*100)
  
  data_freq_mean <- data_freq_ym %>%
    group_by(quarter_com, JENIS, NAMA) %>%
    summarise(mean_duration=mean(duration),
              mean_freq=mean(mean_freq)) %>%
    mutate(KOTA=nama_kota)
  
  return(data_freq_mean)
}


# Windows progress bar
# create progress bar
total = length(kota)
pb <- winProgressBar(title = "progress bar", min = 0,
                     max = total, width = 300)
durasi_panel = list()
for (k in 1:length(kota)) {
  Sys.sleep(0.1)
  kota_nama = kota[k]
  durasi_panel[[kota_nama]] = durasi(kota_nama)
  setWinProgressBar(pb, k, title=paste( round(k/total*100, 0), "% done"))
}
close(pb)
durasi_panel_df = list.rbind(durasi_panel)

#========================================================Part 3====================================================#
# Mean difference and survival analysis pre and during covid-19 pandemic

# Mean difference using paired t test (example: using surabayakota data)
data_part3 = data_raw %>%
  mutate(tanggal=as.Date(TANGGAL, format="%Y-%m-%d")) %>%
  filter(tanggal>="2018-07-01") %>%
  mutate(JENIS=replace(JENIS, JENIS=="BAWANG MERAH", "BAWANG"),
         month_name=format(tanggal, "%m"),
         year_name=format(tanggal, "%Y"),
         month_date=as.Date(paste(year_name, month_name, "01", sep = "-"))) %>%
  group_by(PASAR, month_date, JENIS, NAMA) %>%
  mutate(freq_all=n()) %>%
  filter(PERUB_PERSEN!=0) %>%
  summarise(freq_change = n(),
            freq_all =  first(freq_all)) %>%
  mutate(share_change=freq_change/freq_all) %>%
  ungroup() %>%
  group_by(month_date, JENIS, NAMA) %>%
  summarise(mean_freq = mean(share_change)) %>%
  mutate(duration=-1/(log(1-mean_freq)),
         dummy_covid=ifelse(month_date>="2020-03-01", 1, 0))

data_part3$NAMA[data_part3$NAMA=="Biasa"] = paste("Cabai", "Biasa", sep = " ")
data_part3$NAMA[data_part3$NAMA=="Keriting"] = paste("Cabai", "Keriting", sep = " ")

mean_diff_list = list()
mean_diff_surv = list()
nama_komoditas = unique(data_part3$NAMA)
for (nama in nama_komoditas) {
  mean_test = data_part3 %>%
    filter(NAMA==nama) %>%
    select(-mean_freq)
  duration_before = filter(mean_test, dummy_covid==0)
  duration_after = filter(mean_test, dummy_covid==1)
  if (nrow(duration_before)!=nrow(duration_after)) {
    next
  }
  res <- t.test(duration ~ dummy_covid, data = mean_test, paired = TRUE)
  summary_mean_diff = as.data.frame(cbind(res$estimate, res$p.value))
  names(summary_mean_diff) = c("mean difference", "p value")
  rownames(summary_mean_diff) = c(nama)
  mean_diff_list[[nama]] = summary_mean_diff
  
  # Estimate the survival function
  fit <- survfit(Surv(duration) ~ dummy_covid, data = mean_test)
  # Survival Plot
  plot = ggsurvplot(fit,
                    pval = TRUE, conf.int = TRUE,
                    risk.table = FALSE, # Add risk table
                    # risk.table.col = "strata", # Change risk table color by groups
                    linetype = "strata", # Change line type by groups
                    surv.median.line = "hv", # Specify median survival
                    ggtheme = theme_bw()) #, # Change ggplot2 theme
  # palette = c("#E7B800", "#2E9FDF"))
  mean_diff_surv[[nama]] = plot
}
mean_diff_df = as.data.frame(list.rbind(mean_diff_list))
mean_diff_surv$`BAWANG PUTIH`


