#SV Scarpino
#Analysis of Baidu mobility data
#June 2022

###########
#Libraries#
###########

###############
#Acc functions#
###############
get_dates <- function(x, pat1){
  splitx <- strsplit(x = x, split = "/")[[1]]
  splitx_file <- splitx[length(splitx)]
  splitx_strip <- gsub(pattern = pat1, replacement = "", x = splitx_file)
  splitx_strip2 <- gsub(pattern = ".csv", replacement = "", x = splitx_strip)
  return(splitx_strip2)
}

make_mob <- function(f){
  mob <- read.csv(f)
  mob2 <- as.matrix(mob[,-1])
  row.names(mob2) <- mob[,1]
  for(i in 1:nrow(mob2)){
    mob2[i,which(is.na(mob2[i,])== TRUE)] <- 0
    mob2[i,] <- mob2[i,] - mean(mob2[i,])
  }
  return(mob2)
}

######
#Data#
######
files_in <- list.files("../data/BaiduMobility/inbound/all", full.names = TRUE)
files_out <- list.files("../data/BaiduMobility/outbound/all", full.names = TRUE)

dates_list_in <- lapply(X = files_in, FUN = get_dates, pat1 = "BaiduNet_in_")
dates_in <- unlist(dates_list_in)
dates_in <- strptime(dates_in, format = "%Y%m%d")

dates_list_out <- lapply(X = files_out, FUN = get_dates, pat1 = "BaiduNet_out_")
dates_out <- unlist(dates_list_out)
dates_out <- strptime(dates_out, format = "%Y%m%d")

files_in <- files_in[order(dates_in)] #making sure the files are in the same order
files_out <- files_in[order(dates_out)]

##########
#Analysis#
##########
svds <- rep(NA, length(files_in))
total_mob <- rep(NA, length(files_in))
wuhan_mob <- rep(NA, length(files_in))
nanjing_mob <- rep(NA, length(files_in))
shanghai_mob <- rep(NA, length(files_in))
wu_sha <- rep(NA, length(files_in))

pb <- txtProgressBar(1, length(files_out), style=3)
for(f in 1:length(files_out)){
  mob.in.f <- make_mob(files_in[f])
  mob.out.f <- make_mob(files_out[f])
  mob2 <- mob.in.f - t(mob.out.f)
  svs <- svd(mob2)
  svds[f] <- svs$d[1]
  
  log_mobs <- log(mob2)
  log_mobs[which(is.finite(log_mobs) == FALSE)] <- NA
  total_mob[f] <- sum(log_mobs, na.rm = T)
  wuhan_mob[f] <- sum(log_mobs[which(row.names(mob2) == "武汉市"),], na.rm = T) #wuhan
  nanjing_mob[f] <- sum(log_mobs[which(row.names(mob2) == "南京市"),], na.rm = T) #nanjing
  shanghai_mob[f] <- sum(log_mobs[which(row.names(mob2) == "上海市"),], na.rm = T) #shanghair
  wu_sha[f] <- log_mobs[which(row.names(mob2) == "武汉市"),which(row.names(mob2) == "上海市")]
  setTxtProgressBar(pb, f)
}
dates <- dates_in

plot(dates, svds, type = "l")
plot(dates, total_mob, type = "l")
plot(dates, wuhan_mob, type = "l")
plot(dates, nanjing_mob, type = "l")
plot(dates, shanghai_mob, type = "l")
plot(dates, wu_sha, type = "l", xlab = "2020", ylab = "Relative mobility", bty = "n", lwd = 3)
