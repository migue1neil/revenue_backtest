## 目的: 想要利用月營收成長率看一下歷史回測績效怎麼樣?
## 需要的指標: 月營收成長率、60MA、就這樣

#目前進度 : 處裡完60MA的單一個股，目前需要利用"分組方式"去製作不同時間點的個股MA線
#接著用月營收，最後再下判斷式做回測(可能要考慮一下需不需要把資料作合併)

#有好幾個靠北問題，第一如何選定每個月的10號後下一個交易日，
#第二要計算報酬率的話要先把這個月跟下個月的交易日先選出來，再用shift方式計算出當支股票在這個月的交易報酬率
#要算日波動的話
#應該是可使使用month()抓月份與10號出來

#看套件使用主要還是先看例子比較好

library(plyr)

setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/revenue_backtest") # 設定工作目錄
library(data.table)
#library(dplyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 
library(TTR)
#library(magrittr) 導向使用 %>%
#library(zoo)
#(tseries) #時間序列模組，感覺目前不會使用到。
#library(forecast) #設計MA線的時候會用到的模組

##### 拿台積電先做單一股票測試 #####
TSMC = read.table("2330TSMC_stockprice_2010-2019.txt" , header = TRUE ,encoding = "mbcs" , dec = "\t")
colnames(TSMC) = c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")
TSMC$年月日 = ymd(TSMC$年月日)
TSMC$調整收盤價 = as.numeric(TSMC$調整收盤價)
#TSMC$TSMC_60MA = ma(TSMC$調整收盤價 , order = 60) 用forecast寫的
TSMC$TSMC_60MA = SMA(TSMC$調整收盤價, n = 60) #用TTR套件製作MA  #n是期間的意思，有成功
TSMC_stock_picture = ggplot(TSMC , aes(x = 年月日)) +  # 學一下ggpolt用法 ，第一個要式dataframe 後面是裡面的資料
                    geom_line(aes(y = 調整收盤價)) +
                    geom_line(aes(y = TSMC_60MA))  #未完待續，請參閱教學
TSMC_stock_picture
TSMC$前一天的收盤價 = shift(TSMC$調整收盤價,n = 1)
TSMC$單日漲幅 = (TSMC$調整收盤價-TSMC$前一天的收盤價)/TSMC$前一天的收盤價

#####tej_closed 計算多股60MA #####
stock_price = data.table(read.table("TEJ_closed_2020_2021.txt", header = TRUE ))
colnames(stock_price) = c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")

#媽的終於成功惹，這邊是在跑MA之前，先生成一個資料集，裡面有每支股票的出現天數，再用semi_join方法，把小於60日的資料跟原資料比對

delete_MA = function(stock_price_dataframe, ndays){  #設計成函數，要導入收盤價表，與小於多少的天數，會移除小於設定天數的股價
  ndays_stock_price = data.table(table(stock_price_dataframe$證券代碼)) #先統計證券代碼出現的次數，再將資料轉換成data.table格式
  colnames(ndays_stock_price) = c("證券代碼", "出現天數") #修改欄位名稱
  ndays_stock_price$證券代碼 = as.numeric(ndays_stock_price$證券代碼) #將證券代碼轉換成數字
  ndays_stock_price = ndays_stock_price[ndays_stock_price$出現天數 >= ndays,] #移除小於ndays的row
  forMA_stock_price = semi_join(stock_price,ndays_stock_price, by = "證券代碼") #比較資料，如果左邊的ID有出現在右邊的話，將他留下。
  return (forMA_stock_price) #回傳dataframe出去
}
# new_stock_price = delete_MA(stock_price, 60) #到時候可以在函數裡面在加一個這個，用來移除
#成功了
# dfc = ddply(new_stock_price, c("證券代碼","公司名稱") , 
#             .fun= function(x){
#               transform(x, MA60 = with(x, SMA(x$調整收盤價 , n = 60) ))
#             } )

MA_function = function(stock_price_dataframe, ndays ){ #設計一個函數，裡面包含移除小於天數的股票以及計算60ma
  stock_price_removed = delete_MA(stock_price_dataframe, ndays)
  MA_stock_price = ddply(stock_price_removed, c("證券代碼","公司名稱") , 
                          .fun= function(x){
                            transform(x, MA60 = with(x, SMA(x$調整收盤價 , n = ndays) ))
                          } )
  return (MA_stock_price)
}
MA_stock_price = MA_function(stock_price,ndays = 60)


#####月營收指標計算#####
#我想要月營收成長率

#stock_price = data.table(read.csv("stock_price_closed_MA.csv", header = TRUE ))
revenue_data = data.table(read.csv("month_revenue_2020_2021.txt" ,header = TRUE ,encoding = "mbcs"))
colnames(revenue_data) = c("證券代碼","公司名稱","年月","營收發布日","單月營收")
revenue_data$營收發布日= ymd(revenue_data$營收發布日)
#revenue_data$年月 = ymd(revenue_data$年月)
# as.numeric(revenue_data$證券代碼)
str(revenue_data)

# df = stock_price[]

revenue_data = ddply(revenue_data,c("證券代碼"),.fun = function(x){
  transform(x, 上月營收 = with(x, shift(x$單月營收, n = 1)))
}
  )
revenue_data$單月營收成長率 = round((revenue_data$單月營收 - revenue_data$上月營收)/ revenue_data$上月營收,digits = 2)

