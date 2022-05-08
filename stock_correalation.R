setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/revenue_backtest") # 設定工作目錄
library(data.table)
library(dplyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 

#載入台積電股價資料
TSMC = read.table("2330TSMC_stockprice_2010-2019.txt" , header = TRUE ,encoding = "mbcs" , dec = "\t")
colnames(TSMC) = c("證券代碼","公司名稱","年月日","調整收盤價","成交張數")
TSMC$年月日 = ymd(TSMC$年月日)
TSMC$調整收盤價 = as.numeric(TSMC$調整收盤價)
#str(TSMC)

#載入聯發科股價資料
MTK = read.table("2454MTK_stock_price_2010-2019.txt" , header = TRUE ,encoding = "mbcs" , dec = "\t")
colnames(MTK) = c("證券代碼","公司名稱","年月日","調整收盤價")
MTK$年月日 = ymd(MTK$年月日)
MTK$調整收盤價 = as.numeric(MTK$調整收盤價)
#str(MTK)

#生成一個年月，之後分組使用
table = merge(TSMC,MTK,by = "年月日")
table$年月 = paste0(year(table$年月日), month(table$年月日))
table$年月 = as.numeric(table$年月)
#str(table$年月)

MA_function = function( x , ndays ){ #設計一個函數，裡面包含移除小於天數的股票以及計算60ma
  MA_stock_price = ddply( x , c("年月") , 
                          .fun= function(x){
                            transform( x , COr = with(x, cor(調整收盤價.x,調整收盤價.y) ))
                          } )
   return (MA_stock_price)
 }
table_COr = MA_function(table)

stock_picture = ggplot(table_COr , aes(x = 年月日))+
                 geom_line(aes(y = COr))  #未完待續，請參閱教學
                
stock_picture
