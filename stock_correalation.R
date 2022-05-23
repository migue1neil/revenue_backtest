####
# 現在問題有兩個，第一個試畫圖的問題，需要將月份畫漂亮，低於零的顯示紅色，大於零的顯示綠色
# [已完成]第二個試月份的問題，現在試每天都有很多重複的資料，需要把它削減成一年12比即可，
# [已完成]可能需要做差分，時間序列單跟檢定

setwd("C:/Users/Neil/Documents/git-repos/backtest_in_R/revenue_backtest") # 設定工作目錄
library(data.table)
library(dplyr)
library(plyr)
library(readr) #讀取檔案必備
library(ggplot2) # 畫圖使用
library(lubridate) #轉換日期使用 
library(tseries)
library(forecast)

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

#合併資料，並生成一個年月，之後分組使用
table = merge(TSMC,MTK,by = "年月日")
table$年月 = paste0(year(table$年月日)*10, month(table$年月日))
table$年月 = as.numeric(table$年月)
#str(table$年月)

#算日成長率，移除時間因素，#很陽春的寫法，應該先算完在拆開
table$前一天的調整收盤價.x = shift(table$調整收盤價.x, n=1 )
table$單日漲幅.x = (table$調整收盤價.x - table$前一天的調整收盤價.x)/(table$前一天的調整收盤價.x)
table$前一天的調整收盤價.y = shift(table$調整收盤價.y, n=1 )
table$單日漲幅.y = (table$調整收盤價.y - table$前一天的調整收盤價.y)/(table$前一天的調整收盤價.y)
table = na.omit(table)

#依據月份將每個月的相關係數算出來
Correlation_function = function( panel_data_table ){ #設計一個函數，計算兩table中的相關係數
                                                    correlation_value = ddply( panel_data_table , c("年月") , 
                                                    .fun= function(x){
                                                     transform( x , cor = with(x, cor(單日漲幅.x,單日漲幅.y)))
                                                                     })
                                                    return (correlation_value)
                                                    }
table_cor = Correlation_function(table)


#####
#驗算，取出來個別計算沒錯，但是月份排序錯了
table_test
table_test = table[table$年月 == 201001 , ]
attach(table_test)
cor(單日漲幅.x,單日跌幅.y)

#單根檢定 
adf.test(table_cor$單日漲幅.x)
adf.test(table_cor$單日漲幅.y)

#加入市場資料做計算
stock_index = read.table("Y9999_index.txt" , header = TRUE ,encoding = "mbcs" , dec = "\t")
colnames(stock_index) = c("證券代碼","公司名稱","年月日","還原報酬指數","市場成交張數","市場週轉率")
stock_index$年月日 = ymd(stock_index$年月日)
stock_index$還原報酬指數 = as.numeric(stock_index$還原報酬指數)
stock_index$前一天還原報酬指數 = shift(stock_index$還原報酬指數, n = 1 )
stock_index$指數單日漲幅 = (stock_index$還原報酬指數 - stock_index$前一天還原報酬指數 ) / stock_index$前一天還原報酬指數
stock_index = na.omit(stock_index)
table_cor = merge(table_cor,stock_index, by = "年月日" )

table_cor$年月日[1]
table_cor$年月日[length(table_cor$年月日)]

mounth_return_function = function( panel_data_table ){ #設計一個函數，計算每個月的報酬率
                              return_value = ddply( panel_data_table , c("年月") , 
                             .fun= function(x){
                               transform( x , mounth_return_rate = with(x, (還原報酬指數[length(還原報酬指數)]- 還原報酬指數[1])/還原報酬指數[1]    ))
                                              })
  return (return_value)
}
table_cor = mounth_return_function(table_cor)


#刪除重複行
table_cor_dis = table_cor[!duplicated(table_cor$年月),] 


#畫圖
cor_picture = ggplot(table_cor_dis , aes(x = 年月日)) +
  geom_line(aes(y = cor)) + #未完待續，請參閱教學
  geom_line(aes(y = mounth_return_rate))
cor_picture
