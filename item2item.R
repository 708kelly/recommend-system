## 不直接去拿會員資料，直接算
## 加進購物週期的向度

## 扣除在購買當月為購買的商品，透過最後一次購買紀錄，假設之後商品已下架
## 10/5

# Setting--- ####
load("/Users/kelly/Documents/91_intern/top50_result.RData")
save.image("/Users/kelly/Documents/91_intern/top50_result.RData")
rm(list=ls())
setwd("/Users/kelly/Documents/91_intern/totalyear")
library(magrittr);library(plyr);library(stringr);library(data.table);library(lubridate);library(sqldf);library(xlsx);library(dplyr);library(sigmoid)

# 更改要跑的店家排名
for (shoprank in c(30)){
  timestart <- Sys.time() # record time
  print(shoprank) 
  
  # 0. Files's names ####
  print(paste("0","------",Sys.time()))
  file_in <- paste(shoprank,"_total_data.csv",sep="",collapse = "") # 檔案位置在setwd
  mem_file_in <- paste("/Users/kelly/Documents/91_intern/top50_file/",shoprank,"_mem.csv",sep="",collapse = "")
  cycle_file_in <- '/Users/kelly/Documents/91_intern/totalyear/total_customercycle.csv'
  
  file_route <- "/Users/kelly/Documents/91_intern/total_data_result/" # 輸出檔案的位置
  file_out <- paste(file_route,shoprank,"_new_mix_rec_6_hit_table.csv",sep="",collapse = "")
  file_out2 <- paste(file_route,shoprank,"_new_mix_ib_rec_6_hit_vector.csv",sep="",collapse = "")
  time_file <- paste(file_route,shoprank,"_time.csv",sep="",collapse = "")
  mem_file <- paste(file_route,shoprank,"_new_mem.csv",sep="",collapse = "")
  
  "" ## Preference
  # 1. Read csv and VARs production ####
  print(paste("1","------",Sys.time()))
  js_order_all <- fread(file_in,encoding = "UTF-8",
                        colClasses = c('integer','integer','integer','character',
                                       'integer','integer'))
  js_order_all %>% str
  # 會員的註冊時間
  mem_all <- fread(mem_file_in,encoding = "UTF-8",
                   colClasses = c('integer','character','integer'))
  mem_all %>% str
  
  cycle_all <- fread(cycle_file_in,encoding = "UTF-8",
                   colClasses = c('integer','integer'))
  # 確認店家名一樣
  if (mem_all$SalesOrderDM_ShopId[1] == js_order_all$ShopID[1] )
  {
    print(paste("店家編號：", js_order_all$ShopID[1]))
    
    # only year and date
    js_order_all$yymm <- as.numeric((js_order_all$DateId %>% substr(.,1,6)))
    table(js_order_all$yymm) %>% as.data.frame()
    # Var1  Freq
    # 1  201605 10021
    # 2  201606 12684
    # 3  201607  8831
    # ....
    # 12 201704  8437
    js_order_all$yymm %<>% factor #轉成因子 201608,2016,09......
    js_order_all$DateId <- as.Date(as.character(js_order_all$DateId), "%Y%m%d")
    
    mem_all$MinOrderDate <- as.Date(as.character(mem_all$MinOrderDate), "%Y%m%d")
    
    
    # 2. Split 'train' & 'test' period ####
    print(paste("2","------",Sys.time()))
    js_ib_rec_6_hit_df <- list()
    js_ib_hit_ratio <- list()
    pp_ib_hit_ratio <- list()
    js_rr <- vector()
    js_aa <- vector()
    js_bb <- vector()
    js_cc <- vector()
    js_dd <- vector()
    pref <- c(0,0,0,0,0,0)
    
    # ratio
    tt_ratio <- vector()
    oo_ratio <- vector()
    mm_ratio <- vector()
    pp <- vector()
    # 4:length(levels(js_order_all$yymm))
    # 資料到9月，但不完整
    for (runrunrun in c((length(levels(js_order_all$yymm))-8):length(levels(js_order_all$yymm)))){
      point <- runrunrun # runrunrun為測試的月份
      test_yymm <- runrunrun
      
      levels(js_order_all$yymm)[runrunrun] # 離散值
      x <- levels(js_order_all$yymm)[1:(test_yymm-1)] # train
      y <- levels(js_order_all$yymm)[test_yymm]
      
      js_order_all_before_old <- js_order_all[yymm %in% x] 
      js_order_test_real <- js_order_all[yymm %in% y]
      
      js_member_train <- js_order_all_before_old$MemberId %>% unique
      js_member_test <- js_order_test_real$MemberId %>% unique
      js_all_mem <- c(js_member_test,js_member_train) %>% unique %>% sort
      
      # [Filter] train period: buy items > 1
      js_all_before_member_table <- table(js_order_all_before_old$MemberId) %>% as.data.frame()
      js_all_before_member_2 <- js_all_before_member_table$Var1[js_all_before_member_table$Freq>1] %>% as.character %>% as.integer
      js_order_all_before <- js_order_all_before_old[MemberId %in% js_all_before_member_2] # training set for item sim
      
      js_member_along <- intersect(js_member_test,js_all_before_member_2) # test&train(>1)的交集
      #js_member_along %>% length
      
      js_order_train <- js_order_all_before_old[MemberId %in% js_member_along]# training set (test,train的交集)
      js_order_test <- js_order_test_real[MemberId %in% js_member_along] # testing set
      
      
      
      #時間因素#### 要記得輸入
      cycle <- cycle_all[ShopID == js_order_all$ShopID[1],2] %>% as.integer()
      time_max <- js_order_all_before$DateId %>% max
      js_order_all_before$time_minus <- with(js_order_all_before,as.integer(time_max-js_order_all_before$DateId) )
      js_order_all_before$weight <- with(js_order_all_before,sigmoid(js_order_all_before$time_minus,k=(-3*log(2)/cycle),x0=(3*cycle)))
      
      # test 2
      js_test_buy <- js_order_test %>% dlply(.(MemberId)) # 以memberid切
      js_test_buy_num <- lapply(js_test_buy,nrow) %>% unlist %>% as.numeric
      table(js_test_buy_num>6)
      
      
      
      # 3. item_based recommendation ####
      # 3.1 Create a User-Item matrix ####
      print(paste("3.1 開始建立User-Item Matrix","------",Sys.time()))
      
      # [9] 1169  187
      
      #js_user_item <- table(js_order_all_before$MemberId,js_order_all_before$SalePageId) # train和test的member交集
      #attr(js_user_item, "class") <- "matrix"
      #js_user_item %>% dim
      # [9] 9100  198
      # Time factor
      #js_user_item_to_rec <- table(js_order_train$MemberId,js_order_train$SalePageId) # train裡member所有資料
      #attr(js_user_item_to_rec, "class") <- "matrix"
      #js_user_item_to_rec %>% dim
      
      
      js_user_item <- with(js_order_all_before,{
        js_user_item_weight <- tapply(weight,list(MemberId,SalePageId),sum,na.rm=TRUE)
        js_user_item_weight[is.na(js_user_item_weight)] <- 0
        js_user_item_weight
      })
      #attr(js_user_item, "class") <- "matrix"
      js_user_item %>% dim
      js_user_item_to_rec <- js_user_item[rownames(js_user_item) %in% js_order_train$MemberId,]
      
      # 3.3 Calc item-item similarity matrix ####
      print(paste("3.3 Item-Item Matrix","------",Sys.time()))
      #js_user_item[is.na(js_user_item)] <- 0
      js_item_sim_1 <- crossprod(js_user_item)
      
      d <- diag(js_item_sim_1)
      
      sum <- t(as.matrix(d))
      len <- sqrt(crossprod(sum))
      cos <- js_item_sim_1 / len
      cos [cos == 1] <- 0
      js_item_sim <- cos
      
      js_item_sim %>% summary
      js_item_sim %>% dim
      
      colnames(js_item_sim) <- colnames(js_user_item)
      
      
      
      # 3.4 Calc pref rank ####
      print(paste("3.4","------",Sys.time()))
      # 這部可能不用做ㄌ 沒用ㄌ
      js_small_item_sim <- js_item_sim[colnames(js_item_sim) %in% colnames(js_user_item_to_rec),]
      
      # ui * ii
      js_rec <- list()
      us <- js_user_item_to_rec %*% js_small_item_sim
      us %>% dim
      
      js_rec <- lapply(1:nrow(us), function(i) {
        item <- data.frame(SalePageId = colnames(us)[order(-us[i, ])[1:6]] %>% as.integer,
                           Pref_score = sort(us[i,],decreasing = TRUE)[1:6],stringsAsFactors=FALSE)
        
      })
      
      names(js_rec) <- rownames(js_user_item_to_rec) # lable MemberId
      js_rec_product <- llply(js_rec,function(x) x$SalePageId[1:6]) # first 6 to rec
      Pref_summary <- js_rec %>% laply(function(x) mean(x$Pref_score)) %>% summary
      
      print( "平均偏好權重")
      print( Pref_summary )
      
      
      
      #pref_list <- Pref_summary %>% as.list %>% laply(function(x) x) 
      #pref <- cbind(pref,pref_list)
      #colnames(pref)[point-2] <- paste("pref",point-3)
      
      
      
      # 5. Measure the accurancy ####
      
      
      
      # 5.1.1 Pop rec ####
      # 先測全部 train
      print(paste("5.1.1","------",Sys.time()))
      js_item_member_2 <- js_order_all_before %>% dlply(.(MemberId)) # 在training 買超過兩次
      # 測所有test
      js_bought_test_real <- js_order_test_real %>% select(MemberId,SalePageId) %>% unique
      js_bought_test_real_mem <- js_bought_test_real %>% dlply(.(MemberId)) 
      js_bought_test_real <- js_bought_test_real_mem %>% llply(function(x) x$SalePageId)
      # 測 TRAIN >2 + TEST
      js_bought_test <- js_order_test %>% select(MemberId,SalePageId) %>% unique  # buy same products counted as 1
      js_bought_test_mem <- js_bought_test %>% dlply(.(MemberId)) 
      js_bought_test <- js_bought_test_mem %>% llply(function(x) x$SalePageId) # buy list in test period
      
      js_bought_train <- llply(js_item_member_2,function(x) x$SalePageId)
      
      # 熟客比例 ####
      last_day <- js_order_all_before_old$DateId %>% max
      mem_part <- mem_all[MinOrderDate <= last_day]
      old_mem_num <- intersect(mem_part$SalesOrderDM_MemberId,names(js_bought_test_real_mem)) %>% length
      o_ratio <- length(js_bought_test_mem) / old_mem_num 
      
      
      # to see if buy in train/test period
      # all people in 4 months
      
      # 5.1.2 ####
      print(paste("5.1.2","------",Sys.time()))
      #list()
      
      # FUN
      T_F_NA <- function(x){
        sum <- sum(x)
        if (is.na(sum)) NA
        else if (sum == 0) 0
        else if (sum > 0) 1
      }
      
      #比例
      m_ratio <- length(js_bought_test_mem) / length(js_bought_test_real_mem)
      
      # test 2
      js_test_buy_num_6 <- js_test_buy_num
      js_test_buy_num_6[js_test_buy_num > 6] <- 6
      
      # 5.2.1 Item_based ####
      print(paste("5.2.1","------",Sys.time()))
      js_bought_train_if <- list()
      
      
      js_bought_train_if <- js_rec_product %>% names %>% lapply(function(x) js_rec_product[[x]] %in% js_bought_train[[x]])
      js_bought_test_if <- js_rec_product %>% names %>% lapply( function(x) js_rec_product[[x]] %in% js_bought_test[[x]]) 
      
      js_rec_product %>% names %>% lapply( function(x) js_rec_product[[x]] %in% js_bought_test)
      js_rec_product_result <- list()
      for (i in 1:length(js_rec_product)){
        js_rec_product_result[[i]] <- cbind(js_rec[[i]]$SalePageId[1:6],
                                            js_rec[[i]]$Pref_score[1:6],
                                            js_bought_train_if[[i]],
                                            js_bought_test_if[[i]])
        
      }
      
      js_rec_product_result_df <- js_rec_product_result %>% ldply
      js_rec_product_result_df %>% dim
      js_rec_product_result_df$MemberId <- rep(names(js_rec), each=6)
      js_rec_product_result_df %>% head
      colnames(js_rec_product_result_df)[1:4] <- c('SalePageId','Cos_similarity','if_bought_before',"if_buy_in_test")
      js_rec_product_result_df$Rank <- rep(1:6,nrow(js_rec_product_result_df)/6)
      js_rec_product_result_df %>% head
      dim(js_rec_product_result_df)
      js_rec_product_result_df %>% head
      
      # 5.2.2 calc the %%%% ####
      print(paste("5.2.2","------",Sys.time()))
      bought_test_in_train <- list()
      js_a <- js_bought_test_if %>% lapply(function(x) ifelse(sum(x)>0,1,0)) %>% unlist
      js_a %>% table
      100*mean(js_a)
      for (i in 1:length(js_bought_test_if)){
        bought_test_in_train[[i]] <- js_bought_test_if[[i]] & js_bought_train_if[[i]]
      } 
      bought_test_not_in_train <- list()
      for (i in 1:length(js_bought_test_if)){
        bought_test_not_in_train[[i]] <- js_bought_test_if[[i]] & !js_bought_train_if[[i]]
      } 
      js_bought_num <- js_bought_test_if %>% lapply(sum) %>% unlist
      bought_test_in_train %<>% lapply(sum) %>% unlist
      bought_test_not_in_train %<>% lapply(sum) %>% unlist
      js_b <- sum(js_bought_num/js_test_buy_num_6)/length(js_rec)
      js_c <- sum(bought_test_in_train/js_test_buy_num_6)/length(js_rec)
      js_d <- sum(bought_test_not_in_train/js_test_buy_num_6)/length(js_rec)
      
      
      # 6.2 Item-based Rec Result #####
      print(paste("6.2","------",Sys.time()))
      js_rec_product_result_df %>% colnames
      table(js_rec_product_result_df$if_bought_before,js_rec_product_result_df$if_buy_in_test)
      table_js_a <- table(js_rec_product_result_df$if_bought_before,js_rec_product_result_df$if_buy_in_test)
      js_res <- c(paste("推薦有買過商品的機率： ",round(100*(table_js_a[2,2]+table_js_a[2,1])/(length(js_rec)*6),digits=2),"%",sep = ""),
                  paste("熟客佔比： ",round(100*mean(o_ratio),digits=2),"%",sep = ""),
                  paste("會員命中率： ",round(100*mean(js_a),digits=2),"%",sep = ""),
                  paste("商品命中率： ",round(100*mean(js_b),digits=2),"%",sep = ""),
                  paste("(回購)商品命中率： ",round(100*mean(js_c),digits=2),"%",sep = ""),
                  paste("(新購)商品命中率： ",round(100*mean(js_d),digits=2),"%",sep = ""))
      js_res
      
      # 7.4 end ####
      levels(js_order_all$yymm)[runrunrun]
      print("********************************")
      print(levels(js_order_all$yymm)[runrunrun])
      
      print("Item_based Rec result")
      print(js_res)
      
      print(paste('test人數:',length(js_bought_test_mem)))
      print(Sys.time())
      
      # 8 data storage ####
      js_ib_rec_6_hit_df[[point-3]] <- js_rec_product_result_df
      js_ib_hit_ratio[[point-3]] <- js_res
      names(js_ib_rec_6_hit_df)[point-3] <- levels(js_order_all$yymm)[point]
      names(js_ib_hit_ratio)[point-3] <- levels(js_order_all$yymm)[point]
      
      
      js_rr[point-3] <- round(100*(table_js_a[2,2]+table_js_a[2,1])/(length(js_rec)*6),digits=2)
      js_aa[point-3] <- round(100*mean(js_a),digits=2)
      js_bb[point-3] <- round(100*mean(js_b),digits=2)
      js_cc[point-3] <- round(100*mean(js_c),digits=2)
      js_dd[point-3] <- round(100*mean(js_d),digits=2)
      
      
      
      mm_ratio[point-3] <- round(100*mean(m_ratio),digit=2)
      oo_ratio[point-3] <- round(100*mean(o_ratio),digit=2)
      pp[point-3] <- length(js_bought_test_mem)
      
      #write.csv(data.frame(js_rr,js_aa,js_bb,js_cc,js_dd),file_out)
      #write.csv(data.frame(mm_ratio,oo_ratio),mem_file)
      print("***************************************")
      
    }
    js_ib_rec_6_hit_table <- data.frame(js_rr,js_aa,js_bb,js_cc,js_dd)
    mem_ratio <- data.frame(mm_ratio,oo_ratio,pp)
    colnames(js_ib_rec_6_hit_table) <- c( "item 重複推薦率","item 會員命中率","item 商品命中率","item（回購）商品命中率",
                                          "item（新購）商品命中率")
    colnames(mem_ratio) <- c("item測試佔全部的test比例(會員）","熟客佔比","test人數")
    rownames(js_ib_rec_6_hit_table) <- levels(js_order_all$yymm)[4:length(levels(js_order_all$yymm))]
    rownames(mem_ratio) <- levels(js_order_all$yymm)[4:length(levels(js_order_all$yymm))]
    js_ib_rec_6_hit_vector <- c(length(unique(js_order_all$SalePageId)),
                                length(unique(js_order_all$MemberId)),
                                mean(tt_ratio),mean(mm_ratio),
                                mean(js_rr),mean(oo_ratio),mean(js_aa),mean(js_bb),mean(js_cc),mean(js_dd))
    #write.csv(js_ib_rec_6_hit_table,file_out)
    #write.csv(data.frame(js_ib_rec_6_hit_vector),file_out2)
    #write.csv(data.frame(mem_ratio),mem_file)
    #print(pref)
    print(js_ib_rec_6_hit_table)
    # print(js_ib_rec_6_hit_df)
    # print(pp_ib_rec_6_hit_df)
    print(js_ib_hit_ratio)
    timeend <- Sys.time()
    timestart
    timeend 
    time <- c(timestart,timeend)
    time
    #write.csv(time,time_file)
    rm(list=ls())#就是你QQ
  } else{ print(paste(shoprank,'WRONG!')) }
}

