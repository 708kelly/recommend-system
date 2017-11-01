# Top 50 
# 2017/9/22
# Kelly

# 加入時間因素去考慮
# final edit : 9/26/2017
# editor : Kelly


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
  
  file_route <- "/Users/kelly/Documents/91_intern/item2item/" # 輸出檔案的位置
  file_out <- paste(file_route,shoprank,"_new_mix_rec_6_hit_table.csv",sep="",collapse = "")
  file_out2 <- paste(file_route,shoprank,"_new_mix_ib_rec_6_hit_vector.csv",sep="",collapse = "")
  time_file <- paste(file_route,shoprank,"_time.csv",sep="",collapse = "")
  mem_file <- paste(file_route,shoprank,"_new_mem.csv",sep="",collapse = "")
  
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
  # 確認店家名一樣
  
  cycle_all <- fread(cycle_file_in,encoding = "UTF-8",
                     colClasses = c('integer','integer'))
  
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
    # 8 月
    for (runrunrun in c((length(levels(js_order_all$yymm))-1))){
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
      
      
      Rdata_out <- paste(file_route,shoprank,"_",levels(js_order_all$yymm)[point],"itemmatrix.RData",sep="",collapse = "")
      save(js_item_sim, file = Rdata_out )
      
      #attr(js_user_item, "class") <- "matrix"
      #js_user_item %>% dim
      #js_user_item_to_rec <- js_user_item[rownames(js_user_item) %in% js_order_train$MemberId,]
      
   
      
      
      
      # 4. Measure the accurancy ####
      js_bought_test <- js_order_test %>% select(MemberId,SalePageId) %>% unique  # buy same products counted as 1
      js_bought_test_mem <- js_bought_test %>% dlply(.(MemberId)) 
      js_bought_test <- js_bought_test_mem %>% llply(function(x) x$SalePageId) # buy list in test period
      
      tmp <- js_bought_test %>% lapply(function(x) {
        #x <- js_bought_test[6]
        x %<>% as.character()
        x %<>% as.list()
        Past <- lapply(1:length(x),function(i){ if (x[i] %in% colnames(js_item_sim)) x[i]})
        Past %<>% unlist
        if (!is.null(Past)){
          if (length(Past) > 1){
              #Past %in% colnames(js_user_item)
              Pref_df <- js_item_sim[,Past] 
              Pref_table <- data.table(Pref_df)
              #setnames(Pref_table, colnames(Pref_table), Past)
              #Pref_table[, rowid := rownames(js_item_sim)]
              Pref_table[, sum:= rowSums(Pref_table)]
              Pref_table %<>% as.data.frame()
              rownames(Pref_table) <- rownames(Pref_df)
              
          } else{
            Pref_table <- js_item_sim[,Past] %>% data.frame
            colnames(Pref_table) <- "sum"
            Pref_table
          }
          setorder(Pref_table, -sum)
          list(Past,Pref_table)
          
        }
      })  
      
      tmp_result <- tmp %>% lapply(function(x){ 
        result <- list(
          x[[1]],
          c(rownames(x[[2]])[1:50]),
          c(x[[2]]$sum[1:50])
        )
         })
      result_Rdata_out <- paste(file_route,shoprank,"_",levels(js_order_all$yymm)[point],"itemmatrix.RData",sep="",collapse = "")
      save(tmp_result, file = result_Rdata_out )
    }
    timeend <- Sys.time()
    timestart
    timeend 
    time <- c(timestart,timeend)
    time
    #write.csv(time,time_file)
    #rm(list=ls())#就是你QQ
  } else{ print(paste(shoprank,'WRONG!')) }
}


