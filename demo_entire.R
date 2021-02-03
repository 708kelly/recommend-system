setwd("/Users/kelly/Documents/91_intern/item2item/nine")
# source("auxiliary/mylib.R")
# if(! "dbhandle" %in% ls()) source("auxiliary/dbhandle.R")
# #mylib(c("xml2", "magrittr", "RJSONIO", "shiny"))

# if(! "prod.ls" %in% ls()) prod.ls <- sqlQuery(dbhandle(), "select Top(100) SalePageId from DataServiceDW.dbo.DimSalePage where ShopId = 2202 and SalePageIsClosed = 0 and SalePageValidFlag = 1 and IsGift = 0 and IsSalePageGift=0")


library(magrittr);library(shiny);library(xml2);library(RJSONIO);library(Matrix);library(curl);library(data.table)

# 改python路徑
Sys.setenv(PATH = paste("/Users/kelly/anaconda/bin", Sys.getenv("PATH"),sep=":"))
system("python --version")

#Read shop csv ####
file_in <- "shop_inf.csv"
shop_df <- read.table(file_in,encoding = "UTF-8", header = TRUE, sep = ",",stringsAsFactors=FALSE)

# Read price csv ###
price_file_in <- "price_10.csv"
price_df <- read.table(price_file_in,encoding = "UTF-8", header = TRUE, sep = ",",stringsAsFactors=FALSE)
#price_df[price_df$SalePageId == 1 , 2]

# id #### 
shop_web <- function(x){
  shop_df[shop_df$shopid == x,3]
}

#檢查現在是否有在販賣#
check <- function(shopid,x){
  x %<>% as.integer()
  url <- paste(shop_web(shopid),"/webapi/SalePageStatus/GetSalePageStatus/",x,sep="",collapse = "")
  a <- curl(url)
  open(a, "rb")
  c <- readLines(a, warn=FALSE)
  if (c == "\"Normal\""){
    x
  }else{
    x <- "error"
  }
  x
  
}
# 從網路抓照片 ##
# get.prod.img <- function (input.prod.id) {paste(shop_web(shopid),"/SalePage/Index/", input.prod.id,sep="",collapse = "") %>% read_html() %>%
#     xml_find_all(., "//script") %>% "["(11) %>% xml_text() %>%
#     strsplit(., "= ") %>% "[["(1) %>% "["(3) %>% strsplit(., ";") %>% "[["(1) %>% "["(1) %>%
#     fromJSON(.) %>% "[["("ImageList") %>% unlist(.) %>% "[["("PicUrl")}

get.prod.img.s <- function (shopid,input.prod.id) {
  paste(shop_web(shopid),"/webapi/salepage/GetSalepageDataByIds?ids=",input.prod.id,sep="",collapse = "") %>% read_html() %>%
     xml_text() %>% fromJSON() %>% "[["(1) %>% lapply(function(x){ x %>%  "[["("PicUrl")})}

get.prod.price <- function (shopid,input.prod.id) {
  paste(shop_web(shopid),"/webapi/salepage/GetSalepageDataByIds?ids=",input.prod.id,sep="",collapse = "") %>% read_html() %>%
    xml_text() %>% fromJSON() %>% "[["(1) %>% lapply(function(x){ x %>%  "[["("Price")})}
get.prod.title <- function (shopid,input.prod.id) {
  paste(shop_web(shopid),"/webapi/salepage/GetSalepageDataByIds?ids=",input.prod.id,sep="",collapse = "") %>% read_html() %>%
    xml_text() %>% fromJSON() %>% "[["(1) %>% lapply(function(x){ x %>%  "[["("Title")})}




pref <- function(shopid,x){

  filename <- paste(shopid,'_201709','itemmatrix.Rdata',sep="",collapse = "")
  load(filename)
  
  x <- strsplit(x,',')[[1]] %>% as.list()
  Past <- lapply(1:length(x),function(i){ if (x[i] %in% colnames(js_item_sim)) x[i]})
  if(is.null(Past) ){print("Wrong!")}
  # 計算排行 ####
  Past %<>% unlist
  if (!is.null(Past)){
    if (length(Past) > 1){
      #Past %in% colnames(js_user_item)
      
      Pref_df <- js_item_sim[,Past]
      Pref_table <- data.table(as.matrix(Pref_df))
      #setnames(Pref_table, colnames(Pref_table), Past)
      #Pref_table[, rowid := rownames(js_item_sim)]
      Pref_table[, SalePage:=rownames(Pref_df)]
      
      cols <- which(!colnames(Pref_table) %in% "SalePage")
      Pref_table[, sum:= rowSums(.SD), .SDcol=cols]

      
    } else{
      Pref_table <- js_item_sim[,Past] %>% as.matrix %>% data.table
      setnames(Pref_table,c('sum'))
      Pref_table[, SalePage:=rownames(js_item_sim)]
      #colnames(Pref_table) <- "sum"
      
      #Pref_table
    }
    
    setorder(Pref_table, -sum)
    
    Pref <- Pref_table[1:36,.(sum,SalePage)] 
    Pref %<>% as.data.frame()
    rownames(Pref) <- Pref$SalePage
    Pref[,-2, drop=FALSE]
  }
}

all_pref <- function(shopid,x){

  item_df <- pref(shopid,x)
  item_df$situation <- lapply(rownames(item_df),function(x){check(shopid, x)})
  item_df$price <- lapply(rownames(item_df),function(x){price_df[price_df$SalePageId == x , 2]}) # 這邊好像要改成join比較好
  rec <- item_df[item_df$situation != 'error',]
  python <- paste("python ./fp/rec_algorithm.py",shopid,x,sep=" ")
  system(python)
  fp_rec_path <- paste("./fp/fp_result/shopid_",shopid,"_rec_items.csv",sep="")
  fp_rec <- read.table(fp_rec_path,fill=TRUE)
  rec$fp <- ifelse(rownames(rec) %in% fp_rec[,1], 1, 0)
  #total grade
  total <- get.prod.price(shopid,x) %>% unlist %>% sum
  surplus <- 599 - total
  rec$priceweight <- with(rec,exp(-(abs(as.numeric(price) - surplus)/surplus)))
  rec$grade <- with(rec,(0.3*priceweight+0.4*fp+0.3*sum))
  setorder(rec,-grade)
  # 儲存
  write.csv(rownames(rec),file="rec_thing.csv")
  rec
}

# Real code ####


ui <- pageWithSidebar(
  headerPanel("recommender demo"),
  sidebarPanel(
    textInput("shop.id", label = h4("Input: ShopId"), value = "2131"),
    textInput("prod.id", label = h4("Input: SalePageId"), value = "2139830"),
    #htmlOutput("picture"),
    submitButton("Update View", icon("refresh"))
    # htmlOutput("picture1")
  ),
  mainPanel(
    # Use imageOutput to place the image on the page
    htmlOutput("picture")
  )
)
#shopid <- reactiveValues()

server <- function(input, output, session) {
  
  url <- reactive({get.prod.img.s(input$shop.id,input$prod.id)})
  
  title <- reactive({get.prod.title(input$shop.id,input$prod.id)})
  #input$shop.id
  
  rec_product <- reactive({paste(unlist(all_pref(input$shop.id,input$prod.id)$situation[1:12]), collapse=',')})
  urls <- reactive({get.prod.img.s(input$shop.id,rec_product())})
  titles <- reactive({get.prod.title(input$shop.id,rec_product())})
  price <- reactive({get.prod.price(input$shop.id,rec_product())})
  web <- reactive({paste(shop_web(input$shop.id),"/SalePage/Index/", as.list(strsplit(input$prod.id,',')[[1]]),sep="")})
  price_main <- reactive({get.prod.price(input$shop.id,input$prod.id)})
  output$picture <- renderText({
    c(paste("<div style= 'Display:inline-block;width: 250px; height: 250px'>",'<a href ="',c(web()),'">','<img height = 250, width = 250, src="', c(url()),'"> ',"<h4 style ='color:red'> $ ",c(price_main())," </h4><h4> ",c(title()),"</h4></a>","</div>",sep="",collapse = ""),"<br>",
      #"<h6>",,"</h6></div><br>",sep="",collapse = "") ,
      paste("<div style= 'Display:inline-block;width: 150px; height: 150px' >",'<img height = 150, width = 150, src="', c(urls()), '">',"<h5 style ='color:red'>",' $ ',c(price()),"</h5><h5> ",c(titles()),"</h5>","</div>",collapse='',sep="")
      
      #'<br><br>',
      #paste('<img height = 75, width = 75, src="', c(urls2()), '">')
    )})
  
}

shinyApp(ui, server)
