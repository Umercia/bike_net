#bike_net
# Web scrapping bikes (with shimano XP) in Sweden and Denmark

library(rvest)  # hmtl parsing package

blocket_update <- function(blocket_url,bike_list){
      
      html_block <- read_html(blocket_url)
      
      #extract ID
      sub_block <- html_nodes(html_block, ".media")
      ID <- html_attr(x = sub_block,name = "id")
      #ID <- sub(pattern = "item_",x = ID, replacement = "")
      i <- 1
      
      while (!(ID[i] %in% bike_list$id) & !(i > length(ID))){  # a checker
            
            # title & link    
            CSS_selector <- paste("#",ID[i]," > div:nth-child(2) > h1:nth-child(2) > a:nth-child(1)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            title <- html_attr(x = sub_block,name = "title")
            link <- html_attr(x = sub_block,name = "href")
            
            # date
            CSS_selector <- paste("#",ID[i]," > div:nth-child(2) > header:nth-child(1) > time:nth-child(2)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            date <- html_attr(x = sub_block,name = "datetime")
            date <- as.Date(date)   
            
            # image link and download
            CSS_selector <- paste("#",ID[i]," > a:nth-child(1) > img:nth-child(2)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            img_link <- html_attr(x = sub_block,name = "src")
            
            # special parsing because for the end of the list the link is on an other attribut called "longdesc".
            # it is also possible that no image was provided (img_link = NULL)
            if(length(img_link) != 0){
                  if( img_link == "/img/transparent.gif"){
                        img_link <- html_attr(x = sub_block,name = "longdesc")
                  }
                  download.file(url = img_link, 
                                destfile = paste("./pictures/",date, "_blocket_", ID[i], ".jpg",sep = ""), 
                                mode="wb")
            }else {img_link <- "NA"}
            
            #add row
            row_to_add <- data.frame( site = c("blocket"),
                                      id = ID[i],
                                      title = title,
                                      link = link,
                                      img_link = img_link,
                                      date = date)
            
            bike_list <- rbind(bike_list,row_to_add)
            i <- i + 1
      }
      bike_list
}

dba_update <- function(dba_url,bike_list){
      
      html_block <- read_html(dba_url)
      
      # Extract ID (in dba the ID is found on child element, so the code structure change a little bit. 
      sub_block <- html_nodes(html_block, "tr.dbaListing > td:nth-child(2) > div:nth-child(3) > a:nth-child(1)")
      ID_ <- html_attr(x = sub_block,name = "href")
      ID_ <- substr(ID_,nchar(ID_)-13,nchar(ID_)-1)
      
      i <- 1
      j <- 1
      
      while (!(ID_[j] %in% bike_list$id) & !(j > length(ID_))){
            
            CSS_selector <- paste("tr.dbaListing:nth-child(",i,") > td:nth-child(2) > div:nth-child(3) > a:nth-child(1)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            
            # In dba website, We cannot call the block directly by the ID, we need to iterate one by one on i, and check if the block is not empty.
            if(length(sub_block) != 0){
                  
                  # link 
                  link <- html_attr(x = sub_block,name = "href") 
                  
                  # title
                  splited_string <- strsplit(link, "/")
                  title <- splited_string[[1]][4]
                  
                  # ID
                  ID <- splited_string[[1]][5]
                  
                  # date
                  CSS_selector <- paste("tr.dbaListing:nth-child(",i,") > td[title=Dato]",sep = "" )  #CSS still works without quotes around "Dato"..
                  sub_block <- html_nodes(html_block, CSS_selector)
                  
                  date <- html_text(sub_block) 
                  date <- gsub(pattern = "^\\s+|\\s+$",replacement =  "", x = date) # the magic to remove leading or trailing whitespace
                  date <- 
                  
                  if(   date == "I dag"){
                        date <- as.Date(Sys.time())
                  }else if(date == "I går"){
                        date <- as.Date(Sys.time())-1
                  }else{
                        date <- gsub(pattern = "\\.",replacement = "",x = date)  #"." is a metacharacter --> escape needed \\
                        date <- paste(date, format(Sys.time(), "%Y"),sep = " ") 
                        date <- strptime(date, "%d %B %Y")  #to get 
                        date <- as.Date(date)
                  }
                  
                  # image link and download
                  CSS_selector <- paste("tr.dbaListing:nth-child(",i,") > td:nth-child(1) > div:nth-child(1) > a:nth-child(1) > div:nth-child(1)",sep = "" )  #CSS still works without quotes around "Dato"..
                  sub_block <- html_nodes(html_block, CSS_selector)
                  img_link <- html_attr(x = sub_block,name = "data-original")
                  print(img_link)
                  download.file(url = img_link, 
                                destfile = paste("./pictures/",date, "_dba_", ID, ".jpg",sep = ""), 
                                mode="wb")
                  
                  # Add row to bike_list
                  row_to_add <- data.frame( site = c("dba"),
                                            id = ID,
                                            title = title,
                                            link = link,
                                            img_link = img_link,
                                            date = date)
                  
                  bike_list <- rbind(bike_list,row_to_add)
                  j <- j + 1
            }
            i <- i + 1
      }
      bike_list
}

secondhandbikes_update <- function(secondhandbikes_url,bike_list){
      
      html_block <- read_html(secondhandbikes_url)
      
      #Extract ID table 
      sub_block <- html_nodes(html_block, "div.serp-bike > a.bike-link")
      ID <- html_attr(x = sub_block,name = "href")
      ID <- substr(ID,nchar(ID)-3,nchar(ID))
      
      i <- 1
      
      while (!(ID[i] %in% bike_list$id) & !(i > length(ID))){
            
                  # link 
                  CSS_selector <- paste("div.serp-bike:nth-child(",i,") > a:nth-child(1)",sep = "" )
                  sub_block <- html_nodes(html_block, CSS_selector)
                  link <- html_attr(x = sub_block,name = "href") 
                  link <- paste("https://secondhandbikes.dk", link,sep = "")
                  
                  # title
                  CSS_selector <- paste("div.serp-bike:nth-child(",i,") > a:nth-child(1) > div:nth-child(2) > span:nth-child(1)",sep = "" )
                  sub_block <- html_nodes(html_block, CSS_selector)
                  title <- html_text(sub_block) 
                  title <- gsub(pattern = "^\\s+|\\s+$",replacement =  "", x = title)
                  
                  # image link
                  CSS_selector <- paste("div.serp-bike:nth-child(",i,") > a:nth-child(1) > div:nth-child(1) > img:nth-child(1)",sep = "" )
                  sub_block <- html_nodes(html_block, CSS_selector)
                  img_link <- html_attr(x = sub_block,name = "src")
                  img_link <- paste("https://secondhandbikes.dk", img_link,sep = "")
                  
                  # date
                  date <- strsplit(x = img_link, split = "Thumbs/")[[1]][2]
                  date <- substr(x = date,start = 1,stop = 8)
                  date <- strptime(date, "%y-%m-%d")
                  date <- as.Date(date)
                  
                  #download image
                  download.file(url = img_link, 
                                destfile = paste("./pictures/",date, "_2HB_", ID[i], ".jpg",sep = ""), 
                                mode="wb")
                  
                  # Add row to bike_list
                  row_to_add <- data.frame( site = c("secondhandbikes"),
                                            id = ID[i],
                                            title = title,
                                            link = link,
                                            img_link = img_link,
                                            date = date)
                  
                  bike_list <- rbind(bike_list,row_to_add)
            
                  i <- i + 1
      }
      bike_list
}

facebook_update <- function(facebook_url,bike_list){
      
      library(httr)
      facebook_url <- "https://www.facebook.com/groups/616176611841772/for_sale_search/?forsalesearchtype=for_sale&availability=available&query=cykel"
      
      html_block <- GET(facebook_url, authenticate("cleremaurice@yahoo.fr","4lB4torFa"))
      html_block <- read_html(html_block)

      # Extract ID table 
      #mall_post_1177103902415704 > div:nth-child(1) > div:nth-child(1) > a:nth-child(1)
      
      sub_block <- html_nodes(html_block, "div:nth-child(1) > div:nth-child(1) > a:nth-child(1)")
      ID <- html_attr(x = sub_block,name = "href")
      ID <- substr(ID,nchar(ID)-3,nchar(ID))
      
      i <- 1
      
      while (!(ID[i] %in% bike_list$id) & !(i > length(ID))){
            
            # link 
            CSS_selector <- paste("div.serp-bike:nth-child(",i,") > a:nth-child(1)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            link <- html_attr(x = sub_block,name = "href") 
            link <- paste("https://secondhandbikes.dk", link,sep = "")
            
            # title
            CSS_selector <- paste("div.serp-bike:nth-child(",i,") > a:nth-child(1) > div:nth-child(2) > span:nth-child(1)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            title <- html_text(sub_block) 
            title <- gsub(pattern = "^\\s+|\\s+$",replacement =  "", x = title)
            
            # image link
            CSS_selector <- paste("div.serp-bike:nth-child(",i,") > a:nth-child(1) > div:nth-child(1) > img:nth-child(1)",sep = "" )
            sub_block <- html_nodes(html_block, CSS_selector)
            img_link <- html_attr(x = sub_block,name = "src")
            img_link <- paste("https://secondhandbikes.dk", img_link,sep = "")
            
            # date
            date <- strsplit(x = img_link, split = "Thumbs/")[[1]][2]
            date <- substr(x = date,start = 1,stop = 8)
            date <- strptime(date, "%y-%m-%d")
            date <- as.Date(date)
            
            #download image
            download.file(url = img_link, 
                          destfile = paste("./pictures/",date, "_2HB_", ID[i], ".jpg",sep = ""), 
                          mode="wb")
            
            # Add row to bike_list
            row_to_add <- data.frame( site = c("secondhandbikes"),
                                      id = ID[i],
                                      title = title,
                                      link = link,
                                      img_link = img_link,
                                      date = date)
            
            bike_list <- rbind(bike_list,row_to_add)
            
            i <- i + 1
      }
      bike_list
}
















































# archives previous pictures
list_of_files <- list.files("./pictures/", "*.*")
list_of_files <- paste("./pictures/",list_of_files,sep = "")
file.copy(from = list_of_files, to = "./archives/") #seems long process...
file.remove(list_of_files)

# read previous results from a file
bike_list <- read.csv(file = "bike_list.csv")
bike_list$date <- as.Date(bike_list$date)
bike_list$id <- gsub(pattern = "^\\s+|\\s+$",replacement =  "", x = bike_list$id)  #just in case

# blocket: filter -> All sweden + cyclar + (XT or Btwin) 
bike_list <- blocket_update("https://www.blocket.se/hela_sverige?q=Btwin+OR+XT&cg=6060&w=3&st=s&c=&f=p&ca=23&is=1&l=0&md=th",bike_list)

# dba: filter -> All ?Denmark + cykler + XT
bike_list <- dba_update("http://www.dba.dk/cykler/?soeg=xt&fra=privat&sort=listingdate-desc",bike_list)

# secondHandBikes: filter: Men's bike
bike_list <- secondhandbikes_update("https://secondhandbikes.dk/Search/Bike?type=1&pricemin=0&pricemax=99999&city=-1&page=1",bike_list)

# save result of the updated bike_list in a file
write.csv(x = bike_list,file = "bike_list.csv",row.names = FALSE)










