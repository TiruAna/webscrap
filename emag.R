library(rvest)
library(reshape2)
library(dplyr)
library(httr)


set_config(use_proxy("http://proxy.insro.local", 8080))

# selectori
css     <- list()

## navigare
css$navigare$categorii              <- "h3.ad-department-icon"
css$navigare$categorii.subcategorii <- "div#department-expanded > ul > li > ul >li >a"
css$navigare$nextPage <- "ul.pagination > li > a"
css$navigare$productUrl <- "div#card_grid > div > div > div > div > div >a.js-product-url"

emag_get_subcategories <- function(){
  
  # TODO 
  # test connection

  emag.site.categorii       <- "http://www.emag.ro/all-departments"
  emag.site.categorii.html  <- read_html(GET(emag.site.categorii))
  
  links.categorii.text      <- emag.site.categorii.html %>% html_nodes(css$navigare$categorii) %>% html_text()
  links.categorii.text      <- trimws(links.categorii.text)  
  print("Select one of the following by providing the index: \n") 
  print(links.categorii.text)
  index <- readLines(con = stdin(), n = 1)
  index <- as.integer(index)
  xpath <- paste0("//div/h3[contains(@class,  'ad-department-icon') and contains(.,'", links.categorii.text[index],"')]/following-sibling::ul/li/ul/li/a")

  links.categorii <- emag.site.categorii.html %>% html_nodes(xpath = xpath) %>% html_attr("href")
  links.categorii <- strsplit(links.categorii, "/")
  links.categorii <- sapply(links.categorii, 
                            function(x){
                              if(length(x) == 3){
                              y <- trimws(x[2])
                              } else {
                              y  <- paste0(x[2], "/", x[3], collapse = " ")}
                              return(y)})
  nume.links.categorii      <- emag.site.categorii.html %>% html_nodes(xpath = xpath) %>% html_text()
  names(links.categorii)    <- nume.links.categorii
  
  #### next page ####
  #### No CSS3 for detecting the child before the last one ####
  #### Count all children and select the last one - 1 ###########
  #### 
   
  emag.links <- list()
   for(i in links.categorii[1:40]){
    
    # TODO 
    # Test connection
    emag.stoc <- c("https://www.emag.ro/", "/stoc/p", "/c")
    nextPage <- read_html(GET(paste0(emag.stoc[1], i, emag.stoc[2], 1, emag.stoc[3]))) %>% html_nodes(css$navigare$nextPage) %>% html_text()
    if(length(nextPage) > 1){
      nextPage <- as.integer(nextPage[length(nextPage) - 1])
    } else {
      nextPage <- 1
    }
    
    for(j in 1:nextPage){
      Sys.sleep(3)
      # TODO 
      # test connection
      print("Navigating to:")
      print(paste0(emag.stoc[1], i, emag.stoc[2], j, emag.stoc[3]))
      set_config(use_proxy("http://proxy.insro.local", 8080), override = TRUE)
      err <- tryCatch(expr = {
        productUrl <- read_html(GET(paste0(emag.stoc[1], i, emag.stoc[2], j, emag.stoc[3]))) %>% html_nodes(css$navigare$productUrl) %>% html_attr("href")
      },
      e = function(e) {next})
      #productUrl <- read_html(GET(paste0(emag.stoc[1], i, emag.stoc[2], j, emag.stoc[3]))) %>% html_nodes(css$navigare$productUrl) %>% html_attr("href")
      emag.links[[length(emag.links) + 1]] <- data.frame(categorie = rep(i, length(productUrl)), url_produs = productUrl, stringsAsFactors = FALSE)
    }
  }
  
  emag.links.df <- do.call('rbind', emag.links)
  emag.links <- split(emag.links.df, f = emag.links.df[,1])
  return(emag.links)
}


emag.links <- emag_get_subcategories()



plit_df <- function (emag.links) {
  new_emag.links <- emag.links
  n <- names(emag.links)
  for (i in 1:length(emag.links)) {
    if (length(emag.links[[i]][[1]]) > 500) {
      df_name <- n[i]
      df <- emag.links[[i]]
      x <- split(df, (seq(nrow(df))-1) %/% 500)
      x_name <- names(x)
      names_df_x <- paste(df_name, x_name, sep = "_")
      names(x) <- names_df_x
      new_emag.links <- c(new_emag.links, x)
    }
  }
  row <- which(sapply(new_emag.links, nrow) > 500)
  new_emag.links <- new_emag.links[-row]
  return(new_emag.links)
}

emag.links <- split_df(emag.links = emag.links)




emag_get_products <- function(emag.links = NULL){

  css$produs$nume         <- "h1.page-title"
  css$produs$pret_intreg  <- "div.product-highlight.product-page-pricing  > p.product-old-price"
  css$produs$pret_redus   <- "div.product-highlight.product-page-pricing  > p.product-new-price"
  css$produs$specificatii <- "table.table.table-striped.product-page-specifications"
  css$produs$categorie    <- "div.breadcrumb-inner > ol > li:nth-child(3) > a"
  css$produs$rating       <- "a.star-rating-container.feedback-rating-holder > span.star-rating-text"
  css$produs$nr_review    <- "div.product-highlight > p > a:nth-child(1)"
  

for(i in 1:length(emag.links)){
  Sys.sleep(5)
  produse <- data.frame()
  logger <- data.frame()
  for(j in 1:nrow(emag.links[[i]])){  #nrow(emag.links[[i]])
  
    Sys.sleep(3)
    set_config(use_proxy("http://proxy.insro.local", 8080), override = TRUE)
    x <- as.character(emag.links[[i]][j,2])
    # simple tryCatch
    skip_next <- 0
    err <- tryCatch(expr = {
      y <- httr::GET(x)
      emag.html <- read_html(y)
      },
      e = function(e) {skip_next <<- 999})
    if(skip_next !=999){
        if(y$status_code == 200){
        produs <- list()
      
        # nume
        produs$nume  <- emag.html %>% html_node(css$produs$nume) %>% html_text() %>% trimws()
        if(identical(produs$nume, character(0))){
          produs$nume <- NA
        }
        
        # pret_intreg
        produs$pret_intreg  <- emag.html %>% html_node(css$produs$pret_intreg) %>% html_text() %>% trimws()
        if(identical(produs$pret_intreg, character(0))){
          produs$pret_intreg <- NA
        } else{
          produs$pret_intreg  <- strsplit(produs$pret_intreg, " ")
          produs$pret_intreg  <- produs$pret_intreg[[1]][1]
          produs$pret_intreg  <- gsub("[[:alpha:]]", "", produs$pret_intreg)
          produs$pret_intreg  <- gsub("\n", "", produs$pret_intreg)
          produs$pret_intreg  <- gsub("\\.", "", produs$pret_intreg) %>% as.integer()
          produs$pret_intreg  <- as.numeric(produs$pret_intreg/100, digits = 2)
        }
        
        # pret_redus
        produs$pret_redus   <- emag.html %>% html_node(css$produs$pret_redus) %>% html_text() %>% trimws()
        if(identical(produs$pret_redus, character(0))){
          produs$pret_redus <- NA
        } else {
          produs$pret_redus   <- gsub("\\D+", "", produs$pret_redus) %>% as.numeric()
          produs$pret_redus   <- produs$pret_redus/100
        }
        
        # produs_specificatii
        produs$specificatii <- emag.html %>% html_nodes(css$produs$specificatii) %>% html_table()
        if(length(produs$specificatii) == 0){
          produs$specificatii <- NA
        } else {
            produs$specificatii <- do.call("rbind", produs$specificatii)
            produs$specificatii[,1] <- gsub("[[:punct:]]", "", produs$specificatii[,1])
            produs$specificatii[,1] <- gsub("[[:space:]]", "", produs$specificatii[,1])
            produs$specificatii[,1] <- trimws(tolower(produs$specificatii[,1]))
            produs$specificatii <- dcast(produs$specificatii, . ~ X1, value.var = 'X2')[, -1]
        }
        
        # produs_rating
        produs$rating       <- emag.html %>% html_nodes(css$produs$rating) %>% html_text()
        if(identical(produs$rating, character(0))){
          produs$rating <- NA
        }
        
        # produs_nr_reviews
        produs$nr_reviews   <- emag.html %>% html_nodes(css$produs$nr_review) %>% html_text()
        if(identical(produs$rnr_reviews, character(0))){
          produs$nr_reviews <- NA
        }
        ###########################################################################
        
        produs$df <- data.frame(nume = produs$nume, pret_intreg = produs$pret_intreg, 
                           pret_redus = produs$pret_redus, specificatii = produs$specificatii,
                           rating = produs$rating, nr_reviews = produs$nr_reviews,
                           data_colectare = Sys.Date(), url_pag = i, categorie = emag.links[[i]][j,1], stringsAsFactors = FALSE)

        
        produse <- bind_rows(produse, produs$df)


        
        logger <- bind_rows(logger, data.frame(link = x,  status = y$status_code, time = Sys.time(), date = Sys.Date()))
        
        ###########################################################################
        } else{
        logger <- bind_rows(logger, data.frame(link = x,  status = y$status_code, time = Sys.time(), date = Sys.Date()))
        }
    
      } else{
        logger <- bind_rows(logger, data.frame(link = x,  status = skip_next, time = Sys.time(), date = Sys.Date()))
        next()
        }  
    }    
write.csv(produse, paste0(emag.links[[i]][j,1], "_produse",".csv"), row.names = FALSE)
write.csv(logger, paste0(emag.links[[i]][j,1], "_logger", ".csv"), row.names = FALSE)
}
}

emag_get_products(emag.links[26])
t2 = Sys.time() - t1

