#########################
############################## Scraping Buzzfeed Tasty
############################## MARY RYAN
############################## CREATED: 09.29.2019
############################## UPDATED: 10.27.2019
#########################

#### LOAD LIBRARIES ####
library( rvest )
library( RCurl )
library( curl )
library( httr )
library( jsonlite )

options( stringsAsFactors = FALSE )

### FOUND THE TASTY API ###
# format: 'https://tasty.co/api/recipes/search?size=20&from=140&page=8&q=chicken&filter=&terms=&slugs=&tag_query='
# want to pull the slugs and insert into form URL: "https://tasty.co/recipe/slug-here"
# or if a compilation: "https://tasty.co/comilation/slug-here"
# can pull if recipe or compilation from "type"

size <- 10
from <- 1
page <- 1

query.key <- unlist( strsplit('chicken', " ") )#unlist( strsplit('buddha bowl', " ") )
query.key <- paste0( query.key, collapse="+" )

tasty.api.url <- 'https://tasty.co/api/recipes/search?'

tasty.reply <- getForm( tasty.api.url,
                        size = size,
                        from = from,
                        page = page,
                        q = query.key )

tasty.content <- fromJSON( tasty.reply )

recipe.count <- tasty.content$recipe_count

recipe.names <- recipe.slugs <- recipe.type <- rep(NA, recipe.count)

index.start <- seq( 1, recipe.count, by=size )
index.start <- c( index.start, recipe.count+1 )
from <- seq( 0,recipe.count,by=size )

recipe.names[seq(size)] <- tasty.content[["items"]]$name
recipe.slugs[seq(size)] <- tasty.content[["items"]]$slug
recipe.type[seq(size)] <- tasty.content[["items"]]$type
   
error <- 0

for( i in 2:ceiling(recipe.count/size) ){
   
   page <- i

   tasty.reply <- getForm( tasty.api.url,
                           size = size,
                           from = from[i],
                           page = page,
                           q = query.key )
   ## error handling ##
   res <- try(fromJSON( tasty.reply ))
   if(inherits(res, "try-error"))
   {
      error <- c(error, page)
      #error handling code, maybe just skip this iteration using
      next
   }
   
   
   tasty.content <- fromJSON( tasty.reply )
   
   if( is.null(tasty.content[["items"]]) ) next
   
   
   index <- index.start[i]:(index.start[(i+1)] - 1)
   
   recipe.names[index] <- tasty.content[["items"]]$name
   recipe.slugs[index] <- tasty.content[["items"]]$slug
   recipe.type[index] <- tasty.content[["items"]]$type
   
}
   
recipe.general.info <- cbind(recipe.names, recipe.slugs, recipe.type)


## SCRAPE INDIVIDUAL RECIPES ##
recipe.url <- 'https://tasty.co/recipe/'
full.ingred <- list()

for( i in seq(recipe.count) ){
   
   recipe.html <- read_html( gsub( " ", "", paste(recipe.url, recipe.slugs[i]) ) )
   
   ingred.json <- recipe.html %>%
      html_nodes("script") %>% 
      html_text()
   
   ingred.index <- recipe.html %>% 
      html_nodes("script") %>% 
      html_attr("type") =="application/ld+json"
   
   ingred.json <- na.omit( ingred.json[ingred.index] )
   
   ingred.json.parsed <- fromJSON( ingred.json[1] )$recipeIngredient
   
   full.ingred[[i]] <- cbind(recipe.names[i], ingred.json.parsed)
   
   
}



# within a recipe, you can search for a script of type "application/ld+json" that will give you a json including ingredients

#### Stuff I was dicking around with but use for tips ####
### SEARCH TASTY FOR RECIPE ###
## send the search query to tasty.co



tasty.url <-'https://tasty.co/search?'

query.base <- 'q='
query.key <- unlist( strsplit('chicken', " ") )#unlist( strsplit('buddha bowl', " ") )
query.key <- paste0( query.key, collapse="+" )

tasty.search.html <- read_html( gsub( " ", "", paste(tasty.url,
                                      query.base,
                                      query.key) )
                                      ) 

## get all links from the search results ##
# all recipe links are in hyperlink items with class "analyt-unit-tap"
links <- tasty.search.html %>% 
   html_nodes(".analyt-unit-tap") %>% 
   html_attr("href")

## go to the links and grab the ingredients ##
# ingredients are held in a ul within a div with class "ingredients__section"
recipe.list <- list()
for( i in seq( length(links) ) ){
   
   recipe.html <- read_html( links[i] )
   recipe.title <- recipe.html %>% html_nodes(".recipe-name") %>% html_text()
   full.ingred <- recipe.html %>% html_nodes(".ingredients__section" ) %>% html_nodes("li") %>% html_text()
   
   # take out the "\n"s and make each ingredient a list item #
   cleaner.ingred <- strsplit(full.ingred, "\n")
   
   # trim off all the whitespace #
   trim.ingred <- lapply( cleaner.ingred, trimws )
   
   # get rid of all the blank spots #
   clean.ingred <- lapply( trim.ingred, function(x){
      
      na.omit( ifelse( x == "", NA, x) )
      
   })
   
   clean.ingred <- unlist( lapply( clean.ingred, function(x){
      
      paste(x, collapse=" ")
      
   }) )
      
   # get rid of non-US measurements, so keep anything before a ( #
   us.ingred <- gsub("*\\(.*$","", clean.ingred)
   comma.find <- function( string ){
      
      df <- str_split( string, "," )
      extra <- lapply( df, function(x){
         
         if( length(x) > 1 ){
            
            x[2:length(x)]
            
         }else{
            NA
         }
         
      })
      
      do.call(rbind, extra)
      
   }
   
   
   
   measurements <- c( "cups","cup", "tablespoons", "tablespoon",
                     "tbsp", "teaspoons", "teaspoon", "tsp", "oz", "lb")
   measurements <- paste(measurements, collapse="|")
   number <- c(seq(30), "½", "⅓", "¼")
   number <- paste(number, collapse="|")
   
   ingred.broken <- str_split(us.ingred," ")
   
   recipe.amount <- as.numeric(gsub("([0-9]+).*$", "\\1", us.ingred))
   recipe.measurement <- str_extract(us.ingred, measurements)
   recipe.extra <- comma.find( us.ingred )
   recipe.ingred <- gsub(paste(number, "|", measurements, "|",
                               paste(recipe.extra, collapse="|")), "\\1", us.ingred)

   # make the recipe its own list item #
   recipe.list[[i]] <- cbind( recipe.title, recipe.amount, recipe.measurement,
                              recipe.ingred, recipe.extra)
   
   if( dim(recipe.list[[i]])[2] < 7){
      
      
      recipe.list[[i]] <- cbind(recipe.list[[i]], matrix(NA, nrow=dim(recipe.list[[i]])[1], ncol=7-dim(recipe.list[[i]])[2]))
   }

}

# turn the list into a dataframe #
recipe.df <- do.call( "rbind", recipe.list )
colnames( recipe.df ) <- c("RecipeTitle", "Amount",
                           "Measurements", "Ingredient",
                           "ExtraInstruct_1", "ExtraInstruct_2","ExtraInstruct_3")
