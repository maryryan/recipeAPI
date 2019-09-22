#########################
############################## BREWERY API ANALYSIS
############################## MARY RYAN
############################## CREATED: 09.22.2019
############################## UPDATED:
#########################

#### LOAD LIBRARIES ####
library( rvest )
library( RCurl )
library( httr )
library( jsonlite )

options( stringsAsFactors = FALSE )

#### OPEN BREWERY DB API ####
## SET UP HTML ##
# https://www.openbrewerydb.org/ #
# https://api.openbrewerydb.org/breweries #

OB.url <- 'https://api.openbrewerydb.org/breweries'

## Get all the tags that exist ##
OB.perPage <- 50
i <- 1

OB.tags.all <- list()
content.dim <- 1

while( !( is.null(content.dim) ) ){
   
   OB.reply.gen <- getForm( OB.url,
                            page = i,
                            per_page = OB.perPage )
   OB.content.gen <- fromJSON( OB.url )
   
   content.dim <- dim( OB.content.gen )
   if( !( is.null(content.dim) ) ){
      
      OB.tags.all[[i]] <- unique( OB.content.gen$tag_list )
      
   }
   
   i <- i + 1
   
}


## Find some breweries in San Marcos, CA ##
OB.state <- 'california'
OB.city <- 'san_marcos'

OB.reply <- getForm( OB.url,
                     by_state = OB.state,
                     by_city = OB.city)

OB.content <- fromJSON( OB.reply )

## Let's see what type of breweries we have in the North County area ##
# first get all the cities in North County #
NC.html <- read_html( 'https://en.wikipedia.org/wiki/North_County_(San_Diego_area)#Cities' )
NC.cities.nodes <- NC.html %>% 
   html_nodes("ul") %>% 
   html_text()

NC.IncorpCity.nodes <- trimws( NC.cities.nodes[7] )
NC.UnIncorpCity.nodes <- trimws( NC.cities.nodes[8] )


NC.IncorpCity.tbl <- read.table(text = NC.IncorpCity.nodes, sep = "\n", as.is = TRUE)
NC.UnIncorpCity.tbl <- read.table(text = NC.UnIncorpCity.nodes, sep = "\n", as.is = TRUE)

#gsub("* -.*$", "", string) --> extract any character before " -", which may have any character for any length after the : until the end of the string
NC.IncorpCity.df <- gsub("* -.*$", "", NC.IncorpCity.tbl[,1])
NC.UnIncorpCity.df <- gsub("* -.*$", "", NC.UnIncorpCity.tbl[,1])

NC.city.df <- c(NC.IncorpCity.df, NC.UnIncorpCity.df)

# now we search the API for breweries in those cities #
OB.NC.content <- list()

for( i in seq( length(NC.city.df) ) ){
   
   OB.NC.reply <- getForm( OB.url,
                           by_state = "california",
                           by_city = NC.city.df[i] )
   OB.NC.content[[i]] <- fromJSON( OB.NC.reply )
   
}

OB.NC.content.df <- do.call( "rbind", OB.NC.content )
OB.NC.content.df <- OB.NC.content.df %>% 
   group_by( brewery_type, city ) %>% 
   mutate( count = n() )

OB.NC.content.df %>% 
   ggplot( aes(brewery_type, count,
               fill=factor(city)) ) + #geom_bar()
   geom_bar( position='dodge', stat = 'identity' ) +
   scale_fill_manual(values = c('#8e0152','#c51b7d','#de77ae',
                                '#f1b6da','#fde0ef','#e6f5d0',
                                '#b8e186','#7fbc41','#4d9221','#276419')) + 
   scale_y_continuous(breaks=seq(2,14,2)) +
   labs(title = "Breweries in North County, CA",
        fill  = "City") +
   xlab("Brewery Type") + ylab("") +
   theme_light()

## now let's see what types of breweries are in the OC ##
# first get all the cities in North County #
OC.html <- read_html( 'https://en.wikipedia.org/wiki/Orange_County,_California#Geography' )
OC.cities.nodes <- OC.html %>% 
   html_nodes("ul") %>% 
   html_text()

OC.IncorpCity.nodes <- trimws( OC.cities.nodes[41] )
OC.UnIncorpCity.nodes <- trimws( OC.cities.nodes[42:43] )


OC.IncorpCity.tbl <- read.table(text = OC.IncorpCity.nodes, sep = "\n", as.is = TRUE)
OC.UnIncorpCity.tbl <- read.table(text = OC.UnIncorpCity.nodes, sep = "\n", as.is = TRUE)

OC.city.df <- c(OC.IncorpCity.tbl[,1], OC.UnIncorpCity.tbl[,1])

# now we search the API for breweries in those cities #
OB.OC.content <- list()

for( i in seq( length(OC.city.df) ) ){
   
   OB.OC.reply <- getForm( OB.url,
                           by_state = "california",
                           by_city = OC.city.df[i] )
   OB.OC.content[[i]] <- fromJSON( OB.OC.reply )
   
}

OB.OC.content.df <- do.call( "rbind", OB.OC.content )
OB.OC.content.df <- OB.OC.content.df %>% 
   group_by( brewery_type, city ) %>% 
   mutate( count = n() )

OB.OC.content.df %>% 
   ggplot( aes(brewery_type, count,
               fill=factor(city)) ) + #geom_bar()
   geom_bar( position='dodge', stat = 'identity' ) +
   scale_y_continuous(breaks=seq(2,14,2)) +
   labs(title = "Breweries in Orange County, CA",
        fill  = "City") +
   xlab("Brewery Type") + ylab("") +
   theme_light()
