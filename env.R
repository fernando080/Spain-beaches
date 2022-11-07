library(DT)
library(ggplot2)
library(plotly)
library(leaflet)
library(shiny)
library(shinythemes)
library(gridExtra)
library(stringr)
library(viridis)
library(hrbrthemes)
library(GGally)
library(leaflet)
library(plyr)
library(dplyr)
library(reshape)
library(ggh4x)
library(stringi)

## Aux functions

numextract <- function(string){ 
  str_extract(string, "\\-*\\d+\\,*\\d*")
}

numextractDot <- function(string){ 
  str_extract(string, "\\-*\\d+\\.*\\d*")
}

levels_change_CCAA <- function(facColumn, data){
  out = levels(data[[facColumn]])
  l = length(out)
  
  for (i in 1:l){
    if (out[i] == "comunitat valenciana") {
      
      out[i] <- 'VAL'
      
    } else if (grepl("vasco", out[i], fixed = TRUE)){
      
      out[i] <- 'PV'
      
    } else {
      
      out[i] <- toupper(substr(out[i], 1, 3))
      
    }
  }
  
  levels(data[[facColumn]]) <- out
  return(data)
}
## const

var_texto = '
<span class="home-text">This shiny app show an Story Board using a dataframe of 
Spain Beaches</span>
'

var_texto_mitfm = '
<span class="home-text">In this application we will find a visual summary of 
interesting features from the Spain beaches, in the way to make you an idea of
which province you want to visit</span>
'

var_texto_auth = '
<span class="home-text">
<a style="color:#CA75DB" href="https://www.linkedin.com/in/fernando-ca%C3%B1izares-romero-38aaa017a/">
Fernando Canizares Romero</a></span>
'

var_text_hosp <- '
<span class = "basic-text">For foreign tourists with very young children or 
people with chronic diseases the best options would be Melilla, Cueta, Barcelona 
and Malaga, or perhaps you could consider going to the Community Valencian
</span>
'

var_text_dep <- '
<span class = "basic-text">It should be noted that there are negative results in 
Huelva, the Canary Islands, Cantabria and Guipuzkoa. This can attract attention 
since they are very tourist destinations, and Guipuzkoa is especially visited by 
tourists who want to surf</span>
'

var_text_apar<- '
<span class = "basic-text" style = "text-align:center">It should be noted that 
there are negative results in Huelva, the Canary Islands, Cantabria and 
Guipuzkoa. This can attract attention since they are very tourist destinations, 
and Guipuzkoa is especially visited by tourists who want to surf</span>
'
var_text_urb_posi<- '
<i class="fas fa-thumbs-up" style = "color:#25FC17;"></i> 
<span class = "imp-text">Less uniformity between the three main values 
<span class = "imp-word">isolated</span>, <span class = "imp-word">urban</span>, 
<span class = "imp-word">semi-urban</span></span>
'
var_text_ocu_posi<- '
<i class="fas fa-thumbs-down" style = "color:#EF0808;"></i> 
<span class = "imp-text">Uniform distribution among the main occupancy levels 
<span class = "imp-word">high</span>, <span class = "imp-word">low</span>, 
<span class = "imp-word">medium</span></span>
'

var_text_ocuurb <- '
<span class = "basic-text" style = "text-align:center">In relation to the 
previous graph we can see how there is a certain <span class = "imp-word">relationship between both 
variables</span>. We find that the provinces with the highest prevalence of isolated 
beaches are those with the least occupation and vice versa.
<br>
If we put ourselves in the situation of making inference about a dependent 
variable, this graph warns us that there may be a certain correlation between 
these two variables and give <span class = "imp-word">problems of collinearity or redundant information</span>
</span>
'

var_text_wid <- '
<span class = "basic-text" style = "text-align:center">This may complicate a 
future analysis. Given the large number of outliers. Knowing the Spanish 
orography, these can be caused by the  <span class = "imp-text">proximity of 
mountainous terrain to the coast</span></span>
'

var_text_partcase <- '
<i class="fas fa-circle" style = "color:#CA75DB;"></i> 
<span class = "interesting-hint">There are many points with a value close to one 
and then other points with higher values but more or less uniformly distributed 
over the entire length of the violin graph that reach very high values</span>
'

var_text_partcase2 <- '
<i class="fas fa-circle" style = "color:#CA75DB;"></i> 
<span class = "interesting-hint">In this composition of histograms, the event 
that we mentioned previously can be observed. Many values are 
presented with low length and higher values distributed in a similar to a 
uniform way in a very wide fork</span>
'

currTime <- format(Sys.time(), "_%H_%M_%S")

# Load documentation 
fileName <- 'www/doc.txt'
doc <- readChar(fileName, file.info(fileName)$size)

