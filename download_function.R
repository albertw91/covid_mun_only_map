library(stringr)
library(yaml)


download_function <- function() {
  # en este archivo se descargan los datos usados en la app
  
  links = read_yaml("config.yml")
  data_dir = links[["data_dir"]]
 
  #print(links[[ unlist(str_split(names(links), "\\n")[2])]]["file_name"][1])

  for( i in str_split(names(links), "\\n")) {
    #list.files(dirname(rstudioapi::getSourceEditorContext()$path))
    i = unlist(i)
    if ("data_dir" == i){
      next
    } 
    if (file_test("-f", unlist(links[[i]]["file_name"]) )) {
      next
      
    } else {
      html_shp <- readLines(unlist(links[[i]]["link"]))
      
      rg_shp <- html_shp[grep("Download file", html_shp, perl = F)  ]
      rg2_shp <- regmatches(rg_shp,  regexpr(paste0("(?<=href=\\\").*\\.", i), rg_shp, perl=TRUE))
      download.file(rg2_shp, paste0(data_dir, unlist(links[[i]]["file_name"])), mode = "wb")
      rm(html_shp)
      
      
    }
    
  }
  
  
  
}

