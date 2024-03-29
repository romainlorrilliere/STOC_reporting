## run.rmd()
## source("functions/fun_generic_latin1.r")
source("functions/fun_indicateurs_local_latin1.r")

vecPackage <- c("data.table","devtools","dplyr","ggplot2","ggpubr","lubridate","reshape2","knitr","kableExtra","pander","stringr")

ip <- installed.packages()[,1]

for(p in vecPackage) {
    if (!(p %in% ip))
        install.packages(pkgs=p,repos = "http://cran.univ-paris1.fr/",dependencies=TRUE)
    require(p,character.only=TRUE)
}



run.rmd <- function(file.rmd="functions/rmd_stoc_reporting_utf8.rmd",rep.out="output_rapport_pdf",file.data="data_DB/data3session.csv",file.out=NULL,year = NULL, id_station = "205",format_output="pdf",prefixe_file="",fileLog =NULL,save.fig=FALSE,render.clean=TRUE){


#file.rmd="functions/rmd_stoc_reporting_utf8.rmd";rep.out="output_rapport_pdf";file.data="data_DB/data3session.csv";file.out=NULL;year = 2018; id_station = "205";format_output="pdf"


    id_station_num <-  gsub("[a-z]","",id_station)
    id_station_letter <-  gsub(id_station_num,"",id_station)
    if(nchar(id_station_letter) == 0)id_station_letter <- "_"
    id_station_num <- str_pad(id_station_num, 4, pad = "0")
    id_station_txt <- paste0(id_station_num,id_station_letter)

    if(is.null(year)) year <- as.numeric(substr(Sys.time(),1,4))
    if(is.null(prefixe_file)) prefixe_file <- format(Sys.time(),"%Y-%m-%d_%H-%M")
    if(prefixe_file != "") prefixe_file <- paste0(prefixe_file,"_")
    if(is.null(file.out)) file.out <- paste0("stoc_reporting_",prefixe_file,"station_",  id_station_txt,"_",year,".",format_output)
    if(!is.null(rep.out)) file.out <- paste0(rep.out,"/",file.out)
     format <- paste0(format_output,"_document")

    rep <- getwd()

     cat("rmd :",file.rmd,"\n")
    cat("file.data :",file.data,"\n")
    cat("rep:", rep,"\n")
     cat("id_station :",id_station,"\n")
     cat("output :",file.out,"\n")
     if(!(rep.out) %in% dir()) {
         cat("\n Le repertoire de sortie:",rep.out,"est manquant\n")
         dir.create(rep.out,showWarnings=FALSE)
         cat("\n Répertoire créé !!\n")
     }

      rmarkdown::render(file.rmd,output_file=file.out,output_dir=rep.out,output_format = format,clean=render.clean,encoding="utf-8",params = list(set_rep = rep, set_file_data = file.data,set_fileLog = fileLog,set_site = id_station, set_year = year, set_save_fig = save.fig))

     cat("DONE !!!\n")
}




file.rmd # nom du fichier rmd que tu veux lancer
file.out # nom du fichier html ou pdf que tu veux produire
# si boucle sp par exemple file.out <- paste0(sp".html")

rep.out # repertoire de sortie
format # "html" ou "pdf"
parametres # list des parametres à importer dans le rmd par exemple le nom de l'espèce, son code ...

rmarkdown::render(file.rmd,output_file=file.out,output_dir=rep.out,output_format = format,params = parametres)
