
                                        #fwrite(d,"data/d.csv")

f_fs <- function(d) {

    d <- f_spread_fs(d)
    d <- f_nbnf_fs(d)

    return(d)
}





f_spread_fs <- function(dt) {
    require(data.table)
                                        #d <- fread("data/d.csv")
                                        #dt  <- d

    setDT(dt)

    d_fs <- dt[,.(FS = mean(FS,na.rm=TRUE)),by = .(DATE,NEW.ID_PROG)]
    setorder(d_fs,NEW.ID_PROG,DATE)
    d_fs[,num_session := 1:.N,by= NEW.ID_PROG]
    d_fs[,annee := as.numeric(substr(DATE,1,4))]
                                        # d_fs[,num_session_annee := 1:.N,by=.(NEW.ID_PROG,annee)]
    ## si un seul FS renseigné on l'affecte au session ou FS = NA
    ##d_fs[is.na(FS)& sd_fs > 0,]
    d_fs[,FS.DEDUIT := FS]
    d_fs[,FS_METHODE := ""]
    d_fs[!is.na(FS),FS_METHODE := "renseigné"]

    ## si une seule valeur de FS est renseignée pour le NEW.ID_PROG on l'étend au FS non renseigné
    d_fs[,`:=`(mean_fs = mean(FS,na.rm=TRUE), sd_fs = round(sd(FS,na.rm=TRUE))),by=NEW.ID_PROG]
    d_fs[is.na(FS.DEDUIT) & sd_fs < 1 , `:=`(FS.DEDUIT = mean_fs,FS_METHODE = "étendu")]
    d_fs[,`:=`(mean_fs =NULL, sd_fs = NULL),]

    ## si une seule valeur de FS est renseignée pour le NEW.ID_PROG par an on l'étend au FS non renseigné de la même année
    d_fs[,`:=`(mean_y_fs = mean(FS,na.rm=TRUE), sd_y_fs = round(sd(FS,na.rm=TRUE))),by=.(NEW.ID_PROG,annee)]
    d_fs[is.na(FS.DEDUIT) & sd_y_fs < 1 , `:=`(FS.DEDUIT = mean_y_fs,FS_METHODE = "étendu")]
    d_fs[,`:=`(mean_y_fs =NULL, sd_y_fs = NULL),]

    ## on étend la valeur saisie sur les données qui sont encadrée par une (début et fin de la série temporelle) ou deux valeurs de FS identiques

### on fait des group de saisie de FS.DEDUIT constant dans le temps
    d_fs_prev <- d_fs[,.(NEW.ID_PROG,num_session,FS.DEDUIT)]
    d_fs_prev[,num_session := num_session + 1]
    setnames(d_fs_prev,"FS.DEDUIT","FS.DEDUIT_prev")
    d_fs <- merge(d_fs,d_fs_prev,by=c("NEW.ID_PROG","num_session"),all.x = TRUE)

    ## pour facilité la comparaison on transforme les NA en valeurs abérantes
    d_fs[is.na(FS.DEDUIT), FS.DEDUIT := 9999]
    d_fs[is.na(FS.DEDUIT_prev), FS.DEDUIT_prev := 9999]

    d_fs[,group := cumsum(!as.numeric(FS.DEDUIT == FS.DEDUIT_prev)),by = NEW.ID_PROG]
                                        #d_fs[,group_id := paste0(NEW.ID_PROG,"_",group)]
    d_fs[,group_date := min(DATE),by = .(NEW.ID_PROG,group)]

    d_fs[,FS.DEDUIT_prev := NULL]
    d_fs[FS.DEDUIT == 9999 , FS.DEDUIT := NA]

    d_fs_order <- d_fs[DATE == group_date,.(NEW.ID_PROG,group,group_date,FS.DEDUIT)]
    setorder(d_fs_order,NEW.ID_PROG,group_date)

    d_fs_order[,order := 1:.N,by=NEW.ID_PROG]
    d_fs_prev <- d_fs_order[,.(NEW.ID_PROG,order,FS.DEDUIT)]
    d_fs_prev[,order := order + 1]
    setnames(d_fs_prev,"FS.DEDUIT","FS.DEDUIT_prev")

    d_fs_post <- d_fs_order[,.(NEW.ID_PROG,order,FS.DEDUIT)]
    d_fs_post[,order := order - 1]
    setnames(d_fs_post,"FS.DEDUIT","FS.DEDUIT_post")


    d_fs_order <- merge(d_fs_order,d_fs_prev,by=c("NEW.ID_PROG","order"),all.x = TRUE)
    d_fs_order <- merge(d_fs_order,d_fs_post,by=c("NEW.ID_PROG","order"),all.x = TRUE)
    d_fs_order <- d_fs_order[is.na(FS.DEDUIT),]


    d_fs_order[is.na(FS.DEDUIT) & FS.DEDUIT_post == FS.DEDUIT_prev, FS.DEDUIT := FS.DEDUIT_prev]
    d_fs_order[is.na(FS.DEDUIT) & is.na(FS.DEDUIT_post), FS.DEDUIT := FS.DEDUIT_prev]
    d_fs_order[is.na(FS.DEDUIT) & is.na(FS.DEDUIT_prev), FS.DEDUIT := FS.DEDUIT_post]

    d_fs_order <- d_fs_order[,.(NEW.ID_PROG,group,FS.DEDUIT)]
    setnames(d_fs_order,"FS.DEDUIT","FS.DEDUIT_order")

    d_fs <- merge(d_fs,d_fs_order,by=c("NEW.ID_PROG","group"),all.x = TRUE)
    d_fs[is.na(FS.DEDUIT) & !is.na(FS.DEDUIT_order),`:=`(FS.DEDUIT = FS.DEDUIT_order, FS_METHODE = "étendu")]
    d_fs <- d_fs[,.(NEW.ID_PROG,DATE,FS.DEDUIT,FS_METHODE)]

    dt <- merge(dt,d_fs,by=c("NEW.ID_PROG","DATE"))

    setDF(dt)

    return(dt)

}


f_nbnf_fs <- function(dt) {
    require(data.table)
    setDT(dt)

    dt[,NB.NF := length(unique(NF)),by = .(NEW.ID_PROG,YEAR)]
    dt[is.na(FS.DEDUIT),`:=`(FS.DEDUIT = ifelse(HABITAT == "STOC-rozo",120,12 * NB.NF),FS_METHODE = "déduit")]
    setDF(dt)

    return(dt)

}
