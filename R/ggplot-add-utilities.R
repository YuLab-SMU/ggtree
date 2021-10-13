build_cladeids_df <- function(trdf, nodeids){
    dat <- lapply(seq_along(nodeids), function(i){
             ids <- getSubtree.df(trdf, nodeids[i])
             dt <- trdf[trdf$node %in% ids,]
             dt$clade_root_node <- nodeids[i]
             return(dt)
              })
    return(do.call("rbind", dat))
}

build_cladeids_df2 <- function(trdf, nodeids){
    flagreverse <- check_reverse(data=trdf)
    dat <- lapply(nodeids, function(i)get_clade_position_(data=trdf, node=i, reverse=flagreverse))
    dat <- do.call("rbind", dat)
    dat$clade_root_node <- nodeids
    return(dat)
}

check_reverse <- function(data){
    # extract tip point data
    tiptab <- data[data$isTip,]
    # extract the corresponding parent point data, it might generate NA.
    nodetab <- data[match(tiptab$parent, data$node),]
    # remove the NA, and if all x values of tip are smaller than 
    # corresponding parent x values, the x of tree is reversed.
    if (all(tiptab$x <= nodetab$x, na.rm=TRUE)){
        return(TRUE)
    }else{
        return(FALSE)
    }
}

choose_hilight_layer <- function(object, type){
    if (type=="encircle"){
        if (!is.null(object$mapping)){
            object$mapping <- modifyList(object$mapping, aes_(x=~x, y=~y, clade_root_node=~clade_root_node))
        }else{
            object$mapping <- aes_(x=~x, y=~y, clade_root_node=~clade_root_node)
        }
        params <- c(list(data=object$data, mapping=object$mapping), object$params)
        ly <- do.call("geom_hilight_encircle2", params)
    }else{
        if (!is.null(object$mapping)){
            object$mapping <- modifyList(object$mapping, aes_(xmin=~xmin, xmax=~xmax, 
                                                              ymin=~ymin, ymax=~ymax, 
                                                              clade_root_node=~clade_root_node))
        }else{
            object$mapping <- aes_(xmin=~xmin, xmax=~xmax, ymin=~ymin, ymax=~ymax, clade_root_node=~clade_root_node)
        }
        params <- c(list(data=object$data, mapping=object$mapping), object$params)
        ly <- do.call("geom_hilight_rect2", params)
    }
    return (ly)
}

extract_all_aes_var <- function(mapping, rmvar=c("node", "subset")){
    unlist(lapply(names(mapping)[!names(mapping) %in% rmvar], 
                  function(i) get_aes_var(mapping, i)))
}

remapping <- function(mapping, samevars){
    allvars <- extract_all_aes_var(mapping, rmvar=NULL)
    samevars <- allvars[allvars %in% samevars]
    tmpmap <- lapply(samevars, function(i) paste0(i, ".x"))
    tmpmap <- do.call("aes_string", tmpmap)
    names(tmpmap) <- names(mapping)[allvars %in% samevars]
    mapping <- modifyList(mapping, tmpmap)
    return(mapping)
}

build_cladelabel_df <- function(trdf, nodeids, label, offset, align, angle, horizontal){
    dat <- mapply(function(i, o, a, g, h){get_cladelabel_position(data=trdf, 
                                          node=i, 
                                          offset=o, 
                                          align=a,
                                          adjustRatio = 1.03, 
                                          angle=g, 
                                          horizontal=h)},
                  nodeids,
                  offset,
                  align,
                  angle,
                  horizontal,
                  SIMPLIFY=FALSE)
    dat <- do.call("rbind", dat)
    dat$y <- unlist(mapply(function(x, y){mean(c(x, y))}, dat$y, dat$yend, SIMPLIFY=FALSE))
    dat$label <- label
    dat$node <- nodeids
    return(dat)
}

build_cladebar_df <- function(trdf, nodeids, offset, align, extend){
    dat <- mapply(function(i, o, a, e){get_cladelabel_position(data=trdf, 
                                                           node=i, 
                                                           offset=o, 
                                                           align=a, 
                                                           adjustRatio=1.02,
                                                           angle=0, 
                                                           extend=e)},
                  nodeids,
                  offset,
                  align,
                  extend,
                  SIMPLIFY=FALSE
                 )
    dat <- do.call("rbind", dat)
    dat$node <- nodeids
    return(dat)

}

build_cladelabel_df2 <- function(trdf, nodeids, label, offset, align, angle, horizontal){
    dat <- mapply(get_cladelabel2_position_label,
                           node=nodeids,
                           offset=offset, 
                           align=align, 
                           angle=angle,
                           horizontal=horizontal,
                           MoreArgs=list(
                               data=trdf,
                               adjustRatio = 1.2
                           ),
                  SIMPLIFY=FALSE)
    dat <- do.call("rbind", dat)
    dat$label <- label
    dat$node <- nodeids
    return (dat)
}

build_cladebar_df2 <- function(trdf, nodeids, offset, align){
    dat <- mapply(get_cladelabel2_position_bar,
                    node=nodeids, 
                    offset=offset, 
                    align=align, 
                  MoreArgs=list(
                    data=trdf,
                    adjustRatio=1.1),
               SIMPLIFY=FALSE
           )
    dat <- do.call("rbind", dat)
    colnames(dat) <- c("x", "y", "xend", "yend")
    dat$node <- nodeids
    return(dat)
}

reset_params <- function(defaultp, inputp, type){
    if ("color" %in% names(inputp)){
        inputp$colour <- inputp$color
        inputp <- inputp[names(inputp) != "color"]
    }
    if ("barcolor" %in% names(inputp)){
        inputp$barcolour <- inputp$barcolor
        inputp <- inputp[names(inputp) != "barcolor"]
    }
    if ("textcolor" %in% names(inputp)){
        inputp$textcolour <- inputp$textcolor
        inputp <- inputp[names(inputp) != "textcolor"]
    }
    if ("imagecolor" %in% names(inputp)){
        inputp$imagecolour <- inputp$imagecolor
        inputp <- inputp[names(inputp) != "imagecolor"]
    }
    intdi <- intersect(names(inputp), names(defaultp))
    setd <- setdiff(names(defaultp), names(inputp))
    intdi <- inputp[match(intdi, names(inputp))]
    setd <- defaultp[match(setd, names(defaultp))]
    newp <- c(intdi, setd)
    if (type=="bar"){
        names(newp)[grepl("barsize",names(newp))] <- "size"
        names(newp)[grepl("barcolour", names(newp))] <- "colour"
    }else if (type=="text"){
        names(newp)[grepl("fontsize", names(newp))] <- "size"
        names(newp)[grepl("textcolour", names(newp))] <- "colour"
    }else if (type=="image"){
        names(newp)[grepl("imagesize", names(newp))] <- "size"
        names(newp)[grepl("imagecolour", names(newp))] <- "colour"
    }
    return(newp)
}

transform_df <- function(data, object, default_aes){
    data <- tibble::as_tibble(data)
    if (!is.null(object$mapping)){
        for (i in names(default_aes)){
            if (i %in% names(object$mapping)){
                data[[i]] <- object$data[[as_name(object$mapping[[i]])]]
            }else{
                if (i == "extend" && length(default_aes[[i]]) == 2){
                    data[[i]] <- rep(list(default_aes[[i]]), nrow(data))
                }else{
                    data[[i]] <- default_aes[[i]]
                }
            }
        }
    }else{
        for ( i in names(default_aes)){
            if (i == "extend" && length(default_aes[[i]]) == 2){
                data[[i]] <- rep(list(default_aes[[i]]), nrow(data))
            }else{
                data[[i]] <- default_aes[[i]]
            }
        }
    }
    if ("offset.text" %in% names(object$mapping)){
        data[["offset.text"]] <- data$offset + object$data[[as_name(object$mapping[["offset.text"]])]]
    }else{
        data[["offset.text"]] <- data$offset + default_aes$offset.text
    }
    return(data)
}

reset_mapping <- function(defaultm, inputm){
    ids <- intersect(names(inputm), names(defaultm))
    return(inputm[match(ids, names(inputm))])
}

reset_dot_params <- function(mapping, defaultp, default_aes, params){
    setidx <- intersect(names(default_aes), names(params))
    defaultidx <- setdiff(names(defaultp), names(mapping))
    setidx <- params[match(setidx, names(params))]
    defaultidx <- defaultp[match(defaultidx, names(defaultp))]
    defaultidxs <- setdiff(names(defaultidx), names(setidx))
    setidxs <- setdiff(names(setidx), names(defaultidx))
    defaultidx2 <- intersect(names(defaultidx), names(setidx))
    dot_params <- c(defaultidx[defaultidx2], setidx[setidxs], defaultidx[defaultidxs])
    size_params <- c("barsize", "textsize", "fontsize", "imagesize")
    color_params <- c("barcolor", "textcolor", "fontcolor", "imagecolor")
    colour_params <- c("barcolour", "textcolour", "fontcolour", "imagecolour")
    if (any(size_params %in% names(dot_params))){
        dot_params <- dot_params[names(dot_params)!="size"]
        names(dot_params) <- gsub(".*size", "size", names(dot_params))
    }
    if (any(color_params %in% names(dot_params))){
        dot_params <- dot_params[names(dot_params)!="colour"]
        names(dot_params) <- gsub(".*color", "colour", names(dot_params))
    }
    if (any(colour_params %in% names(dot_params))){
        dot_params <- dot_params[names(dot_params)!="colour"]
        names(dot_params) <- gsub(".*colour", "colour", names(dot_params))
    }
    return(dot_params)
}

build_image_layer <- function(data, object, params){
    image_obj <- list()
    label_geom <- switch(object$geom,
                        image=get_fun_from_pkg("ggimage", "geom_image"),
                        phylopic=get_fun_from_pkg("ggimage", "geom_phylopic")
                        )
    image_obj$data <- data
    image_default_aes <- list(image=NULL,imagesize=0.05, imagecolour=NULL, imagecolor=NULL,
                              size=0.05, colour = NULL, angle = 0, alpha=1, inherit.aes=FALSE,
                              nudge_x=0, nudge_y=0, na.rm = FALSE, by = "width", na.rm = FALSE,position = "identity",
                              stat = "identity", .fun = NULL, image_fun = NULL, hjust = 0.5)
    image_obj$mapping <- reset_mapping(defaultm=image_default_aes,
                                      inputm=object$mapping)
    ifelse(is.null(image_obj$mapping), 
           image_obj$mapping <- aes_(x=~x, y=~y),
           image_obj$mapping <- modifyList(image_obj$mapping, aes_(x=~x, y=~y)))
    image_dot_params <- reset_dot_params(mapping=image_obj$mapping,
                                         defaultp=params,
                                         default_aes=image_default_aes,
                                         params=object$params)
    image_obj <- c(image_obj, image_dot_params)
    image_obj <- do.call("label_geom", image_obj)
    return(image_obj)
}

build_text_layer <- function(data, object, params, layout){
    text_obj <- list()
    text_obj$data <- data
    if (object$geom=="shadowtext"){label_geom <- get_fun_from_pkg("shadowtext", "geom_shadowtext")}
    text_default_aes <- list(textcolour="white", textcolor="white", textsize=3.88, 
                             fontsize=3.88, fontcolor="white", fontcolour="white", 
                             colour="white", size=3.88, angle=0, hjust=0.5, vjust=0.5,
                             alpha=NA, family="", fontface=1, lineheight=1.2, inherit.aes=FALSE,
                             nudge_x=0, nudge_y=0, check_overlap=FALSE, show.legend=NA, na.rm=FALSE,
                             stat="identity", position="identity")
    shadowtext_default_aes <- c(text_default_aes, list(bg.colour="black", bg.r=0.1))
    label_default_aes <- c(text_default_aes, list(fill="white", label.padding = unit(0.25, "lines"),
                                                  label.r = unit(0.15, "lines"), label.size = 0.25))
    text_obj$mapping <- switch(object$geom,
                              text=reset_mapping(defaultm=text_default_aes, inputm=object$mapping),
                              label=reset_mapping(defaultm=label_default_aes, inputm=object$mapping),
                              shadowtext=reset_mapping(defaultm=shadowtext_default_aes, inputm=object$mapping)
                              )
    if(length(text_obj$mapping)==0){
        text_obj$mapping <- aes_(x=~x, y=~y, label=~label, angle=~angle)
    }else{
        text_obj$mapping <- modifyList(text_obj$mapping, aes_(x=~x, y=~y, label=~label, angle=~angle))
    }
    text_dot_params <- switch(object$geom,
                             text= reset_dot_params(mapping=text_obj$mapping,
                                            defaultp=params,
                                            default_aes=text_default_aes,
                                            params=object$params),
                             label=reset_dot_params(mapping=text_obj$mapping,
                                                    defaultp=params,
                                                    default_aes=label_default_aes,
                                                    params=object$params),
                             shadowtext=reset_dot_params(mapping=text_obj$mapping,
                                                         defaultp=params,
                                                         default_aes=shadowtext_default_aes,
                                                         params=object$params)
                             )
    if (object$parse=="emoji"){
        emojifont <- "emojifont"
        emoji <- get_fun_from_pkg("emojifont", "emoji")
        require(emojifont, character.only = TRUE) %>% suppressMessages()
        text_obj$data$label <- emoji(text_obj$data$label)
        text_dot_params$family <- "EmojiOne"
        object$parse <- FALSE
    }
    text_obj <- c(text_obj, text_dot_params)
    if (object$geom == "text"){
        if (layout %in% c("circular", "radial", "daylight", "fan", "unrooted", "ape", "inward_circular", "equal_angle") && 
            (is.null(object$params$horizontal) || object$params$horizontal)){
            m1 <- aes_string(subset="(angle < 90 | angle > 270)", angle="angle")
            m2 <- aes_string(subset="(angle >= 90 & angle <=270)", angle="angle+180")
            m1 <- modifyList(text_obj$mapping, m1)
            m2 <- modifyList(text_obj$mapping, m2)
            text_obj1 <- text_obj2 <- text_obj
            text_obj1$mapping <- m1
            text_obj2$mapping <- m2
            text_obj2$hjust <- 1
            textlayer <- list(do.call("geom_text2", text_obj1), do.call("geom_text2", text_obj2))
        }else{
            textlayer <- do.call("geom_text", text_obj)
        }
    }
    text_obj <- switch(object$geom,
                       text = textlayer, #do.call("geom_text", text_obj),
                       label = do.call("geom_label", text_obj),
                       shadowtext = do.call("label_geom", text_obj)
                      )
    return(text_obj)
}

