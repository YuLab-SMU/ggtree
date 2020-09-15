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
    tiptab <- data[data$isTip,]
    nodetab <- data[match(tiptab$parent, data$node),]
    if (all(tiptab$x < nodetab$x)){
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

reset_params <- function(defaultp, inputp){
    if ("color" %in% names(inputp)){
        inputp$colour <- inputp$color
        inputp <- inputp[names(inputp) != "color"]
    }
    intdi <- intersect(names(inputp), names(defaultp))
    setd <- setdiff(names(defaultp), names(inputp))
    intdi <- inputp[match(intdi, names(inputp))]
    setd <- defaultp[match(setd, names(defaultp))]
    newp <- c(intdi, setd)
    names(newp)[grepl("size", names(newp))] <- "size"
    return(newp)
}

transform_df <- function(data, object, default_aes){
    if (!is.null(object$mapping)){
        for (i in names(default_aes)){
            if (i %in% names(object$mapping)){
                data[[i]] <- object$data[[as_name(object$mapping[[i]])]]
            }else{
                data[[i]] <- default_aes[[i]]
            }
        }    
    }else{
        for ( i in names(default_aes)){
            data[[i]] <- default_aes[[i]]
        }
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
    dot_params <- c(params[match(setidx, names(params))], defaultp[match(defaultidx, names(defaultp))])
    return(dot_params)
}

build_image_layer <- function(data, object, params){
    image_obj <- list()
    label_geom <- switch(object$geom,
                        image=get_fun_from_pkg("ggimage", "geom_image"),
                        phylopic=get_fun_from_pkg("ggimage", "geom_phylopic")
                        )
    image_obj$data <- data
    image_default_aes <- list(image=NULL, size=0.05, colour = NULL, angle = 0, alpha=1)
    image_obj$mapping <- reset_mapping(defaultm=image_default_aes,
                                      inputm=object$mapping)
    ifelse(is.null(image_obj$mapping), 
           image_obj$mapping <- aes_(x=~x, y=~y),
           image_obj$mapping <- modifyList(image_obj$mapping, aes_(x=~x, y=~y)))
    print(image_obj$mapping)
    print(image_obj$data)
    image_dot_params <- reset_dot_params(mapping=image_obj$mapping,
                                         defaultp=params,
                                         default_aes=image_default_aes,
                                         params=object$params)
    print(image_dot_params)
    image_obj <- c(image_obj, image_dot_params)
    image_obj <- do.call("geom_phylopic", image_obj)
    return(image_obj)
}

##' @importFrom ggplot2 GeomText GeomLabel
##' @importFrom shadowtext geom_shadowtext
build_text_layer <- function(data, object, params){
    text_obj <- list()
    text_obj$data <- data
    shadowtext_default_aes <- list(colour="black", 
                                   size=3.88, 
                                   angle=0, 
                                   hjust=0.05,
                                   vjust=0.05,
                                   alpha=NA,
                                   family="",
                                   fontface=1,
                                   lineheight=1.2,
                                   bg.colour="black",
                                   bg.r=0.1)
    text_obj$mapping <- switch(object$geom,
                              text=reset_mapping(defaultm=GeomText$default_aes, inputm=object$mapping),
                              label=reset_mapping(defaultm=GeomLabel$default_aes, inputm=object$mapping),
                              shadowtext=reset_mapping(defaultm=shadowtext_default_aes, inputm=object$mapping)
                              )
    ifelse(is.null(text_obj$mapping), text_obj$mapping <- aes_(x=~x, y=~y, label=~label, angle=~angle), 
           text_obj$mapping <- modifyList(text_obj$mapping, aes_(x=~x, y=~y, label=~label, angle=~angle)))
    text_dot_params <- switch(object$geom,
                             text= reset_dot_params(mapping=text_obj$mapping,
                                            defaultp=params,
                                            default_aes=GeomText$default_aes,
                                            params=object$params),
                             label=reset_dot_params(mapping=text_obj$mapping,
                                                    defaultp=params,
                                                    default_aes=shadowtext_default_aes,
                                                    params=object$params),
                             shadowtext=reset_dot_params(mapping=text_obj$mapping,
                                                         defaultp=params,
                                                         default_aes=shadowtext_default_aes,
                                                         params=object$params)
                             )
    print("text dot")
    print(text_dot_params)
    text_obj <- c(text_obj, text_dot_params)
    text_obj <- switch(object$geom,
                       text = do.call("geom_text", text_obj),
                       label = do.call("geom_label", text_obj),
                       shadowtext = do.call("geom_shadowtext", text_obj)                      
                      )
    return(text_obj)
}

