##' @export
geom_cladelab <- function(data=NULL,
                          mapping=NULL,
                          node=NULL,
                          label=NULL,
                          geom="text",
                          parse=FALSE,
                          ...
                          ){
    params <- list(...)
    structure(
              list(
                  data=data,
                  mapping=mapping,
                  node=node,
                  label=label,
                  geom=geom,
                  parse=parse,
                  params=params
                  ),
              class="cladelab"
              )

}
