library(ggbio)
library(biovizBase)
library(Homo.sapiens)

library(TxDb.Hsapiens.UCSC.hg19.knownGene)
txdb <- TxDb.Hsapiens.UCSC.hg19.knownGene

data(genesymbol, package = "biovizBase")
wh <- genesymbol[c("BRCA1", "NBR1")]
wh <- range(wh, ignore.strand = TRUE)

gr.txdb <- crunch(txdb, which = wh)
## change column to  model
colnames(values(gr.txdb))[4] <- "model"
grl <- split(gr.txdb, gr.txdb$tx_id)
## fake some randome names
set.seed(2016-10-25)
names(grl) <- sample(LETTERS, size = length(grl), replace = TRUE)


## the random tree

library(ggtree)
n <- names(grl) %>% unique %>% length
set.seed(2016-10-25)
tr <- rtree(n)
set.seed(2016-10-25)
tr$tip.label = sample(unique(names(grl)), n)

p <- ggtree(tr, color='grey') + geom_tiplab(align=T, linesize=.05,
                                            linetype='dashed', size=1.2, color='grey')

##  align the alignment with tree
p2 <- facet_plot(p, 'Alignment', grl, geom_alignment, inherit.aes=FALSE,
                 alpha=.6, size=.1, mapping=aes(), color='grey', extend.size=.1)
p2 <- p2 + theme_transparent() + theme(strip.text = element_blank())+xlim_tree(3.4)


#################################
library(hexSticker)
sticker(p2, package="ggtree", p_x=1, p_y=1.5, p_size=9, s_x=.85, s_y = .68, s_width=.95, s_height=.65,
        h_color="#2C3E50", h_fill="#2574A9", filename="docs/ggtree.png")

