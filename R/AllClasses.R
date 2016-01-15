setOldClass("phylo")
setOldClass("multiPhylo")
setOldClass("gg")
setOldClass("ggplot")

setClassUnion("phyloOrmultiPhylo", c("phylo", "multiPhylo"))


supported_tree_object <- function() {
    c("apeBootstrap",
      "beast",
      "codeml_mlc",
      "codeml",
      "hyphy",
      "jplace",
      "nhx",
      "paml_rst",
      "phangorn",      
      "phylip",
      "r8s",
      "raxml")
}

##' Class "apeBootstrap"
##' This class stores ape bootstrapping analysis result
##'
##'
##' @name apeBootstrap-class
##' @docType class
##' @slot phylo phylo object of treetext
##' @slot fields available features
##' @slot bootstrap bootstrap value
##' @slot extraInfo extra information
##' @exportClass apeBootstrap
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @keywords classes
setClass("apeBootstrap",
         representation = representation(
             phylo = "phylo",
             fields = "character",
             bootstrap = "data.frame",
             extraInfo = "data.frame"
         )
         )

##' Class "beast"
##' This class stores information of beast output
##'
##'
##' @name beast-class
##' @aliases beast-class
##'      get.tree,beast-method
##' 
##' @docType class
##' @slot fields beast statistic variables
##' @slot treetext tree text in beast file
##' @slot phylo tree phylo object
##' @slot translation tip number to name translation in beast file
##' @slot stats beast statistics
##' @slot file beast file, nexus format
##' @slot extraInfo extra information
##' @exportClass beast
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @seealso \code{\link{show}} \code{\link{get.fields}}
##'           \code{\link{ggtree}}
##' @keywords classes
setClass("beast",
         representation  = representation(
             fields      = "character",
             treetext    = "character",
             phylo       = "phylo",
             translation = "matrix",
             stats       = "data.frame",
             file        = "character",
             extraInfo   = "data.frame"
             )
         )


##' Class "codeml_mlc"
##' This class stores information of mlc file frm codeml output
##'
##'
##' @name codeml_mlc-class
##' @docType class
##' @slot fields available features
##' @slot treetext tree text
##' @slot phylo phylo object
##' @slot dNdS dN dS information
##' @slot mlcfile mlc file
##' @slot extraInfo extra information
##' @exportClass codeml_mlc
##' @author Guangchuang Yu
##' @seealso \linkS4class{paml_rst} \linkS4class{codeml}
##' @keywords classes
setClass("codeml_mlc",
         representation = representation(
             fields     = "character",
             treetext   = "character",
             phylo      = "phylo",
             dNdS       = "matrix",
             ## seq_type   = "character",
             ## tip_seq    = "character",
             mlcfile    = "character",
             extraInfo  = "data.frame"
             )
         )

##' Class "paml_rst"
##' This class stores information of rst file from PAML output
##'
##'
##' @name paml_rst-class
##' @aliases paml_rst-class
##'   set.subs,paml_rst-method
##'   set.subs<-,paml_rst-method
##' 
##' @docType class
##' @slot fields availabel attributes
##' @slot treetext tree text
##' @slot phylo phylo object
##' @slot seq_type one of "NT" and "AA"
##' @slot tip_seq sequences of tips
##' @slot marginal_ancseq Marginal reconstruction of ancestral sequences
##' @slot joint_ancseq Joint reconstruction of ancestral sequences
##' @slot marginal_subs sequence substitutions based on marginal_ancseq
##' @slot joint_subs sequence substitutions based on joint_ancseq
##' @slot marginal_AA_subs Amino acid sequence substitutions based on marginal_ancseq
##' @slot joint_AA_subs Amino acid sequence substitutions based on joint_ancseq
##' @slot rstfile rst file
##' @slot extraInfo extra information
##' @exportClass paml_rst
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @seealso \linkS4class{codeml} \linkS4class{codeml_mlc}
##' @keywords classes
setClass("paml_rst",
         representation       = representation(
             fields           = "character",
             treetext         = "character",
             phylo            = "phylo",
             seq_type         = "character",
             tip_seq          = "character",
             marginal_ancseq  = "character",
             joint_ancseq     = "character",
             marginal_subs    = "data.frame",
             joint_subs       = "data.frame",
             marginal_AA_subs = "data.frame",
             joint_AA_subs    = "data.frame",
             rstfile          = "character",
             extraInfo        = "data.frame"
         )
         )

##' Class "codeml"
##' This class stores information of output from codeml
##'
##'
##' @name codeml-class
##' @docType class
##' @slot mlc A \code{code_mlc} object
##' @slot rst A \code{paml_rst} object
##' @slot extraInfo extra information
##' @exportClass codeml
##' @seealso \linkS4class{codeml_mlc} \linkS4class{paml_rst}
##' @keywords codeml
setClass("codeml",
         representation = representation(
             mlc       = "codeml_mlc",
             rst       = "paml_rst",
             extraInfo = "data.frame"
             )
         )



##' Class "hyphy"
##' This class stores information of HYPHY output
##'
##'
##' @name hyphy-class
##' @docType class
##' @slot fields available features
##' @slot treetext tree text
##' @slot phylo phylo object
##' @slot seq_type one of "NT" and "AA"
##' @slot subs sequence substitutions
##' @slot AA_subs Amino acid sequence substitution
##' @slot ancseq ancestral sequences
##' @slot tip_seq tip sequences
##' @slot tip.fasfile fasta file of tip sequences
##' @slot tree.file tree file
##' @slot ancseq.file ancestral sequence file, nexus format
##' @slot extraInfo extra information
##' @exportClass hyphy
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @seealso \linkS4class{paml_rst}
##' @keywords classes
setClass("hyphy",
         representation  = representation(
             fields      = "character",
             treetext    = "character",
             phylo       = "phylo",
             seq_type    = "character",
             subs        = "data.frame",
             AA_subs     = "data.frame",
             ancseq      = "character",
             tip_seq     = "character",
             tip.fasfile = "character",
             tree.file   = "character",
             ancseq.file = "character",
             extraInfo   = "data.frame"
             )
         )

##' Class "jplace"
##' This class stores information of jplace file.
##'
##'
##' @name jplace-class
##' @aliases jplace-class
##'   show,jplace-method
##'   get.placements,jplace-method
##'   get.treeinfo,jplace-method
##'   get.fields,jplace-method
##'   get.treetext,jplace-method
##'
##' @docType class
##' @slot fields colnames of first variable of placements
##' @slot treetext tree text
##' @slot phylo tree phylo object
##' @slot placements placement information
##' @slot version version
##' @slot metadata metadata
##' @slot file jplace file
##' @slot extraInfo extra information
##' @exportClass jplace
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @seealso \code{\link{show}} \code{\link{get.tree}}
##'          \code{\link{ggtree}}
##' @keywords classes
setClass("jplace",
         representation = representation(
             fields     = "character",
             treetext   = "character",
             phylo      = "phylo",
             placements = "data.frame",
             version    = "numeric",
             metadata   = "list",
             file       = "character",
             extraInfo  = "data.frame"
             )
         )


##' Class "nhx"
##' This class stores nhx tree
##'
##'
##' @name nhx-class
##' @rdname nhx-class
##' @docType class
##' @slot file input file
##' @slot fields available feature
##' @slot phylo phylo object
##' @slot nhx_tags tag information in nhx file
##' @slot extraInfo extra information
##' @exportClass nhx
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @keywords classes
setClass("nhx",
         representation = representation(
             file = "character",
             fields = "character",
             phylo = "phylo",
             nhx_tags = "data.frame",
             extraInfo = "data.frame"
         )
         )


##' Class "phangorn"
##' This class stores ancestral sequences inferred from 'phangorn'
##'
##'
##' @name phangorn-class
##' @docType class
##' @slot fields available attributes
##' @slot phylo phylo object
##' @slot seq_type one of "NT" and "AA"
##' @slot tip_seq sequences of tips
##' @slot ancseq ancenstral sequences
##' @slot subs sequence substitution
##' @slot AA_subs Amino acid sequence substitution
##' @slot extraInfo extra information
##' @exportClass phangorn
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @seealso \linkS4class{paml_rst}
##' @keywords classes
setClass("phangorn",
         representation = representation(
             fields = "character",
             phylo = "phylo",
             seq_type = "character",
             tip_seq = "character",
             ancseq = "character",
             subs = "data.frame",
             AA_subs = "data.frame",
             extraInfo = "data.frame")
         )


##' Class "phylip"
##' This class stores phylip tree(s)
##'
##'
##' @name phylip-class
##' @docType class
##' @slot file input file
##' @slot fields available feature
##' @slot phylo phylo or multiPhylo
##' @slot ntree number of trees
##' @slot sequence sequences
##' @slot extraInfo extra information
##' @exportClass phylip
##' @author Guangchuang Yu
##' @keywords classes
setClass("phylip",
         representation = representation(
             file = "character",
             fields = "character",
             phylo = "phyloOrmultiPhylo",
             ntree = "numeric",
             sequence = "BStringSet",
             extraInfo = "data.frame")
         )


##' Class "r8s"
##' This class stores output info from r8s
##'
##'
##' @name r8s-class
##' @docType class
##' @slot file input file
##' @slot fields available feature
##' @slot treetext tree text
##' @slot phylo multiPhylo, time tree, rate tree and absolute substitution tree
##' @slot extraInfo extra information
##' @exportClass r8s
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @keywords classes
setClass("r8s",
         representation = representation(
             file      = "character",
             fields    = "character",
             treetext  = "character",
             phylo     = "multiPhylo",
             extraInfo = "data.frame"
             )
         )


##' Class "raxml"
##' This class stores RAxML bootstrapping analysis result
##'
##'
##' @name raxml-class
##' @docType class
##' @slot file input file
##' @slot fields available features
##' @slot treetext tree text
##' @slot phylo phylo object of treetext
##' @slot bootstrap bootstrap value
##' @slot extraInfo extra information
##' @exportClass raxml
##' @author Guangchuang Yu \url{http://guangchuangyu.github.io}
##' @keywords classes
setClass("raxml",
         representation = representation(
             file       = "character",
             treetext   = "character"
         ),
         contains = "apeBootstrap"
         )


