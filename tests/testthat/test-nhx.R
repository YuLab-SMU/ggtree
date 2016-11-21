context('nhx')

# Some sample NHX tree strings
test_nhx_text = "(((ADH2:0.1[&&NHX:S=human], ADH1:0.11[&&NHX:S=human]):0.05[&&NHX:S=primates:D=Y:B=100], ADHY:0.1[&&NHX:S=nematode],ADHX:0.12[&&NHX:S=insect]):0.1[&&NHX:S=metazoa:D=N], (ADH4:0.09[&&NHX:S=yeast],ADH3:0.13[&&NHX:S=yeast], ADH2:0.12[&&NHX:S=yeast],ADH1:0.11[&&NHX:S=yeast]):0.1 [&&NHX:S=Fungi])[&&NHX:D=N];"

test_phyldog_nhx_text = "(((Prayidae_D27SS7@2825365:0.0682841[&&NHX:Ev=S:S=58:ND=0],(Kephyes_ovata@2606431:0.0193941[&&NHX:Ev=S:S=69:ND=1],Chuniphyes_multidentata@1277217:0.0121378[&&NHX:Ev=S:S=70:ND=2]):0.0217782[&&NHX:Ev=S:S=60:ND=3]):0.0607598[&&NHX:Ev=S:S=36:ND=4],((Apolemia_sp_@1353964:0.11832[&&NHX:Ev=S:S=31:ND=9],(((Bargmannia_amoena@263997:0.0144549[&&NHX:Ev=S:S=37:ND=10],Bargmannia_elongata@946788:0.0149723[&&NHX:Ev=S:S=38:ND=11]):0.0925388[&&NHX:Ev=S:S=33:ND=12],Physonect_sp_@2066767:0.077429[&&NHX:Ev=S:S=61:ND=13]):0.0274637[&&NHX:Ev=S:S=24:ND=14],(Stephalia_dilata@2960089:0.0761163[&&NHX:Ev=S:S=52:ND=15],((Frillagalma_vityazi@1155031:0.0906068[&&NHX:Ev=S:S=53:ND=16],Resomia_ornicephala@3111757:1e-06[&&NHX:Ev=S:S=54:ND=17]):1e-06[&&NHX:Ev=S:S=45:ND=18],((Lychnagalma_utricularia@2253871:0.120851[&&NHX:Ev=S:S=65:ND=19],Nanomia_bijuga@717864:0.133939[&&NHX:Ev=S:S=71:ND=20]):1e-06[&&NHX:Ev=S:S=56:ND=21],Cordagalma_sp_@1525873:0.0693814[&&NHX:Ev=S:S=64:ND=22]):1e-06[&&NHX:Ev=S:S=46:ND=23]):0.0333823[&&NHX:Ev=S:S=40:ND=24]):1e-06[&&NHX:Ev=S:S=35:ND=25]):0.0431861[&&NHX:Ev=D:S=24:ND=26]):1e-06[&&NHX:Ev=S:S=19:ND=27],Rhizophysa_filiformis@3073669:0.22283[&&NHX:Ev=S:S=26:ND=28]):0.0292362[&&NHX:Ev=S:S=17:ND=29]):0.185603[&&NHX:Ev=D:S=17:ND=8],(Hydra_magnipapillata@52244:0.0621782[&&NHX:Ev=S:S=16:ND=5],Ectopleura_larynx@3556167:0.332505[&&NHX:Ev=S:S=15:ND=6]):0.185603[&&NHX:Ev=S:S=12:ND=7])[&&NHX:Ev=S:S=9:ND=30];"

test_notung_nhx_text = "((((Rhizophysa_filiformis@2564549:0.09666991738603078[&&NHX:S=Rhizophysa_filiformis],((Marrus_claudanielis@2027078:0.03368582974818837[&&NHX:S=Marrus_claudanielis],((Erenna_richardi@1434201:0.014306889954561298[&&NHX:S=Erenna_richardi],Marrus_claudanielis@2027079:0.010842363778569869[&&NHX:S=Marrus_claudanielis])n5940011:0.01779384958849464[&&NHX:S=n57:D=N],(((Agalma_elegans@88626:0.05872379503260147[&&NHX:S=Agalma_elegans],Lychnagalma_utricularia@1828459:0.04211137470826968[&&NHX:S=Lychnagalma_utricularia])n5940018:0.02375590664436535[&&NHX:S=n47:D=N],(((Bargmannia_amoena@3459111:0.19058396964770352[&&NHX:S=Bargmannia_amoena],Bargmannia_elongata@469437:1.00000050002909E-6[&&NHX:S=Bargmannia_elongata])n5939974:0.11560220708003867[&&NHX:S=n22:D=N],Cordagalma_sp_@1115328:0.04829417133033771[&&NHX:S=Cordagalma_sp_])n5939976:0.011316847557531757[&&NHX:S=n62:D=N],Forskalia_asymmetrica@1220430:0.01667566952752948[&&NHX:S=Forskalia_asymmetrica])n5939978:0.0063213422810751655[&&NHX:S=n62:D=Y])n5940014:0.017792661031819083[&&NHX:S=n62:D=Y],(Resomia_ornicephala@2657185:0.004262563771468986[&&NHX:S=Resomia_ornicephala],Frillagalma_vityazi@663744:0.028441637105547157[&&NHX:S=Frillagalma_vityazi])n5939981:0.006136291467151878[&&NHX:S=n51:D=N])n5940013:0.013546839136761205[&&NHX:S=n62:D=Y])n5940012:0.011839606018978143[&&NHX:S=n62:D=Y])n5940008:0.013840645450221475[&&NHX:S=n62:D=Y],(((Chelophyes_appendiculata@1615707:0.007647023552225329[&&NHX:S=Chelophyes_appendiculata],Clytia_hemisphaerica@756642:0.643907456299178[&&NHX:S=Clytia_hemisphaerica])n5939984:0.08603691877960613[&&NHX:S=n67:D=N],(Chuniphyes_multidentata@930929:0.01248550133310033[&&NHX:S=Chuniphyes_multidentata],Kephyes_ovata@1966030:0.014671165587181996[&&NHX:S=Kephyes_ovata])n5939987:0.013285803501636162[&&NHX:S=n27:D=N])n5939988:0.008000411801689693[&&NHX:S=n67:D=Y],(((Hippopodius_hippopus@1084434:0.0505718831943577[&&NHX:S=Hippopodius_hippopus],Prayidae_D27D2@2878798:0.00905875758406546[&&NHX:S=Prayidae_D27D2])n5939991:0.021772123626769023[&&NHX:S=n38:D=N],Prayidae_D27SS7@2181711:0.029009000260863272[&&NHX:S=Prayidae_D27SS7])n5939993:1.00000050002909E-6[&&NHX:S=n38:D=Y],Prayidae_D27D2@2878801:1.00000050002909E-6[&&NHX:S=Prayidae_D27D2])n5939995:0.00916688375355408[&&NHX:S=n38:D=Y])n5939996:0.05191099091093772[&&NHX:S=n67:D=Y])n5940006:0.03953811931719265[&&NHX:S=n67:D=Y])n5940005:0.10134081070615458[&&NHX:S=n67:D=Y],(Podocoryna_carnea@3033951:0.11270255504816476[&&NHX:S=Podocoryna_carnea],Hydractinia_symbiolongicarpus@1679508:0.030168043235021993[&&NHX:S=Hydractinia_symbiolongicarpus])n5939999:0.17223048099362362[&&NHX:S=n11:D=N])n5940003:0.16233679521228994[&&NHX:S=n67:D=Y],Hydra_magnipapillata@801936:0.585696573276294[&&NHX:S=Hydra_magnipapillata])n5940002:0.4403044529817829[&&NHX:S=n68:D=N],Aegina_citrea@825314:0.4403044529817829[&&NHX:S=Aegina_citrea])n5942419[&&NHX:S=n70:D=N];"

# A function to simplify NHX text so that it can be parsed by
# ape::read.tree(). Discards much useful information. Intent is to
# be able to compare node annotations that have been independently
# parsed with different methods.
simplify_nhx_string <- function( text ){
	# Remove branch lengths so NHX tags are adjacent to nodes
	# Accommodate lengths in scientific notation, eg 1e-6
	text = gsub( "\\:[\\d\\.]+e[\\d\\-]+","", text, perl=TRUE )
	text = gsub( "\\:[\\d\\.]+","", text, perl=TRUE )

	# Remove NHX tags at tips
	text = gsub( "([^\\)])\\[.+?\\]","\\1", text, perl=TRUE )

	# Remove brackets
	text = gsub( "\\[&&NHX:","", text, perl=TRUE )
	text = gsub( "\\]","", text, perl=TRUE )

	# Replace NHX tag formatting characters that aren't allowed
	text = gsub( ":","_", text, perl=TRUE )
	text = gsub( "=","-", text, perl=TRUE )

	return(text)
}


test_that("can parse example ggtree nhx tree string", {
	nhx <- read.nhx( textConnection(test_nhx_text) )
	ntips = length(nhx@phylo$tip.label)

	# Correct number of tips
	expect_equal( ntips , 8 )

	# All node numbers, including tips, are parsed
	expect_true( all(!is.na(nhx@nhx_tags$node)) )
})

test_that("can parse phyldog nhx tree string", {
	nhx <- read.nhx( textConnection(test_phyldog_nhx_text) )
	ntips = length(nhx@phylo$tip.label)

	# Correct number of tips
	expect_equal( ntips , 16 )

	# All node numbers, including tips, are parsed
	expect_true( all(!is.na(nhx@nhx_tags$node)) )

	# Verify that the S field of internal nodes was correctly parsed
	# Assumes that node identity order is the same between phy and nhx@phylo
	tags = nhx@nhx_tags
	tags$node = as.numeric(tags$node)
	tags = tags[order(tags$node),]
	internal_tags = tags[ tags$node > length(nhx@phylo$tip.label), ] # Consider internal nodes only


	phy = read.tree(text=simplify_nhx_string(test_phyldog_nhx_text))
	phy_S=unlist(lapply(strsplit(phy$node.label, "_"), function(x) x[[2]])) # Get the S field
	phy_S=unlist(lapply(strsplit(phy_S, "-"), function(x) x[[2]])) # Get the value
	phy_S=as.numeric(phy_S)
	expect_equal( phy_S, as.numeric(internal_tags$S) )

	# Verify that S fild of tips was correctly parsed
	# by comparison against expected values
	tip_tags = tags[1:length(nhx@phylo$tip.label),]
	tip.labels = c("Prayidae_D27SS7@2825365", "Kephyes_ovata@2606431", "Chuniphyes_multidentata@1277217", "Apolemia_sp_@1353964", "Bargmannia_amoena@263997", "Bargmannia_elongata@946788", "Physonect_sp_@2066767", "Stephalia_dilata@2960089", "Frillagalma_vityazi@1155031", "Resomia_ornicephala@3111757", "Lychnagalma_utricularia@2253871", "Nanomia_bijuga@717864", "Cordagalma_sp_@1525873", "Rhizophysa_filiformis@3073669", "Hydra_magnipapillata@52244", "Ectopleura_larynx@3556167")
	S.tip.values = c(58, 69, 70, 31, 37, 38, 61, 52, 53, 54, 65, 71, 64, 26, 16, 15)
	expect_equal( S.tip.values[match(nhx@phylo$tip.label, tip.labels)], as.numeric(tip_tags$S))

})

test_that("can drop tips", {
	nhx <- read.nhx( textConnection(test_phyldog_nhx_text) )
        to_drop = c("Physonect_sp_@2066767", "Lychnagalma_utricularia@2253871", "Kephyes_ovata@2606431")

	nhx_reduced = drop.tip(nhx, to_drop)

	# Make sure node numbers unique
	expect_false( any(duplicated(nhx_reduced@nhx_tags$node)) )

	expect_equal( length(nhx_reduced@phylo$tip.label), 13 )
        expect_true( all(nhx_reduced@nhx_tags$node %in% fortify(nhx_reduced)$node) )
})
