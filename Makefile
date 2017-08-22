PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: rd readme check clean

alldocs: site rd readme

rd:
	Rscript -e 'roxygen2::roxygenise(".")'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd")'

sticker:
	Rscript -e 'source("ggtree_sticker.R")';
	rm Rplots.pdf

build:
	cd ..;\
	R CMD build $(PKGSRC)

build2:
	cd ..;\
	R CMD build --no-build-vignettes $(PKGSRC)

install:
	cd ..;\
	R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz

check: build
	cd ..;\
	Rscript -e 'rcmdcheck::rcmdcheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

check2: build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

bioccheck:
	cd ..;\
	Rscript -e 'BiocCheck::BiocCheck("$(PKGNAME)_$(PKGVERS).tar.gz")'

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/

site:
	cd site_src;\
	ln -s ../../software/themes themes;\
	Rscript -e 'blogdown::build_site()';\
	rm themes;\
	cd ..

preview:
	Rscript -e 'setwd("site_src"); blogdown::serve_site()'

mkdocs: mdfiles
	cd mkdocs;\
	mkdocs build;\
	cd ../docs;\
	rm -rf fonts;\
	rm -rf css/font-awesome*

mdfiles:
	cd mkdocs;\
	Rscript -e 'source("render.R")';\
	cd docs;\
	ln -f -s ../mysoftware/* ./

svnignore:
	svn propset svn:ignore -F .svnignore .

svncommit:
	git checkout devel;\
	git svn rebase;\
	git merge master --log;\
	git svn dcommit;\
	git push -u origin devel;\
	git checkout master;\
	git merge devel


gitmaintain:
	git gc --auto;\
	git prune -v;\
	git fsck --full


pushX:
	git push -u origin master;\
	git checkout bioc;\
	git pull;\
	git merge master;\
	git push upstream master;\
	git checkout master

update:
	git fetch --all;\
	git checkout master;\
	git merge upstream/master;\
	git merge origin/master

push: update
	git push upstream master;\
	git push origin master
