PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: rd check clean

alldocs: rd readme

rd:
	Rscript -e 'library(methods); devtools::document()'
# Rscript -e 'roxygen2::roxygenise(".")'

readme:
	Rscript -e 'rmarkdown::render("README.Rmd", encoding="UTF-8")'

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

check: rd build
	cd ..;\
	Rscript -e "rcmdcheck::rcmdcheck('$(PKGNAME)_$(PKGVERS).tar.gz')"

check2: rd build
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

check3: rd build2
	cd ..;\
	R CMD check --ignore-vignettes $(PKGNAME)_$(PKGVERS).tar.gz

bioccheck:
	cd ..;\
	Rscript -e "BiocCheck::BiocCheck('$(PKGNAME)_$(PKGVERS).tar.gz')"

clean:
	cd ..;\
	$(RM) -r $(PKGNAME).Rcheck/


gitmaintain:
	git gc --auto;\
	git prune -v;\
	git fsck --full

biocinit:
	git remote add upstream git@git.bioconductor.org:packages/$(PKGNAME).git;\
	git fetch --all

rmoldrelease:
	git branch -D RELEASE_3_9

release:
	git checkout RELEASE_3_10;\
	git fetch --all

update:
	git fetch --all;\
	git checkout master;\
	git merge upstream/master;\
	git merge origin/master

push:
	git push upstream master;\
	git push origin master


# svnignore:
# 	svn propset svn:ignore -F .svnignore .

# svncommit:
# 	git checkout devel;\
# 	git svn rebase;\
# 	git merge master --log;\
# 	git svn dcommit;\
# 	git push -u origin devel;\
# 	git checkout master;\
# 	git merge devel
