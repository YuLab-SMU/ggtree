make update;\
sed -i 's/RELEASE_3_13/RELEASE_3_14/g' Makefile;\
sed -i 's/RELEASE_3_12/RELEASE_3_13/g' Makefile;\
make rmoldrelease;\
make release;\
git add .;\
git commit -m 'update bioc version';\
git push
