cd ENT-rank/trec-car-tools-haskell/simplir/

git push origin HEAD:master

cd ..

git commit -a -m "bump simplir"
git push github HEAD:master
git push origin HEAD:master

cd ..

git commit -a -m "bump trec-car-tools-haskell"
git push origin HEAD:master

cd ..

git commit -a -m "bump ENT-rank"
git push origin HEAD:master

