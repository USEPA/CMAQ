#!/bin/csh 
# Change to user environment built in earlier step
source /home/jbrunton/Sphinx_Env/bin/activate.csh

rm -rf source/DOCS/
mkdir source/DOCS/
cp -r ../DOCS/* source/DOCS/

make clean

python3.11 Relative_Link_Deleter.py

sphinx-build source/ _build/

rm -rf ../docs/
mkdir ../docs/
cp -r _build/* ../docs/
touch ../docs/.nojekyll
