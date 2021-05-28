#!/bin/sh

# cat testdoc-skeletal-2-citations.json | org-citeproc org chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.org
# cat testdoc-skeletal-2-citations.json | org-citeproc html chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.html
# cat testdoc-skeletal-2-citations.json | org-citeproc odt chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.odt
# cat testdoc-skeletal-2-citations.json | org-citeproc native-before chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output-native-before.hs
# cat testdoc-skeletal-2-citations.json | org-citeproc native chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output-native.hs
# cat testdoc-skeletal-2-citations.json | org-citeproc ascii chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.txt


# ~/src/org-citeproc-final$ stack exec org-citeproc -- -h
# org-citeproc [OPTIONS] [BIB-FILES]
#   -s FILE                                     --style=FILE                                      CSL style file
#   -c FILE                                     --citations=FILE                                  Citations as JSON
#   -f ascii|html|native|native-before|odt|org  --format=ascii|html|native|native-before|odt|org  Output format
#   -h                                          --help                                            Print usage information
#   -V                                          --version                                         Print version number


stack exec org-citeproc -- -f org           -s chicago-author-date.csl -c testdoc-skeletal-2-citations.json testdoc.bib > testdoc-skeletal-2-output.org              
stack exec org-citeproc -- -f html          -s chicago-author-date.csl -c testdoc-skeletal-2-citations.json testdoc.bib > testdoc-skeletal-2-output.html             
stack exec org-citeproc -- -f odt           -s chicago-author-date.csl -c testdoc-skeletal-2-citations.json testdoc.bib > testdoc-skeletal-2-output.odt              
stack exec org-citeproc -- -f native-before -s chicago-author-date.csl -c testdoc-skeletal-2-citations.json testdoc.bib > testdoc-skeletal-2-output-native-before.hs 
stack exec org-citeproc -- -f native        -s chicago-author-date.csl -c testdoc-skeletal-2-citations.json testdoc.bib > testdoc-skeletal-2-output-native.hs        
stack exec org-citeproc -- -f ascii         -s chicago-author-date.csl -c testdoc-skeletal-2-citations.json testdoc.bib > testdoc-skeletal-2-output.txt              
