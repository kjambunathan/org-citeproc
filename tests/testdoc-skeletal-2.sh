#!/bin/sh

cat testdoc-skeletal-2-citations.json | org-citeproc org chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.org
cat testdoc-skeletal-2-citations.json | org-citeproc html chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.html
cat testdoc-skeletal-2-citations.json | org-citeproc odt chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.odt
cat testdoc-skeletal-2-citations.json | org-citeproc native-before chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output-native-before.hs
cat testdoc-skeletal-2-citations.json | org-citeproc native chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output-native.hs
cat testdoc-skeletal-2-citations.json | org-citeproc ascii chicago-author-date.csl testdoc.bib > testdoc-skeletal-2-output.txt
