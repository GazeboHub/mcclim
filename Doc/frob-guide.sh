#!/bin/sh

#
## Trival PDF generation - shell script 
#
# Author: Sean Champ
# Date: 16 Dec 2014
# Edition: 1.0
#
### Overview
#
# This script applies Saxon "Home Edition" version 9.4.0.7 (available
# of Debian package `libsaxonhe-java') using the Docbook 5 modular XSL
# stylesheets, in their "Namespace" derivative (available of Debian
# package `docbook-xsl-ns')
#
### PDF Generation
#
# This script is developed singularly for PDF generation. This script
# uses the DocBook 'fo' stylesheet - cf. XSL formatting objects -
# together with the Formatting Objects Processor (FOP) as develpoed
# under the Apache Software Foundation (ASF). FOP is implemented in
# Java. This script uses the simple 'fop' shell command, available of
# Debian package `fop'
#
### Source Doucment Validation
#
# This script does not explicitly depend on any DocBook
# schema. However, for validation of the source document
# 'mcclim-guide.xml', the source tree 'mci-doc-docbook' is required,
# such that provides an extension onto the DocBook 5.0 schema in its  
# RELAX NG compact syntax (RNC) edition
#
### "TO DO"
#
# This is essentially a trivial convenience script, as in lieu of any
# definition of a 'java-application' class, such as in the
# 'dobelle-app' system, or any corresponding component definitions
# onto ASDF.
#
##
#


GUIDE=mcclim-guide.xml
SAXON=/usr/share/java/Saxon-HE-9.4.0.7.jar
XSL_FO=/usr/share/xml/docbook/stylesheet/docbook-xsl-ns/fo/docbook.xsl

GUIDE_PDF=mcclim-guide.pdf

java -jar "${SAXON}" "${GUIDE}" "${XSL_FO}" | fop - "${GUIDE_PDF}"

