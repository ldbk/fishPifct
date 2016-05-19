fishPifct
=====

fishPifct is an R package combining some functions used in the fishPi project
([fishPi](<http://fishPi/link/>)).
Some of these functions are coming from the SDEFQuality package from Norbert Billet ([SDEFQuality](<https://git.outils-is.ird.fr/billet/SDEFQuality>)).

#Installation

The package source can be downloaded here:

https://dl.dropboxusercontent.com/u/6181692/fishPifct_0.9.tar.gz

There are some issues with the openxlsx package installation (needed to import and export csPi and csData object in excel file).
Please read carefully the error messages R gives to you (the way to fix these errors are explained to you in these messages).
The average procedure to fix them should be something like:

`install.packages("installr")`

`installr::installr("Rtools")`

During the installtion, tick the PATH modification option.
Restart your computer.


Or compile yourself the up-to-date version:

Windows users: you have to install this before: https://cran.r-project.org/bin/windows/Rtools/

In a R console:

`install.packages("devtools")`

`library(devtools)`

`install_github("ldbk/fishPifct")`

If needed, COST related package (for windows) can be found here :

https://dl.dropboxusercontent.com/u/6181692/COSTcore_1.4-0.zip

https://dl.dropboxusercontent.com/u/6181692/COSTdbe_1.4-1.zip

https://dl.dropboxusercontent.com/u/6181692/COSTeda_1.4.0.zip

A document :

https://dl.dropboxusercontent.com/u/6181692/COST%20User%20Manual%20V1_1.pdf


#Description

This package is dedicated to the ctPi and csPi formats 
(coded during fishPi and WKRDB
meetings). 
Some functions allow the user to export COST object in the csPi format,
to export and import object in excel, etc...
These functions are coming from different sources: COSTcore package, 
Alastair Pout, Norbert Billet and Laurent Dubroca.

Help pages are generated using the roxygen syntax.

Contribute !!!



