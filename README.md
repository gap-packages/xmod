[![Build Status](https://github.com/gap-packages/xmod/workflows/CI/badge.svg?branch=master)](https://github.com/gap-packages/xmod/actions?query=workflow%3ACI+branch%3Amaster)
[![Code Coverage](https://codecov.io/github/gap-packages/xmod/coverage.svg?branch=master&token=)](https://codecov.io/gh/gap-packages/xmod)

# The GAP 4 package 'XMod' 

## Introduction 

This package allows for computation with crossed modules; cat1-groups; morphisms of these structures; derivations of crossed modules and the corresponding sections of cat1-groups.

In October 2015 a new section on isoclinism of crossed modules was added. 

Functions for crossed squares and cat2-groups have been added during 2019/20.

## Distribution

 * The 'XMod' package is distributed with the accepted GAP packages, see: 
     <https://www.gap-system.org/Packages/xmod.html>
 * It may also be obtained from the GitHub repository at:
     <https://gap-packages.github.io/xmod/> 

## Copyright

The 'XMod' package is Copyright © Chris Wensley et al, 1997--2025. 

'XMod' is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version. 

For details, see <http://www.gnu.org/licenses/gpl.html> 

## Installation

It is assumed that you have a recent working copy of GAP, and that this contains a full set of packages in the `pkg` directory, compiled as appropriate. 

XMod specifies five needed packages: utils; groupoids; HAP; AutPGrp and SmallGrp.  These in turn need or suggest other packages.  Some of these are loaded automatically when GAP starts. 

The full list of packages which are loaded (in addition to those loaded automatically by GAP) when XMod is loaded is as follows: 
 * AutoDoc; Congruence; datastructures; edim; ferret; nq; polymaking; images; GRAPE; singular; Digraphs; Semigroups; groupoids; HAP and HAPcryst.

XMod does not require any compilation, but those in the list above which require compilation are as follows: 
 * datastructures; Digraphs; GRAPE; Semigroups and HAP. 

Once these prerequisites are in place, proceed as follows: 

 * Unpack `xmod-<version_number>.tar.gz` in the `pkg` subdirectory of the GAP root directory.
 * From within GAP load the package with:
    ```
    gap> LoadPackage( "xmod" );
    true
    ```
 * The file manual.pdf is in the `doc` subdirectory.
 * To run the test file read `testall.g` from the `tst` subdirectory. 

## Contact

If you have a question relating to 'XMod', encounter any problems, or have a suggestion for extending the package in any way, please 
 * email: <mailto:cdwensley.maths@btinternet.com> 
 * or report an issue at: <https://github.com/gap-packages/xmod/issues/new> 
