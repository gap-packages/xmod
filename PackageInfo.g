#############################################################################
##
##  PackageInfo.g  file for the package XMod 
##  Chris Wensley and Murat Alp 
##

SetPackageInfo( rec(
PackageName := "XMod",
Subtitle := "Crossed Modules and Cat1-Groups",

Version := "2.44",
Date := "13/11/2015",

##  duplicate these values for inclusion in the manual: 
##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY VERSION "2.44">
##  <!ENTITY TARFILENAME "xmod-2.44.tar.gz">
##  <!ENTITY HTMLFILENAME "xmod244.html">
##  <!ENTITY RELEASEDATE "13/11/2015">
##  <!ENTITY LONGRELEASEDATE "13th November 2015">
##  <!ENTITY COPYRIGHTYEARS "1997-2015">
##  <#/GAPDoc>

PackageWWWHome := 
  "http://pages.bangor.ac.uk/~mas023/chda/xmod/",

ArchiveURL := "http://pages.bangor.ac.uk/~mas023/chda/xmod/xmod-2.44", 
ArchiveFormats := ".tar.gz",

Persons := [
  rec(
    LastName      := "Wensley",
    FirstNames    := "Christopher D.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "c.d.wensley@bangor.ac.uk",
    WWWHome       := "http://www.bangor.ac.uk/~mas023/",
    PostalAddress := Concatenation( [
                       "Dr. C.D. Wensley\n",
                       "School of Computer Science\n",
                       "Bangor University\n",
                       "Dean Street\n",
                       "Bangor\n",
                       "Gwynedd LL57 1UT\n",
                       "UK"] ),
    Place         := "Bangor",
    Institution   := "Bangor University"
  ),
  rec(
    LastName      := "Alp",
    FirstNames    := "Murat",
    IsAuthor      := true,
    Email         := "muratalp@nigde.edu.tr",
    PostalAddress := Concatenation( [ 
                       "Prof. Dr. M. Alp\n",
                       "Nigde Universitesi\n",
                       "Fen-Edebiyat Fakultesi\n",
                       "Matematik Bolumu\n",
                       "Nigde\n",
                       "Turkey"] ),
    Place         := "Nigde",
    Institution   := "Nigde University"
  ),
    rec(
    LastName      := "Odabas",
    FirstNames    := "Alper",
    IsAuthor      := true,
    Email         := "aodabas@ogu.edu.tr",
    PostalAddress := Concatenation( [ 
                       "Dr. A. Odabas \n",
                       "Osmangazi University \n",
                       "Arts and Sciences Faculty \n",
                       "Department of Mathematics and Computer Science \n",
                       "Eskisehir \n",
                       "Turkey"] ),
    Place         := "Eskisehir",
    Institution   := "Osmangazi University"
  ),
    rec(
    LastName      := "Uslu",
    FirstNames    := "Enver Onder",
    IsAuthor      := true,
    Email         := "enveruslu@ogu.edu.tr",
    PostalAddress := Concatenation( [ 
                       "Dr. E. O. Uslu \n",
                       "Osmangazi University \n",
                       "Arts and Sciences Faculty \n",
                       "Department of Mathematics and Computer Science \n",
                       "Eskisehir \n",
                       "Turkey"] ),
    Place         := "Eskisehir",
    Institution   := "Osmangazi University"
  )
],

Status := "accepted",
CommunicatedBy := "Derek Holt (Warwick)",
AcceptDate := "12/1996",

README_URL := 
  Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL := 
  Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),

##  Optional:
##    - Type and the URL of the source code repository
##    - URL of the public issue tracker
##    - Support email address
SourceRepository :=
  rec( Type := "git", # must be one of "git", "hg", "svn", "cvs"
       URL  := "http://github.com/gap-packages/xmod"),
IssueTrackerURL := "http://github.com/gap-packages/xmod/issues",
SupportEmail := "c.d.wensley@bangor.ac.uk",

AbstractHTML :=
 "The <span class=\"pkgname\">XMod</span> package provides a collection \
  of functions for computing with crossed modules and cat1-groups, \
their derivations and sections, morphisms of these structures, \
and higher-dimensional generalisations.",

PackageDoc := rec(
  BookName  := "XMod",
  ArchiveURLSubset := ["doc"],
  HTMLStart := "doc/chap0.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Crossed Modules and Cat1-Groups in GAP",
  Autoload  := true
),

Dependencies := rec(
  GAP := ">=4.7",
  NeededOtherPackages := [ ["Gpd", ">= 1.34"], 
                           ["Hap", ">= 1.10"],
                           ["autpgrp", ">= 1.5"] ], 
  SuggestedOtherPackages := [ ["GAPDoc", ">= 1.5.1"] ],
  ExternalConditions := [ ]
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation( 
  "Loading XMod ", String( ~.Version ), " for GAP 4.7", 
  " - methods for crossed modules and cat1-groups\n",
  "by Chris Wensley (c.d.wensley@bangor.ac.uk), with contributions by:\n", 
  "    Murat Alp (muratalp@nigde.edu.tr),\n", 
  "    Alper Odabas (aodabas@ogu.edu.tr),\n", 
  "and Enver Uslu (enveruslu@ogu.edu.tr).\n" 
),

Autoload := false,

TestFile := "tst/testall.g",

Keywords := ["crossed module", "cat1-group", "derivation", "section", 
             "actor", "crossed square" ]

));
