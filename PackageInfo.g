#############################################################################
##
##  PackageInfo.g  file for the package XMod 
##  Chris Wensley and Murat Alp 
##

SetPackageInfo( rec(
PackageName := "XMod",
Packagename := "xmod",
Subtitle := "Crossed Modules and Cat1-Groups",

Version := "2.58",
Date := "02/11/2016",

##  duplicate these values for inclusion in the manual: 
##  <#GAPDoc Label="PKGVERSIONDATA">
##  <!ENTITY XMODVERSION "2.58">
##  <!ENTITY XMODTARFILENAME "xmod-2.58.tar.gz">
##  <!ENTITY XMODHTMLFILENAME "xmod.html">
##  <!ENTITY XMODRELEASEDATE "02/11/2016">
##  <!ENTITY XMODLONGRELEASEDATE "2nd November 2016">
##  <!ENTITY XMODCOPYRIGHTYEARS "1997-2016">
##  <#/GAPDoc>

Persons := [
  rec(
    LastName      := "Wensley",
    FirstNames    := "Christopher D.",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "c.d.wensley@bangor.ac.uk",
    WWWHome       := "http://pages.bangor.ac.uk/~mas023/",
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
    IsMaintainer  := false,
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
    IsMaintainer  := false,
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
    IsMaintainer  := false,
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

SourceRepository := rec( 
  Type := "git", 
  URL := "https://github.com/gap-packages/xmod"
),
IssueTrackerURL := Concatenation( ~.SourceRepository.URL, "/issues" ),
PackageWWWHome  := "http://gap-packages.github.io/xmod/",
README_URL      := Concatenation( ~.PackageWWWHome, "README" ),
PackageInfoURL  := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
ArchiveURL      := Concatenation( ~.SourceRepository.URL, 
                                  "/releases/download/v", ~.Version, 
                                  "/", ~.PackageName, "-", ~.Version ), 
SupportEmail := "c.d.wensley@bangor.ac.uk",
ArchiveFormats  := ".tar.gz",

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
  GAP := ">=4.8.2",
  NeededOtherPackages := [ ["Utils", ">= 0.43"], 
                           ["Gpd", ">= 1.45"], 
                           ["Hap", ">= 1.11"],
                           ["AutPGrp", ">= 1.6"] ], 
  SuggestedOtherPackages := [ ["GAPDoc", ">= 1.5.1"] ],
  ExternalConditions := [ ]
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation( 
  "Loading XMod ", String( ~.Version ), 
  " (methods for crossed modules and cat1-groups)\n",
  "by Chris Wensley (http://pages.bangor.ac.uk/~mas023/),", 
  " with contributions by:\n", 
  "    Murat Alp (muratalp@nigde.edu.tr),\n", 
  "    Alper Odabas (aodabas@ogu.edu.tr),\n", 
  "and Enver Uslu (enveruslu@ogu.edu.tr).\n", 
  "----------",
  "-------------------------------------------------------------------\n" ), 

TestFile := "tst/testall.g",

Keywords := ["crossed module", "cat1-group", "derivation", "section", 
             "actor", "crossed square" ]

));
