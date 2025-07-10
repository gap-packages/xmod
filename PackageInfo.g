#############################################################################
##
##  PackageInfo.g  file for the package XMod 
##  Chris Wensley et al 

SetPackageInfo( rec( 

PackageName := "XMod",
Subtitle := "Crossed Modules and Cat1-Groups",
Version := "2.95",
Date := "10/07/2025", # dd/mm/yyyy format
License := "GPL-2.0-or-later",

Persons := [
  rec(
    LastName      := "Wensley",
    FirstNames    := "Chris",
    IsAuthor      := true,
    IsMaintainer  := true,
    Email         := "cdwensley.maths@btinternet.com",
    WWWHome       := "https://github.com/cdwensley",
    Place         := "Llanfairfechan"
  ),
  rec(
    LastName      := "Alp",
    FirstNames    := "Murat",
    IsAuthor      := true,
    IsMaintainer  := false,
    Email         := "muratalp@nigde.edu.tr",
    PostalAddress := Concatenation( [ 
                       "Prof. Dr. M. Alp\n",
                       "Ömer Halisdemir University\n",
                       "Art and Science Faculty\n",
                       "Mathematics Department\n",
                       "Nigde\n",
                       "Turkey"] ),
    Place         := "Nigde",
    Institution   := "Ömer Halisdemir University"
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
    IsMaintainer  := false
  )
],

Status := "accepted",
CommunicatedBy := "Derek Holt (Warwick)",
AcceptDate := "12/1996",

SourceRepository := rec( 
    Type             := "git", 
    URL              := "https://github.com/gap-packages/xmod" ),
    IssueTrackerURL  := Concatenation( ~.SourceRepository.URL, "/issues" ),
    PackageWWWHome   := "https://gap-packages.github.io/xmod/",
    README_URL       := Concatenation( ~.PackageWWWHome, "README.md" ),
    PackageInfoURL   := Concatenation( ~.PackageWWWHome, "PackageInfo.g" ),
    ArchiveURL       := Concatenation( ~.SourceRepository.URL, 
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
  HTMLStart := "doc/chap0_mj.html",
  PDFFile   := "doc/manual.pdf",
  SixFile   := "doc/manual.six",
  LongTitle := "Crossed Modules and Cat1-Groups in GAP"
),

Dependencies := rec(
  GAP := ">=4.11.0",
  NeededOtherPackages := [ ["utils", ">= 0.81"], 
                           ["groupoids", ">= 1.77"], 
                           ["HAP", ">= 1.29"],
                           ["AutPGrp", ">= 1.10.2"], 
                           ["SmallGrp", ">= 1.4.2" ] ], 
  SuggestedOtherPackages := [ ],
  ExternalConditions := [ ]
),

AvailabilityTest := ReturnTrue,

BannerString := Concatenation( 
  "Loading XMod ", String( ~.Version ), 
  " (methods for crossed modules and cat1-groups)\n",
  "by Chris Wensley (https://github.com/cdwensley),\n", 
  " with contributions from:\n", 
  "    Murat Alp (muratalp@nigde.edu.tr),\n", 
  "    Alper Odabas (aodabas@ogu.edu.tr),\n", 
  "and Enver Uslu.\n", 
  "----------",
  "-------------------------------------------------------------------\n" ), 


TestFile := "tst/testall.g",

Keywords := ["crossed module", "cat1-group", "derivation", "section", 
             "actor", "crossed square" ], 

AutoDoc := rec(
    TitlePage := rec(
        Copyright := Concatenation(
            "© 1996-2025, Chris Wensley et al. <P/>\n", 
            "The &XMod; package is free software; you can redistribute it ", 
            "and/or modify it under the terms of the GNU General ", 
            "Public License as published by the Free Software Foundation; ", 
            "either version 2 of the License, or (at your option) ", 
            "any later version.\n"
            ),
        Abstract := Concatenation( 
            "The &XMod; package provides functions for computation with\n",
            "<List>\n", 
            "  <Item>\n", 
            "  finite crossed modules of groups and cat1-groups, \n", 
            "  and morphisms of these structures; \n", 
            "  </Item>\n", 
            "  <Item>\n", 
            "  finite pre-crossed modules, pre-cat1-groups, ", 
            "  and their Peiffer quotients;\n", 
            "  </Item>\n", 
            "  <Item>\n", 
            "  isoclinism classes of groups and crossed modules; \n", 
            "  </Item>\n", 
            "  <Item>\n", 
            "  derivations of crossed modules and sections of cat1-groups; \n", 
            "  </Item>\n", 
            "  <Item>\n", 
            "  crossed squares and their morphisms, ", 
            "  including the actor crossed square of a crossed module; \n", 
            "  </Item>\n", 
            "  <Item>\n", 
            "  crossed modules of finite groupoids (experimental version). \n", 
            "  </Item>\n", 
            "</List>\n", 
            "<P/>\n", 
            "&XMod; was originally implemented in 1996 using the &GAP;3 ", 
            "language, when the second author was studying for a Ph.D. ", 
            "<Cite Key='A1'/> in the School of Mathematics and ",
            "Computer Science at Bangor University.\n", 
            "<P/>\n", 
            "In April 2002 the first and third parts were converted ", 
            "to &GAP;4, the pre-structures were added, ", 
            "and version 2.001 was released. \n", 
            "The final two parts, covering derivations, sections and actors, ", 
            "were included in the January 2004 release 2.002 for &GAP; 4.4.\n", 
            "<P/>\n", 
            "In October 2015 functions for computing isoclinism classes of ", 
            "crossed modules, written by Alper Odaba&#x15f; and Enver Uslu, ", 
            "were added.\n",  
            "These are contained in Chapter <Ref Chap='chap-isclnc' />, ", 
            "and are described in detail in the paper <Cite Key='IOU1' />.\n", 
            "<P/>\n", 
            "Bug reports, suggestions and comments are, of course, welcome. ", 
            "Please submit an issue at ", 
            "<URL>https://github.com/gap-packages/xmod/issues/</URL> ", 
            "or send an email to the first author at ", 
            "<Email>cdwensley@btinternet.com</Email>. \n", 
            "<P/>\n"
            ), 
        Acknowledgements := Concatenation( 
            "This documentation was prepared using the ", 
            "&GAPDoc; <Cite Key='GAPDoc'/> and ", 
            "&AutoDoc; <Cite Key='AutoDoc'/> packages.<P/>\n", 
            "The procedure used to produce new releases uses the package ", 
            "<Package>GitHubPagesForGAP</Package> ", 
            "<Cite Key='GitHubPagesForGAP' /> ", 
            "and the package <Package>ReleaseTools</Package>.<P/>" 
            ),
    ) 
),

));
