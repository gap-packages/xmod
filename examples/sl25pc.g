#############################################################################
##
#W  sl25pc.g                  XMOD example files                Chris Wensley
##
##  version 2.43, 10/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

SetInfoLevel( InfoXMod, 2 ); 
Print("\nXMod test file sl25pc.g (version 10/11/15) :-\n\n");

sl25pc := SpecialLinearGroup(2,5); 
SetName( sl25pc, "sl25-pc" ); 
gensl25pc := GeneratorsOfGroup( sl25pc ); 
Print( "sl25pc has generators: ", gensl25pc, "\n" ); 
ccsl := ConjugacyClassesSubgroups( sl25pc ); 
repssl := List( ccsl, c -> Representative(c) );  
Print( "conj. classes: ", List( repssl, r -> Size(r) ), "\n" ); 
Print( List( repssl, r -> StructureDescription(r) ), "\n" ); 

gl25pc := GeneralLinearGroup(2,5); 
SetName( gl25pc, "gl25-pc" ); 
gengl25pc := GeneratorsOfGroup( gl25pc ); 
Print( "gl25pc has generators: ", gengl25pc, "\n" ); 

isofp := IsomorphismFpGroup( gl25pc ); 
imisofp := Image( isofp );
isofpinfo := IsomorphismFpInfo( gl25pc ); 
fgensim := FreeGeneratorsOfFpGroup( imisofp );
gensim := GeneratorsOfGroup( imisofp ); 
ok := fgensim=gensim;
Print( "free gens = gens? ", ok, "\n" ); 
## ok := ok[3]; 

ccgl := ConjugacyClassesSubgroups( gl25pc ); 
repsgl := List( ccgl, c -> Representative(c) );  
Print( "conj. classes: ", List( repsgl, r -> Size(r) ), "\n" ); 
Print( List( repsgl, r -> StructureDescription(r) ), "\n" ); 
lengl := Length( repsgl );
sl25c2pc := repsgl[lengl-1]; 
SetName( sl25c2pc, "sl25.c2-pc" ); 
gensl25c2pc := GeneratorsOfGroup( sl25c2pc );
inc25pc := InclusionMappingGroups( gl25pc, sl25c2pc );
Print( "inc25pc = ", inc25pc, "\n" ); 

X25pc := XModByNormalSubgroup( sl25c2pc, sl25pc );
indX25pc := InducedXMod( X25pc, inc25pc ); 
Display( indX25pc ); 
h25pc := Source( indX25pc ); 
Print( "h25pc = ", StructureDescription( h25pc ), "\n" ); 

