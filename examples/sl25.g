#############################################################################
##
#W  sl25.g                    XMOD example files                Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod test file sl25.g (version 08/11/14) :-\n\n");

sl25p := SpecialLinearGroup(2,5); 
isosl := IsomorphismPermGroup( sl25p );
sl25 := Image( isosl );
gensl25 := GeneratorsOfGroup( sl25 ); 
Print( "sl25 has generators: ", gensl25, "\n" ); 
ccsl := ConjugacyClassesSubgroups( sl25 ); 
repssl := List( ccsl, c -> Representative(c) );  
Print( "conj. classes: ", List( repssl, r -> Size(r) ), "\n" ); 
Print( List( repssl, r -> StructureDescription(r) ), "\n" ); 

gl25p := GeneralLinearGroup(2,5); 
isogl := IsomorphismPermGroup( gl25p );
gl25 := Image( isogl );
gengl25 := GeneratorsOfGroup( gl25 ); 
Print( "gl25 has generators: ", gengl25, "\n" ); 
ccgl := ConjugacyClassesSubgroups( gl25 ); 
repsgl := List( ccgl, c -> Representative(c) );  
Print( "conj. classes: ", List( repsgl, r -> Size(r) ), "\n" ); 
Print( List( repsgl, r -> StructureDescription(r) ), "\n" ); 
