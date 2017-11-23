##############################################################################
##
#W  cat1data.gd                GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##  These functions are used in the construction of the data file cat1data.g 
##  and are not intended for the general user. 

#############################################################################
##
#F  AllCat1DataGroups( <arg> ) 
#O  AllCat1DataGroupsBasic( <gp> ) 
#O  CollectPartsAlreadyDone( <gp>, <deg>, <num>, <ranger> ) 
#O  AllCat1DataGroupsInParts( <gp>, <reps>, <ids>, <range>, <cat1s> ) 
#O  Cat1RepresentativesToFile( <gp> )
#O  Cat1IdempotentsToFile( <gp>, <ireps>, <fst>, <lst> )
#O  MakeAllCat1DataGroups( <gp> )
##
DeclareGlobalFunction( "AllCat1DataGroups" );
DeclareOperation( "AllCat1DataGroupsBasic", [ IsGroup ] ); 
DeclareOperation( "CollectPartsAlreadyDone", 
    [ IsGroup, IsPosInt, IsPosInt, IsList ] ); 
DeclareOperation( "AllCat1DataGroupsInParts", 
    [ IsGroup, IsList, IsList, IsList, IsList ] ); 
DeclareOperation( "Cat1RepresentativesToFile", [ IsGroup ] );
DeclareOperation( "Cat1IdempotentsToFile", 
    [ IsGroup, IsList, IsPosInt, IsPosInt ] );
DeclareOperation( "MakeAllCat1DataGroups", [ IsPosInt, IsPosInt, IsPosInt ] );

#############################################################################
##
#R  IsEndomorphismClassObj( <obj> )
#P  IsEndomorphismClass( <cl> )
#O  EndomorphismClassObj( <nat>, <iso>, <aut>, <conj> )
#A  EndoClassAutoGroup( <class> )
#A  EndoClassNaturalHom( <class> )
#A  EndoClassIsomorphism( <class> )
#A  EndoClassConjugators( <class> )
##
##  An endomorphism class of a group G is a set of endomorphisms G -> G
##  with image in the same conjugacy class of subgroups of G
##
DeclareRepresentation( "IsEndomorphismClassObj",
    IsObject and IsAttributeStoringRep, [ "EndoClassNaturalHom", 
    "EndoClassIsomorphism", "EndoClassAutoGroup",  "EndoClassConjugators" ] );
DeclareProperty( "IsEndomorphismClass", IsObject );
DeclareOperation( "EndomorphismClassObj",
 [IsGroupHomomorphism, IsGroupHomomorphism, IsGroupOfAutomorphisms, IsList] );
DeclareAttribute( "EndoClassAutoGroup", IsEndomorphismClassObj );
DeclareAttribute( "EndoClassNaturalHom", IsEndomorphismClassObj );
DeclareAttribute( "EndoClassIsomorphism", IsEndomorphismClassObj );
DeclareAttribute( "EndoClassConjugators", IsEndomorphismClassObj );

#############################################################################
##
#F  EndomorphismClasses( <G> )
#A  NontrivialEndomorphismClasses( <G> )
#A  NonIntersectingEndomorphismClasses( <G> )
#A  ZeroEndomorphismClass( <G> )
#O  EndomorphismImages( <list> )
#O  IdempotentImages( <list> )
##
DeclareGlobalFunction( "EndomorphismClasses", [ IsGroup, IsInt ] );
DeclareAttribute( "NontrivialEndomorphismClasses", IsGroup );
DeclareAttribute( "NonIntersectingEndomorphismClasses", IsGroup );
DeclareAttribute( "ZeroEndomorphismClass", IsGroup );
DeclareOperation( "EndomorphismImages", [ IsList ] );
DeclareOperation( "IdempotentImages", [ IsList ] );

#############################################################################
##
#E  cat1data.gd . . . . . . . . . . . . . . . . . . . . . . . . . . ends here

