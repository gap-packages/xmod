##############################################################################
##
#W  others.tst                  GAP4 package `XMod'              Chris Wensley
##  
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 

gap> START_TEST( "XMod package: others.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 

gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );;
gap> X8 := XModByAutomorphismGroup( q8 );;
gap> s4b := Range( X8 );; 
gap> SetName( q8, "q8" );  SetName( s4b, "s4b" ); 
gap> a := q8.1;;  b := q8.2;; 
gap> alpha := GroupHomomorphismByImages( q8, q8, [a,b], [a^-1,b] );;
gap> beta := GroupHomomorphismByImages( q8, q8, [a,b], [a,b^-1] );;
gap> k4b := Subgroup( s4b, [ alpha, beta ] );;  SetName( k4b, "k4b" );
gap> Z8 := XModByNormalSubgroup( s4b, k4b );;
gap> SetName( X8, "X8" );  SetName( Z8, "Z8" );  
gap> XZ8 := CoproductXMod( X8, Z8 );; 

gap> A := AutomorphismPermGroup( X8 ); 
<permutation group with 3 generators>
gap> StructureDescription( A ); 
"PSL(3,2)"
gap> Size( A );
168
gap> A := AutomorphismPermGroup( Z8 );
Group([ (1,3)(4,8)(5,6)(7,9), (1,3,2)(4,6,7)(5,8,9), (4,5)(7,9) ])
gap> StructureDescription(A);     
"S4"


gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "others.tst", 10000 );

##############################################################################
##
#E  others.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
