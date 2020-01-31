##############################################################################
##
#W  coprod.tst                   GAP4 package `XMod'             Chris Wensley
#W             
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
gap> START_TEST( "XMod package: coprod.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 1 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );;
gap> SetName( q8, "q8" );; 
gap> X8 := XModByAutomorphismGroup( q8 );; 
gap> s4 := Range( X8 );;
gap> SetName( s4, "s4" );;
gap> nss4 := NormalSubgroups( s4 );;
gap> posa4 := Position( List( nss4, n -> IdGroup(n) ), [12,3] );;
gap> a4 := nss4[posa4];; 
gap> SetName( a4, "a4" );;
gap> Y8 := XModByNormalSubgroup( s4, a4 );; 
gap> posk4 := Position( List( nss4, n -> IdGroup(n) ), [4,2] );;
gap> k4 := nss4[posk4];; 
gap> SetName( k4, "k4" );;
gap> Z8 := XModByNormalSubgroup( s4, k4 );; 

gap> sl := SL(2,3);; 
gap> U8 := XModByAutomorphismGroup( sl );;
gap> iso8 := IsomorphismPermObject( U8 );;
gap> V8 := Image( iso8 );;
gap> RV8 := Range( V8 );;
gap> SV8 := Source( V8 );;
gap> isoR := IsomorphismGroups( RV8, s4 );; 
gap> ok := IsBijective( isoR );;
gap> isoS := IdentityMapping( SV8 );;
gap> ok := IsBijective( isoS );;
gap> mor := IsomorphismByIsomorphisms( V8, [ isoS, isoR ] );;
gap> W8 := Image( mor );; 

gap> ## coproducts  W8 o W8,  X8 o X8,  Y8 o Y8  &  Z8 o Z8 
gap> copWW := CoproductXMod( W8, W8 );;
#I  prexmod is [ [ 576, 5128 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "C3 x SL(2,3)", "S4" ], [ [ 72, 25 ], [ 24, 12 ] ]
gap> copXX := CoproductXMod( X8, X8 );;
#I  prexmod is [ [ 64, 239 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2, [ 2, 1 ]
#I  the coproduct is [ "C2 x C2 x Q8", "S4" ], [ [ 32, 47 ], [ 24, 12 ] ]
gap> copYY := CoproductXMod( Y8, Y8 );;
#I  prexmod is [ [ 144, 184 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2 x C2, [ 4, 2 ]
#I  the coproduct is [ "C3 x A4", "S4" ], [ [ 36, 11 ], [ 24, 12 ] ]
gap> copZZ := CoproductXMod( Z8, Z8 );;
#I  prexmod is [ [ 16, 14 ], [ 24, 12 ] ]
#I  peiffer subgroup is 1, [ 1, 1 ]
#I  this object is already a crossed module!
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ], [ [ 16, 14 ], [ 24, 12 ] ]
gap> ## coproducts  W8 o X8  and  X8 o W8 
gap> copWX := CoproductXMod( W8, X8 );; 
#I  prexmod is [ [ 192, 1022 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "SL(2,3)", "S4" ], [ [ 24, 3 ], [ 24, 12 ] ]
gap> copXW := CoproductXMod( X8, W8 );;
#I  prexmod is [ [ 192, 1007 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "SL(2,3)", "S4" ], [ [ 24, 3 ], [ 24, 12 ] ]
gap> ## coproducts  W8 o Y8  and  Y8 o W8 
gap> copWY := CoproductXMod( W8, Y8 );; 
#I  prexmod is [ [ 288, 859 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "C3 x A4", "S4" ], [ [ 36, 11 ], [ 24, 12 ] ]
gap> copYW := CoproductXMod( Y8, W8 );;
#I  prexmod is [ [ 288, 860 ], [ 24,\
 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "C3 x A4", "S4" ], [ [ 36, 11 ], [ 24, 12 ] ]
gap> ## coproducts  W8 o Z8  and  Z8 o W8 
gap> copWZ := CoproductXMod( W8, Z8 );; 
#I  prexmod is [ [ 96, 203 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "A4", "S4" ], [ [ 12, 3 ], [ 24, 12 ] ]
gap> copZW := CoproductXMod( Z8, W8 );;
#I  prexmod is [ [ 96, 201 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "A4", "S4" ], [ [ 12, 3 ], [ 24, 12 ] ]
gap> ## coproducts  X8 o Y8  and  Y8 o X8 
gap> copXY := CoproductXMod( X8, Y8 );; 
#I  prexmod is [ [ 96, 199 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "A4", "S4" ], [ [ 12, 3 ], [ 24, 12 ] ]
gap> copYX := CoproductXMod( Y8, X8 );;
#I  prexmod is [ [ 96, 204 ], [ 24, 12 ] ]
#I  peiffer subgroup is Q8, [ 8, 4 ]
#I  the coproduct is [ "A4", "S4" ], [ [ 12, 3 ], [ 24, 12 ] ]
gap> ## coproducts  X8 o Z8  and  Z8 o X8 
gap> copXZ := CoproductXMod( X8, Z8 );; 
#I  prexmod is [ [ 32, 47 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2, [ 2, 1 ]
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ], [ [ 16, 14 ], [ 24, 12 ] ]
gap> copZX := CoproductXMod( Z8, X8 );;
#I  prexmod is [ [ 32, 49 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2, [ 2, 1 ]
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ], [ [ 16, 14 ], [ 24, 12 ] ]
gap> ## coproducts  Y8 o Z8  and  Z8 o Y8 
gap> copYZ := CoproductXMod( Y8, Z8 );; 
#I  prexmod is [ [ 48, 50 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2 x C2, [ 4, 2 ]
#I  the coproduct is [ "A4", "S4" ], [ [ 12, 3 ], [ 24, 12 ] ]
gap> copZY := CoproductXMod( Z8, Y8 );;
#I  prexmod is [ [ 48, 49 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2 x C2, [ 4, 2 ]
#I  the coproduct is [ "A4", "S4" ], [ [ 12, 3 ], [ 24, 12 ] ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "coprod.tst", 10000 );

#############################################################################
##
#E  coprod.tst  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
