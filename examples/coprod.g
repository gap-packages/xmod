##############################################################################
##
#W  coprod.g                     GAP4 package `XMod'             Chris Wensley
#W             
#Y  Copyright (C) 2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##############################################################################

Print("\nXMod test file coprod.g (version 05/04/17) :-");
Print("\ntesting constructions of crossed modules\n\n");
level := InfoLevel( InfoXMod ); 
SetInfoLevel( InfoXMod, 1 ); 

q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );
SetName( q8, "q8" ); 
X8 := XModByAutomorphismGroup( q8 ); 
s4 := Range( X8 );
SetName( s4, "s4" );
a4 := NormalSubgroups( s4 )[2]; 
SetName( a4, "a4" );
Y8 := XModByNormalSubgroup( s4, a4 ); 
k4 := NormalSubgroups( s4 )[3]; 
SetName( k4, "k4" );
Z8 := XModByNormalSubgroup( s4, k4 ); 

sl := SL(2,3); 
U8 := XModByAutomorphismGroup( sl );
iso8 := IsomorphismPermObject( U8 );
V8 := Image( iso8 );
RX8 := Range( X8 );
RV8 := Range( V8 );
SV8 := Source( V8 );
isoR := IsomorphismGroups( RV8,RX8); 
ok := IsBijective( isoR );
isoS := IdentityMapping( SV8 );
ok := IsBijective( isoS );
mor := IsomorphismByIsomorphisms( V8, isoS, isoR );
W8 := Image( mor ); 

level := InfoLevel( InfoXMod ); 
SetInfoLevel( InfoXMod, 1 ); 

Print( "\ncoproducts  W8 o W8,  X8 o X8,  Y8 o Y8  &  Z8 o Z8\n" );
copWW := CoproductXMod( W8, W8 );
copXX := CoproductXMod( X8, X8 );
copYY := CoproductXMod( Y8, Y8 );
copZZ := CoproductXMod( Z8, Z8 );

Print( "\ncoproducts  W8 o X8  and  X8 o W8\n" );
copWX := CoproductXMod( W8, X8 );
copXW := CoproductXMod( X8, W8 );

Print( "\ncoproducts  W8 o Y8  and  Y8 o W8\n" );
copWY := CoproductXMod( W8, Y8 );
copYW := CoproductXMod( Y8, W8 );

Print( "\ncoproducts  W8 o Z8  and  Z8 o W8\n" );
copWY := CoproductXMod( W8, Z8 );
copYW := CoproductXMod( Z8, W8 );

Print( "\ncoproducts  X8 o Y8  and  Y8 o X8\n" );
copYX := CoproductXMod( X8, Y8 );
copXY := CoproductXMod( Y8, X8 );

Print( "\ncoproducts  X8 o Z8  and  Z8 o X8\n" );
copYX := CoproductXMod( X8, Z8 );
copXY := CoproductXMod( Z8, X8 );

Print( "\ncoproducts  Y8 o Z8  and  Z8 o Y8\n" );
copYX := CoproductXMod( Y8, Z8 );
copXY := CoproductXMod( Z8, Y8 );

SetInfoLevel( InfoXMod, level ); 
#############################################################################
##
#E  coprod.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
