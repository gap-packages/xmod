##############################################################################
##
#W  coprod.tst                   GAP4 package `XMod'             Chris Wensley
#W             
#Y  Copyright (C) 2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 1 );;
gap> saved_infolevel_gpd := InfoLevel( InfoGpd );; 
gap> SetInfoLevel( InfoGpd, 0 );;

gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) ) ;;
gap> SetName( q8, "q8" ) ;; 
gap> X8 := XModByAutomorphismGroup( q8 ) ;; 
gap> s4 := Range( X8 ) ;;
gap> SetName( s4, "s4" ) ;;
gap> a4 := NormalSubgroups( s4 )[2] ;; 
gap> SetName( a4, "a4" ) ;;
gap> Y8 := XModByNormalSubgroup( s4, a4 ) ;; 
gap> k4 := NormalSubgroups( s4 )[3];; 
gap> SetName( k4, "k4" );;
gap> Z8 := XModByNormalSubgroup( s4, k4 );; 

gap> sl := SL(2,3);; 
gap> U8 := XModByAutomorphismGroup( sl );;
gap> iso8 := IsomorphismPermObject( U8 );;
gap> V8 := Image( iso8 );;
gap> RX8 := Range( X8 );;
gap> RV8 := Range( V8 );;
gap> SV8 := Source( V8 );;
gap> isoR := IsomorphismGroups( RV8,RX8);; 
gap> ok := IsBijective( isoR );;
gap> isoS := IdentityMapping( SV8 );;
gap> ok := IsBijective( isoS );;
gap> mor := PreXModIsomorphismByIsomorphisms( V8, isoS, isoR );;
gap> W8 := Image( mor );; 

gap> ## coproducts  W8 o W8,  X8 o X8,  Y8 o Y8  &  Z8 o Z8 
gap> copWW := CoproductXMod( W8, W8 );;
#I  prexmod is [ "SL(2,3) x SL(2,3)", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "C3 x SL(2,3)", "S4" ]
gap> copXX := CoproductXMod( X8, X8 );;
#I  prexmod is [ "Q8 x Q8", "S4" ]
#I  peiffer subgroup is C2
#I  the coproduct is [ "C2 x C2 x Q8", "S4" ]
gap> copYY := CoproductXMod( Y8, Y8 );;
#I  prexmod is [ "A4 x A4", "S4" ]
#I  peiffer subgroup is C2 x C2
#I  the coproduct is [ "C3 x A4", "S4" ]
gap> copZZ := CoproductXMod( Z8, Z8 );;
#I  prexmod is [ "C2 x C2 x C2 x C2", "S4" ]
#I  peiffer subgroup is 1
#I  this object is already a crossed module!
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ]
gap> ## coproducts  W8 o X8  and  X8 o W8 
gap> copWX := CoproductXMod( W8, X8 );;  copXW := CoproductXMod( X8, W8 );;
#I  prexmod is [ "(Q8 x Q8) : C3", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "SL(2,3)", "S4" ]
#I  prexmod is [ "SL(2,3) x Q8", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "SL(2,3)", "S4" ]
gap> ## coproducts  W8 o Y8  and  Y8 o W8 
gap> copWY := CoproductXMod( W8, Y8 );;  copYW := CoproductXMod( Y8, W8 );;
#I  prexmod is [ "A4 x SL(2,3)", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "C3 x A4", "S4" ]
#I  prexmod is [ "((SL(2,3) : C2) : C2) : C3", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "C3 x A4", "S4" ]
gap> ## coproducts  W8 o Z8  and  Z8 o W8 
gap> copWY := CoproductXMod( W8, Z8 );;  copYW := CoproductXMod( Z8, W8 );;
#I  prexmod is [ "(C2 x C2 x Q8) : C3", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "A4", "S4" ]
#I  prexmod is [ "(SL(2,3) : C2) : C2", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "A4", "S4" ]
gap> ## coproducts  X8 o Y8  and  Y8 o X8 
gap> copYX := CoproductXMod( X8, Y8 );;  copXY := CoproductXMod( Y8, X8 );;
#I  prexmod is [ "A4 x Q8", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "A4", "S4" ]
#I  prexmod is [ "((C2 x D8) : C2) : C3", "S4" ]
#I  peiffer subgroup is Q8
#I  the coproduct is [ "A4", "S4" ]
gap> ## coproducts  X8 o Z8  and  Z8 o X8 
gap> copYX := CoproductXMod( X8, Z8 );;  copXY := CoproductXMod( Z8, X8 );;
#I  prexmod is [ "C2 x C2 x Q8", "S4" ]
#I  peiffer subgroup is C2
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ]
#I  prexmod is [ "(C2 x D8) : C2", "S4" ]
#I  peiffer subgroup is C2
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ]
gap> ## coproducts  Y8 o Z8  and  Z8 o Y8 
gap> copYX := CoproductXMod( Y8, Z8 );;  copXY := CoproductXMod( Z8, Y8 );;
#I  prexmod is [ "(C2 x C2 x C2 x C2) : C3", "S4" ]
#I  peiffer subgroup is C2 x C2
#I  the coproduct is [ "A4", "S4" ]
#I  prexmod is [ "C2 x C2 x A4", "S4" ]
#I  peiffer subgroup is C2 x C2
#I  the coproduct is [ "A4", "S4" ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGpd, saved_infolevel_gpd );; 

#############################################################################
##
#E  coprod.tst  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
