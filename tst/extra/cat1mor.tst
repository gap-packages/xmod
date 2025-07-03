#############################################################################
##
#W  cat1mor.tst                   XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: allxmods.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );

gap> g18gens := [ (1,2,3), (4,5,6), (2,3)(5,6) ];;     
gap> s3agens := [ (7,8,9), (8,9) ];;                
gap> g18 := Group( g18gens );;  SetName( g18, "g18" );;
gap> g18 := Group( g18gens );;  SetName( g18, "g18" );; 
gap> s3a := Group( s3agens );;  SetName( s3a, "s3a" );;
gap> t := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(),(8,9)]);;     
gap> h := GroupHomomorphismByImages(g18,s3a,g18gens,[(7,8,9),(7,8,9),(8,9)]);;
gap> e := GroupHomomorphismByImages(s3a,g18,s3agens,[(1,2,3),(2,3)(5,6)]);;   
gap> C18 := Cat1Group( t, h, e );;
gap> t2 := GroupHomomorphismByImages(g18,s3a,g18gens,[(),(7,8,9),(8,9)]);;     
gap> e2 := GroupHomomorphismByImages(s3a,g18,s3agens,[(4,5,6),(2,3)(5,6)]);;   
gap> B18 := Cat1Group( t2, h, e2 );; 
gap> imgamma := [ (4,5,6), (1,2,3), (2,3)(5,6) ];; 
gap> gamma := GroupHomomorphismByImages( g18, g18, g18gens, imgamma );;
gap> rho := IdentityMapping( s3a );; 
gap> mor := Cat1GroupMorphism( C18, B18, gamma, rho );;
gap> Display( mor );;
Morphism of cat1-groups :- 
: Source = [g18=>s3a] with generating sets:
  [ (1,2,3), (4,5,6), (2,3)(5,6) ]
  [ (7,8,9), (8,9) ]
:  Range = [g18=>s3a] with generating sets:
  [ (1,2,3), (4,5,6), (2,3)(5,6) ]
  [ (7,8,9), (8,9) ]
: Source Homomorphism maps source generators to:
  [ (4,5,6), (1,2,3), (2,3)(5,6) ]
: Range Homomorphism maps range generators to:
  [ (7,8,9), (8,9) ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "cat1mor.tst", 10000 );

#############################################################################
##
#E  cat1mor.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
