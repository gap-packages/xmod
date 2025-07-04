#############################################################################
##
#W  util.tst                      XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: util.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## make independent if gp2ind.tst 
gap> b1 := (11,12,13,14,15,16,17,18);; 
gap> b2 := (12,18)(13,17)(14,16);;
gap> d16 := Group( b1, b2 );;
gap> SetName( d16, "d16" ); 
gap> d8 := Subgroup( d16, [ b1^2, b2 ] );; 
gap> SetName( d8, "d8" ); 
gap> c4 := Subgroup( d8, [ b1^2 ] );; 
gap> SetName( c4, "c4" ); 
gap> Y16 := XModByNormalSubgroup( d16, d8 );;                   
gap> Y8 := SubXMod( Y16, c4, d8 );; 
gap> inc8 := InclusionMorphism2DimensionalDomains( Y16, Y8 );; 
gap> incd8 := RangeHom( inc8 );;

## Chapter 12

## Section 12.1.1
gap> incd8;
[ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ] -> 
[ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ]
gap> imd8 := Image( incd8 );; 
gap> resd8 := GeneralRestrictedMapping( incd8, c4, imd8 );
GeneralRestrictedMapping( [ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) 
 ] -> [ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ], c4, Group([ (11,
13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ]) )
gap> Source( resd8 ); Range( resd8 );
c4
Group([ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ])
gap> MappingToOne( c4, imd8 );
[ (11,13,15,17)(12,14,16,18) ] -> [ () ]

## Section 12.1.2
##  cannot use GeneratorsOfGroup( innd8 ) here 
##  because the answer varies from one run to another 
gap> autd8 := AutomorphismGroup( d8 );;
gap> innd8 := InnerAutomorphismsByNormalSubgroup( d8, d8 );;
gap> IdGroup( innd8 ) = [4,2]; 
true
gap> IsGroupOfAutomorphisms( innd8 );
true

## Section 12.2.1
gap> x := (6,7)(8,9);;  y := (6,8)(7,9);;  z := (6,9)(7,8);;
gap> k4a := Group( x, y );;  SetName( k4a, "k4a" );
gap> gens3a := [ (1,2), (2,3) ];;
gap> s3a := Group( gens3a );;  SetName( s3a, "s3a" );
gap> alpha := GroupHomomorphismByImages( k4a, k4a, [x,y], [y,x] );;
gap> beta := GroupHomomorphismByImages( k4a, k4a, [x,y], [x,z] );;
gap> auta := Group( alpha, beta );;
gap> acta := GroupHomomorphismByImages( s3a, auta, gens3a, [alpha,beta] );;
gap> abmod := AbelianModuleObject( k4a, acta );;
gap> Xabmod := XModByAbelianModule( abmod );
[k4a->s3a]
gap> Display( Xabmod );

Crossed module [k4a->s3a] :- 
: Source group k4a has generators:
  [ (6,7)(8,9), (6,8)(7,9) ]
: Range group s3a has generators:
  [ (1,2), (2,3) ]
: Boundary homomorphism maps source generators to:
  [ (), () ]
: Action homomorphism maps range generators to automorphisms:
  (1,2) --> { source gens --> [ (6,8)(7,9), (6,7)(8,9) ] }
  (2,3) --> { source gens --> [ (6,7)(8,9), (6,9)(7,8) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "util.tst", 10000 );
