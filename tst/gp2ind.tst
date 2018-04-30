#############################################################################
##
#W  gp2ind.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2018, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gp2ind.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

##  make independent of gp2obj.tst  
gap> s4 := Group( (1,2), (2,3), (3,4) );; 
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );; 
gap> SetName( s4, "s4" );  SetName( a4, "a4" ); 
gap> b1 := (11,12,13,14,15,16,17,18);; 
gap> b2 := (12,18)(13,17)(14,16);;
gap> d16 := Group( b1, b2 );;
gap> SetName( d16, "d16" ); 

## Chapter 7

## Section 7.1.1
gap> q8 := Group( (1,2,3,4)(5,8,7,6), (1,5,3,7)(2,6,4,8) );;
gap> X8 := XModByAutomorphismGroup( q8 );;
gap> s4b := Range( X8 );; 
gap> SetName( q8, "q8" );  SetName( s4b, "s4b" ); 
gap> k4b := NormalSubgroups( s4b )[3];;  SetName( k4b, "k4b" );
gap> Z8 := XModByNormalSubgroup( s4b, k4b );;
gap> SetName( X8, "X8" );  SetName( Z8, "Z8" );  
gap> SetInfoLevel( InfoXMod, 1 ); 
gap> XZ8 := CoproductXMod( X8, Z8 );
#I  prexmod is [ [ 32, 47 ], [ 24, 12 ] ]
#I  peiffer subgroup is C2, [ 2, 1 ]
#I  the coproduct is [ "C2 x C2 x C2 x C2", "S4" ], [ [ 16, 14 ], [ 24, 12 ] ]
[Group( [ f1, f2, f3, f4 ] )->s4b]
gap> SetName( XZ8, "XZ8" ); 
gap> info := CoproductInfo( XZ8 );
rec( embeddings := [ [X8 => XZ8], [Z8 => XZ8] ], xmods := [ X8, Z8 ] )
gap> SetInfoLevel( InfoXMod, 0 ); 

## Section 7.2.1
gap> s4gens := GeneratorsOfGroup( s4 );
[ (1,2), (2,3), (3,4) ]
gap> a4gens := GeneratorsOfGroup( a4 );
[ (1,2,3), (2,3,4) ]
gap> s3b := Group( (5,6),(6,7) );;  SetName( s3b, "s3b" );
gap> epi := GroupHomomorphismByImages( s4, s3b, s4gens, [(5,6),(6,7),(5,6)] );;
gap> X4 := XModByNormalSubgroup( s4, a4 );;
gap> indX4 := SurjectiveInducedXMod( X4, epi );
[a4/ker->s3b]
gap> Display( indX4 );

Crossed module [a4/ker->s3b] :- 
: Source group a4/ker has generators:
  [ (1,3,2), (1,2,3) ]
: Range group s3b has generators:
  [ (5,6), (6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,6,7), (5,7,6) ]
: Action homomorphism maps range generators to automorphisms:
  (5,6) --> { source gens --> [ (1,2,3), (1,3,2) ] }
  (6,7) --> { source gens --> [ (1,2,3), (1,3,2) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> morX4 := MorphismOfInducedXMod( indX4 );
[[a4->s4] => [a4/ker->s3b]]


gap> d8 := Subgroup( d16, [ b1^2, b2 ] );  SetName( d8, "d8" ); 
Group([ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ])
gap> c4 := Subgroup( d8, [ b1^2 ] );  SetName( c4, "c4" ); 
Group([ (11,13,15,17)(12,14,16,18) ])
gap> Y16 := XModByNormalSubgroup( d16, d8 );                   
[d8->d16]
gap> Y8 := SubXMod( Y16, c4, d8 );            
[c4->d8]
gap> inc8 := InclusionMorphism2DimensionalDomains( Y16, Y8 ); 
[[c4->d8] => [d8->d16]]
gap> incd8 := RangeHom( inc8 );;
gap> indY8 := InducedXMod( Y8, incd8 );
#I induced group has Size: 16
#I factor 2 is abelian  with invariants: [ 4, 4 ]
i*([c4->d8])
gap> morY8 := MorphismOfInducedXMod( indY8 );
[[c4->d8] => i*([c4->d8])]
gap> s3c := Subgroup( s4, [ (2,3), (3,4) ] );;  
gap> SetName( s3c, "s3c" );
gap> indXs3c := InducedXMod( s4, s3c, s3c );
#I induced group has Size: 48
i*([s3c->s3c])
gap> StructureDescription( indXs3c );
[ "GL(2,3)", "S4" ]

## Section 7.2.2

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp2ind.tst", 10000 );

#############################################################################
##
#E  gp2ind.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
