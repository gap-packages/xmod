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
gap> a := q8.1;;  b := q8.2;; 
gap> alpha := GroupHomomorphismByImages( q8, q8, [a,b], [a^-1,b] );;
gap> beta := GroupHomomorphismByImages( q8, q8, [a,b], [a,b^-1] );;
gap> k4b := Subgroup( s4b, [ alpha, beta ] );;  SetName( k4b, "k4b" );
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
gap> IX4a := SurjectiveInducedXMod( X4, epi );
[a4/ker->s3b]
gap> Display( IX4a );

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

## Section 7.2.2
gap> s5 := Group( (1,2,3,4,5), (4,5) );;
gap> SetName( s5, "s5" ); 
gap> inc45 := InclusionMappingGroups( s5, s4 );;
gap> IX4b := InjectiveInducedXMod( X4, inc45, [ ] );;
gap> StructureDescription( IX4b );
[ "GL(2,4)", "S5" ]
gap> Display( IX4b );

Crossed module i*([a4->s4]) :- 
: Source group has generators:
  [ ( 1, 2, 3)( 5, 8, 9)( 6,10,11)( 7,12,13), 
  ( 2, 4, 5)( 3, 6, 7)( 8,13,14)( 9,11,15) ]
: Range group s5 has generators:
  [ (1,2,3,4,5), (4,5) ]
: Boundary homomorphism maps source generators to:
  [ (2,4,3), (1,4,5) ]
: Action homomorphism maps range generators to automorphisms:
  (1,2,3,4,5) --> { source gens --> 
[ ( 1,12,10)( 2, 7,14)( 3, 4,11)( 6,15,13), 
  ( 1, 4, 9)( 2,11,13)( 5,12,14)( 8,10,15) ] }
  (4,5) --> { source gens --> [ ( 1,15, 3)( 4, 7,12)( 5, 9, 8)( 6,10,14), 
  ( 2, 5, 4)( 3, 7, 6)( 8,14,13)( 9,15,11) ] }
  These 2 automorphisms generate the group of automorphisms.

## Section 7.2.3
gap> iota45 := GroupHomomorphismByImages( s4, s5, s4gens,                   
>                  [ (1,2), (2,3), (1,2) ] );; 
gap> IX4c := InducedXMod( X4, iota45 );
i*(i*([a4->s4]))
gap> StructureDescription( IX4c );
[ "C3 x SL(2,5)", "S5" ]

gap> s3c := Subgroup( s4, [ (2,3), (3,4) ] );;  
gap> SetName( s3c, "s3c" );
gap> indXs3c := InducedXMod( s4, s3c, s3c );
i*([s3c->s3c])
gap> StructureDescription( indXs3c );
[ "GL(2,3)", "S4" ]

## Section 7.2.4
gap> IsInducedXMod( IX4b ); 
true

## Section 7.2.5
gap> morIX4c := MorphismOfInducedXMod( IX4c );
[[a4->s4] => i*(i*([a4->s4]))]

## Section 7.2.6
gap> all := AllInducedXMods( q8 );;
gap> ids := List( all, x -> IdGroup(x) );;
gap> Sort( ids );
gap> ids;
[ [ [ 1, 1 ], [ 8, 4 ] ], [ [ 1, 1 ], [ 8, 4 ] ], [ [ 1, 1 ], [ 8, 4 ] ], 
  [ [ 1, 1 ], [ 8, 4 ] ], [ [ 4, 2 ], [ 8, 4 ] ], [ [ 4, 2 ], [ 8, 4 ] ], 
  [ [ 4, 2 ], [ 8, 4 ] ], [ [ 16, 2 ], [ 8, 4 ] ], [ [ 16, 2 ], [ 8, 4 ] ], 
  [ [ 16, 2 ], [ 8, 4 ] ], [ [ 16, 14 ], [ 8, 4 ] ] ]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gp2ind.tst", 10000 );

#############################################################################
##
#E  gp2ind.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
