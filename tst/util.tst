#############################################################################
##
#W  util.tst                      XMOD test file                Chris Wensley
#W                                                                & Murat Alp
#Y  Copyright (C) 2001-2016, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 10

## Section 10.1.1
gap> L := [ [1,2,3,4], true, [ (1,2), (2,3) ] ];; 
gap> PrintListOneItemPerLine( L ); 
[ [ 1, 2, 3, 4 ],
  true,
  [ (1,2), (2,3) ]
  ]

## Section 10.2.1
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

## Section 10.2.2
##  cannot use GeneratorsOfGroup( innd8 ) here 
##  because the answer varies from one run to another 
gap> autd8 := AutomorphismGroup( d8 );;
gap> innd8 := InnerAutomorphismsByNormalSubgroup( d8, d8 );;
gap> IdGroup( innd8 ) = [4,2]; 
true
gap> IsGroupOfAutomorphisms( innd8 );
true

## Section 10.3.1
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


## Section 10.4.1
gap> J := [ [1,2,3], [3,4], [3,4], [1,2,4] ];;
gap> DistinctRepresentatives( J );
[ 1, 3, 4, 2 ]
gap> K := [ [3,4], [1,2], [2,3], [2,3,4] ];;
gap> CommonRepresentatives( J, K );
[ [ 3, 3, 3, 1 ], [ 1, 3, 4, 2 ] ]
gap> CommonTransversal( d16, c4 );
[ (), (12,18)(13,17)(14,16), (11,12,13,14,15,16,17,18), 
  (11,12)(13,18)(14,17)(15,16) ]
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  util.tst . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
