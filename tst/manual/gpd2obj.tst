#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
gap> START_TEST( "XMod package: gpd2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;
gap> saved_infolevel_groupoids := InfoLevel( InfoGroupoids );; 
gap> SetInfoLevel( InfoGroupoids, 0 );;

## Chapter 10

## Subsection 10.1.1 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> SetName( s4, "s4" );
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );;
gap> SetName( a4, "a4" );
gap> X4 := XModByNormalSubgroup( s4, a4 );; 
gap> DX4 := SinglePiecePreXModWithObjects( X4, [-9,-8,-7], true );
single piece crossed module with objects
  source groupoid:
    homogeneous, discrete groupoid: < a4, [ -9, -8, -7 ] >
  and range groupoid:
    single piece groupoid: < s4, [ -9, -8, -7 ] >
gap> Da4 := Source( DX4 );; 
gap> Ds4 := Range( DX4 );;
gap> CX4 := SinglePiecePreXModWithObjects( X4, [-9,-8,-7], false );
single piece crossed module with objects
  source groupoid:
    single piece groupoid: < a4, [ -9, -8, -7 ] >
  and range groupoid:
    single piece groupoid: < s4, [ -9, -8, -7 ] >
gap> Ca4 := Source( CX4 );; 
gap> Cs4 := Range( CX4 );;

## Subsection 10.1.2 
gap> IsXModWithObjects( DX4 ); 
true
gap> KnownPropertiesOfObject( DX4 ); 
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsDuplicateFree", 
  "IsGeneratorsOfSemigroup", "IsSinglePieceDomain", 
  "IsDirectProductWithCompleteDigraphDomain", "IsPreXModWithObjects", 
  "IsXModWithObjects" ]

## Subsection 10.1.3 
gap> IsPermPreXModWithObjects( CX4 );
true
gap> IsPcPreXModWithObjects( CX4 );  
false
gap> IsFpPreXModWithObjects( CX4 );
false

## Subsection 10.1.4 
gap> Set( KnownAttributesOfObject( CX4 ) );
[ "Boundary", "ObjectList", "Range", "Root2dGroup", "Source", "XModAction" ]
gap> Root2dGroup( CX4 ); 
[a4->s4]
gap> act := XModAction( CX4 );; 
gap> r := Arrow( Cs4, (1,2,3,4), -7, -8 );; 
gap> ImageElm( act, r );            
[groupoid homomorphism : 
[ [ [(1,2,3) : -9 -> -9], [(2,3,4) : -9 -> -9], [() : -9 -> -8], 
      [() : -9 -> -7] ], 
  [ [(2,3,4) : -9 -> -9], [(1,3,4) : -9 -> -9], [() : -9 -> -7], 
      [() : -9 -> -8] ] ] : 0 -> 0]
gap> s := Arrow( Ca4, (1,2,4), -8, -8 );;
gap> ##  calculate s^r 
gap> ims := ImageElmXModAction( CX4, s, r );
[(1,2,3) : -7 -> -7]

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> SetInfoLevel( InfoGroupoids, saved_infolevel_groupoids );; 
gap> STOP_TEST( "gpd2obj.tst", 10000 );
