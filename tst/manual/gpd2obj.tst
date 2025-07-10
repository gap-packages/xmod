#############################################################################
##
#W  gpd2obj.tst                   XMOD test file                Chris Wensley
##
#Y  Copyright (C) 2001-2022, Chris Wensley et al, 
##
gap> START_TEST( "XMod package: gpd2obj.tst" );
gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 10

## Subsection 10.1.1 
gap> s4 := Group( (1,2,3,4), (3,4) );; 
gap> SetName( s4, "s4" );
gap> a4 := Subgroup( s4, [ (1,2,3), (2,3,4) ] );;
gap> SetName( a4, "a4" );
gap> X4 := XModByNormalSubgroup( s4, a4 );; 
gap> CX4 := SinglePiecePreXModWithObjects( X4, [-6,-5,-4], false );
single piece precrossed module with objects
  source groupoid:
    single piece groupoid: < a4, [ -6, -5, -4 ] >
  and range groupoid:
    single piece groupoid: < s4, [ -6, -5, -4 ] >
gap> SetName( CX4, "CX4" );
gap> Ca4 := Source( CX4 );;  SetName( Ca4, "Ca4" );
gap> Cs4 := Range( CX4 );;  SetName( Cs4, "Cs4" );
gap> ## DX4 := SinglePiecePreXModWithObjects( X4, [-9,-8,-7], true );
gap> ## SetName( DX4, "DX4" );
gap> ## Da4 := Source( DX4 );;  SetName( Da4, "Da4" );
gap> ## Ds4 := Range( DX4 );;  SetName( Ds4, "Ds4" );

## Subsection 10.1.2 
gap> Set( KnownPropertiesOfObject( CX4 ) ); 
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsAssociative", 
  "IsDuplicateFree", "IsGeneratorsOfSemigroup", "IsPreXModWithObjects", 
  "IsSinglePieceDomain", "IsXModWithObjects" ]

## Subsection 10.1.3 
gap> IsPermPreXModWithObjects( CX4 );
true
gap> IsPcPreXModWithObjects( CX4 );  
false
gap> IsFpPreXModWithObjects( CX4 );
false

## Subsection 10.1.4 
gap> Root2dGroup( CX4 ); 
[a4->s4]
gap> actC := XModAction( CX4 );; 
gap> Size( Range( actC ) ); 
20736
gap> r1 := Arrow( Cs4, (1,2,3,4), -4, -5 );; 
gap> ImageElm( actC, r1 );            
[groupoid homomorphism : Ca4 -> Ca4
[ [ [(1,2,3) : -6 -> -6], [(2,3,4) : -6 -> -6], [() : -6 -> -5], 
      [() : -6 -> -4] ], 
  [ [(2,3,4) : -4 -> -4], [(1,3,4) : -4 -> -4], [() : -4 -> -6], 
      [() : -4 -> -5] ] ] : 0 -> 0]
gap> s1 := Arrow( Ca4, (1,2,4), -5, -5 );;
gap> ##  calculate s1^r1 
gap> ims1 := ImageElmXModAction( CX4, s1, r1 );
[(1,2,3) : -6 -> -6]
gap> ## repeat using DX4
gap> ## actD := XModAction( DX4 );; 
gap> ## SetName( Range( actD ), "autD" );
gap> ## Size( Range( actD ) ); 
gap> ## r2 := Arrow( Ds4, (1,2,3,4), -7, -8 );; 
gap> ## ImageElm( actD, r2 );            
gap> ## s2 := Arrow( Da4, (1,2,4), -8, -8 );;
gap> ##  calculate s2^r2 
gap> ## ims2 := ImageElmXModAction( DX4, s2, r2 );

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 
gap> STOP_TEST( "gpd2obj.tst", 10000 );
