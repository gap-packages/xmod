#############################################################################
##
#W  gp3objmap.tst                 XMOD test file                Chris Wensley
##
##  version 2.31, 08/11/2014 
##
#Y  Copyright (C) 2001-2014, Murat Alp and Chris Wensley, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 7

## Section 7.1.1
gap> c := (11,12,13,14,15,16);;
gap> d := (12,16)(13,15);;
gap> cd := c*d;;
gap> d12 := Group( [ c, d ] );;
gap> s3a := Subgroup( d12, [ c^2, d ] );;
gap> s3b := Subgroup( d12, [ c^2, cd ] );;
gap> c3 := Subgroup( d12, [ c^2 ] );;
gap> SetName( d12, "d12");  SetName( s3a, "s3a" );
gap> SetName( s3b, "s3b" );  SetName( c3, "c3" );
gap> XSconj := XSqByNormalSubgroups( d12, s3b, s3a, c3 );
[  c3 -> s3b ]
[  |      |  ]
[ s3a -> d12 ]

gap> Name( XSconj );
"[c3->s3b,s3a->d12]"
gap> XStrans := Transpose3dGroup( XSconj );
[  c3 -> s3a ]
[  |      |  ]
[ s3b -> d12 ]

gap> X12 := XModByNormalSubgroup( d12, s3a );
[s3a->d12]
gap> XSact := ActorXSq( X12 );
crossed square with:
      up = Whitehead[s3a->d12]
    left = [s3a->d12]
    down = Norrie[s3a->d12]
   right = Actor[s3a->d12]


## Section 7.1.3
gap> Up2dGroup( XSconj );
[c3->s3b]
gap> Right2dGroup( XSact );
Actor[s3a->d12]
gap> xpconj := XPair( XSconj );;
gap> ImageElmXPair( xpconj, [ (12,16)(13,15), (11,16)(12,15)(13,14) ] );
(11,15,13)(12,16,14)
gap> diag := DiagonalAction( XSact );; 
gap> List( [ (2,3)(5,6), (1,2)(4,6) ], x -> ImageElm( diag, x ) ); 
[ [ (11,15,13)(12,16,14), (12,16)(13,15) ] -> 
    [ (11,13,15)(12,14,16), (12,16)(13,15) ], 
  [ (11,15,13)(12,16,14), (11,13)(14,16) ] -> 
    [ (11,13,15)(12,14,16), (12,16)(13,15) ] ]

## Section 7.2.2
gap> ad12 := GroupHomomorphismByImages( d12, d12, [c,d], [c,d^c] );;
gap> as3a := GroupHomomorphismByImages( s3a, s3a, [c^2,d], [c^2,d^c] );;
gap> as3b := GroupHomomorphismByImages( s3b, s3b, [c^2,cd], [c^2,cd^c] );;
gap> idc3 := IdentityMapping( c3 );;
gap> upconj := Up2dGroup( XSconj );;
gap> leftconj := Left2dGroup( XSconj );; 
gap> downconj := Down2dGroup( XSconj );; 
gap> rightconj := Right2dGroup( XSconj );; 
gap> up := XModMorphismByHoms( upconj, upconj, idc3, as3b );
[[c3->s3b] => [c3->s3b]]
gap> left := XModMorphismByHoms( leftconj, leftconj, idc3, as3a );
[[c3->s3a] => [c3->s3a]]
gap> down := XModMorphismByHoms( downconj, downconj, as3a, ad12 );
[[s3a->d12] => [s3a->d12]]
gap> right := XModMorphismByHoms( rightconj, rightconj, as3b, ad12 );
[[s3b->d12] => [s3b->d12]]
gap> autoconj := XSqMorphism( XSconj, XSconj, up, left, right, down );; 
gap> ord := Order( autoconj );;
gap> Display( autoconj );
Morphism of crossed squares :- 
:    Source = [c3->s3b,s3a->d12]
:     Range = [c3->s3b,s3a->d12]
:     order = 3
:    up-left: [ [ (11,13,15)(12,14,16) ], [ (11,13,15)(12,14,16) ] ]
:   up-right: [ [ (11,13,15)(12,14,16), (11,16)(12,15)(13,14) ], 
  [ (11,13,15)(12,14,16), (11,12)(13,16)(14,15) ] ]
:  down-left: [ [ (11,13,15)(12,14,16), (12,16)(13,15) ], 
  [ (11,13,15)(12,14,16), (11,13)(14,16) ] ]
: down-right: [ [ (11,12,13,14,15,16), (12,16)(13,15) ], 
  [ (11,12,13,14,15,16), (11,13)(14,16) ] ]
gap> KnownPropertiesOfObject( autoconj );
[ "CanEasilyCompareElements", "CanEasilySortElements", "IsTotal", 
  "IsSingleValued", "IsInjective", "IsSurjective", "IsPreXSqMorphism", 
  "IsXSqMorphism", "IsEndomorphism3dDomain" ]
gap> IsAutomorphism3dDomain( autoconj );
true
gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp3objmap.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
