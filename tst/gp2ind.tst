#############################################################################
##
#W  gp2ind.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
##  version 2.43, 10/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 7

## Section 7.1.1
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
gap> inc8 := InclusionMorphism2dDomains( Y16, Y8 ); 
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

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp2ind.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
