#############################################################################
##
#W  gp2ind.tst                    XMOD test file                Chris Wensley
#W                                                                & Murat Alp
##  version 2.43, 09/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> saved_infolevel_xmod := InfoLevel( InfoXMod );; 
gap> SetInfoLevel( InfoXMod, 0 );;

## Chapter 6

## Section 6.1.1
gap> s4gens := [ (1,2), (2,3), (3,4) ];;
gap> s4 := Group( s4gens );; SetName(s4,"s4");
gap> a4gens := [ (1,2,3), (2,3,4) ];;
gap> a4 := Subgroup( s4, a4gens );;  SetName( a4, "a4" );
gap> s3 := Group( (5,6),(6,7) );;  SetName( s3, "s3" );
gap> epi := GroupHomomorphismByImages( s4, s3, s4gens, [(5,6),(6,7),(5,6)] );;
gap> X4 := XModByNormalSubgroup( s4, a4 );;
gap> indX4 := SurjectiveInducedXMod( X4, epi );
[a4/ker->s3]
gap> Display( indX4 );

Crossed module [a4/ker->s3] :- 
: Source group a4/ker has generators:
  [ (1,3,2), (1,2,3) ]
: Range group s3 has generators:
  [ (5,6), (6,7) ]
: Boundary homomorphism maps source generators to:
  [ (5,6,7), (5,7,6) ]
: Action homomorphism maps range generators to automorphisms:
  (5,6) --> { source gens --> [ (1,2,3), (1,3,2) ] }
  (6,7) --> { source gens --> [ (1,2,3), (1,3,2) ] }
  These 2 automorphisms generate the group of automorphisms.

gap> morX4 := MorphismOfInducedXMod( indX4 );
[[a4->s4] => [a4/ker->s3]]


gap> d8 := Subgroup( d16, [ d1^2, d2 ] );  SetName( d8, "d8" ); 
Group([ (11,13,15,17)(12,14,16,18), (12,18)(13,17)(14,16) ])
gap> c4 := Subgroup( d8, [ d1^2 ] );  SetName( c4, "c4" ); 
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
gap> s3b := Subgroup( s4, [ (2,3), (3,4) ] );;  
gap> SetName( s3b, "s3b" );
gap> indX3 := InducedXMod( s4, s3b, s3b );
#I induced group has Size: 48
i*([s3b->s3b])

gap> SetInfoLevel( InfoXMod, saved_infolevel_xmod );; 

#############################################################################
##
#E  gp2ind.tst . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
