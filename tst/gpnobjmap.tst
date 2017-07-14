#############################################################################
##
#W  gpnobjmap.tst                 XMOD test file                Chris Wensley
##                                                               Alper Odabas
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> s3 := SmallGroup(6,1);;
gap> homs := AllHomomorphisms(s3,s3);;
gap> idem := Filtered( homs, i -> CompositionMapping(i,i) = i );
[ [ f1, f2 ] -> [ <identity> of ..., <identity> of ... ],
  [ f1, f2 ] -> [ f1, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2^2, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2, <identity> of ... ], [ f1, f2 ] -> [ f1, f2 ] ]
gap> pc1 := PreCat1GroupByEndomorphisms( idem[1], idem[1] );
[Group( [ f1, f2 ] )=>Group( [ <identity> of ..., <identity> of ... ] )]
gap> pc2 := PreCat1GroupByEndomorphisms( idem[2], idem[2] );
[Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> C12 := CatnGroup( [ pc1, pc2 ] );
generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ <identity> of ..., <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]

gap> HigherDimension(C12);
3

gap> 



gap> pc3 := PreCat1GroupByEndomorphisms( idem[5], idem[5] );
[Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
gap> C233 := CatnGroup( [pc2, pc3, pc3] );
generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

gap> IsPreCatnGroup( C233 );
true
gap> IsCatnGroup( C233 );
true

gap> C5 := CatnGroup( [ Cat1Group(8,2,1), Cat1Group(8,2,2),
>          Cat1Group(8,2,4), Cat1Group(8,2,6), Cat1Group(8,2,6) ] );
generating (pre-)cat1-groups:
1 : [C4 x C2=>Group( [ <identity> of ..., <identity> of ..., <identity> of ... ] )]
2 : [C4 x C2=>Group( [ <identity> of ..., f2 ] )]
3 : [C4 x C2=>Group( [ f1, <identity> of ... ] )]
4 : [C4 x C2=>C4 x C2]
5 : [C4 x C2=>C4 x C2]

gap> Display(C5);
generating (pre-)cat1-groups:
1 :
Cat1-group :-
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: tail homomorphism maps source generators to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: head homomorphism maps source generators to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: range embedding maps range generators to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: kernel has generators:
  [ f1, f2, f3 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ..., <identity> of ..., <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f1, f2, f3 ]
: associated crossed module is [Group( [ f1, f2, f3 ] )->Group(
[ <identity> of ..., <identity> of ..., <identity> of ... ] )]

2 :
Cat1-group :-
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ <identity> of ..., f2 ]
: tail homomorphism maps source generators to:
  [ <identity> of ..., f2, <identity> of ... ]
: head homomorphism maps source generators to:
  [ <identity> of ..., f2, <identity> of ... ]
: range embedding maps range generators to:
  [ <identity> of ..., f2 ]
: kernel has generators:
  [ f1, f3 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ..., <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f1, f3 ]
: associated crossed module is [Group( [ f1, f3 ] )->Group(
[ <identity> of ..., f2 ] )]

3 :
Cat1-group :-
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group has generators:
  [ f1, <identity> of ... ]
: tail homomorphism maps source generators to:
  [ f1, <identity> of ..., f3 ]
: head homomorphism maps source generators to:
  [ f1, <identity> of ..., f3 ]
: range embedding maps range generators to:
  [ f1, <identity> of ... ]
: kernel has generators:
  [ f2 ]
: boundary homomorphism maps generators of kernel to:
  [ <identity> of ... ]
: kernel embedding maps generators of kernel to:
  [ f2 ]
: associated crossed module is [Group( [ f2 ] )->Group(
[ f1, <identity> of ... ] )]

4 :
Cat1-group [C4 x C2=>C4 x C2] :-
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group C4 x C2 has generators:
  [ f1, f2, f3 ]
: tail homomorphism maps source generators to:
  [ f1, f2, f3 ]
: head homomorphism maps source generators to:
  [ f1, f2, f3 ]
: range embedding maps range generators to:
  [ f1, f2, f3 ]
: the kernel is trivial.
: associated crossed module is [Group( <identity> of ... )->C4 x C2]

5 :
Cat1-group [C4 x C2=>C4 x C2] :-
: Source group C4 x C2 has generators:
  [ f1, f2, f3 ]
: Range group C4 x C2 has generators:
  [ f1, f2, f3 ]
: tail homomorphism maps source generators to:
  [ f1, f2, f3 ]
: head homomorphism maps source generators to:
  [ f1, f2, f3 ]
: range embedding maps range generators to:
  [ f1, f2, f3 ]
: the kernel is trivial.
: associated crossed module is [Group( <identity> of ... )->C4 x C2]

gap> C6 := Cat2Group(Cat1Group(6,2,1),Cat1Group(6,2,4));
generating (pre-)cat1-groups:
1 : [C6=>Group( [ <identity> of ..., <identity> of ... ] )]
2 : [C6=>C6]

gap> IsCat2Group(C6);
true

gap> XS6 := CrossedSquareByCat2Group(C6);
crossed square with:
      up = [Group( () )->Group( () )]
    left = [Group( () )->Group( [ (1,2), (3,4,5) ] )]
    down = [Group( [ (1,2), (3,4,5) ] )->Group( () )]
   right = [Group( () )->Group( () )]
   
gap> IsCrossedSquare(XS6);
true

gap> C7 := Cat2GroupByCrossedSquare(XS6);
generating (pre-)cat1-groups:
1 : [Group( [ (1,2), (3,4,5) ] )=>Group( [ (), () ] )]
2 : [Group( [ (1,2), (3,4,5) ] )=>Group( [ (1,2), (3,4,5) ] )]

gap> IsCat2Group(C7);
true

gap> idC233 := IdentityMapping( C233 );
<mapping: generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
 -> generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
 >
 
gap> Display( idC233 );
Morphism of pre-cat3-groups :- 
: Source has generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

: Range has generating (pre-)cat1-groups:
1 : [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
2 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
3 : [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

: MappingGeneratorsImages for the source homomorphisms:
1 : [ [ f1, f2 ], [ f1, f2 ] ]
2 : [ [ f1, f2 ], [ f1, f2 ] ]
3 : [ [ f1, f2 ], [ f1, f2 ] ]

gap> IsBijective( idC233 );
true

#############################################################################
##
#E  gpnobjmap.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
