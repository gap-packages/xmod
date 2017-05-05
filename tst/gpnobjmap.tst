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
condition  [kert,kerh] = 1  is not satisfied 
[Group( [ f1, f2 ] )=>Group( [ <identity> of ..., <identity> of ... ] )]
gap> pc2 := PreCat1GroupByEndomorphisms( idem[2], idem[2] );
[Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> C12 := CatnGroup( [ pc1, pc2 ] );
condition  [kert,kerh] = 1  is not satisfied
    2DimensionalDomain-1 = [Group( [ f1, f2 ] )=>Group(
[ <identity> of ..., <identity> of ... ] )]
    2DimensionalDomain-2 = [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> PreCatnDimension(C12);
2
gap> pc3 := PreCat1GroupByEndomorphisms(idem[5],idem[5]);
[Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
gap> C233 := CatnGroup( [pc2, pc3, pc3] );
    2DimensionalDomain-1 = [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
    2DimensionalDomain-2 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
    2DimensionalDomain-3 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

gap> IsPreCatnGroup( C233 );
true
gap> IsCatnGroup( C233 );
true
gap> Display( C233 );
(pre-)cat3-group with:
     2DimensionalDomain-1 = [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
     2DimensionalDomain-2 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
     2DimensionalDomain-3 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

#############################################################################
##
#E  gpnobjmap.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
