#############################################################################
##
#W  gpnobjmap.tst                 XMOD test file                Chris Wensley
##																Alper Odabas
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

gap> s3 := SmallGroup(6,1);
<pc group of size 6 with 2 generators>
gap> f := AllHomomorphisms(s3,s3);
[ [ f1, f2 ] -> [ <identity> of ..., <identity> of ... ],
  [ f1, f2 ] -> [ f1, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2^2, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2, <identity> of ... ], [ f1, f2 ] -> [ f1, f2 ],
  [ f1, f2 ] -> [ f1*f2^2, f2 ], [ f1, f2 ] -> [ f1*f2, f2 ],
  [ f1, f2 ] -> [ f1, f2^2 ], [ f1, f2 ] -> [ f1*f2, f2^2 ],
  [ f1, f2 ] -> [ f1*f2^2, f2^2 ] ]
gap> idem_f := Filtered(f, i -> CompositionMapping(i,i) = i);
[ [ f1, f2 ] -> [ <identity> of ..., <identity> of ... ],
  [ f1, f2 ] -> [ f1, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2^2, <identity> of ... ],
  [ f1, f2 ] -> [ f1*f2, <identity> of ... ], [ f1, f2 ] -> [ f1, f2 ] ]
gap> pc1 := PreCat1ByEndomorphisms(idem_f[1],idem_f[1]);
[Group( [ f1, f2 ] )=>Group( [ <identity> of ..., <identity> of ... ] )]
gap> pc2 := PreCat1ByEndomorphisms(idem_f[2],idem_f[2]);
[Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> pC2 := CatnGroup([pc1,pc2]);
condition  [kert,kerh] = 1  is not satisfied
         2dDomain-1 = [Group( [ f1, f2 ] )=>Group(
[ <identity> of ..., <identity> of ... ] )]
         2dDomain-2 = [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
gap> PreCatnDimension(pC2);
2
gap> pc3 := PreCat1ByEndomorphisms(idem_f[5],idem_f[5]);
[Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
gap> pC3 := CatnGroup([pc2,pc3,pc3]);
         2dDomain-1 = [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
         2dDomain-2 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
         2dDomain-3 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

gap> IsPreCatnGroup(pC3);
true
gap> IsCatnGroup(pC3);
true
gap> Display(pC3);
(pre-)cat3-group with:

         2dDomain-1 = [Group( [ f1, f2 ] )=>Group( [ f1, <identity> of ... ] )]
         2dDomain-2 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]
         2dDomain-3 = [Group( [ f1, f2 ] )=>Group( [ f1, f2 ] )]

#############################################################################
##
#E  gpnobjmap.tst . . . . . . . . . . . . . . . . . . . . . . . . . ends here
