#############################################################################
##
#W  gp2up.g                  XMOD example files                 Chris Wensley
#W                                                                & Murat Alp
##  version 2.43, 10/11/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod example file gp2up.g (version 10/11/15) :-");
Print("\ntesting derivations of crossed modules\n\n");

c5 := Group( (5,6,7,8,9) );
SetName( c5, "c5" );
X1 := XModByAutomorphismGroup( c5 );        
chi1 := DerivationByImages( X1, [ (), () ] );
Print( "chi1 = ", chi1, "\n" );
ok1 := IsDerivation( chi1 );
Print("chi1 is a derivation? ", ok1, "\n" );
Print("chi1 has Object2d = ", Object2d( chi1 ), "\n" );
Print("chi1 has UpGeneratorImages = ", UpGeneratorImages( chi1 ), "\n" );
Print("chi1 has source = ", Source(chi1), "\n" );
Print("chi1 has range = ", Range(chi1), "\n" );
Print("known properties of chi1:\n", KnownPropertiesOfObject(chi1), "\n" );
Print("known attributes of chi1:\n", KnownAttributesOfObject(chi1), "\n" );

C1 := Cat1OfXMod( X1 );
Print("\nX1 is associated to cat1-group C1 =\n", C1, "\n" );
xi1 := SectionByDerivation( chi1 );
Print("xi1 = ", xi1, "\n" );

reg1 := RegularDerivations( X1 );
Print("regular derivations of X1 have images list\n" );
imreg1 := ImagesList( reg1 );
Display( imreg1 );
Print("images table for these derivations:\n" );
Display( ImagesTable( reg1 ) );
all1 := RegularDerivations( X1 );
Print( "reg1 = all1 ?  -  ", reg1=all1, "\n" );

chi2 := DerivationByImages( X1, imreg1[2] );
chi3 := DerivationByImages( X1, imreg1[3] );
Print( "chi2 = ", chi2, "\n" );
Print( "chi3 = ", chi3, "\n" );
im2 := UpImagePositions( chi2 );
im3 := UpImagePositions( chi3 );
Print("chi2 has image positions : ", im2, "\n" );
Print("chi3 has image positions : ", im3, "\n" );

chi23 := chi2 * chi3;
im23 := UpImagePositions( chi23 );
Print( "chi2 * chi3= ", chi23, "\nwith image positions", im23, "\n" );

g1 := (5,6,7,8,9);
iota := PrincipalDerivation( X1, g1 );
ipos := UpImagePositions( iota );
Print("iota has image positions : ", ipos, "\n" );

Print("\n\n################################################\n\n");

g18 := Group( (1,2,3), (4,5,6), (2,3)(5,6) );
SetName( g18, "g18" );
gen18 := GeneratorsOfGroup( g18 );
a := gen18[1];  b := gen18[2];  c := gen18[3];
s3 := Subgroup( g18, gen18{[2..3]} );
SetName( s3, "s3" );
t := GroupHomomorphismByImages( g18, s3, gen18, [ b,b,c] );
h := GroupHomomorphismByImages( g18, s3, gen18, [(),b,c] );
e := GroupHomomorphismByImages( s3, g18, [b,c], [b,c] );

C3 := Cat1( t, h, e );
Print("the cat1-group C3 :-\n");
Display(C3);
SetName( Kernel(t), "c3" );

X3 := XModOfCat1( C3 );
Print("the crossed module X3 associated to C3 :-\n");
Display( X3 );

Print("X3 has attributes:\n", KnownAttributesOfObject(X3), "\n");
Print("C3 has attributes:\n", KnownAttributesOfObject(C3), "\n\n");

imchi := [ (1,2,3)(4,6,5), (1,2,3)(4,6,5) ];
chi := DerivationByImages( X3, imchi );
Print("chi has UpGeneratorImages = ", UpGeneratorImages(chi), "\n" );
Print("chi has UpImagePositions  = ", UpImagePositions( chi ), "\n\n" );
xi := SectionByDerivation( chi );
Print("\nxi = ", xi, "\n" );

reg3 := RegularDerivations( X3 );
Print("regular derivations of X3 have images list\n" );
imreg3 := ImagesList( reg3 );
Display( imreg3 );
Print("images table for these derivations:\n" );
Display( ImagesTable( reg3 ) );
wgt3 := WhiteheadGroupTable( X3 );
Print("X3 has Whitehead Group Table:\n");
Display( wgt3);
wpg3 := WhiteheadPermGroup( X3 );
gwpg3 := GeneratorsOfGroup( wpg3);
Print("Whitehead perm group wpg3 of X3 is ", wpg3, "\n");
Print("and has generators ", gwpg3, "\n\n");

all3 := AllDerivations( X3 );
Print("all derivations of X3 have images list\n" );
Display( ImagesList( all3 ) );
Print("images table for these derivations:\n" );
Display( ImagesTable( all3 ) );
wmt3 := WhiteheadMonoidTable( X3 );
Print("X3 has Whitehead Monoid Table:\n");
Display( wmt3);
wtm3 := WhiteheadTransMonoid( X3 );
Print("\nwtm3 = ", wtm3, "\n" ); 
Print("\nWhitehead monoid of X3 has trans rep with generators:\n");
Print(GeneratorsOfMonoid(wtm3),"\n\n");

chi4 := DerivationByImages( X3, imreg3[4] );
chi5 := DerivationByImages( X3, imreg3[5] );
Print( "chi4 = ", chi4, "\n" );
Print( "chi5 = ", chi5, "\n" );
im4 := UpImagePositions( chi4 );
im5 := UpImagePositions( chi5 );
Print("chi4 has image positions : ", im4, "\n" );
Print("chi5 has image positions : ", im5, "\n" );

chi45 := chi4 * chi5;
im45 := UpImagePositions( chi45 );
Print( "chi4 * chi5 = ", chi45, "\nwith image positions ", im45, "\n" );
pos := Position( imreg3, UpGeneratorImages( chi45 ) );
Print("chi4 * chi5 is the derivation in position ", pos, "\n" );

#### In Sept.'03 changed generators of W to strong generating set
#### instead of all the elements, so the following no longer works:
## p45 :=  gwpg3[4]*gwpg3[5];
## Print("perm4 * perm5 is ", p45 );
## Print(" which is also in position ", Position( gwpg3, p45 ), "\n");
## Print("CONCLUSION: the perm rep _is_ isomorphic to the Whitehead group!\n");

Print("\n\n################################################\n\n");

Print("Testing principal derivations in PX2 = perm rep of X2:\n\n");
isoPX2 := IsomorphismPermObject( X2 );
PX2 := Image( isoPX2 ); 
gens2 := GeneratorsOfGroup( Source( PX2 ) );
g1 := gens2[1];  g2 := gens2[2];
iotag1 := PrincipalDerivation( PX2, g1 );
ipos1 := UpImagePositions( iotag1 );
Print("iota(g1) has image positions : ", ipos1, "\n" );
iotag2 := PrincipalDerivation( PX2, g2 );
ipos2 := UpImagePositions( iotag2 );
Print("iota(g2) has image positions : ", ipos2, "\n" );
g3 := g1*g2;
Print("g3 = g1*g2 = ", g3, "\n");
iotag3 := PrincipalDerivation( PX2, g3 );
ipos3 := UpImagePositions( iotag3 );
Print("iota(g3) has image positions : ", ipos3, "\n" );
prod12 := iotag1 * iotag2; 
ipos12 := UpImagePositions( prod12 );
Print("(iotag1 * iotag2) has image positions : ", ipos12, "\n" );
comp21 := CompositeDerivation( iotag2, iotag1 ); 
ipos21 := UpImagePositions( comp21 );
Print("(iotag2 circ iotag1) has image positions : ", ipos21, "\n" );
prod21 := iotag2 * iotag1; 
ipos21z := UpImagePositions( prod21 );
Print("(iotag2 * iotag1) has image positions : ", ipos21z, "\n" );

prin3 := PrincipalDerivations( X3 );
Print( "\nX3 has principal derivations:\n", prin3, "\n" );

Print("\nStill a problem with  SectionByDerivation ? \n\n" );

###############################################################################
##
#E  gp2up.g  . . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
