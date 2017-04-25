#############################################################################
##
#W  isoclinic.g         XMOD example files       Chris Wensley & Alper Odabas
##                                                               & Enver Uslu
#Y  Copyright (C) 2001-2017, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
##
#############################################################################

Print("\nXMod example file isoclinic.g (version 23/04/17) :-\n\n");
Print("isoclinism tests\n\n" ); 
level := InfoLevel( InfoXMod ); 
SetInfoLevel( InfoXMod, 0 );

d24 := DihedralGroup(24);  
SetName( d24, "d24" );
X24 := XModByAutomorphismGroup( d24 );
Print( "X24 has size ", Size(X24), "\n" );
nsx := NormalSubXMods( X24 ); 
ids := List( nsx, n -> IdGroup(n) ); 
pos1 := Position( ids, [ [4,1], [8,3] ] ); 
Xn1 := nsx[pos1];  
Print( "normal subxmod Xn1 = ", Xn1, "\n" ); 
Print( "Xn1 has size ", Size( Xn1 ), "\n" );
nat1 := NaturalMorphismByNormalSubPreXMod( X24, Xn1 ); 
Print( "nat1 = ", nat1, "\n" ); 
Qn1 := FactorPreXMod( X24, Xn1 );  
Print( "factor xmod Qn1 has size ", Size( Qn1 ), "\n\n" );


pos2 := Position( ids, [ [24,6], [12,4] ] );
Xn2 := nsx[pos2]; 
Print( "normal subxmod Xn2 = ", Xn2, "\n" ); 
Print( "Xn2 has size ", Size( Xn2 ), "\n" );
pos3 := Position( ids, [ [12,2], [24,5] ] );
Xn3 := nsx[pos3]; 
Print( "normal subxmod Xn3 = ", Xn3, "\n" ); 
Print( "Xn3 has size ", Size( Xn3 ), "\n" );
Xn23 := IntersectionSubXMods( X24, Xn2, Xn3 );
Print( "Xn23 is the intersection of crossed  modules Xn2, Xn3 :-\n" );
Print( Xn3, "\n" ); 
Print( "Xn2,Xn3,Xn23 have sizes: ", [Size(Xn2),Size(Xn3),Size(Xn23) ], "\n\n" );


pos4 := Position( ids, [ [6,2], [24,14] ] );;
Xn4 := nsx[pos4];; 
Print( "normal subxmod Xn4 = ", Xn4, "\n" ); 
Print( "Xn4 has size ", Size( Xn4 ), "\n" );
Sn4 := Source(Xn4);;  SetName( Sn4, "c6" ); 
Rn4 := Range(Xn4);;  SetName( Rn4, "c2c2s3" );
Display(Xn4);
r := Rn4.1;  s := Sn4.1; 
d := Displacement( XModAction(Xn4), r, s ); 
Print( "in Xn4 the displacement <r,s> = <", r, ",", s, "> is d = ", d, "\n" ); 
bn4 := Boundary( Xn4 );
imd := Image( bn4, d ); 
ims := Image( bn4, s ); 
Print( "the boundary of Xn4 maps s = ", s, " to ims = ", ims, "\n" ); 
Print( "the boundary of Xn4 maps d to imd = ", imd, "\n" );  
Print( "which should equal [r,ims] = [", r, ",", ims, "] = " ); 
Print( Comm( r, ims ), "\n" );  
Print( "the displacement subgroup of Xn4 is: " ); 
Print( DisplacementSubgroup( Xn4 ), "\n\n" );

Print( "the cross action subgroup for Xn2 and Xn3 in X24 is: " );
Print( CrossActionSubgroup( X24, Xn2, Xn3 ), "\n" );
Cn23 := CommutatorSubXMod( X24, Xn2, Xn3 );
Print( "The commutator subxmod  [Xn2,Xn3] in XN24 is Cn23 = ", Cn23, "\n" ); 
Print( "Cn23 has size ", Size(Cn23) );
Print( " and IdGroup ", IdGroup( Cn23 ), "\n" ); 
Print( "Xn23 = Cn23 ? ", Xn23 = Cn23, "\n" ); 

DXn4 := DerivedSubXMod( Xn4 );  
Print( "Xn4 has derived subxmod DXn4 = ", DXn4, "\n" );
fix := FixedPointSubgroupXMod( Xn4, Sn4, Rn4 );
Print( "Xn4 has fixed point subgroup ", fix, "\n" );
stab := StabilizerSubgroupXMod( Xn4, Sn4, Rn4 );
Print( "Xn4 has stabilizer subgroup ", stab, "\n" ); 

ZXn4 := CentreXMod( Xn4 );      
Print( "Xn4 has centre ", ZXn4, "\n" ); 
Print( "with IdGroup ", IdGroup( ZXn4 ), "\n" );
CDXn4 := Centralizer( Xn4, DXn4 );
Print( "the centralizer of DXn4 in Xn4 is ", CDXn4, "\n" ); 
Print( "with IdGroup ", IdGroup( CDXn4 ), "\n" );
NDXn4 := Normalizer( Xn4, DXn4 ); 
Print( "the normalizer of DXn4 in Xn4 is ", NDXn4, "\n" ); 
Print( "with IdGroup ", IdGroup( NDXn4 ), "\n" );

Q24 := CentralQuotient( d24);       
Print( "the central quotient of d24 is Q24 = ", Q24, "\n" ); 
Print( "Q24 has size ", Size( Q24 ) );
Print( " and structure ", StructureDescription( Q24 ), "\n\n" );  

Print( "Xn4 is abelian? ", IsAbelian2DimensionalGroup(Xn4), "\n" ); 
Print( "X24 is abelian? ", IsAbelian2DimensionalGroup(X24), "\n" );
pos7 := Position( ids, [ [3,1], [6,1] ] );;
Print( "nsx[", pos7, "] is the normal subcrossed module:\n" ); 
Print( nsx[pos7], "\n" );
Print( "this crossed module is aspherical? ", 
        IsAspherical2DimensionalGroup(nsx[pos7]), "\n" );
Print( "X24 is aspherical? ", IsAspherical2DimensionalGroup(X24), "\n" );
Print( "Xn4 is simply connected? ", 
        IsSimplyConnected2DimensionalGroup(Xn4), "\n" ); 
Print( "X24 is simply connected? ", 
        IsSimplyConnected2DimensionalGroup(X24), "\n" );
Print( "Xn4 is faithful? ", IsFaithful2DimensionalGroup(Xn4), "\n" ); 
Print( "X24 is faithful? ", IsFaithful2DimensionalGroup(X24), "\n" ); 

Print( "X24 has lower central series:\n", LowerCentralSeries(X24), "\n" );
Print( "X24 is nilpotent? ", IsNilpotent2DimensionalGroup(X24), "\n" ); 
Print( "X24 has nilpotency class ", 
        NilpotencyClassOf2DimensionalGroup(X24), "\n\n" );

xc6s3 := AllXMods( SmallGroup(6,2), SmallGroup(6,1) );;   
Print( "there are ", Length(xc6s3), " crossed modules with\n" ); 
Print( "source SmallGroup(6,2) and range SmallGroup(6,1)\n" );            
x66 := AllXMods( [6,6] );;   
Print( "there are ", Length(x66), " crossed modules with " ); 
Print( "both source and range of size 6\n" );            
x36 := AllXMods( 36 );; 
Print( "there are ", Length(x36), " crossed modules with" ); 
Print( "total size 36\n" );            
Print( "the numbers of these for each possible size are:\n" ); 
size36 := List( x36, x -> [ Size(Source(x)), Size(Range(x)) ] );
Print( Collected(size36), "\n\n" );

Print( "here is an isomorphism between x66[1] and x66[2]:\n" ); 
Print( IsomorphismXMods( x66[1], x66[2] ), "\n" );

G := SmallGroup( 64, 6 );  
Print ( "G = SmallGroup(64,6) has structure ", StructureDescription(G), "\n" ); 
QG := CentralQuotient( G );  
Print( "QG, the central quotient of G, has IdGroup ", IdGroup(QG), "\n" );
H := SmallGroup( 32, 41 );
Print ( "H = SmallGroup(32,41) has structure ", StructureDescription(H), "\n" ); 
QH := CentralQuotient( H );  
Print( "QH, the central quotient of H, has IdGroup ", IdGroup(QH), "\n" );
Print( "here is an isoclinism from G to H:\n", Isoclinism(G,H), "\n" );
K := SmallGroup( 32, 43 );  
Print ( "K = SmallGroup(32,43) has structure ", StructureDescription(K), "\n" ); 
QK := CentralQuotient( K );  
Print( "QK, the central quotient of K, has IdGroup ", IdGroup(QK), "\n" );
Print( "G and K are isoclinic? ", AreIsoclinicDomains(G,K), "\n\n" );

Print( "G has derived subgroup ", DerivedSubgroup(G), "\n" );     
Print( "G is a stem group? ", IsStemDomain(G), "\n" ); 
isgG := IsoclinicStemDomain( G );
Print( "here is an isoclinic stem group for G: ", isgG, "\n" );
Print( "which has IdGroup ", IdGroup(isgG), "\n" );
Print( "the stem groups of size 32 have IdGroups:\n" ); 
Print( AllStemGroupIds(32), "\n" );
Print( "and these form families:\n" );
Print( AllStemGroupFamilies(32), "\n\n" );

Print( "G has middle length ", IsoclinicMiddleLength(G), "\n" );
Print( "X1 has rank ", IsoclinicRank(X1), "\n\n" ); 

C8 := Cat1Group(16,8,1);
Print( "C8 := Cat1Group(16,8,1) = ", C8, "\n" ); 
X8 := XMod(C8); 
Print( "the associated crossed module is X8 = ", X8, "\n" ); 
Print( "with IdGroup ", IdGroup( X8 ), "\n" );
C9 := Cat1Group(32,9,1);
Print( "C9 := Cat1Group(32,9,1) = ", C9, "\n" ); 
X9 := XMod(C9); 
Print( "the associated crossed module is X9 = ", X9, "\n" ); 
Print( "with IdGroup ", IdGroup( X9 ), "\n" );
Print( "X8 and X9 are isoclinic? ", AreIsoclinicDomains(X8,X9), "\n" );
Print( "here is an isoclinism from X8 to X9\n" );
ism89 := Isoclinism( X8, X9 );
Display(ism89); 
Print( "X8 is a stem crossed module? ", IsStemDomain(X8), "\n" );;
Print( "X9 is a stem crossed module? ", IsStemDomain(X9), "\n" );;
Print( "X24 has middle length ", IsoclinicMiddleLength(X24), "\n" );

SetInfoLevel( InfoXMod, level );
#############################################################################
##
#E  isoclinic.g . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
