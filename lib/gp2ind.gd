##############################################################################
##
#W  gp2ind.gd                   GAP4 package `XMod'             Chris Wensley
##
#Y  Copyright (C) 2001-2024, Chris Wensley et al,  
##  
##  This file declares functions for induced crossed modules. 
##  
###############################################################################
##  Layout of groups for induced crossed module calculations:
## 
##          Fin. Pres. Groups              Groups
##          =================           ============
##
##        FN=FM -------> FI            N=M -------> I
##            |          |              |           |
##            |          |              |           |
##            V          V              V           V
##           FP -------> FQ             P --------> Q
##
##############################################################################
##  Layout of groups for induced cat1-group calculations:
##
##                                          iota*
##         G ------------> I         PG ------------> PI
##         ||             ||         ||               || 
##         ||             ||         ||               ||  
##         th            t*h*        th              t*h* 
##         ||             ||         ||               || 
##         ||             ||         ||               || 
##         VV             VV         VV               VV 
##         R ------------> Q         PR ------------> PQ
##                                          iota
##
##############################################################################

#############################################################################
##
#A  InducedXModData( <IX> )
##
##  DeclareAttribute( "InducedXModData", Is2DimensionalDomain, "mutable" );

#############################################################################
##
#O  CoproductXMod( <xmod>, <xmod> )
#A  CoproductInfo( <xmod> ) 
##
DeclareOperation( "CoproductXMod", [ IsXMod, IsXMod ] ); 
DeclareOperation( "CoproductXMod", [ IsList ] ); 
DeclareAttribute( "CoproductInfo", IsXMod, "mutable" );

#############################################################################
##
#P  IsInducedXMod( <IX> )
##
DeclareProperty( "IsInducedXMod", IsXMod );

#############################################################################
##
#A  MorphismOfInducedXMod( <IX> )
##
DeclareAttribute( "MorphismOfInducedXMod", IsInducedXMod );

#############################################################################
##
#F  InducedXMod( <args> )
#O  InducedXModBySurjection( <xmod>, <hom> )
#O  InducedXModByCoproduct( <xmod>, <hom> )
#O  InducedXModByBijection( <xmod>, <hom> )
#O  InducedXModByCopower( <grp>, <hom>, <trans> )
#O  InducedXModFromTrivialSource( <xmod>, <hom> )
#O  InducedXModFromTrivialRange( <xmod>, <hom> )
##
DeclareGlobalFunction( "InducedXMod" );
DeclareOperation( "InducedXModBySurjection", 
    [ IsXMod, IsGroupHomomorphism ] );
DeclareOperation( "InducedXModByCoproduct", 
    [ IsXMod, IsGroupHomomorphism ] );
DeclareOperation( "InducedXModByBijection", 
    [ IsXMod, IsGroupHomomorphism ] );
DeclareOperation( "InducedXModByCopower", 
    [ IsXMod, IsGroupHomomorphism, IsList ] );
DeclareOperation( "InducedXModFromTrivialSource", 
    [ IsXMod, IsGroupHomomorphism ] );
DeclareOperation( "InducedXModFromTrivialRange", 
    [ IsXMod, IsGroupHomomorphism ] );

#############################################################################
##
#F  InducedCat1Group( <args> )
#O  InducedCat1Data( <grp>, <hom>, <trans> )
#O  InducedCat1GroupByFreeProduct( [ <grp>, <hom> ] )  ???
##
DeclareGlobalFunction( "InducedCat1Group" );
DeclareOperation( "InducedCat1Data", 
    [ IsCat1Group, IsGroupHomomorphism, IsList ] );
DeclareOperation( "InducedCat1GroupByFreeProduct", [ IsList ] );

#############################################################################
##
#O  AllInducedXMods( <grp> )
#O  AllInducedCat1Groups( <grp> )
##
DeclareGlobalFunction( "AllInducedXMods" );
DeclareGlobalFunction( "AllInducedCat1Groups" );
