###############################################################################
##
#W  gp2ind.gd                   GAP4 package `XMod'               Chris Wensley
##
##  version 2.32, 26/02/2015 
##
#Y  Copyright (C) 2001-2015, Murat Alp and Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
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
##  #A  InducedXModData( <IX> )
##
##  DeclareAttribute( "InducedXModData", Is2dDomain, "mutable" );

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
#O  InclusionInducedXModByCopower( <grp>, <hom>, <trans> )
#O  SurjectiveInducedXMod( <xmod>, <hom> )
##
DeclareGlobalFunction( "InducedXMod" );
DeclareOperation( "InclusionInducedXModByCopower", 
    [ IsXMod, IsGroupHomomorphism, IsList ] );
DeclareOperation( "SurjectiveInducedXMod", [ IsXMod, IsGroupHomomorphism ] );

##############################################################################
##
#F  InducedCat1( <args> )
#O  InclusionInducedCat1Data( <grp>, <hom>, <trans> )
#O  InducedCat1ByFreeProduct( [ <grp>, <hom> ] )  ???
##
DeclareGlobalFunction( "InducedCat1" );
DeclareOperation( "InclusionInducedCat1Data", 
    [ IsCat1, IsGroupHomomorphism, IsList ] );
DeclareOperation( "InducedCat1ByFreeProduct", [ IsList ] );

##############################################################################
##
#O  AllInducedXMods( <grp> )
#O  AllInducedCat1s( <grp> )
##
DeclareGlobalFunction( "AllInducedXMods" );
DeclareGlobalFunction( "AllInducedCat1s" );

##############################################################################
##
#E  gp2ind.gd . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
