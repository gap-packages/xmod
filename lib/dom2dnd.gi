##############################################################################
##
#W  dom2dnd.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  


###########################  DOMAIN WITH OBJECTS  ########################### 

################################  MAGMAS  ################################### 

############################################################################# 
## 
#F  Make2DimensionalMagma( <mag>, <obs> ) 
##
InstallGlobalFunction( Make2DimensionalMagma, function( arg ) 

    local obs, mag;

    Print( "Usage: Make2DimensionalMagma( <src>, <rng> )" ); 
    Print( " (but not yet installed\n" ); 
    return fail; 
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . test if two 2Dimensional-magmas are equal
##
InstallMethod( \=, "for 2Dimensional-magmas", IsIdenticalObj,
    [ Is2DimensionalMagma, Is2DimensionalMagma ], 0, 
function ( m1, m2 ) 
    local i, p1, p2;
    Print( " (not yet installed)\n" ); 
    return fail; 
end );


#################################  SUBDOMAINS  ############################## 

#############################################################################
##
#F  IsSub2DimensionalDomain( <M>, <U> )
##
InstallMethod( IsSub2DimensionalDomain, "for two 2Dimensional-domains", true, 
    [ Is2DimensionalDomain, Is2DimensionalDomain ], 0, 
function( D, U )
    local compU, obj, p, ok; 
    Print( " (not yet installed)\n" ); 
    return fail; 
end );


################################  SEMIGROUPS  ############################### 

############################################################################# 
## 
#F  Make2DimensionalSemigroup( <sgp>, <obs> ) 
##
InstallGlobalFunction( Make2DimensionalSemigroup, function( arg ) 

    local obs, sgp; 
    Print( "Usage: Make2DimensionalSemigroup( <src>, <rng> )" ); 
    Print( " (but not yet installed)\n" ); 
    return fail; 
end ); 


##################################  MONOIDS  ################################ 

############################################################################# 
## 
#F  Make2DimensionalMonoid( <mon>, <obs> ) 
##
InstallGlobalFunction( Make2DimensionalMonoid, function( arg ) 

    local obs, mon;
    Print( "Usage: Make2DimensionalMonoid( <src>, <rng> )" ); 
    Print( " (but not yet installed)\n" ); 
    return fail; 
end ); 
