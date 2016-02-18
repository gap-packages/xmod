##############################################################################
##
#W  dom2d3d.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 


###########################  DOMAIN WITH OBJECTS  ########################### 

############################################################################# 
## 
#M  TypeOf2dDomain( <m2d> ) 
##
InstallMethod( TypeOf2dDomain, "for list of 2d-domains", true, [ IsList ], 0, 

    function( pieces ) 
    local  type; 
    ## type:  1=gpd, 2=mon, 3=sgp, 4=mgm, 5=dom 
    type := 0; 
    return type; 
end );


################################  MAGMAS  ################################### 

############################################################################# 
## 
#F  Make2dMagma( <mag>, <obs> ) 
##
InstallGlobalFunction( Make2dMagma, function( arg ) 

    local  obs, mag;
    Print( "Usage: Make2dMagma( <src>, <rng> ); but not yet installed\n" ); 
    return fail; 
end ); 

#############################################################################
##
#M  \=( <m1>, <m2> )  . . . . . . . test if two 2d-magmas are equal
##
InstallMethod( \=, "for 2d-magmas", IsIdenticalObj,
    [ Is2dMagma, Is2dMagma ], 0, 
function ( m1, m2 ) 
    local  i, p1, p2;
    return fail; 
end );


#################################  SUBDOMAINS  ############################## 

#############################################################################
##
#F  IsSub2dDomain( <M>, <U> )
##
InstallMethod( IsSub2dDomain, "for two 2d-domains", true, 
    [ Is2dDomain, Is2dDomain ], 0, 
    function( D, U )

    local  compU, obj, p, ok; 
    return fail; 
end );


################################  SEMIGROUPS  ############################### 

############################################################################# 
## 
#F  Make2dSemigroup( <sgp>, <obs> ) 
##
InstallGlobalFunction( Make2dSemigroup, function( arg ) 

    local  obs, sgp; 
    Print( "Usage: Make2dSemegroup( <src>, <rng> ); not yet installed\n" ); 
    return fail; 
end ); 


##################################  MONOIDS  ################################ 

############################################################################# 
## 
#F  Make2dMonoid( <mon>, <obs> ) 
##
InstallGlobalFunction( Make2dMonoid, function( arg ) 

    local  obs, mon;
    Print( "Usage: Make2dMonoid( <src>, <rng> ); but not yet installed\n" ); 
    return fail; 
end ); 

#############################################################################
##
#E  dom2d3d.gi  . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
