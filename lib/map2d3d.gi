##############################################################################
##
#W  map2d3d.gi                 GAP4 package `XMod'               Chris Wensley
##
##  version 2.43, 21/10/2015 
##
#Y  Copyright (C) 2001-2015, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#M  Is2dSemigroupMorphism
#M  Is2dMonoidMorphism 
##
InstallMethod( Is2dSemigroupMorphism, 
    "for a 2d-mapping", true, [ Is2dMagmaMorphism ], 0,
    function( map ) 
    return ( Is2dSemigroup( Source(map) ) and Is2dSemigroup( Range(map) ) ); 
end );

InstallMethod( Is2dMonoidMorphism, "for a 2d-mapping", 
    true, [ Is2dMagmaMorphism ], 0,
    function( map ) 
    return ( Is2dMonoid( Source(map) ) and Is2dMonoid( Range(map) ) ); 
end );

##############################################################################
##
#M  \=( <mor>, <phi> ) . . . . . test if two morphisms of 2d-objects are equal
##
InstallMethod( \=,
    "generic method for two 2d-morphisms",
    IsIdenticalObj, [ Is2dMapping, Is2dMapping ], 0,
    function ( mor, phi )
    return (     ( Source( mor ) = Source( phi ) )
             and ( Range( mor ) = Range( phi ) )
             and ( SourceHom( mor ) = SourceHom( phi ) )
             and ( RangeHom( mor ) = RangeHom( phi ) ) );
end );

##############################################################################
##
#E  map2d3d.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##
