##############################################################################
##
#W  map2dnd.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

#############################################################################
##
#M  Is2DimensionalSemigroupMorphism
#M  Is2DimensionalMonoidMorphism 
##
InstallMethod( Is2DimensionalSemigroupMorphism, 
    "for a 2d-mapping", true, [ Is2DimensionalMagmaMorphism ], 0,
function( map ) 
    return ( Is2DimensionalSemigroup( Source(map) ) 
             and Is2DimensionalSemigroup( Range(map) ) ); 
end );

InstallMethod( Is2DimensionalMonoidMorphism, "for a 2d-mapping", 
    true, [ Is2DimensionalMagmaMorphism ], 0,
function( map ) 
    return ( Is2DimensionalMonoid( Source(map) ) 
             and Is2DimensionalMonoid( Range(map) ) ); 
end );

##############################################################################
##
#M  \=( <mor>, <phi> ) . . . . . test if two morphisms of 2d-objects are equal
##
InstallMethod( \=,
    "generic method for two 2d-morphisms",
    IsIdenticalObj, [ Is2DimensionalMapping, Is2DimensionalMapping ], 0,
function ( mor, phi )
    return (     ( Source( mor ) = Source( phi ) )
             and ( Range( mor ) = Range( phi ) )
             and ( SourceHom( mor ) = SourceHom( phi ) )
             and ( RangeHom( mor ) = RangeHom( phi ) ) );
end );
