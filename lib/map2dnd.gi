##############################################################################
##
#W  map2dnd.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2025, Chris Wensley et al,  

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
