##############################################################################
##
#W  gp4obj.gi                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements generic methods for (pre-)crossed cubes 
##  and (pre-)cat3-groups.
##
#Y  Copyright (C) 2001-2019, Chris Wensley et al, 
#Y  School of Computer Science, Bangor University, U.K. 
    
##############################################################################
##
#M  PreCat3GroupObj( [<front>,<up>,<left>,<right>,<down>,<back>] ) 
##
InstallMethod( PreCat3GroupObj, "for a list of pre-cat1-groups", true,
    [ IsList ], 0,
function( L )

    local len, d, PC, ok;

    len := Length( L ); 
    if not ( len = 6 ) then 
        Error( "there should be 6 pre-cat2-groups in the list L" ); 
    fi; 
    PC := rec();
    ObjectifyWithAttributes( PC, PreCat2GroupObjType, 
      Front3DimensionalGroup, L[1], 
      Up3DimensionalGroup, L[1], 
      Left3DimensionalGroup, L[2],
      Right3DimensionalGroup, L[3],
      Down3DimensionalGroup, L[4],
      Back3DimensionalGroup, d, 
      GeneratingCat1Groups, [ Up2DimensionalGroup( L[1] ),
                              Left2DimensionalGroup( L[1] ), 
                              Up2DimensionalGroup( L[2] ) ],
      HigherDimension, 4, 
      IsHigherDimensionalGroup, true, 
      IsPreCat3Group, true, 
      IsPreCatnGroup, true );
    ok := IsCat3Group( PC );
    return PC;
end );

