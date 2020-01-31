#############################################################################
##
#W  hap.gi                    GAP4 package `XMod'               Chris Wensley
#W                                                              
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  HapToXMod( <cat1> )
##  
InstallMethod( HapToXMod, "for a HAP cat1-group", true, 
    [ IsHapCatOneGroupRep ], 0,
function( G )

    local  S, R, tm, hm, t, h, e, C;

    tm := G!.sourceMap; 
    hm := G!.targetMap; 
    S := Source( tm ); 
    R := ImagesSource( tm );
    if not ( ImagesSource( hm ) = R ) then 
        Error( "source and target maps do not have the same image" ); 
    fi; 
    t := GeneralRestrictedMapping( tm, S, R ); 
    h := GeneralRestrictedMapping( hm, S, R ); 
    e := InclusionMappingGroups( S, R ); 
    C := PreCat1GroupByTailHeadEmbedding( t, h, e ); 
    SetCatOneGroupXModVersion( G, C ); 
    SetCat1GroupHapVersion( C, G ); 
    return C; 
end ); 
