#############################################################################
##
#W  hap.gi                    GAP4 package `XMod'               Chris Wensley
#W                                                              
#Y  Copyright (C) 2001-2020, Chris Wensley et al, 

##############################################################################
##
#M  CatOneGroupToXMod( <cat1> )
##  
InstallMethod( CatOneGroupToXMod, "for a HAP cat1-group", true, 
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
    return C; 
end ); 

##############################################################################
##
#M  Cat1GroupToHAP( <cat1> )
##  
InstallMethod( Cat1GroupToHAP, "for an XMod cat1-group", true, 
    [ IsCat1Group ], 0,
function( C )

    local G, gens, t, h;

    G := Source( C ); 
    gens := GeneratorsOfGroup( G ); 
    t := TailMap( C ) * RangeEmbedding( C ); 
    h := HeadMap( C ) * RangeEmbedding( C ); 
    return Objectify( HapCatOneGroup,
        rec( sourceMap := GroupHomomorphismByImagesNC( G, G, gens,
                              List( gens, g -> ImageElm( t, g ) ) ),
             targetMap := GroupHomomorphismByImagesNC( G, G, gens,
                              List( gens, g -> ImageElm( h, g) ) ) ) ); 
end ); 

##############################################################################
##
#M  SmallCat1Group( <n i j> )
##  
InstallMethod( SmallCat1Group, "for 3 positive integers", true, 
    [ IsPosInt, IsPosInt, IsPosInt ], 0,
function( n, i, j )
    local C; 
    C := SmallCatOneGroup( n, i, j ); 
    return CatOneGroupToXMod( C );
end ); 

##############################################################################
##
#M  IdCat1Group( <cat1> )
#M  IdQuasiCat1Group( <cat1> )
##  
InstallMethod( IdCat1Group, "for a cat1-group", true, [ IsCat1Group ], 0,
function( C )
    local B; 
    B := Cat1GroupToHAP( C ); 
    return IdCatOneGroup( B );
end ); 

InstallMethod( IdQuasiCat1Group, "for a cat1-group", true, [ IsCat1Group ], 0,
function( C )
    local B; 
    B := Cat1GroupToHAP( C ); 
    return IdQuasiCatOneGroup( B );
end ); 

##############################################################################
##
#M  QuasiIsomorphCat1Group( <cat1> )
##  
InstallMethod( QuasiIsomorphCat1Group, "for a cat1-group", true, 
    [ IsCat1Group ], 0,
function( C )
    local HC, HQ, Q; 
    HC := Cat1GroupToHAP( C ); 
    HQ := QuasiIsomorph( HC ); 
    Q := CatOneGroupToXMod( HQ ); 
    return Q; 
end ); 


