##############################################################################
##
#W  gpd2obj.gi                 GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2016, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

##############################################################################
##
#M  PreXModWithObjectsObj( <obs>, <bdy>, <act> ) . . . make pre-crossed module
##
InstallMethod( PreXModWithObjectsObj, "for objects, morphism and action", true,
    [ IsHomogeneousList, IsGeneralMappingWithObjects, 
      IsGeneralMappingWithObjects ], 0,
function( obs, bdy, act )

    local  filter, fam, PM, ok, src, rng, aut, name;

    fam := Family2dGroupWithObjects;
    filter := IsPreXModWithObjectsObj; 
    if not IsConstantOnObjects( bdy ) then 
        Error( "objects not fixed by the boundary" ); 
    fi;
    src := Source( bdy );
    rng := Range( bdy ); 
    if not ( IsGroupoid( src ) and IsGroupoid( rng ) ) then 
        Error( "source/range of boundary should be groupoids" ); 
    fi; 
    if not ( rng = Source( act ) ) then
        Error( "require Range( bdy ) = Source( act )" );
    fi;
    aut := Range( act ); 
    if not IsGroupOfAutomorphisms( aut!.magma ) then
        Error( "Range( act ) must be a group of automorphisms" );
    fi;
    if ( IsPermGroupoid( src ) and IsPermGroupoid( rng ) ) then
        filter := filter and IsPermPreXMod;
    fi;
    PM := rec();
    ObjectifyWithAttributes( PM, 
      NewType( fam, filter ),
      ObjectList, obs, 
      Source, src,
      Range, rng,
      Boundary, bdy,
      AutoGroup, aut,
      XModAction, act,
      Is2dDomain, true, 
      IsPreXModDomain, true );
    if not IsPreXMod( PM ) then
        Info( InfoXMod, 1, "Warning: not a pre-crossed module." );
    fi; 
    SetIsPreXModWithObjects( PM, true ); 
    ok := IsXMod( PM ); 
    # name := Name( PM );
    return PM;
end );

#############################################################################
##
#M  IsXMod                   check that the second crossed module axiom holds
##
InstallMethod( IsXMod, "generic method for pre-crossed modules",
    true, [ IsPreXModWithObjects ], 0,
    function( XM )

    local  gensrc, genrng, x2, y2, a2, w2, z2, bdy, act;

    Info( InfoXMod, 2, "using IsXMod from gpd2obj.gi" ); 
    bdy := Boundary( XM );
    act := XModAction( XM );
    gensrc := Flat( GeneratorsOfGroupoid( Source( XM ) ) );
    genrng := GeneratorsOfGroupoid( Range( XM ) );
    for x2 in gensrc do
        for y2 in gensrc do
            Info( InfoXMod, 3, "x2,y2 = ", x2, ",  ", y2 ); 
            a2 := ImageElm( act, ImageElm( bdy, y2 ) ); 
            z2 := ImageElm( a2![1], x2 ); 
            w2 := ConjugateArrow( x2, y2 );
            Info( InfoXMod, 3, "w2,z2 = ", w2, ",  ", z2 ); 
            if ( z2 <> w2 ) then
                Info( InfoXMod, 2,
                      "CM2) fails at  x2 = ", x2, ",  y2 = ", y2, "\n",
                      "x2^(bdy(y2)) = ", z2, "\n","      x2^y2 = ", w2, "\n" );
                return false;
            fi;
        od;
    od;
    return true;
end );

##############################################################################
##
#M  DiscreteNormalPreXModWithObjects( <gpd>, <gp> ) .. make pre-crossed module
##
InstallMethod( DiscreteNormalPreXModWithObjects, 
    "for a single piece groupoid and a subgroup of the root group", true,
    [ IsSinglePiece, IsGroup ], 0,
function( R, gpS )

    local  gpR, obs, ro, nobs, S, ok, inc, AR, AS, idS, AS0, gengpR, gengpS, 
           ngengpR, genR, lenR, igpR, imact, c, i, g, x, imgengpS, h, head, 
           pos, imobs, rays, imhom, hom, action, P0; 

    gpR := R!.magma; 
    obs := R!.objects; 
    ro := obs[1]; 
    nobs := Length( obs ); 
    if not IsSubgroup( gpR, gpS ) then 
        Error( "gpS not a subgroup of the root group gpR of gpd" ); 
    fi; 
    S := DiscreteSubgroupoid( R, List( obs, o -> gpS ), obs ); 
    ok := IsHomogeneousDiscreteGroupoid( S );
    inc := InclusionMappingGroupoids( R, S ); 
    AR := AutomorphismGroupOfGroupoid( R ); 
    AS := AutomorphismGroupOfGroupoid( S ); 
    idS := One( AS ); 
    AS0 := DomainWithSingleObject( AS, 0 ); 
    gengpR := GeneratorsOfGroup( gpR ); 
    gengpS := GeneratorsOfGroup( gpS ); 
    ngengpR := Length( gengpR );
    genR := GeneratorsOfGroupoid( R ); 
    lenR := Length( genR ); 
    igpR := One( gpR ); 
    imact := ListWithIdenticalEntries( lenR, 0 ); 
    c := gengpR[1]; 
    for i in [1..lenR] do 
        g := genR[i]; 
        if ( g![2] = g![3] ) then 
            c := g![1]; 
            x := 0;
            imgengpS := List( gengpS, x -> x^c ); 
            h := GroupHomomorphismByImages( gpS, gpS, gengpS, imgengpS ); 
            imact[i] := GroupoidAutomorphismByGroupAutos( S,  
                            ListWithIdenticalEntries( nobs, h ) ); 
        elif ( g![1] = igpR ) then 
            head := g![3]; 
            pos := Position( obs, head ); 
            imobs := ShallowCopy( obs ); 
            imobs[1] := head; 
            imobs[pos] := ro; 
            imact[i] := GroupoidAutomorphismByObjectPerm( S, imobs ); 
        else 
            Error( "unexpected generating element in source of xmod" ); 
        fi; 
    od; 
    imobs := List( obs, o -> 0 ); 
    rays := List( obs, o -> idS ); 
    imhom := List( [1..ngengpR], j -> imact[j] ); 
    hom := GroupGeneralMappingByImagesNC( gpR, AS, gengpR, imhom); 
    SetIsMapping( hom, true ); 
    action := GroupoidHomomorphismFromSinglePiece( R, AS0, hom, imobs, rays ); 
    P0 := PreXModWithObjectsObj( R!.objects, inc, action); 
    return P0; 
end ); 

##############################################################################
##
#M  \=( <P>, <Q> )  . . . . . . . . . .  test if two 2d-groups over a groupoid
##
InstallMethod( \=, "generic method for two xmods over a groupoid",
    IsIdenticalObj, [ IsPreXModWithObjects, IsPreXModWithObjects ], 0,
    function ( P, Q ) 
    return ( ( ObjectList(P) = ObjectList(Q) ) 
             and ( Boundary(P) = Boundary(Q) )
             and ( XModAction(P) = XModAction(Q) ) );
end );

#############################################################################
##
#E  gpd2obj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
