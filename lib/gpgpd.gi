#############################################################################
##
#W  gpgpd.gi                   GAP4 package `XMod'              Chris Wensley
##
#Y  Copyright (C) 2001-2024, Chris Wensley et al,  

#############################################################################
##
#M  GroupGroupoidElement( <cat1> <obj> <obj> ) . . . for a cat1-group element
##
InstallMethod( GroupGroupoidElement, "for a precat1-group and an element",
    true, [ IsPreCat1Group, IsObject, IsObject ], 0,
function( C, r, e )

    local te, he, tid, hid, elt;

    te := ImageElm( TailMap( C ), e );  
    he := ImageElm( HeadMap( C ), e );
    tid := ImageElm( RangeEmbedding( C ), te ); 
    hid := ImageElm( RangeEmbedding( C ), he ); 
    elt := rec( precat1 := C, root := r, element := e, 
                tail := te, tailid := tid, head := he, headid := hid );
    ObjectifyWithAttributes( elt, IsGroupGroupoidElementType, 
        IsGroupGroupoidElement, true ); 
    return elt;
end );

#############################################################################
##
#M  PrintObj( <e> ) . . . . . . . . . . . . . . for a group-groupoid element
##
InstallMethod( PrintObj, "for a group-groupoid element", true, 
    [ IsGroupGroupoidElement ], 0,
function( e )
    Print( e!.tail, ">-", e!.element, "->", e!.head ); 
end ); 

#############################################################################
##
#M  \*( <e1> <e2> ) . . . . . . . . . . . . . for two group-groupoid elements
##
InstallMethod( \*, "for two group-groupoid elements", true, 
    [ IsGroupGroupoidElement, IsGroupGroupoidElement ], 0,
function( e1, e2 )

    local id, e;

    if ( e1!.precat1 <> e2!.precat1 ) or ( e1!.head <> e2!.tail ) then 
        return fail; 
    fi; 
    id := e1!.headid; 
    e := e1!.element * id^(-1) * e2!.element; 
    return GroupGroupoidElement( e1!.precat1, e1!.root, e ); 
end );

#############################################################################
##
#M  \=( <e1> <e2> ) . . . . . . . . . . . . . for two group-groupoid elements
##
InstallMethod( \=, "for two group-groupoid elements", true, 
    [ IsGroupGroupoidElement, IsGroupGroupoidElement ], 0,
function( e1, e2 )
    return ( e1!.precat1 = e2!.precat1 ) and ( e1!.element = e2!.element ); 
end );

#############################################################################
##
#M  \<( <e1> <e2> ) . . . . . . . . . . . . . for two group-groupoid elements
##
InstallMethod( \<, "for two group-groupoid elements", true, 
    [ IsGroupGroupoidElement, IsGroupGroupoidElement ], 0,
function( e1, e2 )
    return ( e1!.precat1 = e2!.precat1 ) and ( e1!.element < e2!.element ); 
end );

#############################################################################
##
#M  OneOp( <e> ) . . . . . . . . . . . . . . . . for a group-groupoid element
##
InstallMethod( OneOp, "for a group-groupoid element", true, 
    [ IsGroupGroupoidElement ], 0,
function( e ) 
    local C, r, g; 
    C := e!.precat1; 
    r := e!.root;
    g := ImageElm( RangeEmbedding( C ), r );
    return GroupGroupoidElement( C, e!.root, g );   
end );

#############################################################################
##
#M  InverseOp( <e> ) . . . . . . . . . . . . . . for a group-groupoid element
##
InstallMethod( InverseOp, "for a group-groupoid element", true, 
    [ IsGroupGroupoidElement ], 0,
function( e ) 
    local ig; 
    ig := e!.headid * e!.element^(-1) * e!.tailid; 
    return GroupGroupoidElement( e!.precat1, e!.root, ig );   
end );

#############################################################################
##
#M  GroupGroupoid( <cat1> ) . . . . . . . . . . . . . . for a pre-cat1-group 
##
InstallMethod( GroupGroupoid, "for a precat1-group", true, 
    [ IsPreCat1Group ], 0,
function( C )

    local R, G, t, h, e, kert, kerh, kerth, genkerth, nloops, bdy, imbdy, 
          cosbdy, repbdy, np, eR, pieces, obs, root, eroot, loopgp, cosgp, 
          repsgp, L1, rays1, size, i, rays, gpd;

    R := Range( C ); 
    G := Source( C );
    t := TailMap( C ); 
    h := HeadMap( C ); 
    e := RangeEmbedding( C ); 
    kert := Kernel( t ); 
    kerh := Kernel( h ); 
    kerth := Intersection( kert, kerh );
    genkerth := GeneratorsOfGroup( kerth ); 
    if ( genkerth = [ ] ) then 
        genkerth := [ One( G ) ]; 
    fi; 
    Info( InfoXMod, 1, "genkerth: ", genkerth );
    nloops := Size( kerth ); 
    bdy := RestrictedMapping( h, kert ); 
    imbdy := Image( bdy );
    cosbdy := RightCosets( R, imbdy ); 
    repbdy := List( cosbdy, Representative ); 
    np := Length( repbdy );
    Info( InfoXMod, 1, "repbdy = ", repbdy ); 
    eR := Image( e, R );
    pieces := ListWithIdenticalEntries( np, 0 ); 
    Info( InfoXMod, 1, "-------- constructing the first piece --------" );  
    root := One( R ); 
    eroot := One( G ); 
    loopgp := List( genkerth, g -> GroupGroupoidElement( C, root, g ) ); 
    Info( InfoXMod, 1, "loopgp generators: ", loopgp ); 
    loopgp := GroupWithGenerators( loopgp );
    cosgp := RightCosets( kert, kerth ); 
    repsgp := List( cosgp, Representative ); 
    L1 := List( repsgp, g -> g*eroot ); 
    obs := List( repsgp, g -> ImageElm( h, g ) );
    rays1 := List( L1, g -> GroupGroupoidElement( C, root, g ) ); 
    Info( InfoXMod, 1, "root = ", root, ",  obs = ", obs ); 
    pieces[1] := SinglePieceGroupoidWithRays( loopgp, obs, rays1 ); 
    size := Size( pieces[1] );
    ## now for the remaining pieces 
    for i in [2..np] do 
        Info( InfoXMod, 1, "-------- constructing piece ", i, " --------" );  
        obs := Elements( cosbdy[i] ); 
        Info( InfoXMod, 1, "obs = ", obs );
        root := repbdy[i]; 
        eroot := ImageElm( e, root ); 
        Info( InfoXMod, 1, "root = ", root, ",  eroot = ", eroot ); 
        loopgp := List( genkerth, g -> g*eroot ); 
        Info( InfoXMod, 1, "(1) loopgp generators: ", loopgp ); 
        loopgp := List( loopgp, g -> GroupGroupoidElement( C, root, g ) );
        Info( InfoXMod, 1, "(2) loopgp = ", loopgp );
        loopgp := GroupWithGenerators( loopgp );
        Info( InfoXMod, 1, "(3) loopgp = ", loopgp );
        rays := List( rays1, r -> r!.element*eroot ); 
        Info( InfoXMod, 1, "(4) rays = ", rays ); 
        obs := List( rays, g -> ImageElm( h, g ) );
        rays := List( rays, r -> GroupGroupoidElement( C, root, r ) ); 
        Info( InfoXMod, 1, "(5) rays = ", rays ); 
        pieces[i] := SinglePieceGroupoidWithRays( loopgp, obs, rays ); 
    od;
    gpd := UnionOfPieces( pieces ); 
    SetSize( gpd, size * np ); 
    return gpd; 
end );