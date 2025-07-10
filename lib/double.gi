############################################################################# 
## 
#W  double.gi                   GAP4 package `XMod'             Chris Wensley 
##
#Y  Copyright (C) 2000-2025, Chris Wensley,  
##  
##  This file contains the implementations for double groupoids 
##  

###################  DOUBLE DOMAIN WITH OBJECTS   ########################## 

InstallMethod( SinglePieceDoubleGroupoid, 
    "for groupoid and precrossed module", true, [ IsGroupoid, IsPreXMod ], 0, 
function( gpd, px ) 

    local dgpd; 

    dgpd := rec( groupoid := gpd, prexmod := px, 
                 objects := ObjectList( gpd ) ); 
    ObjectifyWithAttributes( dgpd, IsDoubleGroupoidType, 
        IsSinglePiece, true, 
        IsAssociative, true, 
        IsCommutative, IsCommutative( gpd!.magma ) 
                       and IsCommutative( Source( px ) ),
        IsDoubleGroupoidWithPreXMod, true ); 
    return dgpd; 
end ); 

InstallMethod( EnhancedBasicDoubleGroupoid, "for a basic double groupoid", 
    true, [ IsBasicDoubleGroupoid ], 0, 
function( bdg ) 

    local gpd, obs, gp, px, dgpd; 

    if not IsSinglePiece( bdg ) then 
        Error( "only working with single pieces so far" ); 
    fi; 
    gpd := bdg!.groupoid; 
    obs := bdg!.objects; 
    gp := RootGroup( gpd ); 
    px := XModByNormalSubgroup( gp, gp ); 
    dgpd := SinglePieceDoubleGroupoid( gpd, px );
    return dgpd; 
end ); 

InstallMethod( DoubleGroupoidWithZeroBoundary, "for a groupoid and a group", 
    true, [ IsGroupoid, IsGroup ], 0, 
function( gpd, src ) 

    local gp, px, dgpd; 

    if not IsSinglePiece( gpd ) then 
        Error( "only working with single pieces so far" ); 
    fi; 
    gp := RootGroup( gpd ); 
    px := PreXModWithTrivialRange( src, gp ); 
    dgpd := SinglePieceDoubleGroupoid( gpd, px );
    return dgpd; 
end ); 

InstallMethod( ViewObj, "for a double groupoid with prexmod", true, 
    [ IsDoubleGroupoidWithPreXMod and IsSinglePiece ], 0,   
function( dgpd )
    local pxm, isxmod;
    pxm := dgpd!.prexmod;
    isxmod := HasIsXMod( pxm ) and IsXMod( pxm );
    Print( "single piece double groupoid with:\n" );
    if isxmod then
        Print( "     xmod = ", pxm, "\n" ); 
    else
        Print( "  prexmod = ", pxm, "\n" );
    fi;
    Print( " groupoid = ", dgpd!.groupoid, "\n" ); 
    Print( "    group = ", dgpd!.groupoid!.magma, "\n" ); 
    Print( "  objects = ", dgpd!.objects ); 
end );

InstallMethod( PrintObj, "for a double groupoid with prexmod", true, 
    [ IsDoubleGroupoidWithPreXMod and IsSinglePiece ], 0, 
function( dgpd )
    local pxm, isxmod;
    pxm := dgpd!.prexmod;
    isxmod := HasIsXMod( pxm ) and IsXMod( pxm );
    Print( "single piece double groupoid with:\n" );
    if isxmod then
        Print( "     xmod = ", pxm, "\n" ); 
    else
        Print( "  prexmod = ", pxm, "\n" );
    fi;
    Print( " groupoid = ", dgpd!.groupoid, "\n" ); 
    Print( "    group = ", dgpd!.groupoid!.magma, "\n" ); 
    Print( "  objects = ", dgpd!.objects, "\n" ); 
end );

InstallMethod( SquareOfArrows, 
    "for double groupoid with objects, element, up, left, right and down", 
    true, [ IsDoubleGroupoid, IsMultiplicativeElement, 
            IsObject, IsObject, IsObject, IsObject ], 0, 
function( dgpd, e, u, l, r, d ) 

    local  gpd, px, bdy, ime, ok, piece, loop, sq; 

    gpd := dgpd!.groupoid; 
    px := dgpd!.prexmod; 
    bdy := Boundary( px ); 
    ime := ImageElm( bdy, e ); 
    ok := ForAll( [u,l,r,d], x -> x in gpd ) 
             and ( TailOfArrow( u ) = TailOfArrow( l ) ) 
             and ( HeadOfArrow( u ) = TailOfArrow( r ) ) 
             and ( HeadOfArrow( l ) = TailOfArrow( d ) ) 
             and ( HeadOfArrow( r ) = HeadOfArrow( d ) ); 
    if not ok then 
        Info( InfoXMod, 1, "the four arrows do not form a square" ); 
        return fail; 
    else 
        if IsSinglePiece( gpd ) then 
            piece := dgpd; 
        else 
            piece := PieceOfObject( gpd, TailOfArrow( d ) ); 
        fi; 
        loop := d^-1 * l^-1 * u * r; 
        ok := ( ime = loop![2] );
        if not ok then 
            Info( InfoXMod, 1, "element ", e, " has image ", ime, 
                               " <> boundary element ", loop![2] );
            return fail; 
        fi; 
        sq := SquareOfArrowsNC( dgpd, e, u, l, r, d );
        return sq; 
    fi; 
end );

############################################################################# 
## 
#M  VerticalProduct( s1, s2 ) 
##  . . . . . . . vertical composition of squares in a basic double groupoid 
## 
InstallMethod( VerticalProduct, "for two squares in a basic double groupoid", 
    true, [ IsDoubleGroupoidElement, IsDoubleGroupoidElement], 0, 
function( s1, s2 ) 

    local dgpd, m, px, act, aut; 

    ## elements are composable? 
    if not ( s1![6] = s2![3] ) then 
        Info( InfoXMod, 1, "down arrow of s1 <> up arrow of s2" ); 
        return fail; 
    fi; 
    if not ( s1![1] = s2![1] ) then 
        Info( InfoXMod, 1, "s1 and s2 are in different double groupoids" ); 
        return fail; 
    fi;
    dgpd := s1![1];
    px := dgpd!.prexmod; 
    act := XModAction( px ); 
    aut := ImageElm( act, s2![5]![2] ); 
    m := s2![2] * ImageElm( aut, s1![2] ); 
    return SquareOfArrowsNC( dgpd, m, s1![3], s1![4]*s2![4], 
                                      s1![5]*s2![5], s2![6] ); 
end );

############################################################################# 
## 
#M  HorizontalProduct( s1, s2 ) 
##      . . . . horizontal composition of squares in a basic double groupoid 
## 
InstallMethod( HorizontalProduct, 
    "for two squares in a basic double groupouid", true, 
    [ IsDoubleGroupoidElement, IsDoubleGroupoidElement], 0, 
function( s1, s2 ) 

    local dgpd, m, px, act, aut;

    ## elements are composable? 
    if not ( s1![5] = s2![4] ) then 
        Info( InfoXMod, 1, "right arrow of s1 <> left arrow of s2" ); 
        return fail; 
    fi; 
    if not ( s1![1] = s2![1] ) then 
        Info( InfoXMod, 1, "s1 and s2 are in different double groupoids" ); 
        return fail; 
    fi;
    dgpd := s1![1];
    px := dgpd!.prexmod; 
    act := XModAction( px ); 
    aut := ImageElm( act, s2![6]![2] ); 
    m := ImageElm( aut, s1![2] ) * s2![2];
    return SquareOfArrowsNC( dgpd, m, s1![3]*s2![3], s1![4], 
                                      s2![5], s1![6]*s2![6] ); 
end );

############################################################################
##
#E double.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
