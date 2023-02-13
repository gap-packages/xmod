############################################################################# 
## 
#W  double.gi                   GAP4 package `XMod'             Chris Wensley 
##
#Y  Copyright (C) 2000-2023, Chris Wensley,  
#Y  School of Computer Science, Bangor University, U.K. 
##  
##  This file contains the implementations for double groupoids 
##  

###################  DOUBLE DOMAIN WITH OBJECTS   ########################## 

InstallMethod( SinglePieceDoubleGroupoid, "for a groupoid and a precrossed module", true, 
    [ IsGroupoid, IsPreXMod ], 0, 
function( gpd, px ) 

    local dgpd; 

    dgpd := rec( groupoid := gpd, prexmod := px, 
                 objects := ObjectList( gpd ) ); 
    ObjectifyWithAttributes( dgpd, IsDoubleGroupoidType, 
        IsSinglePiece, true, 
        IsAssociative, true, 
        IsCommutative, IsCommutative( gpd!.magma ) 
                       and IsCommutative( Source( px ) ) ); 
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

InstallMethod( DoubleGroupoidWithZeroBoundary, "for a groupoid zand a group", 
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
        Info( InfoGroupoids, 1, "the four arrows do not form a square" ); 
        return fail; 
    else 
        if IsSinglePiece( gpd ) then 
            piece := dgpd; 
        else 
            piece := PieceOfObject( gpd, TailOfArrow( d ) ); 
        fi; 
        loop := d^-1 * l^-1 * u * r; 
        ok := ( ime = loop![1] ); 
        if not ok then 
Print( "[ime,loop![1]] = ", [ime,loop![1]], "\n" ); 
            Info( InfoGroupoids, 2, "here" ); 
            Info( InfoGroupoids, 1, "element ", e, " has image ", ime, 
                               " <> boundary element ", loop![1] ); 
            return fail; 
        fi; 
        sq := SquareOfArrowsNC( e, u, l, r, d ); 
        SetBoundaryOfSquare( sq, e ); 
        return sq; 
    fi; 
end );

############################################################################# 
## 
#M  UpDownProduct( dgpd, s1, s2 ) 
##  . . . . . . . vertical composition of squares in a basic double groupoid 
## 
InstallMethod( UpDownProduct, "for two squares in a basic double groupoid", 
    true, [ IsDoubleGroupoid, IsDoubleGroupoidElement, 
            IsDoubleGroupoidElement], 0, 
function( dgpd, s1, s2 ) 

    local m, px, act, aut; 

    ## elements are composable? 
    if not ( ( s1![5] = s2![2] ) and 
             ( FamilyObj( s1![1] ) = FamilyObj( s2![1] ) ) ) then 
        Info( InfoGroupoids, 1, "down arrow of s1 <> up arrow of s2" ); 
        return fail; 
    fi; 
    px := dgpd!.prexmod; 
    act := XModAction( px ); 
    aut := ImageElm( act, s2![4]![1] ); 
    m := s2![1] * ImageElm( aut, s1![1] ); 
    return SquareOfArrowsNC( m, s1![2], s1![3]*s2![3], 
                                s1![4]*s2![4], s2![5] ); 
end );

############################################################################# 
## 
#M  LeftRightProduct( dgpd, s1, s2 ) 
##      . . . . horizantal composition of squares in a basic double groupoid 
## 
InstallMethod( LeftRightProduct, 
    "for two squares in a basic double groupouid", true, 
    [ IsDoubleGroupoid, IsDoubleGroupoidElement, 
      IsDoubleGroupoidElement], 0, 
function( dgpd, s1, s2 ) 

    local m, px, act, aut;

    ## elements are composable? 
    if not ( ( s1![4] = s2![3] ) and 
             ( FamilyObj( s1![1] ) = FamilyObj( s2![1] ) ) ) then 
        Info( InfoGroupoids, 1, "right arrow of s1 <> left arrow of s2" ); 
        return fail; 
    fi; 
    px := dgpd!.prexmod; 
    act := XModAction( px ); 
    aut := ImageElm( act, s2![5]![1] ); 
    m := ImageElm( aut, s1![1] ) * s2![1];
    return SquareOfArrowsNC( m, s1![2]*s2![2], s1![3], 
                                s2![4], s1![5]*s2![5] ); 
end );


############################################################################
##
#E double.gi . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
