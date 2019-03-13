##############################################################################
##
#W  map2arg.gi                   XMOD Package                    Chris Wensley
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##
#F  Mapping2ArgumentsByFunction( <D>, <E>, <fun> ) . create map from function
##
InstallMethod( Mapping2ArgumentsByFunction, "for a list, domain, and function", 
    true, [ IsList, IsDomain, IsFunction ], 0, 
function ( L, R, f )

    local dp, p1, p2, map; 

    # ensure that the entries in source and the range are domains
    if not ( Length(L)=2 and IsDomain(L[1]) and IsDomain(L[2]) ) then
	 Error( "the list L does not contain 2 domains" );
    fi;
    dp := DirectProduct( L[1], L[2] ); 
    p1 := Projection( dp, 1 );
    p2 := Projection( dp, 2 );
    map := MappingByFunction( dp, R, 
               x -> f( [ ImageElm( p1, x ), ImageElm( p2, x ) ] ) );
    SetIsMapping2ArgumentsByFunction( map, true );
    return map; 
end );

#############################################################################
##
#M  ImageElmMapping2ArgumentsByFunction( <map>, <elm> ) 
##
InstallMethod( ImageElmMapping2ArgumentsByFunction, "for mapping of 2 args", 
    true, [ IsMapping2ArgumentsByFunction, IsList ], 0,
function ( map, elm )

    local dp, x; 

    dp := Source( map );
    x := ImageElm( Embedding( dp, 1 ), elm[1] ) 
         * ImageElm( Embedding( dp, 2 ), elm[2] ); 
    return ImageElm( map, x );
end );
