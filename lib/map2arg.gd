##############################################################################
##
#W  map2arg.gd                   XMOD Package                    Chris Wensley
##
#Y  Copyright (C) 2001-2018, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 

#############################################################################
##                                         
#O  Mapping2ArgumentsByFunction( <S>, <R>, <fun> ) . create map from function
#P  IsMapping2ArgumentsByFunction( <map> ) 
#O  ImageElmMapping2ArgumentsByFunction( <map>, <pair> ) 
##              
DeclareOperation( "Mapping2ArgumentsByFunction", 
    [ IsList, IsDomain, IsFunction ] );
DeclareProperty( "IsMapping2ArgumentsByFunction", IsGeneralMapping );
DeclareOperation( "ImageElmMapping2ArgumentsByFunction", 
    [ IsMapping2ArgumentsByFunction, IsList ] );

#############################################################################
##
#E map2arg.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
