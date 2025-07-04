############################################################################# 
## 
#W  double.gd                  GAP4 package `XMod'              Chris Wensley 
##
#Y  Copyright (C) 2000-2025, Chris Wensley,  
##  
##  This file contains the declarations for double groupoids
##  

############################################################################# 
##  
#O  SinglePieceDoubleGroupoid( <gpd>, <pxmod> ) 
## 
DeclareOperation( "SinglePieceDoubleGroupoid", [ IsGroupoid, IsPreXMod ] ); 

############################################################################# 
##  
#O  EnhancedBasicDoubleGroupoid( <bdg> ) 
## 
DeclareOperation( "EnhancedBasicDoubleGroupoid", [ IsBasicDoubleGroupoid ] ); 

############################################################################# 
##  
#O  DoubleGroupoidWithZeroBoundary( <gpd>, <src> ) 
## 
DeclareOperation( "DoubleGroupoidWithZeroBoundary", 
    [ IsGroupoid, IsGroup ] ); 

############################################################################# 
##  
#P  IsDoubleGroupoidWithPreXMod( <dg> ) 
## 
DeclareProperty( "IsDoubleGroupoidWithPreXMod", IsDoubleGroupoid ); 

############################################################################
##
#E double.gd . . . . . . . . . . . . . . . . . . . . . . . . . . . ends here
##  
