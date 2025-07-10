##############################################################################
##
#W  apps.gd                    GAP4 package `XMod'               Chris Wensley
##
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

##############################################################################
##
#O  LoopClasses( <X0> )
#O  LoopsXMod( <X0>, <a> )
#O  AllLoopsXMod( <X0> ) 
##  
DeclareOperation( "LoopClasses", [ IsPreXMod ] );
DeclareOperation( "LoopClass", [ IsPreXMod, IsObject ] );
DeclareOperation( "LoopClassOld", [ IsPreXMod, IsObject ] );
DeclareOperation( "LoopClassesNew", [ IsPreXMod ] );
DeclareOperation( "LoopClassRepresentatives", [ IsPreXMod ] );
DeclareOperation( "LoopsXMod", [ IsPreXMod, IsObject ] );
DeclareOperation( "AllLoopsXMod", [ IsPreXMod ] );
