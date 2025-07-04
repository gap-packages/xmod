##############################################################################
##
#W  util.gd                    GAP4 package `XMod'               Chris Wensley
#W                                                                 & Murat Alp
#Y  Copyright (C) 2001-2020, Chris Wensley et al,  

##############################################################################
##
#O  InnerAutomorphismsByNormalSubgroup( <G>, <N> )
##  
DeclareOperation( "InnerAutomorphismsByNormalSubgroup", [ IsGroup, IsGroup ] );

##############################################################################
##
#O  TrivialAction( <G>, <H> ) 
##
DeclareOperation( "TrivialAction", [ IsGroup, IsGroup ] );

##############################################################################
##
#R  IsAbelianModuleObj( <obj> )
#V  IsAbelianModuleFamily 
#T  IsAbelianModuleType 
#P  IsAbelianModule( <obj> )
#A  AbelianModuleGroup( <obj> )
#A  AbelianModuleAction( <obj> )
#O  AbelianModuleObject( <abgrp>, <act> )
#O  AbelianModuleWithTrivialAction( <abgrp>, <grp> )
##
DeclareRepresentation( "IsAbelianModuleObj", 
  IsObject and IsAttributeStoringRep,
    [ "AbelianModuleGroup", "AbelianModuleAction" ] );
DeclareProperty( "IsAbelianModule", IsObject ); 
BindGlobal( "IsAbelianModuleFamily", 
            NewFamily( "IsAbelianModuleFamily", 
            IsGroup, IsGroupHomomorphism ) ); 
BindGlobal( "IsAbelianModuleType", 
            NewType( IsAbelianModuleFamily, IsAbelianModuleObj ) ); 
DeclareAttribute( "AbelianModuleGroup", IsAbelianModule );
DeclareAttribute( "AbelianModuleAction", IsAbelianModule );
DeclareOperation( "AbelianModuleObject", [ IsGroup, IsGroupHomomorphism ] ); 
DeclareOperation( "AbelianModuleWithTrivialAction", [ IsGroup, IsGroup ] );

#############################################################################
##
#A  AutomorphismClass( <G> )
##
DeclareAttribute( "AutomorphismClass", IsGroup );

##############################################################################
##
#A  GenerationOrder         elements of G generated as words in the generators
#A  GenerationPairs         elements of G generated as words in the generators
#O  CheckGenerationPairs             G.generationPairs, G.generationOrder ok ?
##
DeclareAttribute( "GenerationOrder", IsGroup );
DeclareAttribute( "GenerationPairs", IsGroup );
DeclareOperation( "CheckGenerationPairs", [ IsGroup ] );

####################### items added May 2002 ################################

#############################################################################
##
#F  TzCommutatorPair( <tietze record>, <rel> )
#F  TzPartition( <tietze record> )
#F  FactorsPresentation( <tietze record> )
##
DeclareGlobalFunction( "TzCommutatorPair" );
DeclareGlobalFunction( "TzPartition" );
DeclareGlobalFunction( "FactorsPresentation" );

#############################################################################
##
#A  IsomorphismFpInfo( <G> )
#A  IsomorphismPermInfo( <G> )
#A  IsomorphismPcInfo( <G> )
#A  IsomorphismPermOrPcInfo( <G> )
##
DeclareAttribute( "IsomorphismFpInfo", IsGroup, "mutable" );
DeclareAttribute( "IsomorphismPermInfo", IsGroup, "mutable" );
DeclareAttribute( "IsomorphismPcInfo", IsGroup, "mutable" );
DeclareAttribute( "IsomorphismPermOrPcInfo", IsGroup, "mutable" );

#############################################################################
##
#F  IsomorphismPermObject( <obj> )
#F  IsomorphismFpObject( <obj> )
#F  IsomorphismPcObject( <obj> )
#F  IsomorphismPermOrPcObject( <obj> )
##
DeclareGlobalFunction( "IsomorphismPermObject" );
DeclareGlobalFunction( "IsomorphismFpObject" );
DeclareGlobalFunction( "IsomorphismPcObject" );
DeclareGlobalFunction( "IsomorphismPermOrPcObject" );

#############################################################################
##
#F  RegularActionHomomorphismObject( <obj> )
##
DeclareGlobalFunction( "RegularActionHomomorphismObject" );

#############################################################################
##
#P  IsSubEndoMapping( <map> )
##
DeclareProperty( "IsSubEndoMapping", IsGeneralMapping );

#############################################################################
##
#O  MetacyclicGroup( <m>, <n>, <l> )
##
DeclareOperation( "MetacyclicGroup", [ IsPosInt, IsPosInt, IsPosInt ] );

#############################################################################
##
#O  AutomorphismsFixingSubgroups( G, H ) 
##
DeclareOperation( "AutomorphismsFixingSubgroups", [ IsGroup, IsList ] );
