##############################################################################
##
#W  gpnobj.gi                   GAP4 package `XMod'              Chris Wensley
##                                                                Alper Odabas
##  This file implements generic methods for (pre-)catn-groups.
##
#Y  Copyright (C) 2001-2017, Chris Wensley et al,  
#Y  School of Computer Science, Bangor University, U.K. 


#############################################################################
##
#M  IsPreCatnGroup . . . . . . . . . . . . . . . . .  check that the is pre-catn-group
##
InstallMethod( IsPreCatnGroup, "generic method for a pre-catn-group",
    true, [ IsndGroup ], 0,
function( P )

    local  G, gensrc, C, x, y, z, L, PL, n, i, j, tmaps, hmaps, end_tmaps, end_hmaps, h, t;

    if not ( IsPreCatnObj ( P )  ) then
        return false;
    fi;
	
    L := 2dGroups( P );
    n := PreCatnDimension( P );
	PL := [];
		for i in [1..n] do 
			if not ( IsPerm2dGroup( L[i] ) ) then
				C := Image(IsomorphismPermObject( L[i] ) );
			else
				C := L[i];
			fi;
			Add(PL,C,i);
        od;
		
	tmaps := [];
	hmaps := [];
		for i in [1..n] do 
			Add(tmaps,TailMap(PL[i]),i);
			Add(hmaps,HeadMap(PL[i]),i);
        od;
		
	
	# --- Actually not necessary
	
	for i in [1..n] do 
			if ( Source( tmaps[i] ) <> Source( hmaps[i] ) ) then
			        Info( InfoXMod, 2, "Incompatible source" );
					return false;	
			fi;
			if ( Range( tmaps[i] ) <> Range( hmaps[i] ) ) then
			        Info( InfoXMod, 2, "Incompatible range" );
					return false;	
			fi;
     od;
	 
	# -------------------------

	G := Source(hmaps[1]);
	gensrc := GeneratorsOfGroup(G);
	
	if not ForAll(tmaps, x -> Source(x) = G ) then
	    Info( InfoXMod, 2, "Incompatible Tail maps source" );
        return false;		
	fi;
	
	if not ForAll(hmaps, x -> Source(x) = G ) then
	    Info( InfoXMod, 2, "Incompatible Head maps source" );
        return false;		
	fi;
	
	end_tmaps := [];
	end_hmaps := [];
	
		for i in [1..n] do 
		t := GroupHomomorphismByImagesNC(G, G, gensrc, 
                  List(gensrc, x -> Image( tmaps[i], x ) )  ); 
				  
		h := GroupHomomorphismByImagesNC(G, G, gensrc, 
                  List(gensrc, x -> Image( hmaps[i], x ) )  ); 		
				  
			Add(end_tmaps,t,i);
			
			Add(end_hmaps,h,i);
        od;
	
	# check the condition 1 / 2
			
	for i in [1..n-1] do 
		for j in [i+1..n] do
			if not ( end_hmaps[i] *  end_hmaps[j] = end_hmaps[j] * end_hmaps[i]  ) then
			Info( InfoXMod, 2, "Condition 1 is not provided" );
			#	Print("Condition 1 is not provided \n");
			return false;
			fi;		

			if not ( end_tmaps[i] *  end_tmaps[j] = end_tmaps[j] * end_tmaps[i]  ) then
			Info( InfoXMod, 2, "Condition 2 is not provided" );
			#	Print("Condition 2 is not provided \n");
			return false;
			fi;	
		od;
	od;
	
	# check the condition 3
			
	for i in [1..n] do 
		for j in [1..n] do
			if i = j then 
				continue;
			fi;
			if not ( end_hmaps[i] *  end_tmaps[j] = end_tmaps[j] * end_hmaps[i]  ) then
			Info( InfoXMod, 2, "Condition 3 is not provided" );
			#	Print("Condition 3 is not provided \n");
			return false;
			fi;		
		od;
	od;

        return true;
end );

#############################################################################
##
#M  IsCatnGroup . . . . . . . . . . . . . . . . .  check that the object is a catn-group
##
InstallMethod( IsCatnGroup, "generic method for a catn-group",
    true, [ IsndGroup ], 0,
function( P )

    local  L;

    if not ( IsPreCatnGroup(P) ) then
        Info( InfoXMod, 2, "Pre-Catn-Group is not provided" );
        return false;
    fi;
	
	L := 2dGroups( P );
	
	if ForAny(L, x -> not IsCat1(x) ) then 
        Info( InfoXMod, 2, "Each item in the list must be Cat1-Group" );
        return false;
	fi;
	
    return true;
end );

##############################################################################
##
#M  PreCatnObj ( <up>, <down> ) . .					make a PreCatn Object
##
InstallMethod( PreCatnObj, "for precatn", true,
    [ IsList ], 0,
function( L )

    local  filter, fam, PC, ok, name, n;
	
	if ForAny(L, x -> not IsPreCat1Obj(x) ) then 
		Print("Each item in the list must be PreCat1-group \n");
		return fail;
	fi;
	n := Length(L);
    fam := FamilyndGroup;
    filter := IsPreCatnObj;
    PC := rec();
    ObjectifyWithAttributes( PC, NewType( fam, filter), 
        2dGroups, L, 
        PreCatnDimension, n, 
        IsndGroup, true );
    if not IsPreCatnGroup( PC ) then
        Info( InfoXMod, 1, "Warning: not a pre-catn-group" );
    fi;
    # ok := IsCatnGroup( PC );
    # name:= Name( PC );
    return PC;
end );

#############################################################################
##
#F  CatnGroup( <size>, <gpnum>, <dim>, <num> )     catn-group from data in CATn_LIST
#F  CatnGroup( C1G1, C1G2, ... )               	   catn-group from cat1-groups
##
InstallGlobalFunction( CatnGroup, function( arg )

    local  nargs, CnG, ok, usage1, usage2;

    nargs := Length( arg );
	
	usage1 := "standard usage: CatnGroup( [pre_cat1-group, pre_cat1-group, ... ] );\n" ;
    usage2 := "            or: CatnGroup( size, gpnum, dimension, num );\n" ;
    if (  ( nargs = 1 ) and ( IsList(arg[1]) ) ) then
	
		if ForAny(arg[1], x -> not IsPreCat1Obj(x) ) then 
		Print("Each item in the list must be Pre-Cat1-group \n");
		return fail;
		elif ( not (Length(arg[1]) > 1) ) then
		        Print( usage1 );
				Print( usage2 );
		else
			CnG := PreCatnObj( arg[1] );
			ok := IsPreCatnGroup( CnG );
			if ok then
				return CnG;
			else
				return fail;
			fi;				
		fi;        
	elif ( (nargs = 4) and IsInt( arg[1] ) and IsInt( arg[2] ) and  IsInt( arg[3] ) and IsInt( arg[4] )  ) then
		Print( "CatnSelect is not yet implemented\n" );
        # return CatnSelect( arg[1], arg[2], arg[3], arg[4]  );
    else   
        Print( usage1 );
        Print( usage2 );
    fi;
end );

#############################################################################
##
#M  PrintObj( <gnd> . . . . . . . . . . . . . . . . . . . . print a nd-group 
#M  ViewObj( <gnd> ) . . . . . . . . . . . . . . . . . . . . view a nd-group 
##
InstallMethod( PrintObj, "method for a nd-group", true, [ IsndGroup ], 0,
function( gnd )

    local  i, n;

    if HasName( gnd ) then
        Print( Name( gnd ), "\n" );
    else 
		n := PreCatnDimension(gnd);
		Print( "\n" );
		for i in [1..n] do 
            Print( "\t ", "2dDomain-",i ," = ",  2dGroups( gnd )[i], "\n" );
        od;
              
    fi;
end );

InstallMethod( ViewObj, "method for a nd-group", true, [ IsndGroup ], 0,
    PrintObj ); 

#############################################################################
##
#M Display( <gnd> . . . . . . . . . . . . . . . . . . . . display a nd-group 
##
InstallMethod( Display, "method for a nd-group", true, [ IsndGroup ], 0,
function( gnd )

    local  i, n;

	n := PreCatnDimension(gnd);
    if IsPreCatnGroup(gnd) then 
        Print( "(pre-)cat",n,"-group with:\n" ); 

    fi; 	
    if HasName( gnd ) then
        Print( Name( gnd ), "\n" );
    else 
		
		Print( "\n" );
		for i in [1..n] do 
            Print( "\t ", "2dDomain-",i ," = ",  2dGroups( gnd )[i], "\n" );
        od;
              
    fi;
end );

#############################################################################
##
#E gpnobj.gi . . . . . . . . . . . . . . . . . . . . . . . . . . .  ends here
