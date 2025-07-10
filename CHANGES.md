## CHANGES to the 'XMod' package

## 2.94 -> 2.95 (10/07/2025)
 * (10/07/25) added PreXModWithObjectsByBoundaryAndAction
 * (30/06/25) make the new groupoids 1.77 a prerequisite and revise material
              on double groupoids to agree with the changes in groupoids

## 2.93 -> 2.94  (08/07/25)
 * (08/07/25) temporary fix to work with the recently released groupoids 1.77

## 2.92 -> 2.93 (27/04/2025)
 * (27/04/25) added sub-crossed square and sub-cat2-group operations
 * (25/04/25) simplified the construction of crossed pairings so that 
              files map2arg.g{d,i} can be removed
 * (20/12/24) changed the examples of induced crossed modules in section 7.2
 * (17/12/24) implemented (Inner)ActorCat1Group, fixing issue #144
 * (20/11/24) added AutomorphismPermGroup method for cat1-groups
              renamed PermAutomorphismAsXModMorphism as
              PermAutomorphismAs2dGroupMorphism and added cat1-group method
              added WhiteheadRegularGroup and WhiteheadGroupIsomorphism
              removed the unused CompositeDerivation (= WhiteheadProduct)
              removed the unused (List)InverseDerivations
              renamed WhiteheadGroupGeneratingDerivations as ...UpMappings
              added WhiteheadGroupInverseIsomorphism
              fixed error in CrossedPairingByDerivations

## 2.91 -> 2.92 (23/01/2024)
 * (11/10/23) methods for WhiteheadXMod etc for xmods which are not perm
 * (16/02/23) now using the NC version of PreImagesRepresentative 

## 2.89 -> 2.91 (16/02/2023) 
 * (16/02/23) revised tst/extra/d24.tst so that it works with option -A 
 * (09/02/23) added a chapter on double groupoids with operations 
              EnhancedBasicDoubleGroupoid, DoubleGroupoidWithZeroBoundary
 * (05/02/23) added PreXModWithTrivialRange 

## 2.88 -> 2.89 (04/02/2023) 
 * (01/02/23) added RegularActionHomomorphism(Object/2DimensionalGroup) 
 * (23/12/22) changed email address and other personal details 
 * (24/08/22) manual: documented DerivationImage; 
              corrected CrossDiagonalActions

## 2.86 -> 2.88 (28/04/2022) 
 * (27/04/22) introduced Size2d for 2d-objects and Size3d for 3d-objects 
              added the Arvasi/Odabas/Wensley paper to the list of references

## 2.85 -> 2.86 (14/03/2022) 
 * (14/03/22) moved d24.tst to tst/failing temporarily 

## 2.84 -> 2.85 (12/03/2022) 
 * (12/03/22) commented out some tests that failed in gapdev 
 * (24/11/21) NaturalHomomorphism -> NaturalHomomorphismByNormalSubgoup 

## 2.83 -> 2.84 (15/11/2021) 
 * (15/11/21) corrected address for CDW: https://github.com/cdwensley
 * (08/04/21) Switch CI to use GitHub Actions 

## 2.82 -> 2.83 (17/02/2021) 
 * (17/02/21) added functions for quasi-isomorphisms - manual section 3.5
 * (26/10/20) corrected \^ for morphisms of 2dimensional groups 
 * (25/10/20) added examples/cpcpcp.g constructing 23 cat2-groups on (Cp)^3 

## 2.81 -> 2.82 (23/10/2020) 
 * (22/10/20) revised IsomorphismPreCat1Groups 
 * (19/10/20) added IsSymmetric2DimensionalGroup 
 * (25/08/20) removed perm groups as range in KernelCokernelXMod 
 * (07/08/20) added the property IsSubEndoMapping to util.{gd,gi} 
 * (20/07/20) revised the function AllXModsUpToIsomorphism etc. 
 * (15/06/20) renamed/revised PreXModRecordOfPreCat1Group (and conversely) 
 * (10/06/20) restructured cat1data.g so that the numbering agrees with HAP
 * (30/05/20) added HAP chapter to the manual plus XMod <-> HAP functions 

## 2.79 -> 2.81 (25/05/2020) 
 * (24/05/20) added AllCat2GroupsWithFixedUp(AndLeftRange) 
 * (10/05/20) split gp3objmap.tst into gp3xs.tst and gp3cat2.tst 
              added operation AllCat2GroupMatrix 
 * (08/05/20) added DirectProductOp method for PreCat2Groups 
              added PreCat2GroupMorphismByGroupHomomorphisms method

## 2.77 -> 2.79 (04/05/2020) 
 * (04/05/20) added operation LoopClasses to go with (All)LoopsXMod 
 * (03/05/20) reinstalling the package after hard disc replacement
 * (19/04/20) added operation DisplayLeadMaps for a HigherDimensionalDomain 
              fixed an error in AllCat2GroupsWithImagesUpToIsomorphism 
 * (16/04/20) fixed an error in the function GroupGroupoid
 * (03/04/20) added fields .symm1 and .siso1 to CatnGroupNumbers 
 * (02/04/20) revised IsomorphismPreCat1Groups 
 * (27/03/20) IsPreCat1GroupByEndomorphisms (ditto CatnGroup) 
              -> IsPreCat1GroupWithIdentityEmbedding 
              (IsomorphismTo)EndomorphismCat1Group 
              -> IsomorphicPreCat1GroupWithIdentityEmbedding 
 * (12/03/20) now SmallerDegreePermutationRepresentation2DimensionalGroup in
              place of SmallerDegreePerm2DimensionalDomain
 * (06/03/20) now running under GAP 4.11.0 
 * (02/03/20) fixed issue 41, (Direct/Co)Product for more than two objects
 * (01/03/20) corrected DiagonalCat1Group 
 * (29/02/20) added SubdiagonalCat1Group and IsSubXMod, IsSub*** 
 * (30/01/20) added tests on kernels in IsomorphismCat1Groups - valid?? 
 * (14/01/20) fixed IdentityMapping for cat2-groups and catn-groups  
 * (10/01/20) revised CatnGroupNumbers fields for crossed squares & cat2-groups 
 * (24/11/19) added DirectProduct for cat1-groups (& revised for xmods) 

## 2.74 -> 2.77 (17/11/2019) 
 * (17/11/19) fixed XModByGroupOfAutomorphisms and updated manual
 * (16/11/19) corrected Transpose3DimensionalGroup for cat2-groups 
 * (12/11/19) reverted need for a cat2-group to have a diagonal cat1-group

## 2.73 -> 2.74 (11/10/2019) 
 * (09/10/19) Cat1GroupOfXMod now a record storing embeddings of S,R in G 
 * (01/10/19) added second example in section 8.2.14 
 * (10/09/19) added methods for (Pre)CrossedSquareBy(Pre)XMods 
 * (09/09/19) revised CrossedSquare consructors to use diag, not the action
 * (06/09/19) added allcat2pos to CatnGroupLists(G) 
 * (03/09/19) major revision: PreCat2Group and DetermineRemainingCat1Groups
 * (02/09/19) replaced IsEndomorphismCat1Group by IsCat1GroupByEndomorphisms
 * (27/08/19) added AllCat3Groups, AllCat3GroupTriples, AllCat3GroupsNumber
 * (14/08/19) added AllCat1GroupMorphisms and simplified AllCat2GroupMorphisms
 * (13/08/19) changes to gp{2,3,4}map.gi to make cat2-xs.tst work for the paper
 * (07/08/19) functions for cat3-groups in gp4obj.g{d,i} 
 * (30/07/19) modified induced.tst and isoclinic.tst to avoid gapdev diffs 
 * (26/07/19) added .pairs and .fams to CatnGroupLists record 
 * (17/07/19) modified .travis.yml in line with Max' changes in Utils 
 * (17/07/19) added operation AllCat2GroupFamilies and updated the manual 
 * (16/07/19) added attribute record CatnGroupNumbers for a group 
 * (11/07/19) added new method for IsPreCat2GroupMorphism 
 * (17/06/19) revised AllCat{1,2}... functions using iterators in utils-0.64
 * (10/06/19) complete rewrite of IsomorphismCat2Groups 
 * (15/05/19) new AllCat{1,2}GroupsIterator, AllCat{1,2}GroupsNumber 
 * (10/05/19) added in new functions from Alper for AllCat2Groups (up to iso) 
 * (18/04/19) reorganised all .tst files into /tst/manual/ and /tst/extra/
 * (11/04/19) corrected UnionOfPiecesOp for prexmods with objects 
 * (09/04/19) added names and examples of crossed pairing functions to manual 
 * (13/03/19) added initial implementation of group groupoids (manual 2.8) 

## 2.72 -> 2.73 (04/03/2019) 
 * (04/03/19) Cat2GroupOfCrossedSquare now giving results again 
              crossed pairings now consistently maps : N x M -> L 
 * (16/02/19) added License field in PackageInfo.g 
 * (08/12/18) added InducedXModByBijection, InducedXModByCoproduct and 
              renamed SurjectiveInducedXMod, InclusionInducedXModByCopower 
 * (06/12/18) added a CoproductXMod method for a list with more than two xmods 
 * (05/12/18) added CrossedSquareByXModSplitting 
 * (03/12/18) add CrossedSquareByAutomorphismGroup, CrossedPairingByConjugators
 * (01/12/18) added method for IsCrossedSquare 
 * (28/11/18) new version of 2-argument functions in map2arg.{gd,gi} 
 * (26/11/18) added CrossedSquareByPullback and XModByPullback  
 * (21/11/18) added PrincipalCrossedPairing 
 * (19/11/18) added IsNormalSub2DimensionalDomain and used this when calling 
              CrossedSquareByNormalSubXMod
 * (15/11/18) renamed CrossedSquareMorphismByMorphisms 
              as CrossedSquareMorphismByXModMorphisms 
              and added CrossedSquareMorphismByGroupHomomorphisms; 
              ditto for Cat2GroupMorphisms 
              XModMorphismByHoms -> XModMorphismByGroupHomomorphisms 
 * (14/11/18) Cat1Morphism -> Cat1GroupMorphism, ditto Cat2Morphism, etc. 
              removed IsomorphismSmallPermGroup, NiceObject, etc from util.*
 * (08/11/18) added KernelCokernelXMod, fixing issue #34 
 * (07/11/18) XModByAutomorphismGroup is now an attribute, not a global fn. 
 * (07/11/18) fixed problem with Cat2GroupOfCrossedSquare 
 * (06/11/18) added special case of InducedXMod when iota is bijective 

## 2.69 -> 2.72 (16/09/2018) 
 * (10/09/18) replaced PrintOneItemPerLine(L); with Perform(L,Display); 
 * (02/09/18) added CrossedPairingByXModAction, CrossedSquareByNormalSubXMod
 * (30/08/18) PreCat2GroupOfPreCrossedSquare revised : q8xscat2.tst, d24.tst 
 * (07/08/18) added PreCat2GroupByPreCat1Groups and other changes to gp3obj.*
 * (01/08/18) fixed issue #24 and added xtst/cat1data.tst 
 * (30/07/18) fixed issue #9 

## 2.68 -> 2.69 (20/07/2018) 
 * (12/07/18) added operations InducedXModFromTrivialSource/Range  
 * (05/07/18) major revision of InclusionInducedXModByCopower 
 * (29/06/18) added method for IsCentralExtension2DimensionalGroup 
 * (28/06/18) added operation DisplacementGroup; changed DisplacementSubgroup
 * (17/05/18) corrected SinglePiecePreXModWithObjects in the discrete case 

## 2.67 -> 2.68 (01/05/2018) 
 * (30/04/18) made tests more robust; moved coprod.tst to /xtst 

## 2.66 -> 2.67 (29/04/2018) 
 * (23/04/18) avoid use of FactorGroup - just use Image( nat ) 
 * (19/04/18) replace Image(f,x) by ImageElm(f,x) throughout 

## 2.65 -> 2.66 (12/04/2018) 
 * (12/04/18) added LoopsXMod functions: apps.{gd,gi,xml}, loops.{g,tst} 

## 2.64 -> 2.65 (21/03/2018) 
 * (18/03/18) remove some of the temporary synonyms (09/08/17) 
 * (13/02/18) CompositeDerivation/Section now WhiteheadProduct 
              SectionByImages now SectionByHomomorphism 
              added IdentityDerivation; IdentitySection; WhiteheadOrder  
 * (12/02/18) AllOrRegular now DerivationClass; revised PrincipalDerivations
 * (10/01/12) converted LaTeX entities back in to newcommands 
 * (09/01/18) now using AutoDoc to build the manual 
 * (15/12/17) removed examples/; added expt/; *.tst files in xtst/ 

## 2.63 -> 2.64 (30/11/2017) 
 * (29/11/17) fixed tests to work with both 4r8 and dev in 3 options  
 * (23/11/17) removed attribute AutoGroup (=Range(XModAction)) for xmods 
 * (22/11/17) added operations SinglePiecePreXModWithObjects(NC)  
 * (16/11/17) added Root2dGroup for prexmods with objects   
 * (17/10/17) ImageElmXModAction now working for xmods of groupoids 
 * (04/10/17) added operation ImageElmXModAction 
 * (01/10/17) renamed test files and made them independent 
 * (27/09/17) added tst/testextra.g, tst/allxmods.xtst, tst/cat1mor.xtst 

## 2.61 -> 2.63 (26/09/2017) 
 * (26/09/17) added a number of Types and modified Objectify statements 
              removed Up2DimensionalMappingObj
 * (24/08/17) changed the name of Murat's university
 * (09/08/17) in order to keep XModAlg-1.12 working, added synonyms 
              Is2dDomain for Is2DimensionalDomain, 
              PreCat1ByTailHeadEmbedding for PreCat1Obj, 
              PreCat1ByEndomorphisms for PreCat1GroupByEndomorphisms 
              PreCat1ByPreXMod for PreCat1GroupOfPreXMod 
              IsGeneral2dMapping for IsGeneral2DimensionalMapping
          and Kernel2dMapping for Kernel2DimensionalMapping

## 2.59 -> 2.61 (07/08/2017) 
 * (07/08/17) Gpd changed to groupoids in list of required packages 
 * (07/08/17) testall.g now calls TestDirectory; test files renamed 
 * (07/08/17) temporarily renamed 08-gpd2obj.tst and 11-gpnobjmap.tst 
              so that they do not get tested by TestDirectory("tst")
 * (21/07/17) replaced PreXModIsomorphismByIsomorphisms by the more general 
              IsomorphismByIsomorphisms; then functions such as 
              IsomorphismPerm2DimensionalGroup rewritten to use it. 
 * (20/07/17) added IsomorphismByIsomorphisms for pre1-cat-groups 
 * (20/07/17) Removed duplication in 'Of' and 'By' operations  
 * (17/07/17) new functions added by Alper for crossed square <-> cat2-group: 
              ConjugationActionForCrossedSquare; 
              ElementsRelationsForSemidirectProduct
 * (14/06/17) added methods for String, ViewString, PrintString 
 * (09/05/17) `dom2d3d.g{d,i}` renamed `dom2dnd.g{d,i}`
              hdim-morphisms now expect [ src, rng, list of maps ] 
              PreCatn(Mapping)Dimension now HigherDimension 
              added attribute VertexGroups for HigherDimensionalGroups 
 * (08/05/17) Removed lots of 3Dim stuff since HigherDim stuff is better
 * (05/05/17) AllCat1Groups\* -> AllCat1DataGroups\* 
 * (04/05/17) Alper add catn-morphism functions in `gpnmor.g{d,i}` 
 * (29/04/17) added lines to `makedoc.g` to allow for xymatrix commands 
 * (24/04/17) 2d<anything> becomes 2Dimensional<anything> (except Object2d) 
 * (22/04/17) Alper added `lib/gpnobj.gd`, `lib/gpnobj.gi`, `tst/gpnobjmap.tst` 
 * (10/04/17) Alper added a number of functions for cat2-groups and morphisms 
              Replaced XPairing with CrossedPairing 
 * (04/04/17) added CoproductInfo for coproducts 
              FactorXMod now FactorPreXMod (ditto NaturalMorphisms) 
 * (27/03/17) added Is2dGroupMorphismData and modified construction functions 
              Method for IsNormal now applies to precrossed modules 
              added PeifferSub2dGroup 
 * (24/03/17) added operation DiagonalCat1 and example S3xS3 => S3
 * (22/03/17) added operation CoproductXMod and files `coprod.g`, `coprod.tst` 

## 2.58 -> 2.59 (21/03/2017) 
 * (21/03/17) added property IsEndomorphismPreCat1 
              modified IsomorphismPerm2dGroup for PreCat1 objects 
 * (14/11/16) "first author" -> "second author" in `manual.xml` (issue #4) 

## 2.56 -> 2.58 (02/11/2016) 
 * (18/10/16) now using bibliography file `bib.xml` of type `bibxmlext.dtd`
 * (13/10/16) changed package releases to <https://gap-packages.github.io/xmod>
 * (24/03/16) added files `hap.gd`, `hap.gi`
 * (16/03/16) edited `utils.tst` to remove functions transferred to Utils

## 2.51 -> 2.56 (08/03/2016) 
 * (23/02/16) remove functions from `util.g{d/i}` now in Utils; require Utils 
 * (18/02/16) removed date/version info from file headers 
 * (16/02/16) require Gpd >= 1.42;  

## 2.45 -> 2.51 (09/02/2016) 
 * (08/02/16) necessary changes to test files to fix new output errors

## 2.44 -> 2.45 (29/12/2015) 
 * (29/12/15) fixed changes in output from `\*.tst` files 

## 2.42 -> 2.44 (13/11/2015) 
 * (11/11/15) renamed Rank and MiddleLength and now for p-groups only 
 * (10/11/15) changes examples in tests to avoid repeated variable names 
 * (09/11/15) XPair -> XPairing, etc.
 * (08/11/15) IsomorphismPermPre(XMod/Cat1) -> IsomorphismPerm2dGroup, etc. 
 * (07/11/15) Revised manual, chapter 7, and isoclinic.tst
 * (06/11/15) IsStemGroup & IsStemXMod -> IsStemDomain etc. 
 * (04/11/15) RestrictionMappingGroups -> GeneralRestgrictedMapping 
 * (01/11/15) `makedocrel.g` : added MathJax; changed "doc" to XModDoc 
 * (20/10/15) moved code from FactorXMod to NaturalMorphismByNormalSubXMod 
              corrected formulae in Displacement and DisplacementSubgroup 
 * (19/10/15) revised Isoclinism for xmods; IsoAllXMods -> AllXModsUpToIso..
 * (16/10/15) changed all occurrences of XSq to CrossedSquare 
 * (15/10/15) fixed Iterator bug in Isoclinism for groups 
 * (13/10/15) CentralQuotient now a crossed module or a crossed square 
              added LeftRightMorphism and UpDownMorphism for crossed squares 
 * (07/10/15) AllXMods and AllPreXMods now global functions 
              added property IsStemXMod 
 * (06/10/15) renamed IsIsomorphicXMod as IsomorphismXMods; 
              fixed the call to IsXMod in PreXModObj 
 * (05/10/15) added Centralizer and Normalizer methods for crossed modules 
 * (02/10/15) added ExternalSetXMod 
 * (01/10/15) added operation Displacement
 * (28/09/15) added AllStemGroupFamilies and Set various Names 
 * (26/09/15) added NaturalHomomorphismByNormalSubXMod  
 * (24/09/15) added CrossActionSubgroup 
 * (23/09/15) removed last occurrences of AllIsomorphisms 
 * (22/09/15) added functions for isoclinism of groups 
 * (18/09/15) processed a number of properties in the isoclinic files 
 * (17/09/15) added example in manual subsection 2.1.4. 
 * (15/09/15) methods for InnerMorphism, ActorXMod, CentreXMod, etc. 
              now specified only for permutation crossed modules 
 * (24/08/15) major edits to `README`, including GitHub issues link 

## 2.41 -> 2.42 (24/08/2015) 
 * (24/08/15) made version to be moved from Bitbucket to GitHub 
 * (20/07/15) fixed bug in IsomorphismPcPreCat1 reported by Charles Melville 

## 2.32 -> 2.41 (05/06/2015) 
 * (05/06/15) `gpd2obj.gd` changed to reflect changes in package Gpd 

## 2.31 -> 2.32 (26/02/2015) 
 * (26/02/15) reverted AllInducedXMods(Cat1s) to global functions 
 * (03/02/15) added properties IsPreXModDomain and IsPreCat1Domain 
              which are meant to apply to both group and algebra cases 
 * (02/02/15) took out all MultiplicativeElementWithTuple's etc. 
              and Multiplicative2dElement's etc. 
 * (29/01/15) made copies of `dom2d3d,g{d,i}` and `gp2obj.g{d,i}` 
              before embarking on a major rewrite of the category structure: 
              specifically, changing lots of '2dDomain's into '2dGroup's

## 2.26 -> 2.31 (17/12/2014) 
 * (17/12/14) moved package home page to <pages.bangor.ac.uk/~mas023/chda/xmod/>
 * (08/11/14) created BitBucket repository for 'XMod' 

## 2.22 -> 2.26 (26/11/2013) 
 * (26/11/13) minor mod to WhiteheadTransMonoid to fix `gp2up.tst` output
 * (03/11/13) fixed errors in `cat1data.g` reported by Van Luyen Le. 
 * (15/10/13) fixed error in Cat1MorphismByXModMorphism. 
 * (03/10/13) completed work on groups of order [64..70] in `cat1data.g` 
 * (02/10/13) added operation CollectPartsAlreadyDone. 
 * (01/10/13) added operation PreCat1IsomorphismByIsomorphisms. 
 * (17/08/13) fixed bug in PermCat1Select sent by Van Luyen Le 
 * (26/03/13) put groups of order 64 back into `cat1data.g` 
 * (05/02/13) rewrite of `testall.g` following Alex K's wedderga example 

## 2.19 -> 2.22 (25/01/2013) 
 * (25/01/13) Minor change to test file output to avoid problems in tests. 
 * (09/01/13) Changed PackageWWWHome, using "gap4" rather than "gap4r5". 
              Unable to fix problems related to changes in `lib/ghom.gi`, 
              so removed `gpd2obj.tst` from the set of standard tests: 
              the problem is with changes to GroupGeneralMappingByImages. 
 * (26/09/12) Moved functions to the new `cat1data.gd`, `cat1data.gi`. 
 * (21/09/12) Edited test files to reflect recent changes
 * (12/07/12) Changed the format and rebuilt the data file `cat1data.g` :- 
              it is now assumed that the SmallGroups library is used. 
              Extended `cat1data.g` from order 47 to most of order 64. 
              Major revision of AllCat1s, MakeAllCat1s, Cat1Select 
 * (04/07/12) Added AllCat1s and IsomorphismPreCat1s (from GAP3 version). 
              Removed batch of functions from `util.g{d,i}` involving 
              EndomorphismClasses, IdempotentImages, etc. 
              Replaced their use in AllCat1s with calls to GQuotients. 

## 2.18 -> 2.19 (09/06/2012) 
 * (08/06/12) Following the replacement of IdentitySubgroup by TrivialSubgroup 
              in Gpd, renamed IdentitySubXMod by TrivialSubXMod, etc. 
              All calls of GroupHomomorphismByImagesTriv returned to 
              GroupHomomorphismByImages (see item dated 30/04/08) 
 * (07/06/12) Renamed special Display for lists as PrintListOneItemPerLine following email from Max Horn 

## 2.17 -> 2.18 (23/04/2012) 
 * (23/04/12) added autpgrp as a needed package 
 * (26/01/12) replaced XModActedUpon by AutomorphismDomain in `gp2act.g{d,i}` 
 * (25/01/12) fixed error in XModByGroupOfAutomorphisms (oneG now oneP) 
 * (10/01/12) updated to 2012; corrected URLs in `intro.xml` 
 * (15/12/11) now requires version 1.13 of 'Gpd' 

## 2.15 -> 2.17 (21/09/2011) 
 * (20/09/11) new version of `makedocrel.g` for building the manual 
              added file `gpd/examples/readall.g` for testing purposes 
 * (17/09/11) Shortened the banner. 
 * (16/09/11) Renamed subdirectory `xmod/gap` as `xmod/lib` 

## 2.14 -> 2.15 (06/09/2011) 
 * (04/09/11) Changed BIND_GLOBAL to BindGlobal, since it is safer 
 * (16/08/11) changed directory for archive to `.../chda/gap4r5/xmod/` 

## 2.13 -> 2.14 (30/07/2011)
 * (30/07/11) final adjustments to the test files 
 * (29/07/11) major changes to `gp3objmap.g{d,i}`. 
 * (28/07/11) new 16/07/11 files now `dom2d3d.gd` etc. 
 * (27/07/11) ViewObj, PrintObj, Display, Name now methods for 2d-groups. 
 * (19/07/11) IsUp2dMapping now a category. 
              InducedXMod( s4, s3b, s3b ) fails due to iso to pc-group, 
              fo forced (for now) an isomorphism to a perm group. 
 * (18/07/11) Centre of an xmod no longer works, so brought back XModCentre. 
 * (16/07/11) Introduced a range of new categories in files 
              `dom2d.gd`, `dom2d.gi`, `map2d.gd`, `map2d.gi`. 
              As a result there were many changes in all files. 
 * (08/07/11) Changed headers of all files. 
 * (06/07/10) Changed XModByGroupOfAutomorphisms(G) in case G is pcgroup
 * (02/07/10) New email address for Murat
 * (30/06/10) Added check for IsPc2dDomain to PreXModObj 
 * (30/06/10) InducedXModByCopower now constructs a PcXMod if appropriate 
 * (28/06/10) Added methods for IsomorphismPcPreXMod, IsomorphismPcPreCat1
 * (12/03/10) Moved 'XMod' development to IMac at home, and started v.2.14 
 
## 2.12 -> 2.13 (21/01/2010)
 * (21/01/10) Changed output of AllInducedXMods to list of StructureDesc.  

## 2.11 -> 2.12 (24/11/2008)
 * (19/11/08) GapDoc relegated to "suggested other packages". 

## 2.008 -> 2.11 (13/11/2008)
 * (13/11/08) added GNU General Public License declaration,
               moved some 'XMod' utilities to `gpd/gap/util.g{d,i}`, 
 * (07/11/08) Changed website to: <www.maths.bangor.ac.uk/chda/xmod/> 
               Split the test file into one file per chapter. 
 * (30/04/08) Added GroupHomomorphismByImagesTriv to `util.gd`, `util.gi` 
               to allow both <gens> and <imgs> to be empty: triv -> triv! 
               Fixed bug in XModOfCat1 in assigning names. 
               FixedBug in Boundary for a pre-cat1-group. 
 * (31/03/08) Added function GpdBuildManualHTML()
 * (16/03/08) renamed Tail,Head as TailMap,HeadMap (for benefit of 'Gpd')
 * (08/10/07) started this CHANGES file; 
                 changed directory permissions from 711 to 755;
                 renamed files:  `obj2` -> `gp2obj`, etc, ready for groupoids; 
                 added file  `testall.g`  in `xmod/tst` 

## 2.007 -> 2.008 (25/10/2006)
 * Fixed broken links in `PackageInfo.g`.

## 2.006 -> 2.007 (20/10/2006)
 * Added basic functions for crossed squares, considered as 3dObjects  
   with crossed pairings, and their morphisms. 
   (Groups with two normal subgroups, and the actor of a crossed module, 
    provide standard examples. Cat2-groups are not yet implemented.) 
 * Converted the documentation to the format of the GAPDoc package, 
   and added the function XModBuildManual() to the `util.gi` file.
 * Improved AutomorphismPermGroup for crossed modules, and introduced 
   a special method for conjugation crossed modules.
 * Substantial revisons made to XModByCentralExtension, NorrieXMod, 
   LueXMod, ActorXMod, and InclusionInducedXModByCopower. 
 * Reintroduced the Cat1Select operation.

## 2.005 -> 2.006 (04/09/2004)
 * Changed morphism functions to return fail when invalid data is supplied, 
   rather than calling Error.  
 * Fixed a bug in XModByGroupOfAutomorphisms. 

## 2.004 -> 2.005 (16/04/2004)
 * Moved the example files from `tst/test_i.g` to `examples/example_i.g`,
 * converted `testmanual.g` to a proper test file `tst/xmod_manual.tst`. 
 * Replaced OperationHomomorphism by ActionHomomorphism, 
   a general GAP;4.4 change.
 * Finished replacing RModule by AbelianModule. 
 * Renamed UpMapping as Up2dMapping. 
 * Added MappingGeneratorsImages and InverseGeneralMapping  
   for a 2dMapping.
 * A significant change was the conversion of the actor crossed module 
   functions from the 3.4.4 version, including AutomorphismPermGroup 
   for a crossed module, WhiteheadXMod, NorrieXMod, LueXMod, ActorXMod, 
   Centre of a crossed module, InnerMorphism and InnerActorXMod. 
 * Added SmallerDegreePermPreXMod after discovering, in the library, 
   the very useful SmallerDegreePermutationRepresentation.

## 2.002 -> 2.004 (14/04/2004)
 * gave a new email address for Murat Alp
 * added the Cat1Select functionality of version 1 to the Cat1 function

## 2.001 -> 2.002 (January 2004)
 * Version 2.002 was prepared for the 4.4 release at the end of January 2004,
and so required a `PackageInfo.g` file.

# Induced Crossed Modules (May 2002)

Converted combinatorial functions -- DistinctRepresentatives, 
CommonRepresentatives, CommonTransversal and IsCommonTransversal.

Converted Tietze modification functions
TzCommutatorPair, TzPartition and FactorsPresentation.

Introduced global functions
IsomorphismPermObject, IsomorphismFpObject, and IsomorphismPcObject
which call IsomorphismPermGroup etc. when the object is a group.
Added functions IsomorphismPermPreXMod, IsomorphismPermPreCat1, 
etc. to be called when the object is a 2d-object.

Added IsomorphismXModByNormalSubgroup which applies when the boundary
of the xmod is injective.

Added PreXModIsomorphismByIsomorphisms (but renamed 21/07/17) 
(we also need a similar function PreCat1IsomorphismByIsomorphisms)
where the data consists of a crossed module, an isomorphism of the source,
and an isomorphism of the range.

Changed RModule to AbelianModule.

# Version 2.001 for GAP 4 (April 2002)

Generic name UpMapping chosen for derivations and sections 
(later changed to Up2dMapping).

File names changed to `obj2.gd`, `map2.gi`, `up2.tex`, etc.

Added alternative methods for IsomorphismPermGroup for 2dObjects. 

Sorted a problem with fixing the generating set for R when used to 
define derivations.  The (old) code used an fp-group version of R 
and checks that all the relators map by chi to 1.
Unfortunately, IsomorphismFpGroup sometimes permutes the order
of the R-generators, with unfortunate effects.  
The fix used IsomorphismFpGroupByGenerators, which returns the 
images of the generators specified in the function call.  
We have also used 
genR := StrongGeneratorsStabChain( StabChain( rng ) ); 
throughout to specify the generators of R.

Operation XModMorphism renamed as XModMorphismByHoms, and a new 
global function XModMorphism introduced (ditto for other 2dMappings).

Now using \chi_1 \star \chi_2 for Whitehead multiplication (on the right), 
with CompositeDerivation still giving multiplication on the left.  
This means that the second axiom for derivations and for sections has changed 
-- see Chapter 4.
