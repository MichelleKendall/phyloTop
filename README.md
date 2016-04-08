[![Travis-CI Build Status](https://travis-ci.org/MichelleKendall/phyloTop.png?branch=master)](https://travis-ci.org/MichelleKendall/phyloTop)

# phyloTop
Calculating topological properties of phylogenies

The package ‘phyloTop’ was removed from the CRAN repository and archived on 29/10/2015 as it was no longer maintained by its creator, Michael Boyd.

I am reviving the package, speeding up some of the computations, simplifying the format requirements and dependencies, and adding help files and examples.

# Updates

08/04/2016: Reinstated the function subtreeShow.

07/04/2016: Fixed bug in ladderSizes (which also affected avgLadder) and improved ladderShow plotting.

06/04/2016: Added tests, and the functions nodeImbFrac and nodeDepthFrac. The function nodeFrac is now deprecated. All previous functions are now updated or deprecated and replaced.

05/04/2016: Added functions makeEpiRecord, getLabGenealogy and makePhyloTree (internal). Functions sortmyepi and makeTransTree are deprecated as they are no longer needed after these updates.

18/03/2016: Functions avgLadder, cherries, ILnumber, maxHeight, pitchforks, and sackin.phylo now contain normalisation options - thanks to Giacomo Plazzotta for providing me with their maximum values. Errors arising from submitting a tree with two tips to the tree statistic functions have now been removed - thanks to Scott Ward for spotting this problem and supplying code fixes.

08/02/2016: Package now includes a function called "phyloTop" which takes a list of trees and applies a variety of topological functions (faster than calling each function individually). Deprecated functions issue warnings. configShow and ladderShow now use the faster implementations of nConfig and ladderSizes, and phylo rather than phylo4 format. Dependency on phangorn removed.

07/02/2016: version 2.0.0. Some functions removed, renamed or changed: the package is not backwards compatible. The remaining functions accept both phylo and phylo4 objects, and are documented. The functions nConfig, ladderSizes and getDepths (on which most of the others depend) have been freshly implemented by Caroline Colijn and are significantly faster.

16/12/2015: approximately half of the functions checked and documented. Package not yet stable.


