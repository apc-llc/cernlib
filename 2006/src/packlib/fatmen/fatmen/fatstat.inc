*
* $Id: fatstat.inc,v 1.1.1.1 1996/03/07 15:17:52 mclareni Exp $
*
* $Log: fatstat.inc,v $
* Revision 1.1.1.1  1996/03/07 15:17:52  mclareni
* Fatmen
*
*
#ifndef CERNLIB_FATMEN_FATSTAT_INC
#define CERNLIB_FATMEN_FATSTAT_INC
*
*
* fatstat.inc
*
*
*     Count number of server operations:
*        Number of different users, nodes, files, commands,
*        PUTs, MODs, DELs, MKDIRs, RMDIRs, OPENs and CLOSEs.
*        NFMUSR and NMFNOD not currently counted
*     Problems:
*        Number of warnings from RZDELK
*        Number of PUTs for existing entries
*        Number of REPs for non-existing entries
*        Number of DELs for non-existing entries
*        Number of BAD  (unrecognised) commands
*        Number of logging records
*
      COMMON/FATSTAT/NFMUSR,NFMNOD,NFMFIL,NFMCOM,NFMPUT,
     +               NFMMOD,NFMDEL,NFMMDR,NFMRMD,NFMOPN,
     +               NFMCLS,NFMPDK,NFMPRP,NFMPDL,NFMBAD,
     +               NFMLOG

#endif
