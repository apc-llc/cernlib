*
* $Id: abend.F,v 1.1.1.1 1996/03/07 15:17:41 mclareni Exp $
*
* $Log: abend.F,v $
* Revision 1.1.1.1  1996/03/07 15:17:41  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE ABEND
#include "fatmen/fatstat.inc"
#include "fatmen/fatbug.inc"
      INTEGER SYSTEMF
 
      PRINT *,'ABEND. printing final FATSRV statistics'
 
      WRITE(LPRTFA,9001) NFMUSR,NFMNOD,NFMFIL,NFMCOM
     +,NFMPUT,NFMMOD,NFMDEL,NFMMDR,NFMRMD,NFMOPN,NFMCLS
     +,NFMPDK,NFMPRP,NFMPDL,NFMBAD,NFMLOG
 
 9001 FORMAT(' FATSRV. termination statistics: ',/,
     +       ' Number of different users = ',I6,' (not yet counted) ',/,
     +       ' Number of different nodes = ',I6,' (not yet counted) ',/,
     +       ' Number of files           = ',I6,/,
     +       ' Number of commands        = ',I6,/,
     +       ' Number of PUTs            = ',I6,/,
     +       ' Number of MODs            = ',I6,/,
     +       ' Number of DELs            = ',I6,/,
     +       ' Number of MKDIRs          = ',I6,/,
     +       ' Number of RMDIRs          = ',I6,/,
     +       ' Number of OPENs           = ',I6,/,
     +       ' Number of CLOSEs          = ',I6,' (not yet counted) ',/,
     +       ' Number of RZDELK warnings = ',I6,/,
     +       ' Number of REP failures    = ',I6,' (no such entry)   ',/,
     +       ' Number of DEL failures    = ',I6,' (no such entry)   ',/,
     +       ' Number of BAD commands    = ',I6,' (unrecognised)    ',/,
     +       ' Number of LOG commands    = ',I6)
 
#if defined(CERNLIB_UNIX)
      PRINT *,'ABEND. calling FATABEND script'
      IC = SYSTEMF('fatabend')
#endif
#if defined(CERNLIB_VAXVMS)
      PRINT *,'ABEND. calling FATABEND command file'
      IC = SYSTEMF('@FATABEND')
#endif
#if defined(CERNLIB_IBMVM)
      PRINT *,'ABEND. calling FATABEND exec'
      IC = SYSTEMF('EXEC FATABEND')
#endif
 
      END
