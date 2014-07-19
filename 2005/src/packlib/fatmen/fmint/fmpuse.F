*
* $Id: fmpuse.F,v 1.1.1.1 1996/03/07 15:18:11 mclareni Exp $
*
* $Log: fmpuse.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:11  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMPUSE(GNAME,LBANK,KEYS,CHOPT,IRC)
*CMZ :          23/01/91  13.28.01  by  Jamie Shiers
*-- Author :    Jamie Shiers   23/01/91
      CHARACTER*(*) GNAME
      CHARACTER*256 DSN
#include "fatmen/fmnkeys.inc"
      DIMENSION KEYS(LKEYFA)
      DIMENSION NDAYS(3)
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
#include "fatmen/fatopts.inc"
      IRC    = 0
      LGN    = LENOCC(GNAME)
      ISIZE  = IQ(LBANK+KOFUFA+MFSZFA)
      IUSE   = IQ(LBANK+KOFUFA+MUSCFA)
*     Number of days since last access = NDAYS(3)
      CALL FMDAYS(GNAME(1:LGN),LBANK,KEYS,NDAYS,' ',IC)
*
      DSN = ' '
      IF(KEYS(MKMTFA).EQ.1) THEN
         CALL UHTOC(IQ(LBANK+KOFUFA+MFQNFA),4,DSN,NFQNFA)
      ELSE
         CALL UHTOC(IQ(LBANK+KOFUFA+MVIDFA),4,DSN,6)
      ENDIF
      LDSN = LENOCC(DSN)
      CALL FMEXST(GNAME(1:LGN),NCOP)
      IF(IOPTH.NE.0) PRINT 9000,
     +'+++ Generic name +++ Generic name +++ Generic name +++ ','KSN',
     +  'Datasetname/VID','#MB','#ACC','L/A','Copies'
9000  FORMAT(1X,A80,1X,A5,1X,A20,1X,A4,1X,A4,1X,A4,1X,A6)
      PRINT 9001,GNAME(1:MIN(80,LGN)),KEYS(1),DSN(1:MIN(20,LDSN)),
     +           ISIZE,IUSE,NDAYS(3),NCOP
9001  FORMAT(1X,A80,1X,I5,1X,A20,1X,I4,1X,I4,1X,I4,1X,I4)
      END
