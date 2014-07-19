*
* $Id: cdlsc.F,v 1.2 1996/03/12 13:08:46 cernlib Exp $
*
* $Log: cdlsc.F,v $
* Revision 1.2  1996/03/12 13:08:46  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLSC
      CHARACTER*255 PATH,CHPATH,CHDIR
      CHARACTER*36  CHOPT
#include "hepdb/cdunit.inc"
#include "hepdb/hdboptd.inc"
#include "hepdb/hdbopt.inc"
#include "hepdb/cdlscm.inc"
      EXTERNAL      CDLS
#include "hepdb/cdopen.inc"
*
 
      NOBJT  = 0
      NDIRT  = 0
      NOBJM  = 0
 
      CALL RZCDIR(CHDIR,'R')
      LCHDIR = LENOCC(CHDIR)
 
      CALL KUGETC(PATH,LPATH)
      CALL KUGETI(KSN)
      NKEYSN = KSN
      CALL KUGETC(CHBANK,LBANK)
      CALL KUGETI(ILNK1)
      CALL KUGETI(ILNK2)
      CALL KUGETI(IDAT1)
      CALL KUGETI(IDAT2)
      CALL KUGETC(CHOPT,LCHOPT)
      CALL CDFIXF(PATH,CHPATH)
      LPATH = LENOCC(CHPATH)
 
      IF(LCHOPT.GT.0) THEN
         CHOPTT = CHOPT(1:LCHOPT)
      ELSE
         CHOPTT = ' '
      ENDIF
 
#include "hepdb/hdboptc.inc"
*
* Look for objects in specified path
*
      IF(LLOGCD.GE.2) PRINT *,'CDLSC. enter for ',CHPATH(1:LPATH),
     +                ',',CHOPT
*
      IF(IOPTH.NE.0) THEN
         WRITE(LOUTCD,*) '>>> Command: LS ',
     +   'Options: ',CHOPT(1:LCHOPT),' key serial number: ',KSN
         WRITE(LOUTCD,*) '>>> Path: ',CHPATH(1:LPATH)
         WRITE(LOUTCD,*) '>>> Current directory: ',
     +   CHDIR(1:LCHDIR)
         WRITE(LOUTCD,*) '>>> Bank name: ',CHBANK,
     +      ' first/last links,data words = ',ILNK1,ILNK2,IDAT1,IDAT2
      ENDIF
*
      IF(ICFMUL('*%(<>[]',CHPATH,1,LPATH).GT.LPATH) THEN
         CALL RZCDIR(CHPATH(1:LPATH),' ')
         CALL CDLS(CHPATH(1:LPATH),IRC)
      ELSE
         CALL CDSCAN(CHPATH(1:LPATH),99,CDLS,IRC)
      ENDIF
 
      WRITE(LOUTCD,9001) NOBJT,NDIRT,NOBJM
 9001 FORMAT(' Total of ',I6,' objects in ',I6,' directories',
     +       ' of which ',I6,' match selection criteria')
 
      CALL RZCDIR(CHDIR(1:LCHDIR),' ')
 
      END
