*
* $Id: cdlist.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdlist.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDLIST(PATH,KSN,CHBANC,JLNK1,JLNK2,JDAT1,JDAT2,
     +                  CHOPT,IRC)
      CHARACTER         PATH*(*),CHBANC*(*),CHOPT*(*),CHPATH*255
#include "hepdb/cdlscm.inc"
#include "hepdb/cduscm.inc"
#include "hepdb/hdbopt.inc"
      EXTERNAL      CDLS
#include "hepdb/hdboptd.inc"
*
*     Interface to CDLS routine
*
      CALL CDLDDI (PATH, CHPATH, LPATH)
      LBANK = LENOCC(CHBANC)
      IF(LBANK.EQ.0) THEN
         CHBANK = ' '
      ELSE
         CHBANK = CHBANC(1:MIN(LEN(CHBANK),LBANK))
      ENDIF
 
      LCHOPT    = LENOCC(CHOPT)
      IF(LCHOPT.EQ.0) THEN
         CHOPTT = ' '
      ELSE
         CHOPTT = CHOPT(1:LCHOPT)
      ENDIF
 
      ILNK1  = JLNK1
      ILNK2  = JLNK2
      IDAT1  = JDAT1
      IDAT2  = JDAT2
      NKEYSN = KSN
 
#include "hepdb/hdboptc.inc"
*
* Look for objects in specified path
*
      IF(IDEBCD.GE.2) PRINT *,'CDLIST. enter for ',CHPATH(1:LPATH),
     +                ',',CHOPT
*
      IF(IOPTH.NE.0) THEN
         WRITE(LPRTCD,*) '>>> Command: LS ',
     +   'Options: ',CHOPT(1:LCHOPT),' key serial number: ',KSN
         WRITE(LPRTCD,*) '>>> Path: ',CHPATH(1:LPATH)
         WRITE(LPRTCD,*) '>>> Bank name: ',CHBANK,
     +      ' first/last links,data words = ',ILNK1,ILNK2,IDAT1,IDAT2
      ENDIF
*
      IF(ICFMUL('*%(<>[]',CHPATH,1,LPATH).GT.LPATH) THEN
         CALL RZCDIR(CHPATH(1:LPATH),' ')
         CALL CDLS(CHPATH(1:LPATH),IRC)
      ELSE
         CALL CDSCAN(CHPATH(1:LPATH),99,CDLS,IRC)
      ENDIF
 
      WRITE(LPRTCD,9001) NOBJT,NDIRT,NOBJM
 9001 FORMAT(' Total of ',I6,' objects in ',I6,' directories',
     +       ' of which ',I6,' match selection criteria')
 
      END
