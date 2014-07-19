*
* $Id: cdobjc.F,v 1.2 1996/03/12 13:08:51 cernlib Exp $
*
* $Log: cdobjc.F,v $
* Revision 1.2  1996/03/12 13:08:51  cernlib
* Build hepdb programs: hepdb, cdserv, cdnew, and cdmove
*
* Revision 1.1.1.1  1996/02/28 16:23:31  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDOBJC
 
#include "hepdb/cdunit.inc"
 
      CHARACTER*255 PATH,CHPATH,CHDIR
      CHARACTER*255 PATH2
      CHARACTER*36  CHOPT
#include "hepdb/cdoccm.inc"
      EXTERNAL      CDOBJX
#include "hepdb/cdopen.inc"
*
      CALL RZCDIR(CHDIR,'R')
      LCHDIR = LENOCC(CHDIR)
 
      PATH = ' '
      CALL KUGETC(PATH,LPATH)
      CALL KUGETC(CHOPT,LCHOPT)
 
      CALL CDFIXF(PATH,CHPATH)
      LPATH = LENOCC(CHPATH)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      IF(INDEX(CHOPT(1:LCHOPT),'H').NE.0) THEN
         WRITE(LOUTCD,*) '>>> Command: COUNT ',
     +   'Options: ',CHOPT(1:LCHOPT)
         WRITE(LOUTCD,*) '>>> Path: ',PATH(1:LPATH)
         WRITE(LOUTCD,*) '>>> Current directory: ',CHDIR(1:LCHDIR)
      ENDIF
*
 
 
      IOPTD = 0
      IOPTF = 0
      IOPTL = 0
      IOPTZ = 0
      IF(INDEX(CHOPT(1:LCHOPT),'D').NE.0) IOPTD = 1
      IF(INDEX(CHOPT(1:LCHOPT),'F').NE.0) IOPTF = 1
      IF(INDEX(CHOPT(1:LCHOPT),'L').NE.0) IOPTL = 1
      IF(INDEX(CHOPT(1:LCHOPT),'Z').NE.0) IOPTZ = 1
 
      IF(IOPTL.NE.0) IOPTD = 1
      IF(IOPTZ.NE.0) IOPTF = 1
*
      NFOUND = 0
      NMATCH = 0
      NDIRT  = 0
 
      CALL CDSCAN(CHPATH(1:LPATH),99,CDOBJX,IRC)
 
      IF(LLOGCD.GE.-2) THEN
         WRITE(LOUTCD,9001) NFOUND,NDIRT
 9001    FORMAT(' Total of ',I6,' objects in ',I6,
     +          ' directories')
      ENDIF
 
      CALL RZCDIR(CHDIR(1:LCHDIR),' ')
      RETURN
      END
