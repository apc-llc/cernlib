*
* $Id: fmfilc.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmfilc.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMFILC(PATH,NFILES,IRC)
      CHARACTER*(*) PATH
      CHARACTER*255 CHPATH
#include "fatmen/fatsys.inc"
#include "fatmen/fatbank.inc"
#include "fatmen/fatpara.inc"
      LOGICAL IEXIST,IOPEN
#include "fatmen/fafccm.inc"
      EXTERNAL      FAFC
#include "fatmen/fatinit.inc"
*
*     IQUEST(11) = NMATCH
*     IQUEST(12) = NFOUND
*     IQUEST(13) = NDIRT
*
      LPATH = LENOCC(PATH)
*
      IOPTD = 0
      IOPTF = 0
      IOPTL = 0
      IOPTZ = 0
*
      NFOUND = 0
      NMATCH = 0
      NDIRT  = 0
*
      LWRITE = LPRTFA
*
*     Count number of files matching pattern
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LF     = LENOCC(CHFILE)
      IF(IDEBFA.GE.2) PRINT *,'FMFILC. enter for ',PATH(1:LPATH)
 
      IF(ICFMUL('*%(<>',CHPATH,1,LP).GT.LP) THEN
         CALL FACDIR(CHPATH(1:LP),' ')
         CALL FAFC(CHPATH(1:LP),IRC)
      ELSE
         CALL FMSCAN(CHPATH(1:LPATH),99,FAFC,IRC)
      ENDIF
 
      IQUEST(11) = NMATCH
      IQUEST(12) = NFOUND
      IQUEST(13) = NDIRT
      NFILES     = NMATCH
 
      RETURN
      END
