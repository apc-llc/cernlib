*
* $Id: fmls.F,v 1.1.1.1 1996/03/07 15:18:09 mclareni Exp $
*
* $Log: fmls.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:09  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMLS(PATH,CHOPT,IRC)
      CHARACTER*(*) CHOPT,PATH
      CHARACTER*255 CHPATH
#include "fatmen/fatpara.inc"
#include "fatmen/fatloc.inc"
#include "fatmen/fatmtp.inc"
#include "fatmen/fatcpl.inc"
#include "fatmen/fatsys.inc"
#include "fatmen/fatout.inc"
#include "fatmen/fatsho.inc"
      DIMENSION     JOPT(36)
#include "fatmen/fatbank.inc"
      EXTERNAL      FALS
#include "fatmen/falscm.inc"
*
      LWRITE = LPRTFA
 
      NFILES = 0
      NDIRT  = 0
      NFILT  = 0
 
      CWDSHO = ' '
 
      LCHOPT = LENOCC(CHOPT)
      LPATH  = LENOCC(PATH)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      CALL FMOPTC(CHOPT,ALFNUM,JOPT)
      CALL UCOPY(JOPT,IOPT,36)
 
      NFILES = 0
*
* Look for some files and show the bank and key content:
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LF     = LENOCC(CHFILE)
 
      CALL CLTOU(CHPATH(1:LP))
      CALL CLTOU(CHFILE(1:LF))
 
      IF(IDEBFA.GE.2) PRINT *,'FMLS. enter for ',PATH(1:LPATH),
     +                ',',CHOPT
*
      IF(ICFMUL('*%(<>',CHPATH,1,LP).GT.LP) THEN
         CALL FACDIR(CHPATH(1:LP),' ')
         CALL FALS(CHPATH(1:LP),IRC)
      ELSE
         CALL FMSCAN(CHPATH(1:LP),99,FALS,IRC)
      ENDIF
 
      IQUEST(11) = NFILT
      IQUEST(12) = NDIRT
 
      END
