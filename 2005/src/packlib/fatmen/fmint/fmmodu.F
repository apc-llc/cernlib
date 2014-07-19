*
* $Id: fmmodu.F,v 1.1.1.1 1996/03/07 15:18:12 mclareni Exp $
*
* $Log: fmmodu.F,v $
* Revision 1.1.1.1  1996/03/07 15:18:12  mclareni
* Fatmen
*
*
#include "fatmen/pilot.h"
      SUBROUTINE FMMODU(PATH,UFORM,IVECT,UCOMM,CHOPT,IRC)
      CHARACTER*(*) CHOPT,PATH,UFORM,UCOMM
      DIMENSION  IVECT(10)
      CHARACTER*255 CHPATH
#include "fatmen/fatpara.inc"
#include "fatmen/fatsys.inc"
      DIMENSION     JOPT(36)
#include "fatmen/fatbank.inc"
      EXTERNAL      FAMODU
#include "fatmen/famucm.inc"
*
      LCHOPT = LENOCC(CHOPT)
      LPATH  = LENOCC(PATH)
 
      IF(LCHOPT.EQ.0) THEN
         CHOPT  = ' '
         LCHOPT = 1
      ENDIF
 
      CALL FMOPTC(CHOPT,ALFNUM,JOPT)
      CALL UCOPY(JOPT,IOPT,36)
*
      IF(IOPTC+IOPTF+IOPTV.EQ.0) THEN
         IF(IDEBFA.GE.-3) PRINT *,'FMMODU. error - please specify',
     +      ' at least one of the options C,F,V'
         IRC = -1
         RETURN
      ENDIF
 
      IF(IOPTF.NE.0) CHUFRM = UFORM(1:LENOCC(UFORM))
      IF(IOPTV.NE.0) CALL UCOPY(IVECT,IWORDS,NUSWFA)
      IF(IOPTC.NE.0) CHUCOM = UCOMM(1:LENOCC(UCOMM))
*
      LSLASH = INDEXB(PATH(1:LPATH),'/')
      CHPATH = PATH(1:LSLASH-1)
      CHFILE = PATH(LSLASH+1:LPATH)
      LP     = LENOCC(CHPATH)
      LF     = LENOCC(CHFILE)
 
      IF(IDEBFA.GE.2) PRINT *,'FMMODU. enter for ',PATH(1:LPATH),
     +                ',',CHOPT
*
      IF(ICFMUL('*%(<>',CHPATH,1,LP).GT.LP) THEN
         CALL FACDIR(CHPATH(1:LP),' ')
         CALL FAMODU(CHPATH(1:LP),IRC)
      ELSE
         CALL FMSCAN(CHPATH(1:LP),99,FAMODU,IRC)
      ENDIF
 
      END
