*
* $Id: hbproy.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbproy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 22/05/89  16.23.19  by  Rene Brun
*-- Author :
      SUBROUTINE HBPROY(ID2,VALMAX)
*.==========>
*.           BOOK PROJECTION Y
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
      NARG   = 2
      CALL NOARG (NARG)
      VALMA  = 0.
      IF (NARG .EQ. 2)        VALMA = FLOARG (VALMAX)
*
*               computes number of words per channel
*               from VALMAX
*
      IF (VALMA .GE. 1.)THEN
         NBPROX = LOG (VALMA)/LOG (2.) +1
         IF(NBPROX.GT.16)NBPROX=32
      ELSE
         NBPROX = 32
      ENDIF
      IF(NBPROX.EQ.32)THEN
         IODES=IOCF1
      ELSE
         IODES=IOCB1
      ENDIF
      NB  = 32/NBPROX
      IRET= 3
*
   20 CALL HLOOP (ID2,'HBPROY',IRET)
      IF (IRET .EQ. 0)               GO TO 99
*
      CALL HDCOFL
      IF (I230 .EQ. 0)              GO TO 40
      LPROY=LQ(LCID-3)
      IF(LPROY.NE.0)GO TO 40
*
*               Check if enough space
*
      NC    = IQ(LCID+KNCY)
      NW    = (NC+1)/NB+KCON1
      NTOT  = NW+I10*(NC+10)+12
      CALL HSPACE(NTOT,'HBPROY',ID2)
      IF(IERR.NE.0)GO TO 40
*
*               Create data structure PROX
*
      CALL MZBOOK(IHDIV,LPROY,LCID,-3,'PROY',2,2,NW,IODES,0)
      IQ(LPROY+KNBIT)=NBPROX
      IF(I10.NE.0)THEN
         CALL MZBOOK(IHDIV,LW,LPROY,0,'PRYE',0,0,NC,3,0)
      ENDIF
      CALL HSIFLA(8,1)
      IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
*
   40 IRET = 2
      GO TO 20
  99  RETURN
      END
