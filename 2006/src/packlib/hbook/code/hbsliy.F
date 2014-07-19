*
* $Id: hbsliy.F,v 1.1.1.1 1996/01/16 17:07:32 mclareni Exp $
*
* $Log: hbsliy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:32  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 22/05/89  16.23.19  by  Rene Brun
*-- Author :
      SUBROUTINE HBSLIY(ID2,NSL,VALMAX)
*.==========>
*.           BOOK SLICES ALONG Y AXIS
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
      NARG   = 3
      CALL NOARG (NARG)
      VALMA  = 0.
      IF (NARG .EQ. 3)        VALMA = FLOARG (VALMAX)
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
      IRET= 3
*
   20 CALL HLOOP (ID2,'HBSLIY',IRET)
      IF (IRET .EQ. 0)               GO TO 99
*
      CALL HDCOFL
      IF (I230 .EQ. 0)              GO TO 40
      LSLIY=LQ(LCID-5)
      IF(LSLIY.NE.0)GO TO 40
*
*               Check if enough space
*
      NC    = IQ(LCID+KNCY)
      NB    = 32/NBPROX
      NW    = (NC+1)/NB+KCON1
      NTOT  = NSL*(NW+I10*(NC+10)+12)+NSL+10
      CALL HSPACE(NTOT,'HBSLIY',ID2)
      IF(IERR.NE.0)GO TO 40
*
*               Create data structure PROX
*
      CALL MZBOOK(IHDIV,LSLIY,LCID,-5,'SLIY',NSL,NSL,0,0,0)
      DO 30 I=1,NSL
         CALL MZBOOK(IHDIV,LCONT,LSLIY,-I,'SLIY',2,2,NW,IODES,0)
         IQ(LCONT+KNBIT)=NBPROX
         IF(I10.NE.0)THEN
            CALL MZBOOK(IHDIV,LW,LCONT,0,'SLYE',0,0,NC,3,0)
         ENDIF
  30  CONTINUE
      CALL HSIFLA(8,1)
      IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
*
   40 IRET = 2
      GO TO 20
  99  RETURN
      END
