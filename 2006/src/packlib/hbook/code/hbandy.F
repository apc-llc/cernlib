*
* $Id: hbandy.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hbandy.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 22/05/89  16.23.18  by  Rene Brun
*-- Author :
      SUBROUTINE HBANDY(ID2,XX0,XX1,VALMAX)
*.==========>
*.           book a band along axis y for XX0<X<XX1
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcform.inc"
#include "hbook/hcbits.inc"
*.___________________________________________
      NARG   = 4
      CALL NOARG (NARG)
      VALMA  = 0.
      IF (NARG .EQ. 4)        VALMA = FLOARG (VALMAX)
*
*               COMPUTES NUMBER OF WORDS PER CHANNEL
*               DEPENDING FROM VALUE OF VALMAX
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
   20 CALL HLOOP (ID2,'HBANDY',IRET)
      IF (IRET .EQ. 0)               GO TO 99
*
      CALL HDCOFL
      IF (I230 .EQ. 0)              GO TO 40
*
*               Check if enough space
*
      NC    = IQ(LCID+KNCY)
      NB    = 32/NBPROX
      NW    = (NC+1)/NB+KCON1
      NTOT  = NW+I10*(NC+10)+25
      CALL HSPACE(NTOT,'HBANDY',ID2)
      IF(IERR.NE.0)GO TO 40
*
*               Create data structure BANX
*
      LR1=LQ(LCID-7)
      IF(LR1.EQ.0)THEN
         LR1=LCID
         JBIAS=-7
      ELSE
         JBIAS=0
   30    IF(LQ(LR1).NE.0)THEN
            LR1=LQ(LR1)
            GO TO 30
         ENDIF
      ENDIF
      CALL MZBOOK(IHDIV,LBANY,LR1,JBIAS,'BANY',1,1,2,3,0)
      Q(LBANY+1)=FLOARG(XX0)
      Q(LBANY+2)=FLOARG(XX1)
      CALL MZBOOK(IHDIV,LCONT,LBANY,-1,'BANY',2,2,NW,IODES,0)
      IQ(LCONT+KNBIT)=NBPROX
      IF(I10.NE.0)THEN
         CALL MZBOOK(IHDIV,LW,LCONT,0,'BAYE',0,0,NC,3,0)
      ENDIF
      CALL HSIFLA(8,1)
      IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
*
   40 IRET = 2
      GO TO 20
  99  RETURN
      END
