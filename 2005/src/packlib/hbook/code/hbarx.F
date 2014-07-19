*
* $Id: hbarx.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hbarx.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 30/06/89  10.52.39  by  Rene Brun
*-- Author :
      SUBROUTINE HBARX(IDD)
*.==========>
*.           create bank to store sum of square of weigths
*.           for projections  X
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      IRET = 3
   10 CALL HLOOP (IDD,'HBARX ',IRET)
      IF (IRET .EQ. 0)                 GO TO 999
      CALL HDCOFL
      IF(I9.NE.0)GO TO 100
      NCX=IQ(LCID+KNCX)
*
*            1-DIM histogram
*
      IF(I1.NE.0)THEN
         LCONT=LQ(LCID-1)
         NTOT=IQ(LCID+KNCX)+10
         CALL HSPACE(NTOT,'HBARX ',IDD)
         IF(IERR.NE.0)GO TO 100
         CALL MZBOOK(IHDIV,LW,LCONT,0,'HI1E',0,0,NCX,3,0)
         NB=IQ(LCONT+KNBIT)
         DO 15 I=1,NCX
            Q(LW+I)=HCX(I,1)
  15     CONTINUE
         GO TO 80
      ENDIF
*
*            2-DIM histogram
*
      IF(I230.EQ.0)GO TO 100
      NTOT=0
*
*            PROX
*
      LPROX=LQ(LCID-2)
      IF(LPROX.NE.0)THEN
         NW=NCX+10
         CALL HSPACE(NW,'HBARX ',IDD)
         IF(IERR.NE.0)GO TO 100
         CALL MZBOOK(IHDIV,LW,LPROX,0,'PRXE',0,0,NCX,3,0)
         NTOT=NTOT+NW
         LKEEP=LCONT
         LCONT=LPROX
         NB=IQ(LCONT+KNBIT)
         DO 16 I=1,NCX
            Q(LW+I)=HCX(I,1)
  16     CONTINUE
         LCONT=LKEEP
      ENDIF
*
*            SLIX
*
      LSLIX=LQ(LCID-4)
      IF(LSLIX.NE.0)THEN
         NW=(NCX+10)*IQ(LSLIX-2)
         CALL HSPACE(NW,'HBARX ',IDD)
         IF(IERR.NE.0)GO TO 100
         DO 20 I=1,IQ(LSLIX-2)
            LCONT=LQ(LSLIX-I)
            CALL MZBOOK(IHDIV,LW,LCONT,0,'SLXE',0,0,NCX,3,0)
            NB=IQ(LCONT+KNBIT)
            DO 17 J=1,NCX
               Q(LW+J)=HCX(J,1)
  17        CONTINUE
  20     CONTINUE
         NTOT=NTOT+NW
      ENDIF
*
*             BANX
*
      LBANX=LQ(LCID-6)
  30  IF(LBANX.NE.0)THEN
         NW=NCX+10
         CALL HSPACE(NW,'HBARX ',IDD)
         IF(IERR.NE.0)GO TO 100
         LCONT=LQ(LBANX-1)
         CALL MZBOOK(IHDIV,LW,LCONT,0,'BAXE',0,0,NCX,3,0)
         NB=IQ(LCONT+KNBIT)
         DO 35 I=1,NCX
            Q(LW+I)=HCX(I,1)
  35     CONTINUE
         NTOT=NTOT+NW
         LBANX=LQ(LBANX)
         GO TO 30
      ENDIF
*
  80  CALL HSIFLA (9,1)
      CALL HSIFLA(11,0)
      IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
*
  100 IRET  = 2
      GO TO 10
 999  RETURN
      END
