*
* $Id: hbary.F,v 1.1.1.1 1996/01/16 17:07:31 mclareni Exp $
*
* $Log: hbary.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:31  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.10/05 30/06/89  10.51.36  by  Rene Brun
*-- Author :
      SUBROUTINE HBARY(IDD)
*.==========>
*.           create bank to store sum of square of weigths
*.           for projections  Y
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
*.___________________________________________
      IRET = 3
   10 CALL HLOOP (IDD,'HBARY ',IRET)
      IF (IRET .EQ. 0)                 GO TO 999
      CALL HDCOFL
      IF(I10.NE.0)GO TO 100
      IF(I230.EQ.0)GO TO 100
      NCY=IQ(LCID+KNCY)
      NTOT=0
*
*            PROY
*
      LPROY=LQ(LCID-3)
      IF(LPROY.NE.0)THEN
         NW=NCY+10
         CALL HSPACE(NW,'HBARY ',IDD)
         IF(IERR.NE.0)GO TO 100
         CALL MZBOOK(IHDIV,LW,LPROY,0,'PRYE',0,0,NCY,3,0)
         NTOT=NTOT+NW
         LKEEP=LCONT
         LCONT=LPROY
         NB=IQ(LCONT+KNBIT)
         DO 16 I=1,NCY
            Q(LW+I)=HCX(I,1)
  16     CONTINUE
         LCONT=LKEEP
      ENDIF
*
*            SLIY
*
      LSLIY=LQ(LCID-5)
      IF(LSLIY.NE.0)THEN
         NW=(NCY+10)*IQ(LSLIY-2)
         CALL HSPACE(NW,'HBARY ',IDD)
         IF(IERR.NE.0)GO TO 100
         DO 20 I=1,IQ(LSLIY-2)
            LCONT=LQ(LSLIY-I)
            CALL MZBOOK(IHDIV,LW,LCONT,0,'SLYE',0,0,NCY,3,0)
            NB=IQ(LCONT+KNBIT)
            DO 17 J=1,NCY
               Q(LW+J)=HCX(J,1)
  17        CONTINUE
  20     CONTINUE
         NTOT=NTOT+NW
      ENDIF
*
*             BANY
*
      LBANY=LQ(LCID-7)
  30  IF(LBANY.NE.0)THEN
         NW=NCY+10
         CALL HSPACE(NW,'HBARY ',IDD)
         IF(IERR.NE.0)GO TO 100
         LCONT=LQ(LBANY-1)
         CALL MZBOOK(IHDIV,LW,LCONT,0,'BAYE',0,0,NCY,3,0)
         NB=IQ(LCONT+KNBIT)
         DO 35 I=1,NCY
            Q(LW+I)=HCX(I,1)
  35     CONTINUE
         NTOT=NTOT+NW
         LBANY=LQ(LBANY)
         GO TO 30
      ENDIF
*
      CALL HSIFLA(10,1)
      CALL HSIFLA(11,0)
      IQ(LCID+KNTOT)=IQ(LCID+KNTOT)+NTOT
*
  100 IRET  = 2
      GO TO 10
 999  RETURN
      END
