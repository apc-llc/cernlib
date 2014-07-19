*
* $Id: hpstat.F,v 1.1.1.1 1996/01/16 17:07:47 mclareni Exp $
*
* $Log: hpstat.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:47  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.23/01 20/02/95  09.51.33  by  Julian Bunn
*-- Author :
      SUBROUTINE HPSTAT(IDD)
*.==========>
*.           To print statistics for ID
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcflag.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcprin.inc"
#include "hbook/hcunit.inc"
      DIMENSION STAT(9)
*.___________________________________________
*
      IRET=1
  10  CALL HLOOP(IDD,'HSTAT ',IRET)
      IF(IRET.EQ.0)GO TO 99
      CALL HDCOFL
*
*            1-DIM histogram
*
      IF(I1.NE.0)THEN
         LCONT=LQ(LCID-1)
         NB=IQ(LCONT+KNBIT)
         NCX=IQ(LCID+KNCX)
         NWTIT=IQ(LCID-1)-KTIT1+1
         WRITE(LOUT,1000)ID,(IQ(LCID+KTIT1+I-1),I=1,NWTIT)
         ALLCHA=0.
         DO 15 I=1,NCX
            ALLCHA=ALLCHA+HCX(I,1)
  15     CONTINUE
         UNDER=HCX(0,1)
         OVER=HCX(NCX+1,1)
         WRITE(LOUT,2000)IQ(LCONT+KNOENT),ALLCHA
         WRITE(LOUT,3000)UNDER,OVER
         XMEAN=HSTATI(ID,1,'HIST',1)
         XRMS =HSTATI(ID,2,'HIST',1)
         WRITE(LOUT,4000)XMEAN,XRMS
         GO TO 90
      ENDIF
*
*            2-DIM histogram
*
      IF(I230.NE.0)THEN
         LCONT=LQ(LCID-1)
         LSCAT=LCONT
         NB=IQ(LCONT+KNBIT)
         NCX=IQ(LCID+KNCX)
         NCY=IQ(LCID+KNCY)
         NWTIT=IQ(LCID-1)-KTIT2+1
         WRITE(LOUT,1000)ID,(IQ(LCID+KTIT2+I-1),I=1,NWTIT)
         WRITE(LOUT,5000)IQ(LCONT+KNOENT)
C
C             STAT(1)= (X-UFLW, Y-OFLW)
C
         STAT(1)=HCXY(0,NCY+1,1)
         STAT(2)=0.
C
C             STAT(3)= (X-UFLW, Y-OFLW)
C
         STAT(3)=HCXY(NCX+1,NCY+1,1)
         STAT(4)=0.
         STAT(5)=0.
         STAT(6)=0.
C
C             STAT(4)= (X-UFLW, Y-RANGE)
C             STAT(5)= (X-RANGE, Y-RANGE)
C             STAT(6)= (X-OFLW, Y-RANGE)
C
         DO 40 I=1,NCY
            STAT(4)=STAT(4)+HCXY(0,I,1)
            DO 45 J=1,NCX
               STAT(5)=STAT(5)+HCXY(J,I,1)
  45        CONTINUE
            STAT(6)=STAT(6)+HCXY(NCX+1,I,1)
  40     CONTINUE
C
C             STAT(7)= (X-UFLW, Y-UFLW)
C
         STAT(7)=HCXY(0,0,1)
         STAT(8)=0.
C
C             STAT(2)= (X-RANGE, Y-OFLW)
C             STAT(8)= (X-RANGE, Y-UFLW)
C
         DO 30 I=1,NCX
            STAT(2)=STAT(2)+HCXY(I,NCY+1,1)
            STAT(8)=STAT(8)+HCXY(I,0,1)
  30     CONTINUE
C
C             STAT(9)= (X-OFLW, Y-UFLW)
C
         STAT(9)=HCXY(NCX+1,0,1)
         WRITE(LOUT,6100)STAT(1),STAT(2),STAT(3)
         WRITE(LOUT,6200)
         WRITE(LOUT,6100)STAT(4),STAT(5),STAT(6)
         WRITE(LOUT,6200)
         WRITE(LOUT,6100)STAT(7),STAT(8),STAT(9)
         GO TO 90
      ENDIF
*
*            N-Tuple
*
      IF(I4.NE.0)THEN
         IF (IQ(LCID-2) .EQ. 2) THEN
            CALL HPRNTU(ID)
         ELSE
            CALL HPRNT(ID)
         ENDIF
      ENDIF
*
  90  IRET=2
      GO TO 10
*
 1000 FORMAT(/,' =====> ID =',I8,3X,20A4)
 2000 FORMAT(8X,' ENTRIES    =',I9, 9X,  ' ALL CHANN =',G13.7)
 3000 FORMAT(8X,' UNDERFLOW  =',G13.7,5X,' OVERFLOW  =',G13.7)
 4000 FORMAT(8X,' MEAN VALUE =',G13.7,5X,' R . M . S =',G13.7)
 5000 FORMAT(8X,' ENTRIES    =',I9)
 6100 FORMAT(8X,1X,G13.7,' I ',G13.7,' I ',G13.7)
 6200 FORMAT(8X,' --------------I---------------I--------------')
  99  RETURN
      END
