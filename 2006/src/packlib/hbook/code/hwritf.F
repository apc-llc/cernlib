*
* $Id: hwritf.F,v 1.1.1.1 1996/01/16 17:07:49 mclareni Exp $
*
* $Log: hwritf.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 09/10/92  17.51.13  by  Janusz Chwastowski
*-- Author :
      SUBROUTINE HWRITF (ITAP)
*.==========>
*.      WRITE FORTRAN FUNCTION FPARAM ON TAPE ITAP
*.      THE FORTRAN CODE GENERATED COMPUTES THE
*.      REGRESSION VALUE AT A POINT X AND IS COMPLETELY
*.      INDEPENDENT FROM THE HPARAM PACKAGE
*..=========> ( D.Lienart )
      CHARACTER NORM*1
#include "hbook/hcpar1.inc"
#include "hbook/hcpar2.inc"
#if defined(CERNLIB_DOUBLE)
      DOUBLE PRECISION COEFF
#endif
*
*  WRITE DECLARATIONS AND POSSIBLY NORMALIZATION CODE
*
      NORM=' '
      IF (IOPT(8).GT.0) NORM='N'
      WRITE (ITAP,500)
      WRITE (ITAP,510) NCO,ND,NCO
      IF (IOPT(8).EQ.3) WRITE (ITAP,520) ND,ND
      IF (IOPT(8).EQ.1.OR.IOPT(8).EQ.2) WRITE (ITAP,530) ND,ND,ND,ND,ND
      WRITE (ITAP,540) (COEFF(I),I=1,NCO)
      WRITE (ITAP,560)
      WRITE (ITAP,550) ((IBASFT(I,J),I=1,ND),J=1,NCO)
      WRITE (ITAP,560)
      IF (IOPT(8).EQ.3) THEN
         WRITE (ITAP,570) (XMIN(I),I=1,ND)
         WRITE (ITAP,590) ND
      ENDIF
      IF (IOPT(8).EQ.1.OR.IOPT(8).EQ.2) THEN
         WRITE (ITAP,580) (XMIN(I),I=1,ND),(XMAX(I),I=1,ND),
     +   (ALIM(I),I=1,ND),(BLIM(I),I=1,ND)
         WRITE (ITAP,600) ND
      ENDIF
*
*  WRITE REGRESSION CODE
*
      WRITE (ITAP,610) NCO,ND
      IF (IOPT(4).EQ.0) WRITE (ITAP,620) NORM,NORM
      IF (IOPT(4).EQ.1) WRITE (ITAP,630) NORM,NORM
      IF (IOPT(4).EQ.2) WRITE (ITAP,640) NORM,NORM
      IF (IOPT(4).EQ.3) WRITE (ITAP,650) NORM,NORM
      IF (IOPT(4).EQ.4) WRITE (ITAP,660) NORM,NORM
      IF (IOPT(4).EQ.5) WRITE (ITAP,670) NORM,NORM
      WRITE (ITAP,680) NORM,NORM
*
#if !defined(CERNLIB_DOUBLE)
  500 FORMAT (6X,'FUNCTION FPARAM (X)')
#endif
#if defined(CERNLIB_DOUBLE)
  500 FORMAT (6X,'DOUBLE PRECISION FUNCTION FPARAM (X)',/,
     +        6X,'DOUBLE PRECISION COEFF,P,P0,P1,P2,HELEFT,HBASFT')
#endif
  510 FORMAT (6X,'DIMENSION X(1),COEFF(',I2,'),IBASFT(',I2,',',I2,')')
  520 FORMAT (5X,'+,XN(',I2,'),XMIN(',I2,')')
  530 FORMAT (5X,'+,XN(',I2,'),XMIN(',I2,'),XMAX(',I2,'),ALIM(',I2,
     +        '),BLIM(',I2,')')
#if !defined(CERNLIB_DOUBLE)
  540 FORMAT (6X,'DATA COEFF/',4(G12.5,:,','),14(/,5X,'+',
     +        4(G12.5,:,',')))
#endif
#if defined(CERNLIB_DOUBLE)
  540 FORMAT (6X,'DATA COEFF/',3(D15.8,:,','),19(/,5X,'+',
     +        3(D15.8,:,',')))
#endif
  550 FORMAT (6X,'DATA IBASFT/',12(I3,:,','),49(/,5X,'+',
     +        12(I3,:,',')))
  560 FORMAT (5X,'+/')
  570 FORMAT (6X,'DATA XMIN/',4(E12.5,:','),/,5X,'+',4(E12.5,:,','),
     +        /,5X,'+',2(E12.5,:,','))
  580 FORMAT (6X,'DATA XMIN,XMAX,ALIM,BLIM/',2(E12.5,:','),10(/,5X,'+',
     +        4(E12.5,:',')))
  590 FORMAT (5X,'+/',/,6X,'DO 5 I=1,',I2,/,4X,'5 XN(I)=X(I)-XMIN(I)')
  600 FORMAT (5X,'+/',/,6X,'DO 5 I=1,',I2,/,4X,'5 XN(I)=ALIM(I)+',
     +        '(BLIM(I)-ALIM(I))*(X(I)-XMIN(I))/(XMAX(I)-XMIN(I))')
  610 FORMAT (6X,'FPARAM=0.',/,6X,'DO 25 K=1,',I2,/,6X,'P=1.',/,6X,
     +        'DO 15 I=1,',I2,/,6X,'NUM=IBASFT(I,K)/10',/,6X,
     +        'ITYP=IBASFT(I,K)-NUM*10',/,6X,'IF (NUM.NE.0) THEN',/,
     +        6X,'IF (ITYP.EQ.0) THEN',/,6X,'P0=1.')
  620 FORMAT (6X,'P1=X',A1,'(I)',/,6X,'DO 10 J=2,NUM',/,
     +        6X,'P2=P1*X',A1,'(I)')
  630 FORMAT (6X,'P1=X',A1,'(I)',/,6X,'DO 10 J=2,NUM',/,
     +        6X,'P2=2*X',A1,'(I)*P1-P0')
  640 FORMAT (6X,'P1=X',A1,'(I)',/,6X,'DO 10 J=2,NUM',/,
     +        6X,'P2=(2*J-1)/J*P1*X',A1,'(I)-(J-1)/J*P0')
  650 FORMAT (6X,'P1=2*X',A1,'(I)-1.',/,6X,'DO 10 J=2,NUM'
     +        ,/,6X,'P2=2*(2*X',A1,'(I)-1.)*P1-P0')
  660 FORMAT (6X,'P1=1-X',A1,'(I)',/,6X,'DO 10 J=2,NUM',/,
     +        6X,'P2=(2*J-1-X',A1,'(I))*P1-(J-1)**2*P0')
  670 FORMAT (6X,'P1=2*X',A1,'(I)',/,6X,'DO 10 J=2,NUM',/,
     +        6X,'P2=2*X',A1,'(I)*P1-2*(J-1)*P0')
  680 FORMAT (6X,'P0=P1',/,3X,'10 P1=P2',/,6X,'P=P*P1',/,6X,'END IF',
     +        /,6X,'IF (ITYP.EQ.1) P=P*HELEFT(NUM,X',A1,'(I))',/,6X,
     +        'IF (ITYP.EQ.2) THEN',/,6X,'P=HBASFT(NUM,X',A1,')',/,6X,
     +        'GOTO 20',/,6X,'END IF',/,6X,'END IF',/,3X,'15 CONTINUE'
     +        ,/,3X,'20 FPARAM=FPARAM+COEFF(K)*P',/,3X,'25 CONTINUE',
     +        /,6X,'RETURN',/,6X,'END')
      END
