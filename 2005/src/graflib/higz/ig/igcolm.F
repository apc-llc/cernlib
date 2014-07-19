*
* $Id: igcolm.F,v 1.2 2002/04/04 10:18:20 couet Exp $
*
* $Log: igcolm.F,v $
* Revision 1.2  2002/04/04 10:18:20  couet
* - new routine IGCOL1 allowing to modify the number of axis divisions on
*   the axis color palette
*
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.23/06 10/11/95  09.45.01  by  O.Couet
*-- Author : O.Couet
      SUBROUTINE IGCOLM(X1I,X2I,Y1I,Y2I,IC1,IC2,ZMIN,ZMAX,CHOPT)
#include "higz/hiatt.inc"
      CHARACTER*(*) CHOPT
      CHARACTER*8 CHOPAX
      DIMENSION IC2(*),IOPT(9)
      EQUIVALENCE (IOPTC,IOPT(1)),(IOPTB,IOPT(2))
      EQUIVALENCE (IOPTA,IOPT(3)),(IOPTH,IOPT(4))
      EQUIVALENCE (IOPTP,IOPT(5)),(IOPTG,IOPT(6))
      EQUIVALENCE (IOPTL,IOPT(7)),(IOPTR,IOPT(8))
      EQUIVALENCE (IOPTT,IOPT(9))
      DIMENSION LUT(8)
      COMMON /HICOLM/ NDIVZ
      DATA NDIVZ /10/
      DATA LUT /0,5,7,3,6,2,4,1/
*.______________________________________
*
      CALL UOPTC(CHOPT,'CBAHPGLRT',IOPT)
      X1 = X1I
      X2 = X2I
      Y1 = Y1I
      Y2 = Y2I
*
      IF(IOPTL.NE.0)THEN
         IF(INBCOL.GT.8)THEN
            IC1=8
            IC2(1)=INBCOL
         ELSE
            IC1=0
            IC2(1)=7
         ENDIF
      ENDIF
*
      IF(X1.EQ.X2.OR.Y1.EQ.Y2)THEN
         XX = 0.01*(RWXMAX-RWXMIN)
         X1 = RWXMAX+XX
         X2 = RWXMAX+4.*XX
         Y1 = RWYMIN
         Y2 = RWYMAX
      ENDIF
*
      IF(IOPTP.NE.0)THEN
         N=IC1
      ELSE
         N=IC2(1)-IC1+1
      ENDIF
      DY=(Y2-Y1)/FLOAT(N)
      DX=(X2-X1)/FLOAT(N)
*
*              Draw the levels with colors
*
      IF(IOPTC.NE.0)THEN
         CALL IGSET('BORD',0.)
         CALL IGSET('PLCI',1.)
         CALL IGSET('FAIS',1.)
         DO 10 I=1,N
            IF(IOPTP.NE.0)THEN
               ICOL=IC2(I)
            ELSE
               ICOL=IC1+I-1
            ENDIF
            IF(ICOL.LE.7.AND.IOPTP.EQ.0)THEN
               CALL ISFACI(LUT(ICOL+1))
            ELSE
               CALL ISFACI(ICOL)
            ENDIF
            IF(IOPTH.NE.0)THEN
               CALL IGBOX(X1+DX*FLOAT(I-1),X1+DX*FLOAT(I),Y1,Y2)
            ELSE
               CALL IGBOX(X1,X2,Y1+DY*FLOAT(I-1),Y1+DY*FLOAT(I))
            ENDIF
   10    CONTINUE
         CALL IGSET('BORD',0.)
         CALL IGSET('FAIS',0.)
         CALL IGBOX(X1,X2,Y1,Y2)
      ENDIF
*
*              Draw the levels with boxes
*
      IF(IOPTB.NE.0)THEN
         IF(IOPTH.NE.0)THEN
            YS=(Y2-Y1)/(2.*FLOAT(N))
            XS=DX/(2.*FLOAT(N))
         ELSE
            XS=(X2-X1)/(2.*FLOAT(N))
            YS=DY/(2.*FLOAT(N))
         ENDIF
         DO 20 I=1,N
            ST=FLOAT(N-I)
            IF(IOPTH.NE.0)THEN
               CALL IGBOX(X1+DX*FLOAT(I-1)+ST*XS,X1+DX*FLOAT(I)-ST*XS
     +,                   Y1+ST*YS,Y2-ST*YS)
            ELSE
               CALL IGBOX(X1+ST*XS,X2-ST*XS
     +,                   Y1+DY*FLOAT(I-1)+ST*YS,Y1+DY*FLOAT(I)-ST*YS)
            ENDIF
   20    CONTINUE
      ENDIF
*
*              Draw the axis
*
      IF(IOPTA.NE.0)THEN
         IF(IOPTH.NE.0)THEN
            CALL IGSET('TMSI',Y2-Y1)
            IF(IOPTG.NE.0)THEN
               IF(ZMAX.GT.0.)THEN
                  ZMING = ZMIN
                  IF(ZMIN.LE.0.)ZMING = 0.001*ZMAX
                  IF(IOPTT.NE.0)THEN
                     CHOPAX = '-DHSG'
                     XX1    = X1
                     XX2    = X2
                     YY1    = Y2
                     YY2    = Y2
                  ELSE
                     CHOPAX = 'DHSG'
                     XX1    = X1
                     XX2    = X2
                     YY1    = Y1
                     YY2    = Y1
                  ENDIF
               ENDIF
            ELSE
               IF(IOPTT.NE.0)THEN
                  CHOPAX = '-DHS'
                  XX1    = X1
                  XX2    = X2
                  YY1    = Y2
                  YY2    = Y2
               ELSE
                  CHOPAX = 'DHS'
                  XX1    = X1
                  XX2    = X2
                  YY1    = Y1
                  YY2    = Y1
               ENDIF
            ENDIF
         ELSE
            CALL IGSET('TMSI',X2-X1)
            IF(IOPTG.NE.0)THEN
               IF(ZMAX.GT.0.)THEN
                  ZMING = ZMIN
                  IF(ZMIN.LE.0.)ZMING = 0.001*ZMAX
                  IF(IOPTR.NE.0)THEN
                     CHOPAX = 'DHSGR'
                     XX1    = X2
                     XX2    = X2
                     YY1    = Y1
                     YY2    = Y2
                  ELSE
                     CHOPAX = 'DHS+GL'
                     XX1    = X2
                     XX2    = X2
                     YY1    = Y1
                     YY2    = Y2
                  ENDIF
               ENDIF
            ELSE
               IF(IOPTR.NE.0)THEN
                  CHOPAX = 'DHSR'
                  XX1    = X1
                  XX2    = X1
                  YY1    = Y1
                  YY2    = Y2
               ELSE
                  CHOPAX = 'DHS+L'
                  XX1    = X2
                  XX2    = X2
                  YY1    = Y1
                  YY2    = Y2
               ENDIF
            ENDIF
         ENDIF
         IF (NDIVZ.LT.0) THEN
            CHOPAX(8:8) = 'N'
            NDIV = -NDIVZ
         ELSE
            NDIV = NDIVZ
         ENDIF
         CALL IGAXIS(XX1,XX2,YY1,YY2,ZMIN,ZMAX,NDIV,CHOPAX)
      ENDIF
*
      END

      SUBROUTINE IGCOL1(NDIVZI)
      COMMON /HICOLM/ NDIVZ
      NDIVZ = MOD(NDIVZI,100)
      END
