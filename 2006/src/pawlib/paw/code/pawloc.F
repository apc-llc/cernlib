*
* $Id: pawloc.F,v 1.1.1.1 1996/03/01 11:38:43 mclareni Exp $
*
* $Log: pawloc.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/08 08/06/94  10.14.01  by  O.Couet
*-- Author :    Rene Brun   03/01/89
      SUBROUTINE PAWLOC(NP,XP,YP,NTPRI,IWKID,CHOPT)
*
************************************************************************
*
* Locate pairs of points on normalization transformation NTPRI and
* store them in XP(1:NP) and YP(1:NP).
*
* If NTPRI<0 the last (current) normalization transformation is used.
*
* NP in input contains the maximum dimension of arrays XP and YP.
*
* CHOPT='*' to draw a '*' at each point located
* CHOPT='P' to draw the current polymarker at each point located
* CHOPT='+' to use tracking cross (default is cross-hair)
* CHOPT='-' to use rubber line in locating points
* CHOPT='L' to connect together, with a polyline, all the points located
* CHOPT='S' the sample mode is used (if possible)
*
*     Input/Output :
*
      INTEGER NP
      REAL XP(*)
      REAL YP(*)
      INTEGER NTPRI
*
*     Input :
*
      CHARACTER*(*) CHOPT
*
************************************************************************
*
#include "paw/quest.inc"
      REAL X(2),Y(2)
      CHARACTER*20 CHLOC(3)
      DIMENSION IOPT(6)
      EQUIVALENCE (IOPT(1),IOPTST),(IOPT(2),IOPTPL),(IOPT(3),IOPTMI)
      EQUIVALENCE (IOPT(4),IOPTP) ,(IOPT(5),IOPTL) ,(IOPT(6),IOPTS)
      LOGICAL LOLOGX,LOLOGY
*
*-----------------------------------------------------------------------
*
      MAXP=NP
      CALL UOPTC(CHOPT,'*+-PLS',IOPT)
      CALL IGQ('MTYP',RMKOLD)
      IF (IOPTST.GT.0) CALL IGSET('MTYP',3.)
      NP=0
      CALL HPLGIV(XL,YL,XH,YH)
      NTHIST=IQUEST(12)
*
   10 LCDNR = 21
      IF (NP.EQ.0) THEN
         IF (IOPTPL.GT.0) LCDNR = 11
      ELSE
         IF (IOPTMI.GT.0) LCDNR = 41
         IF (IOPTPL.GT.0) LCDNR = 11
      ENDIF
      IF(IOPTS.NE.0)LCDNR=-LCDNR
*
      IF(NTPRI.GE.0)THEN
         IF(NTPRI.NE.1)CALL ISELNT(1)
         CALL ISELNT(NTPRI)
      ELSE
         CALL ISELNT(1)
         DO 20 I=10,NTHIST,10
            CALL ISELNT(I)
   20    CONTINUE
      ENDIF
*
   30 CALL IRQLC(IWKID,LCDNR,ISTAT,NT,XX,YY)
      IF (ISTAT.EQ.0) GO TO 40
*
      XLOC   = XX
      YLOC   = YY
      LOLOGX = .FALSE.
      LOLOGY = .FALSE.
      CALL HPLCHA(NT,XX,YY,IDH,XLOC,YLOC,ICX,ICY)
      IF (IQUEST(1).NE.0) LOLOGX=.TRUE.
      IF (IQUEST(2).NE.0) LOLOGY=.TRUE.
      IF (IOPTS.NE.0.AND.ISTAT.NE.1)THEN
         WRITE(CHLOC(1),'('' NP ='',I8)')    NP+1
         WRITE(CHLOC(2),'('' X  ='',G12.4)') XLOC
         WRITE(CHLOC(3),'('' Y  ='',G12.4)') YLOC
         CALL IGMESS(3,CHLOC,'VLOCATE','P')
         GOTO 30
      ENDIF
*
      IF (NP.EQ.0) THEN
         NTPRI=NT
         CALL ISELNT(NTPRI)
      ENDIF
      IF (NT.EQ.NTPRI) THEN
         NP=NP+1
         IF (NP.GT.MAXP) THEN
            NP=NP-1
            CALL KUALFA
            PRINT 10000,MAXP
10000     FORMAT(' Maximum number of points=',I3,'  reached')
            GO TO 40
         ENDIF
         XP(NP) = XLOC
         YP(NP) = YLOC
         X(2)   = XLOC
         IF (LOLOGX.AND.X(2).GT.0.) X(2)=LOG10(X(2))
         Y(2)   = YLOC
         IF (LOLOGY.AND.Y(2).GT.0.) Y(2)=LOG10(Y(2))
         IF (IOPTP.GT.0.OR.IOPTST.GT.0) CALL IPM(1,X(2),Y(2))
         IF ((NP.GT.1).AND.(IOPTL.GT.0.OR.IOPTMI.GT.0)) CALL IPL(2,X,Y)
         X(1)  = XLOC
         IF (LOLOGX.AND.X(1).GT.0.) X(1)=LOG10(X(1))
         Y(1)  = YLOC
         IF (LOLOGY.AND.Y(1).GT.0.) Y(1)=LOG10(Y(1))
      ENDIF
      GO TO 10
*
   40 CALL IGSET('MTYP',RMKOLD)
      IF (IOPTS.NE.0) CALL IGMESS(1,' ',' ','C')
*
      END
