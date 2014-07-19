*
* $Id: igrng.F,v 1.1.1.1 1996/02/14 13:10:28 mclareni Exp $
*
* $Log: igrng.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:28  mclareni
* Higz
*
*
#if defined(CERNLIB_DI3000)
#include "higz/pilot.h"
*CMZ :  1.07/01 06/06/89  09.18.17  by  O.Couet
*-- Author :
      SUBROUTINE IGRNG(XSIZ,YSIZ)
*.==========>
*.
*.   This routines returns the screen range and map the workstation window
*. on the whole screen. If the screen is not a square a part of the NDC space
*. is not visible .
*.
*. _Input parameters:
*.
*.  INTEGER WKID : Worsktation identifier .
*.  REAL XMAX YMAX : Screen range .
*.
*..=========> (O.Couet, H.Johnstad, L.Roberts)
#include "higz/hipaw.inc"
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/hiflag.inc"
#endif
#include "higz/hiatt.inc"
#include "higz/himeta.inc"
      INTEGER*4 MAXDEV
      PARAMETER (MAXDEV = 9)
      INTEGER*4 DEVICES(0:MAXDEV)
      EQUIVALENCE (ILOPWK,DEVICES)

#include "higz/di3seg.inc"
      INTEGER WKID
      REAL XMAX,YMAX
      INTEGER LIST
      DIMENSION LIST(4)
      REAL MAXDIM(4)
      LOGICAL PHYS
*.______________________________________
*
#if defined(CERNLIB_MAIL)
*
*              Mail option
*
      IF(MFLAG)THEN
         WRITE (CHMAIL,'(I3,2E16.7)') 201,XSIZ,YSIZ
         CALL IMWRIT(1)
         READ (5,'(5E15.6)') RQUEST(11),RQUEST(12),RQUEST(13)
     +,                      RQUEST(14),RQUEST(15)
         CALL IMWRIT(5)
         MFLAG=.FALSE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         CALL ISWN(1,0.,XSIZ,0.,YSIZ)
         CALL ISVP(1,RQUEST(12),RQUEST(14)+RQUEST(12)
     +,              RQUEST(13),RQUEST(15)+RQUEST(13))
         CALL ISELNT(1)
         MFLAG=.TRUE.
         GLFLAG=(ZFLAG.OR.PFLAG.OR.MFLAG)
         RETURN
      ENDIF
#endif
*
      IF(XSIZ.LE.0..OR.YSIZ.LE.0.)THEN
         CALL IGERR('X size and Y size must be positive','IGRNG')
         GO TO 999
      ENDIF
      IF(SEGOPN) THEN
         CALL JCLOSE
         SEGOPN = .FALSE.
      ENDIF
      CALL IUWK(IDID,1)
*
*     Perform calculations for primary workstation (IDID).
*
      IF(TEKACT.OR.METACT)THEN
*
*        DETERMINE IF THE DEVICE IS CAPABLE OF PHYSICAL SIZING AND
*        THE PHYSICAL SIZE OF THE VIEWPORT
*
         WKID = IDID
         IF (WKID.NE.0) THEN
            CALL JIQDIM(WKID,PHYS,MAXDIM)
            XMIN = MAXDIM(1)
            XMAX = MAXDIM(2)
            YMIN = MAXDIM(3)
            YMAX = MAXDIM(4)
         ELSE
            CALL JIQDEV(WKID,20,LIST)
            XMIN = FLOAT(LIST(1))
            XMAX = FLOAT(LIST(2))
            YMIN = FLOAT(LIST(3))
            YMAX = FLOAT(LIST(4))
         ENDIF
*
*        SET THE ORIENTATION OF THE MAXIMUMS RETURNED SINCE NOT ALL
*        DEVICES REFER TO THE LOWER LEFT AS X0PHY, Y0PHY AND UPPER RIGHT
*        AS X1PHY, Y1PHY.
*
         IF (XMIN .GT. XMAX) THEN
            TEMP = XMIN
            XMIN = XMAX
            XMAX = TEMP
         ENDIF
         IF (YMIN .GT. YMAX) THEN
            TEMP = YMIN
            YMIN = YMAX
            YMAX = TEMP
         ENDIF
*
         IF (WKID.NE.0) CALL ISWKVP(WKID,XMIN,XMAX,YMIN,YMAX)
*
*              Ecran horizontal
*
         IF(XMAX.GE.YMAX)THEN
            XWKWN=1.
            YWKWN=YMAX/XMAX
            CALL ISWKWN(WKID,0.,1.,0.,YWKWN)
         ELSE
*
*              Ecran vertical
*
            YWKWN=1.
            XWKWN=XMAX/YMAX
            CALL ISWKWN(WKID,0.,XWKWN,0.,1.)
         ENDIF
      ENDIF
*
*     Perform calculations for secondary workstations.
*
      IF(TEKACT.OR.METACT)THEN
*
*        DETERMINE IF THE DEVICE IS CAPABLE OF PHYSICAL SIZING AND
*        THE PHYSICAL SIZE OF THE VIEWPORT
*
         DO 100 WKID = 0,MAXDEV
            IF ((DEVICES(WKID).NE.0).AND.(DEVICES(WKID).NE.IDID)) THEN
               IF (WKID.NE.0) THEN
                  CALL JIQDIM(WKID,PHYS,MAXDIM)
                  XMINT = MAXDIM(1)
                  XMAXT = MAXDIM(2)
                  YMINT = MAXDIM(3)
                  YMAXT = MAXDIM(4)
               ELSE
                  CALL JIQDEV(WKID,20,LIST)
                  XMINT = FLOAT(LIST(1))
                  XMAXT = FLOAT(LIST(2))
                  YMINT = FLOAT(LIST(3))
                  YMAXT = FLOAT(LIST(4))
               ENDIF
C
C              SET THE ORIENTATION OF THE MAXIMUMS RETURNED SINCE NOT ALL
C              DEVICES REFER TO THE LOWER LEFT AS X0PHY, Y0PHY AND UPPER RIGHT
C              AS X1PHY, Y1PHY.
C
               IF (XMINT .GT. XMAXT) THEN
                  TEMP  = XMINT
                  XMINT = XMAXT
                  XMAXT = TEMP
               ENDIF
               IF (YMINT .GT. YMAXT) THEN
                  TEMP  = YMINT
                  YMINT = YMAXT
                  YMAXT = TEMP
               ENDIF
*
               IF (WKID.NE.0) THEN
                  IF ((XMAXT/XMAX).GE.(YMAXT/YMAX)) THEN
                     CALL ISWKVP(WKID,
     +                  MAX(0.5*(XMAXT-(XMAX/YMAX)*YMAXT),XMINT),
     +                  MIN(0.5*(XMAXT+(XMAX/YMAX)*YMAXT),XMAXT),
     +                  YMINT,YMAXT)
                  ELSE
                     CALL ISWKVP(WKID,XMINT,XMAXT,
     +                  MAX(0.5*(YMAXT-(YMAX/XMAX)*XMAXT),YMINT),
     +                  MIN(0.5*(YMAXT+(YMAX/XMAX)*XMAXT),YMAXT))
                  ENDIF
               ENDIF
*
*              Ecran horizontal
*
               IF(XMAX.GE.YMAX)THEN
                  CALL ISWKWN(WKID,0.,1.,0.,YWKWN)
               ELSE
*
*              Ecran vertical
*
                  CALL ISWKWN(WKID,0.,XWKWN,0.,1.)
               ENDIF
            ENDIF
  100    CONTINUE
      ENDIF
*
*              Transformation no 1 : cm <---> espace ecran
*
      IF (XSIZ.LT.YSIZ) THEN
         XNCM=YWKWN*XSIZ/YSIZ
         YNCM=YWKWN
         DXWN=(XWKWN-XNCM)/2.
         IF(DXWN.LT.0.)THEN
            XNCM=XWKWN
            DXWN=0.
            YNCM=XWKWN*YSIZ/XSIZ
            DYWN=(YWKWN-YNCM)/2.
            XYTWN=XWKWN/XSIZ
         ELSE
            DYWN=0.
            XYTWN=YWKWN/YSIZ
         ENDIF
      ELSE
         XNCM=XWKWN
         YNCM=XWKWN*YSIZ/XSIZ
         DYWN=(YWKWN-YNCM)/2.
         IF(DYWN.LT.0.)THEN
            YNCM=YWKWN
            DYWN=0.
            XNCM=YWKWN*XSIZ/YSIZ
            DXWN=(XWKWN-XNCM)/2.
            XYTWN=YWKWN/YSIZ
         ELSE
            DXWN=0.
            XYTWN=XWKWN/XSIZ
         ENDIF
      ENDIF
*
      CALL ISWN(1,0.,XSIZ,0.,YSIZ)
      CALL ISVP(1,DXWN,XNCM+DXWN,DYWN,YNCM+DYWN)
      CALL ISELNT(1)
      RQUEST(11)=XYTWN
      RQUEST(12)=DXWN
      RQUEST(13)=DYWN
      RQUEST(14)=XNCM
      RQUEST(15)=YNCM
*
 999  END
#endif
