*
* $Id: igclip.F,v 1.1.1.1 1996/02/14 13:10:34 mclareni Exp $
*
* $Log: igclip.F,v $
* Revision 1.1.1.1  1996/02/14 13:10:34  mclareni
* Higz
*
*
#include "higz/pilot.h"
*CMZ :  1.10/01 10/05/90  17.32.50  by  O.Couet
*-- Author :    O.Couet   24/07/89
      FUNCTION IGCLIP(X,Y,XCLIPL,XCLIPR,YCLIPB,YCLIPT)
*.===========>
*.
*.   Clipping routine: Cohen Sutherland algorithm.
*. If IGCLIP=0 the segment is outside the boundary.
*.
*. _Input parameters:
*.
*.  REAL X(2),Y(2) : Segment coordinates
*.  REAL XCLIPL,XCLIPR,YCLIPB,YCLIPT : Clipping boundary
*.
*. _Output parameters:
*.
*.  REAL X(2),Y(2) : New segment coordinates
*.
*. If IQUEST(50).NE.0 after a call to this function, the
*. line has been clipped.
*.
*..==========> (O.Couet)
#include "higz/hiques.inc"
      DIMENSION X(2),Y(2)
      PARAMETER (P=10000.)
*.______________________________________
*
      IGCLIP=0
*
      DO 10 I=1,2
         IF(ABS(XCLIPL-X(I)).LE.ABS((XCLIPR-XCLIPL)/P))X(I)=XCLIPL
         IF(ABS(XCLIPR-X(I)).LE.ABS((XCLIPR-XCLIPL)/P))X(I)=XCLIPR
         IF(ABS(YCLIPB-Y(I)).LE.ABS((YCLIPT-YCLIPB)/P))Y(I)=YCLIPB
         IF(ABS(YCLIPT-Y(I)).LE.ABS((YCLIPT-YCLIPB)/P))Y(I)=YCLIPT
   10 CONTINUE
*
*              Compute the first endpoint codes.
*
      ICODE1=IGCLI1(X(1),Y(1),XCLIPL,XCLIPR,YCLIPB,YCLIPT)
      ICODE2=IGCLI1(X(2),Y(2),XCLIPL,XCLIPR,YCLIPB,YCLIPT)
*
      IQUEST(50)=0
   20 IF((ICODE1+ICODE2).EQ.0)GOTO 30
      IQUEST(50)=1
*
*              The line lies entirely outside the clipping boundary
*
      IF(IAND(ICODE1,ICODE2).NE.0)THEN
         IGCLIP=0
         RETURN
      ENDIF
*
*              The line is subdivide into several parts
*
      IC=ICODE1
      IF(IC.EQ.0)IC=ICODE2
      IF(JBIT(IC,1).NE.0)THEN
         YT=Y(1)+(Y(2)-Y(1))*(XCLIPL-X(1))/(X(2)-X(1))
         XT=XCLIPL
      ENDIF
      IF(JBIT(IC,2).NE.0)THEN
         YT=Y(1)+(Y(2)-Y(1))*(XCLIPR-X(1))/(X(2)-X(1))
         XT=XCLIPR
      ENDIF
      IF(JBIT(IC,3).NE.0)THEN
         XT=X(1)+(X(2)-X(1))*(YCLIPB-Y(1))/(Y(2)-Y(1))
         YT=YCLIPB
      ENDIF
      IF(JBIT(IC,4).NE.0)THEN
         XT=X(1)+(X(2)-X(1))*(YCLIPT-Y(1))/(Y(2)-Y(1))
         YT=YCLIPT
      ENDIF
      IF(IC.EQ.ICODE1)THEN
         X(1)=XT
         Y(1)=YT
         ICODE1=IGCLI1(XT,YT,XCLIPL,XCLIPR,YCLIPB,YCLIPT)
      ELSE
         X(2)=XT
         Y(2)=YT
         ICODE2=IGCLI1(XT,YT,XCLIPL,XCLIPR,YCLIPB,YCLIPT)
      ENDIF
      GOTO 20
   30 IGCLIP=1
*
      END
