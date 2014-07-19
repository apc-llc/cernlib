*
* $Id: paplot.F,v 1.1.1.1 1996/03/01 11:38:42 mclareni Exp $
*
* $Log: paplot.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:42  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/04 28/06/95  10.31.20  by  O.Couet
*-- Author :    O.Couet   21/08/92
      SUBROUTINE PAPLOT(ID,CHOPT,CHCASE,NUM,ICRANG,ICX1,ICX2,ICY1,ICY2)
*.===========>
*.
*.   This routine allows to plot 1D and 2D histogram. It has
*. the calling sequence of the routine HPLOT with the
*. subranges in addition.
*.
*..==========>
#include "hbook/hcbits.inc"
#include "paw/pawnpu.inc"
#include "paw/quest.inc"
#include "paw/pcwk.inc"
      CHARACTER*(*) CHOPT,CHCASE
      DIMENSION PAR(6)
*.______________________________________
*
*              Graphical output ...
*
         IF(IWK.NE.0)THEN
*
* ... Plot all the histograms
*
            IF(ID.EQ.0)THEN
               CALL HPLOT(0,' ',' ',0)
               RETURN
            ENDIF
*
* ... 1D histograms
*
            IF(I1.NE.0)THEN
               IF(INDEX(CHOPT,'LEGO').NE.0.OR.
     +            INDEX(CHOPT,'SURF').NE.0)THEN
                  PAR(1)     = 1.
                  PAR(2)     = -1.
                  PAR(3)     = 0.
                  PAR(4)     = 0.
                  PAR(5)     = -ICX1
                  IQUEST(60) = ICX2
                  PAR(6)     = 0.
                  CALL HPLTAB(ID,6,PAR,CHOPT)
               ELSE
                  IF(ICRANG.EQ.0)THEN
                     CALL HPLOT(ID,CHOPT,CHCASE,NUM)
                  ELSE
                     CALL HPLZOM(ID,CHOPT,ICX1,ICX2)
                  ENDIF
               ENDIF
            ELSE
*
* ... 2D histograms
*
* If CHCASE.NE.'HIST' HPLOT should be called because SLIY, SLIX, BANX
* etc .. are managed in HPLOT.
* If CHCASE.EQ.'HIST' HPLTAB is called in order to use the angles.
*
               IF(INDEX(CHCASE,'HIST').EQ.0)THEN
                  CALL HPLOT(ID,CHOPT,CHCASE,NUM)
               ELSE
                  IF(INDEX(CHOPT,'LEGO').NE.0.OR.
     +               INDEX(CHOPT,'SURF').NE.0)THEN
                     PAR(1)=ANGLE1
                     PAR(2)=ANGLE2
                  ELSEIF(INDEX(CHOPT,'CONT').NE.0)THEN
                     IMODE=1
                     IF(INDEX(CHOPT,'0').NE.0)IMODE=0
                     IF(INDEX(CHOPT,'2').NE.0)IMODE=2
                     IF(INDEX(CHOPT,'3').NE.0)IMODE=3
                     PAR(1)=10
                     PAR(2)=IMODE
                  ELSE
                     PAR(1)=0.
                     PAR(2)=0.
                  ENDIF
                  PAR(3)     = 0.
                  PAR(4)     = 0.
                  PAR(5)     = -ICX1
                  IQUEST(60) = ICX2
                  PAR(6)     = -ICY1
                  IQUEST(61) = ICY2
                  CALL HPLTAB(ID,6,PAR,CHOPT)
               ENDIF
            ENDIF
         ENDIF
*
*              Alphanumeric output ...
*
         IF(IWK.EQ.0.OR.IWK.EQ.-2)THEN
            IF(CHCASE.EQ.'HIST')THEN
               CALL PAHPR(ID)
            ELSE
               CALL HPHIST(ID,CHCASE,NUM)
            ENDIF
         ENDIF
*
      END
