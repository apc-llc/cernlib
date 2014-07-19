*
* $Id: pspush.F,v 1.1.1.1 1996/03/01 11:39:28 mclareni Exp $
*
* $Log: pspush.F,v $
* Revision 1.1.1.1  1996/03/01 11:39:28  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.03/07 22/08/93  11.47.32  by  Fons Rademakers
*-- Author :    Alfred Nathaniel   08/04/93
      SUBROUTINE PSPUSH
************************************************************************
*                                                                      *
* Push all 1d and 2d histograms in PAWC to client or master server.    *
* Only push objects that have been changed since last time they were   *
* sent. Bit 6 in the object's status word will be 0 in that case.      *
*                                                                      *
************************************************************************
*
#include "hbook/hcbook.inc"

*--- 1d and 2d histograms
      IF(LCDIR.NE.0) THEN
         NRHIST=IQ(LCDIR+KNRH)
         DO 10 I=1,NRHIST
            IDTYPE=IQ(LQ(LTAB-I)+KBITS)
            IF(JBYT(IDTYPE,1,3).NE.0 .AND.
     +                               JBIT(IQ(LQ(LTAB-I)),6).EQ.0) THEN
               CALL PFHOUT(IQ(LTAB+I),ISTAT)
            ENDIF
 10      CONTINUE
      ENDIF

      END
