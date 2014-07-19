*
* $Id: phinfo.F,v 1.2 1999/04/08 09:37:58 couet Exp $
*
* $Log: phinfo.F,v $
* Revision 1.2  1999/04/08 09:37:58  couet
* - New version of the system function $HINFO and $HTITLE to make
*   them work on NT
*
* Revision 1.1.1.1  1996/03/01 11:38:46  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.07/11 01/09/95  13.37.22  by  O.Couet
*-- Author :    Alfred Nathaniel   27/08/93
      SUBROUTINE PHINFO(RESULT,WHICH,ID)
#include "hbook/hcbook.inc"
      CHARACTER*(*) WHICH
      CHARACTER CHTITL*80
      DIMENSION RVAL(2),KIND(32)
*
*  interface between HBOOK query functions and KUFDEF
*
      IF(WHICH.EQ.'HMIN') THEN
         RESULT = HMIN(ID)
      ELSEIF(WHICH.EQ.'HMAX') THEN
         RESULT = HMAX(ID)
      ELSEIF(WHICH.EQ.'HSUM') THEN
         RESULT = HSUM(ID)
      ELSEIF(WHICH.EQ.'MEAN') THEN
         RESULT = HSTATI(ID,1,' ',0)
      ELSEIF(WHICH.EQ.'RMS') THEN
         RESULT = HSTATI(ID,2,' ',0)
      ELSEIF(WHICH.EQ.'EVENTS') THEN
         RESULT = HSTATI(ID,3,' ',0)
      ELSEIF(WHICH.EQ.'XBINS') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = NX
      ELSEIF(WHICH.EQ.'YBINS') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = NY
      ELSEIF(WHICH.EQ.'XMIN') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = XMIN
      ELSEIF(WHICH.EQ.'XMAX') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = XMAX
      ELSEIF(WHICH.EQ.'YMIN') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = YMIN
      ELSEIF(WHICH.EQ.'YMAX') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = YMAX
      ELSEIF(WHICH.EQ.'UNDERFLOW') THEN
         RESULT = HI(ID,0)
      ELSEIF(WHICH.EQ.'OVERFLOW') THEN
         CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
         RESULT = HI(ID,NX+1)
      ELSEIF(WHICH.EQ.'1DHISTO') THEN
         CALL VZERO(KIND,32)
         CALL HKIND(ID,KIND,'A')
         RESULT = FLOAT(KIND(1))
      ELSEIF(WHICH.EQ.'2DHISTO') THEN
         CALL VZERO(KIND,32)
         CALL HKIND(ID,KIND,'A')
         RESULT = FLOAT(KIND(2))
      ELSEIF(WHICH.EQ.'TABLE') THEN
         CALL VZERO(KIND,32)
         CALL HKIND(ID,KIND,'A')
         RESULT = FLOAT(KIND(3))
      ELSEIF(WHICH.EQ.'PROFILE') THEN
         CALL VZERO(KIND,32)
         CALL HKIND(ID,KIND,'A')
         RESULT = FLOAT(KIND(8))
      ELSEIF(WHICH.EQ.'NTUPLE') THEN
         CALL VZERO(KIND,32)
         CALL HKIND(ID,KIND,'A')
         RESULT = FLOAT(KIND(4))
      ELSEIF(WHICH.EQ.'LOG') THEN
         CALL VZERO(KIND,32)
         CALL HKIND(ID,KIND,'A')
         RESULT = FLOAT(KIND(26))
      ELSEIF(WHICH.EQ.'TXAL' .OR. WHICH.EQ.'TXFP') THEN
         CALL IGQ(WHICH,RVAL)
         RESULT = ABS(RVAL(1)) * 10 + RVAL(2)
         IF(RVAL(1).LT.0) RESULT = -RESULT
      ELSE
         CALL HFIND(ID,'PHINFO')
         IF(LCID.EQ.0)RETURN
         CALL HDCOFL
         RESULT = 0.
         IF(WHICH.EQ.'NSLIX') THEN
            LSLIX = LQ(LCID-4)
            IF(LSLIX.NE.0) RESULT = IQ(LSLIX-2)
         ELSEIF(WHICH.EQ.'NSLIY') THEN
            LSLIY = LQ(LCID-5)
            IF(LSLIY.NE.0) RESULT = IQ(LSLIY-2)
         ELSEIF(WHICH.EQ.'NBANX') THEN
            LBANX = LQ(LCID-6)
   10       IF(LBANX.NE.0)THEN
               RESULT = RESULT+1.
               LBANX  = LQ(LBANX)
               IF(LBANX.NE.0)GOTO 10
            ENDIF
         ELSEIF(WHICH.EQ.'NBANY') THEN
            LBANY = LQ(LCID-7)
   20       IF(LBANY.NE.0)THEN
               RESULT = RESULT+1.
               LBANY  = LQ(LBANY)
               IF(LBANY.NE.0)GOTO 20
            ENDIF
         ELSEIF(WHICH.EQ.'NPROX') THEN
            IF(LQ(LCID-2).NE.0) RESULT = 1.
         ELSEIF(WHICH.EQ.'NPROY') THEN
            IF(LQ(LCID-3).NE.0) RESULT = 1.
         ELSE
            CALL KUALFA
            PRINT *,' *** PHINFO: ??? ',WHICH
            RESULT = 1.2345E6
         ENDIF
      ENDIF
*
      END

      SUBROUTINE PHTIT(CHTITL,ID)
      CHARACTER*(*) CHTITL
      CALL HGIVE(ID,CHTITL,NX,XMIN,XMAX,NY,YMIN,YMAX,NWT,LOC)
      END
