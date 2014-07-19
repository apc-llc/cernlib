*
* $Id: cdinct.F,v 1.2 1999/08/17 12:42:38 mclareni Exp $
*
* $Log: cdinct.F,v $
* Revision 1.2  1999/08/17 12:42:38  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:40  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDINCT (IDTMI, ISADD, IDTMO, IRC)
*     ============================================
*
************************************************************************
*                                                                      *
*        SUBR. CDINCT (IDTMI, ISADD, IDTMO*, IRC*)                     *
*                                                                      *
*   Converts packed date and time (ala CDPKTS) to a similar packed     *
*   number after adding a fixed time in seconds                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDTMI    Packed date-time (ala CDPKTS) on input                  *
*     ISADD    Incremental time to IDTMI in seconds                    *
*     IDTMO    Packed date-time (ala CDPKTS) on output                 *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/ctmpak.inc"
*     ------------------------------------------------------------------
*
* *** Get the real Year/Month/Date Hour/Minute/Second
*
      IRC    = 0
      III    = IDTMI
      ISEC   = MOD (III, MXSEC)
      III    = (III-ISEC)/MXSEC
      IMIN   = MOD (III, MXMIN)
      III    = (III-IMIN)/MXMIN
      IHOU   = MOD (III, MXHOU)
      III    = (III-IHOU)/MXHOU
      IDAY   = MOD (III, MXDAY)
      III    = (III-IDAY)/MXDAY
      IMON   = MOD (III, MXMON)
      IYEA   = (III-IMON)/MXMON
*
* *** Add the second to real seconds in the day
*
      ISECS  = ISEC  + 60*(IMIN + 60*IHOU)
      ISECS  = ISECS + ISADD
      IF (ISECS.GE.0.AND.ISECS.LT.86400) THEN
        ISEC   = MOD (ISECS, 60)
        ISECS  = (ISECS-ISEC)/60
        IMIN   = MOD (ISECS, 60)
        IHOU   = (ISECS-IMIN)/60
      ELSE
        IF (ISECS.LT.0) THEN
          III    = -ISECS
          IDADD  = -(III/86400 + 1)
        ELSE
          IDADD  = ISECS/86400
        ENDIF
        ISECS  = ISECS - IDADD*86400
        ISEC   = MOD (ISECS, 60)
        ISECS  = (ISECS-ISEC)/60
        IMIN   = MOD (ISECS, 60)
        IHOU   = (ISECS-IMIN)/60
        IF (IYEA.LT.1) THEN
          IADD   = 0
          ITYP   = 2
        ELSE IF (IYEA.GT.55) THEN
          IADD   = IDAYY(55)
          ITYP   = 2
        ELSE
          IADD   = IDAYY(IYEA)
          ITYP   = MOD (IYEA, 4)
          IF (ITYP.EQ.0) THEN
            ITYP = 2
          ELSE
            ITYP = 1
          ENDIF
        ENDIF
        IDAYS  = IDAY + IADD + IDAYM(IMON,ITYP) + IDADD
        DO 10 I = 1, 55
          IF (IDAYS.LE.IDAYY(I)) THEN
            IYEA   = I - 1
            IF (IYEA.GT.0) IDAYS  = IDAYS - IDAYY(IYEA)
            GO TO 15
          ENDIF
   10   CONTINUE
        IYEA   = 55
        IDAYS  = 365
   15   IF (MOD(IYEA,4).EQ.0) THEN
          ITYP   = 2
        ELSE
          ITYP   = 1
        ENDIF
        DO 20 I = 2, 12
          IF (IDAYS.LE.IDAYM(I,ITYP)) THEN
            IMON   = I - 1
            GO TO 25
          ENDIF
   20   CONTINUE
        IMON   = 12
   25   IDAY   = IDAYS - IDAYM(IMON,ITYP)
      ENDIF
*
* *** Now reconvert into a packed time
*
      IDTMO  = ISEC
      MAXX   = MXSEC
      IDTMO  = IDTMO + MAXX*IMIN
      MAXX   = MAXX*MXMIN
      IDTMO  = IDTMO + MAXX*IHOU
      MAXX   = MAXX*MXHOU
      IDTMO  = IDTMO + MAXX*IDAY
      MAXX   = MAXX*MXDAY
      IDTMO  = IDTMO + MAXX*IMON
      MAXX   = MAXX*MXMON
      IDTMO  = IDTMO + MAXX*IYEA
*                                                             END CDINCT
      END
