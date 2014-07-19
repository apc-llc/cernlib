*
* $Id: cdstat.F,v 1.1.1.1 1996/02/28 16:24:42 mclareni Exp $
*
* $Log: cdstat.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:42  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDSTAT (LUN, IRC)
*     ============================
*
************************************************************************
*                                                                      *
*        SUBR. CDSTAT (LUN, IRC*)                                      *
*                                                                      *
*   Prints accumulated summary for the given run with the Data Base    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LUN      Logical unit on which statistics to be printed          *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 98 : Invalid path name in Node bank                  *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER     CHSTR*132, CFMT*136
*
*     ------------------------------------------------------------------
*
      IRC    = 0
      IF (LUN.LE.0) GO TO 999
*
* *** Loop over all node banks
*
      CALL CDPRNT (LUN, '(/,'' CDSTAT : Summary of Database Elements '//
     +     'used '',/,'' ========================================== '')'
     +,    IARGCD, 0)
      CALL CDPRNT (LUN, '(/,20X,''Path name'',28X,''CDUSE Call/Data '//
     +     'Read So.ID'',5X,''User Keys'',/)', IARGCD, 0)
      CFMT(1:2)     = '('''
      CFMT(135:136) = ''')'
      CFMT(3:134)   = ' '
*
      LBUPCD = LTOPCD
   10 IF (LBUPCD.EQ.0)        GO TO 80
      NPARCD = IQ(KOFUCD+LBUPCD+MUPAIR)
      IHFLCD = IQ(KOFUCD+LBUPCD+MUPHFL)
      NUSF   = NOF2CK + 2*NPARCD + 1
*
      LBNOCD = LQ(KOFUCD+LBUPCD-1)
      IF (LBNOCD.EQ.0)        GO TO 70
*
*  ** Store the number of structural links already looked
*  ** at bank number address
*
   20 IQ(KOFUCD+LBNOCD-5) = IQ(KOFUCD+LBNOCD-2)
*
*  *  If all the links have not been looked at; go the next link
*
   30 IF (IQ(KOFUCD+LBNOCD-5).NE.0) THEN
        IQ(KOFUCD+LBNOCD-5) = IQ(KOFUCD+LBNOCD-5) - 1
        N      = IQ(KOFUCD+LBNOCD-2) - IQ(KOFUCD+LBNOCD-5)
        LBAD   = LQ(KOFUCD+LBNOCD-N)
        IF (LBAD.NE.0)  THEN
          LBNOCD = LBAD
          GO TO 20
        ELSE
          GO TO 30
        ENDIF
*
*  *  Otherwise look at this Node bank
*
      ELSE
*
        NDK    = IQ(KOFUCD+LBNOCD+MNDNWD)
        NCHAR  = IQ(KOFUCD+LBNOCD+MNDNCH)
        CALL UHTOC (IQ(KOFUCD+LBNOCD+MNDNAM), 4, PAT2CT, NCHAR)
        PAT2CT = PAT2CT(1:NCHAR)
*
        CALL RZCDIR (PAT2CT, ' ')
        IF (IQUEST(1).NE.0)  THEN
          IRC    = 98
#if defined(CERNLIB__DEBUG)
          IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDSTAT : Illeg'//
     +    'al Path name '//PAT2CT(1:60)//' in node bank'')', IARGCD, 0)
#endif
          GO TO 999
        ENDIF
*
        LCDRCD = IQUEST(11)
        IKDRCD = IQUEST(13)
        NKEYCK = IQUEST(7)
        NWKYCK = IQUEST(8)
        CALL CDKEYT
*
*  *    Loop over all key banks at this Node
*
        LBKYCD = LQ(KOFUCD+LBNOCD-KLKYCD)
   40   IF (LBKYCD.EQ.0)      GO TO 60
*
*  *    Prepare the header part
*
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
        WRITE (CHSTR, 8001) PAT2CT, IQ(KOFUCD+LBKYCD+NDK+MKYCRU),
     +                      IQ(KOFUCD+LBKYCD+NDK+MKYRID),
     +                      IQ(KOFUCD+LBKYCD+IDHUSI)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
        IARGCD(1) = IQ(KOFUCD+LBKYCD+NDK+MKYCRU)
        IARGCD(2) = IQ(KOFUCD+LBKYCD+NDK+MKYRID)
        IARGCD(3) = IQ(KOFUCD+LBKYCD+IDHUSI)
        CHSTR  = ' '
        CALL UTWRIT (CHSTR,'('' '//PAT2CT(1:56)//''',2I10,I6)',IARGCD,3)
#endif
        NS1    = 84
*
*  *    Loop over user keys
*
        IF (NWKYCK.GE.NUSF)  THEN
          DO 50 I = NUSF, NWKYCK
            IF (NS1.GT.120)  THEN
              CFMT(3:134)   = CHSTR
              CALL CDPRNT (LUN, CFMT, IARGCD, 0)
              CHSTR  = ' '
              NS1    = 84
            ENDIF
            IF (IOTYCK(I).EQ.2)  THEN
              NS2    = NS1 + 9
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
              WRITE (CHSTR(NS1:NS2), 8002) IQ(KOFUCD+LBKYCD+I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
              CALL UTWRIT (CHSTR(NS1:NS2), '(I10)',
     +                     IQ(KOFUCD+LBKYCD+I), 1)
#endif
            ELSE IF (IOTYCK(I).EQ.5 .OR. IOTYCK(I).EQ.6)  THEN
              NS2    = NS1 + 7
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
              WRITE (CHSTR(NS1:NS2), 8003) IQ(KOFUCD+LBKYCD+I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
              CALL UTWRIT (CHSTR(NS1:NS2), '(4X,A4)',
     +                     IQ(KOFUCD+LBKYCD+I), 1)
#endif
            ELSE
              NS2    = NS1 + 9
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
              WRITE (CHSTR(NS1:NS2), 8004) IQ(KOFUCD+LBKYCD+I)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
              CALL UTWRIT (CHSTR(NS1:NS2), '(Z10)',
     +                     IQ(KOFUCD+LBKYCD+I), 1)
#endif
            ENDIF
            NS1    = NS1 + 12
   50     CONTINUE
        ENDIF
*
        CFMT(3:134)   = CHSTR
        CALL CDPRNT (LUN, CFMT, IARGCD, 0)
*
        LBKYCD = LQ(KOFUCD+LBKYCD)
        GO TO 40
*
   60   LBNOCD = LQ(KOFUCD+LBNOCD+1)
        IF (LBNOCD.NE.LBUPCD) GO TO 30
      ENDIF
*
   70 LBUPCD = LQ(KOFUCD+LBUPCD)
      GO TO 10
   80 CONTINUE
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 8001 FORMAT (1X,A56,2I10,I6)
 8002 FORMAT (I10)
 8003 FORMAT (4X,A4)
 8004 FORMAT (Z10)
#endif
*                                                             END CDSTAT
  999 END
