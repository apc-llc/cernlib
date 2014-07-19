*
* $Id: cdshow.F,v 1.1.1.1 1996/02/28 16:24:44 mclareni Exp $
*
* $Log: cdshow.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:44  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDSHOW (PATH, ISEL, IMASK, KEYS, CHBANK, IDBANK, ND1,
     +                   ND2, CHOPT, IRC)
*     ================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDSHOW (PATH, ISEL, IMASK, KEYS, CHBANK, IDBANK, ND1,   *
*                      ND2, CHOPT, IRCC*)                              *
*                                                                      *
*   Displays the objects in a given directory. One or all banks with   *
*   the name CHBANK are displayed in a form similar to DZSHOW. If a    *
*   specific bank is required. this may be specified by IDBANK. A      *
*   range of data words specified by ND1 and ND2 may also be selected  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     ISEL     Integer vector specifying the instant of validity       *
*     IMASK    Integer vector indicating which elements of KEYS are    *
*              significant for selection. If MASK corresponding to     *
*              one of the fields of 'Beginning' validity range is set, *
*              it will select objects with start validity smaller than *
*              those requested in KEYS. If MASK corresponding to one   *
*              of the fields of 'End' validity range is set, it will   *
*              select objects with end validity larger than those in   *
*              KEYS. If MASK corresponding to time of insertion is set,*
*              objects inserted earlier than KEYS(IDHINS) are selected *
*     KEYS     Vector of keys. Only the elements declared in CHOPT are *
*              assumed to contain useful information.                  *
*     CHBANK   ZEBRA bank name                                         *
*     IDBANK   ZEBRA numeric bank identifier                           *
*     ND1      First data word to be shown                             *
*     ND2      Last  data word to be shown                             *
*     CHOPT    Character string with any of the following characters   *
*         ' '  find all banks with position in walk > IDBANK           *
*          S   find bank with ZEBRA ID = IDBANK                        *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/csavbk.inc"
      CHARACTER       PATH*(*), CHBANK*(*), CHOPT*(*)
      CHARACTER       CHBK*4, CFMT*200
      DIMENSION       ISEL(9), IMASK(9), KEYS(9)
*
*     ------------------------------------------------------------------
*
* *** Retrieve the object first
*
      IF (LOBJCS(1).NE.0) THEN
        CALL MZDROP (IDISCD, LOBJCS(1), 'L')
        LOBJCS(1) = 0
      ENDIF
      CALL CDUSEM (PATH, LOBJCS(1), ISEL, IMASK, KEYS, ' ', IRC)
      IF (IRC.NE.0) GO TO 100
*
* *** Now dump the bank(s)
*
      CALL UOPTC (CHOPT, 'S', IOPTS)
      LBANK  = LENOCC (CHBANK)
      IF (LBANK.EQ.0) THEN
        CHBK = ' '
      ELSE
        CHBK = CHBANK(1:MIN(LEN(CHBK),LBANK))
      ENDIF
      LOBJCS(2) = LQ(KOFUCD+LOBJCS(1)-KLDACD)
*
      LPATH  = LENOCC (PATH)
      CALL CDKYTG
      WRITE (LPRTCD, '('' Directory: '',A)') PATH(1:LPATH)
      WRITE (LPRTCD, '(8(4X,A8))') (CTAGCK(J), J = 1, NWKYCK)
      DO K1 = 1, NWKYCK, 8
        K2     = K1 + 7
        IF (K2.GT.NWKYCK) K2 = NWKYCK
        CFMT   = '('
        NC1    = 2
        DO K = K1, K2
          IF (IOTYCK(K).LE.1) THEN
            CFMT(NC1:NC1+6) = ',2X,Z10'
          ELSE IF (IOTYCK(K).EQ.2) THEN
            CFMT(NC1:NC1+6) = ',1X,I11'
          ELSE
            CFMT(NC1:NC1+6) = ',4X,A8 '
          ENDIF
          NC1    = NC1 + 6
        ENDDO
        CFMT(NC1:NC1) = ')'
        WRITE (LPRTCD, CFMT) (IQ(KOFUCD+LOBJCS(1)+K), K = K1, K2)
      ENDDO
      IF (LOBJCS(2).EQ.0) GO TO 100
*
      IF (LBANK.EQ.0) THEN
        CALL DZSNAP ('CDSHOW', IDISCD, LOBJCS(2), 'LV', 0, 0,
     +               ND1, ND2)
      ELSE IF (IOPTS.NE.0) THEN
        CALL UCTOH (CHBK, ICHBK, 4, 4)
        LGO    = 0
        LF     = LZFID (IDISCD, ICHBK, IDBANK, LGO)
        IF (LF.GT.0) CALL DZSNAP ('CDSHOW', IDISCD, LF, 'B',
     +                             0, 0, ND1, ND2)
      ELSE
        CALL UCTOH (CHBK, ICHBK, 4, 4)
        LGO    = 0
        LF     = LZFIDH (IDISCD, ICHBK, LGO)
        IF (LF.GT.0) THEN
          LGO    = LZHEAD (IDISCD, LF)
          IBK    = 0
   10     IF (LGO.NE.0) THEN
            IBK    = IBK + 1
            IF (IBK.GE.IDBANK.AND.IQ(KOFUCD+LGO-4).EQ.ICHBK)
     +      CALL DZSNAP ('CDSHOW', IDISCD, LGO, 'B', 0, 0, ND1, ND2)
            LGO    = LQ(KOFUCD+LGO)
            GO TO 10
          ENDIF
        ENDIF
      ENDIF
*
  100 IF (LOBJCS(1).NE.0) THEN
        CALL MZDROP (IDISCD, LOBJCS(1), 'L')
        LOBJCS(1) = 0
      ENDIF
*                                                             END CDSHOW
      END
