*
* $Id: cdprdt.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdprdt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDPRDT (IRC)
*     =======================
*
************************************************************************
*                                                                      *
*        SUBR. CDPRDT (IRC*)                                           *
*                                                                      *
*   Prints all objects for the current subdirectory                    *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDPRIN                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 38 : Read error in getting the RZ date and time      *
*                                                                      *
************************************************************************
*
#include "hepdb/hdbkeys.inc"
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/clinks.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       ITIME(MXPACD)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*     ------------------------------------------------------------------
*
* *** Find the objects to be printed
*
      IRC    = 0
      IOKYCA(IDHKSN) = 1
      DO 5 I = 1, NPARCD
        ITIME(I) = 1
    5 CONTINUE
      CALL VZERO (INDKCK, NKEYCK)
      IL     = 1
   10 IN     = IL
      MNKYCT = 0
      KEY6CT = 0
      DO 20 IK = 1, NKEYCK
        IF (INDKCK(IK).NE.0)                    GO TO 20
        CALL CDKEYR (IK, NWKYCK, KEYVCK)
        IF (MNKYCT.EQ.0)  THEN
          MNKYCT = KEYVCK(IDHKSN)
          KEY6CT = KEYVCK(IDHFLG)
          CALL UCOPY (KEYVCK, KEYNCK, NWKYCK)
        ELSE
          IF (KEYVCK(IDHUSI).NE.KEYNCK(IDHUSI)) GO TO 20
          DO 15 I = 1, NWKYCK
            IF ((I.GT.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD).OR.
     +          (I.GT.NOF2CK+2*NPARCD)) THEN
              IF (KEYVCK(I).NE.KEYNCK(I))       GO TO 20
            ENDIF
   15     CONTINUE
          IF (MNKYCT.LT.KEYNCK(IDHKSN))  THEN
            MNKYCT = KEYNCK(IDHKSN)
            KEY6CT = KEYNCK(IDHFLG)
          ENDIF
        ENDIF
        INDKCK(IK) = 1
   20 CONTINUE
*
* *** Now print the objects
*
      IF (MNKYCT.NE.0)  THEN
        KEY6   = JBIT (KEY6CT, JIGNCD)
        IFLG   = 0
        DO 30 IK = IN, NKEYCK
          IF (INDKCK(IK).NE.1)  THEN
            IFLG   = 1
          ELSE
            INDKCK(IK) = 2
            IF (IFLG.EQ.0) IL = IK + 1
            IF (KEY6.EQ.0)  THEN
              CALL CDKEYR (IK, NWKYCK, KEYNCK)
              IF (IOPDCA.NE.0) CALL CDPRNT (LPRTCD, '(//,15X,''======'//
     +        '====== Keys and Time ============'',/)', IARGCD, 0)
              CALL CDPRKY (NWKYCK, KEYNCK, IOTYCK, IRC)
              IF (IRC.NE.0)                     GO TO 990
              IF (IOPICA.NE.0) THEN
                CALL RZIN (IDISCD, LDUMM, JBIAS, KEYNCK, 9999, 'C')
                IF (IQUEST(1).NE.0) THEN
                  IRC    = 38
#if defined(CERNLIB__DEBUG)
                  IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPRDT'//
     +            ' : RZIN error in getting the cycle information'')',
     +            IARGCD, 0)
#endif
                  GO TO 990
                ENDIF
                INSTM = IQUEST(14)
                CALL RZDATE (INSTM, IARGCD(1), IARGCD(2), 1)
                CALL CDPRNT (LPRTCD, '(12X,''Date : '',I10,''   Time '//
     +               ': '',I10)', IARGCD, 2)
              ENDIF
              IF (IOPDCA.NE.0)  THEN
                LAUXCL(8) = 0
                CALL VZERO (KEYVCK, NWKYCK)
                KEYVCK(IDHKSN) = IK
                CALL CDKXIN (ITIME, IDISCD, LAUXCL(8), LAUXCL(8),
     +                       JBIAS, NWKEY, KEYVCK, IPREC, IRC)
                IF (LAUXCL(8).NE.0) THEN
                  CALL DZSHOW ('*CDPRDT*', IDISCD, LAUXCL(8), 'BLV',
     +                         0, 0, 0, 0)
                  CALL MZDROP (IDISCD, LAUXCL(8), 'L')
                ENDIF
                IF (IRC.NE.0)                   GO TO 990
              ENDIF
            ENDIF
          ENDIF
   30   CONTINUE
*
        IF (IL.EQ.IN) IL = IN + 1
        IF (IL.LE.NKEYCK)                       GO TO 10
      ENDIF
*
  990 IOKYCA(IDHKSN) = 0
*                                                             END CDPRDT
  999 END
