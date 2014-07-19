*
* $Id: cdpurg.F,v 1.1.1.1 1996/02/28 16:24:24 mclareni Exp $
*
* $Log: cdpurg.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:24  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDPURG (PATHN, KYDAT, KYTIM, CHOPT, IRC)
*     ===================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDPURG (PATHN, KYDAT, KYTIM, CHOPT, IRC*)               *
*                                                                      *
*   Purges/Deletes objects in a directory PATHN                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     PATHN    Character string describing the pathname                *
*     KYDAT    Key element number (for option K in CHOPT) or           *
*              Minimum value of Key 1 to be deleted (for option S)     *
*     KYTIM    Cutoff value for the key (for option K) or              *
*              Maximum value of Key 1 to be deleted (for option S)     *
*     CHOPT    Character string with any of the following characters   *
*          A   Deletes all data objects                                *
*          B   Save in the special backup file; not in standard Journal*
*          K   Deletes all data objects for which KEY(KYDAT) .lt. KYTIM*
*          L   Deletes all but the last (one with highest KEY(1) value)*
*              data objects                                            *
*          P   Deletes all data objects with identical keys (default:  *
*              E+V+U keys, see below) but those having the highest     *
*              Program Version number (i.e., KEY(6) value).            *
*              ** The same may be done for KEY(1) instead of KEY(6) ** *
*              **                     if KYDAT=1 !!!                ** *
*              Additional options for keys range for identity test:    *
*                      - default: E+V+U keys ( E means Key8-Key10)     *
*            V         - val.keys only                                 *
*            VE or E   - E+V keys                                      *
*            VX or X   - E+V+Key7 (or Key6-Key7 if KYDAT=1)            *
*            VU or U   - V+U keys                                      *
*            VEU or EU - same as default                               *
*            VXU or XU - the maximum range                             *
*                                                                      *
*          S   Deletes all data objects with Serial number (KEY(1)) in *
*              the range KYDAT-KYTIM (the terminal points included)    *
*     IRC      Return Code (See below)                                 *
*                                                                      *
*   Called by user,   CDFZUP                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =111 : Illegal path name                               *
*               =112 : No key or data for the path name                *
*                                                                      *
*     If IRC = 0, IQUEST(2) carries information on number of data      *
*     objects deleted in the disk                                      *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/clinks.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/hdbkeys.inc"
#include "hepdb/ctpath.inc"
      CHARACTER       PATHN*(*), CHOPT*(*), PATHY*80
      INTEGER         KEYS(MXDMCK)
#include "zebra/q_jbit.inc"
* Ignoring t=pass
*
*     ------------------------------------------------------------------
*
      KEY7   = KEY7CK
      KEY7CK = 0
      NDEL   = 0
      CALL CDOPTS (CHOPT, IRC)
      IF (IRC.NE.0)                               GO TO 999
*
* *** Set the current directory
*
      CALL CDLDUP (PATHN, 0, IRC)
      IF (IRC.NE.0)                               GO TO 999
      PATHY  = PAT1CT
      NCHAR  = LENOCC (PATHY)
      CALL CDKEYT
      IOPTP  = JBIT (IQ(KOFSCD+LCDRCD+IKDRCD+IDHFLG), JPRTCD)
*
* *** Check the number of keys
*
      IF (NKEYCK.EQ.0) THEN
        IRC    = 112
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : No key '//
     +     'or data for Path Name '//PATHY(1:NCHAR)//''')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
*
* *** Save the command in the journal file
*
      CALL VZERO (KEYS, NSYSCK)
      KEYS(IDHKSN) = KYDAT
      KEYS(IDHPTR) = KYTIM
      KEY7CK  = KEY7
      CALL CDSPUR (PATHY, NWKYCK, -1, KEYS, 0, IRC)
      KEY7CK  = 0
      IF (IRC.NE.0)                               GO TO 999
      IF (IOPPCD.NE.0)                            GO TO 999
#if !defined(CERNLIB__P3CHILD)
*
      IF (IOPACA.NE.0)  THEN
*
* ***   Delete all keys
*
        IF (IOPTP.EQ.0)  THEN
          IF (KEY7.LE.0) THEN
            IF (IOPSCD.NE.0) CALL RZLOCK ('CDPURG')
            CALL RZDELK (KDUM, ICDUM, 'K')
            IF (IOPSCD.NE.0) CALL RZFREE ('CDPURG')
            NDEL   = NKEYCK
          ELSE
            CALL VZERO (IPURCK, NKEYCK)
            IPNT   = KOFSCD + LCDRCD + IKDRCD
            ISTP   = NWKYCK + 1
            NKEEP  = 0
            DO 10 JK = 1, NKEYCK
              IP     = IPNT + (JK - 1) * ISTP
              KEY1CK(JK) = IQ(IP+MPSRCD)
              IF (IQ(IP+IDHINS).GT.KEY7) THEN
                IPURCK(JK) = 2
                NKEEP  = NKEEP +1
              ENDIF
   10       CONTINUE
            IF (NKEEP.GT.0) THEN
              CALL CDDELK (IRC)
              NDEL   = IQUEST(2)
            ELSE
              IF (IOPSCD.NE.0) CALL RZLOCK ('CDPURG')
              CALL RZDELK (KDUM, ICDUM, 'K')
              IF (IOPSCD.NE.0) CALL RZFREE ('CDPURG')
              NDEL   = NKEYCK
            ENDIF
          ENDIF
        ELSE
          NKEYS  = NKEYCK
          DO 20 IK = 1, NKEYS
            CALL CDPATH (TOP1CT, IK)
            PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            NKEYCK = IQUEST(7)
            IF (NKEYCK.GT.0)  THEN
              IF (KEY7.LE.0) THEN
                IF (IOPSCD.NE.0) CALL RZLOCK ('CDPURG')
                CALL RZDELK (KDUM, ICDUM, 'K')
                IF (IOPSCD.NE.0) CALL RZFREE ('CDPURG')
                NDEL   = NDEL + NKEYCK
              ELSE
                NWKYCK = IQUEST(8)
                LCDRCD = IQUEST(11)
                IKDRCD = IQUEST(13)
                IPNT   = KOFSCD + LCDRCD + IKDRCD
                ISTP   = NWKYCK + 1
                CALL VZERO (IPURCK, NKEYCK)
                NKEEP  = 0
                DO 15 JK = 1, NKEYCK
                  IP     = IPNT + (JK - 1) * ISTP
                  KEY1CK(JK) = IQ(IP+MPSRCD)
                  IF (IQ(IP+IDHINS).GT.KEY7) THEN
                    IPURCK(JK) = 2
                    NKEEP  = NKEEP +1
                  ENDIF
   15           CONTINUE
                IF (NKEEP.GT.0) THEN
                  CALL CDDELK (IRC)
                  NDEL   = NDEL + IQUEST(2)
                ELSE
                  IF (IOPSCD.NE.0) CALL RZLOCK ('CDPURG')
                  CALL RZDELK (KDUM, ICDUM, 'K')
                  IF (IOPSCD.NE.0) CALL RZFREE ('CDPURG')
                  NDEL   = NDEL + NKEYCK
                ENDIF
              ENDIF
            ENDIF
   20     CONTINUE
        ENDIF
*
      ELSE IF (IOPLCA.NE.0)  THEN
*
* ***   Delete all but last keys (except those another key may depend
*  **   on) Highest number of KEY(1)
*
        IF (IOPTP.EQ.0) THEN
          CALL VZERO (IPURCK, NKEYCK)
          NO1    = 0
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          ISTP   = NWKYCK + 1
          DO 25 IK = 1, NKEYCK
            IP     = IPNT + (IK - 1) * ISTP
            KEY1CK(IK) = IQ(IP+IDHKSN)
            IF (KEY7.LE.0) THEN
              IF (KEY1CK(IK).GT.NO1) NO1 = KEY1CK(IK)
            ELSE
              IF (IQ(IP+IDHINS).GT.KEY7) THEN
                IPURCK(IK) = 2
              ELSE
                IF (KEY1CK(IK).GT.NO1) NO1 = KEY1CK(IK)
              ENDIF
            ENDIF
   25     CONTINUE
*
          II     = IUCOMP (NO1, KEY1CK, NKEYCK)
          IF (II.GT.0) IPURCK(II) = 2
          CALL CDDELK (IRC)
          NDEL   = IQUEST(2)
*
        ELSE
*
          NKEYS  = NKEYCK
          IFLG   = 0
          DO 35 JK1 = 1, NKEYS
            JK     = NKEYS + 1 - JK1
            CALL CDPATH (TOP1CT, JK)
            PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            NKEYCK = IQUEST(7)
            IF (NKEYCK.GT.0.AND.IFLG.EQ.0)  THEN
              NWKYCK = IQUEST(8)
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              IPNT   = KOFSCD + LCDRCD + IKDRCD
              ISTP   = NWKYCK + 1
*
              CALL VZERO (IPURCK, NKEYCK)
              NO1    = 0
              DO 30 IK = 1, NKEYCK
                IP     = IPNT + (IK - 1) * ISTP
                KEY1CK(IK) = IQ(IP+IDHKSN)
                IF (KEY7.LE.0) THEN
                  IF (KEY1CK(IK).GT.NO1) NO1 = KEY1CK(IK)
                ELSE
                  IF (IQ(IP+IDHINS).GT.KEY7) THEN
                    IPURCK(IK) = 2
                  ELSE
                    IF (KEY1CK(IK).GT.NO1) NO1 = KEY1CK(IK)
                  ENDIF
                ENDIF
   30         CONTINUE
*
              II     = IUCOMP (NO1, KEY1CK, NKEYCK)
              IF (II.GT.0) THEN
                IPURCK(II) = 2
                IFLG   = 1
              ENDIF
              CALL CDDELK (IRC)
              NDEL   = NDEL + IQUEST(2)
*
            ELSE IF (NKEYCK.GT.0) THEN
*
*  *          Delete all keys in the remaining subdirectories
*
              IF (IOPSCD.NE.0) CALL RZLOCK ('CDPURG')
              CALL RZDELK (KDUM, ICDUM, 'K')
              IF (IOPSCD.NE.0) CALL RZFREE ('CDPURG')
              NDEL   = NDEL + NKEYCK
*
            ENDIF
   35     CONTINUE
        ENDIF
*
      ELSE IF (IOPPCA.NE.0)  THEN
*
* ***   Delete all objects with identical start and end validity but
* ***   those having the highest program version #, i.e. the highest
* ***   KEY(6)
*
* +++   a) Additional variant: If KYDAT=1,
* +++      then Key1 will be looked instead of Key6 ! (BKh)
* +++   b) Variable keys range for identity test with options V,E,U,
* +++      which may be added to "P" (BKh)
*
        NKLAST=IDHUSI
        IF(KYDAT.EQ.1) NKLAST=KYDAT
*
*       - select K-s for identity test with IOKYCA
*
        N1=KOFEX1
        N2=NWKYCK
        IF(IOPXCA.NE.0) IOPECA=1
        IF(IOPVCA.NE.0) THEN
          N1=KOFVAL
          N2=NSYSCK
          IF(IOPECA.NE.0) N1=KOFEX1
          IF(IOPUCA.NE.0) N2=NWKYCK
        ELSE IF(IOPECA*IOPUCA.EQ.0) THEN
          IF(IOPECA.NE.0) N2=NSYSCK
          IF(IOPUCA.NE.0) N1=KOFVAL
        ENDIF
        IF(IOPXCA.NE.0) N1=MAX0(KOFUSI,NKLAST+1)
*
        CALL VZERO (IOKYCA,NWKYCK)
        DO IK=N1,N2
          IOKYCA(IK)=1
        ENDDO
*
        IF (IOPTP.EQ.0)  THEN
          CALL VZERO (IPURCK, NKEYCK)
*
*  **     Label by '2' the object of the highest program version #
*
          DO 50 IK = 1, NKEYCK
            IPURCK(IK) = 2
            CALL CDKEYR (IK, NWKYCK, KEYVCK)
            KEY1CK(IK) = KEYVCK(IDHKSN)
            IF (KEY7.GT.0) THEN
              IF (KEYVCK(IDHINS).GT.KEY7)         GO TO 50
            ENDIF
            DO 45 JK = 1, NKEYCK
              IF (IK.EQ.JK.OR.IPURCK(JK).EQ.-1)   GO TO 45
              CALL CDKEYR (JK, NWKYCK, KEYNCK)
              DO 40 K = 1, NWKYCK
                IF (IOKYCA(K).NE.0.AND.KEYNCK(K).NE.KEYVCK(K))
     +                                            GO TO 45
   40         CONTINUE
              IF (KEYNCK(NKLAST).GT.KEYVCK(NKLAST))  THEN
                IPURCK(IK) = -1
                GO TO 50
              ENDIF
   45       CONTINUE
   50     CONTINUE
          CALL CDDELK (IRC)
          NDEL   = IQUEST(2)
*
        ELSE
*
          NKEYS  = NKEYCK
          DO 70 JK2 = 1, NKEYS
            JK1    = NKEYS + 1 - JK2
            CALL CDPATH (TOP1CT, JK1)
            PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            NKEYCK = IQUEST(7)
            IF (NKEYCK.LE.0)                      GO TO 70
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
*
            CALL VZERO (IPURCK, NKEYCK)
*
*  **       Label by '2' the object of the highest program version #
*
            DO 65 IK = 1, NKEYCK
              IPURCK(IK) = 2
              CALL CDKEYR (IK, NWKYCK, KEYVCK)
              KEY1CK(IK) = KEYVCK(IDHKSN)
              IF (KEY7.GT.0) THEN
                IF (KEYVCK(IDHINS).GT.KEY7)       GO TO 65
              ENDIF
              DO 60 JK = 1, NKEYCK
                IF (IK.EQ.JK.OR.IPURCK(JK).EQ.-1) GO TO 60
                CALL CDKEYR (JK, NWKYCK, KEYNCK)
                DO 55 K = 1, NWKYCK
                  IF (IOKYCA(K).NE.0.AND.KEYNCK(K).NE.KEYVCK(K))
     +                                            GO TO 60
   55           CONTINUE
                IF (KEYNCK(NKLAST).GT.KEYVCK(NKLAST))  THEN
                  IPURCK(IK) = -1
                  GO TO 65
                ENDIF
   60         CONTINUE
   65       CONTINUE
            CALL CDDELK (IRC)
            NDEL   = NDEL + IQUEST(2)
   70     CONTINUE
*
*  *      Now delete in one subdirectory from results of other
*
C\\\          JK2    = NKEYS + 1
C\\\   75     IF (JK2.GT.2)  THEN
C\\\            JK2    = JK2 - 1
*
*         - master directories loop
*
          DO 118 JK2=NKEYS,1,-1
            CALL CDPATH (TOP1CT, JK2)
            PAT3CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT3CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PAT3CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            NKEY2  = IQUEST(7)
            IF (NKEY2.EQ.0)        GO TO 118
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
*
*  *        Get the master object names in the master subdirectory
*
            CALL VZERO (INDKCK, NKEY2)
            DO 90 IK = 1, NKEY2
              INDKCK(IK) = 2
C\\\              CALL CDKEYR (IK, NWKYCK, KEYVCK)
C\\\              IF (KEY7.GT.0) THEN
C\\\                IF (KEYVCK(IDHINS).GT.KEY7)       GO TO 90
C\\\              ENDIF
C\\\              DO 85 JK = 1, NKEY2
C\\\                IF (IK.EQ.JK.OR.INDKCK(JK).EQ.1)  GO TO 85
C\\\                CALL CDKEYR (JK, NWKYCK, KEYNCK)
C\\\                DO 80 K = 1, NWKYCK
C\\\                  IF (IOKYCA(K).NE.0.AND.KEYVCK(K).NE.KEYNCK(K))
C\\\     +                                            GO TO 85
C\\\   80           CONTINUE
C\\\                IF (KEYNCK(NKLAST).GT.KEYVCK(NKLAST))  THEN
C\\\                  INDKCK(IK) = 1
C\\\                  GO TO 90
C\\\                ENDIF
C\\\   85         CONTINUE
   90       CONTINUE
C\\\            II     = IUCOMP (2, INDKCK, NKEY2)
C\\\            IF (II.EQ.0)                          GO TO 75
C\\\\       I1     = 1
*
*  *        Loop over slave subdirectories
*
            DO 115 JK3 = 1, JK2-1
              JK1    = JK2 - JK3
              CALL CDPATH (TOP1CT, JK1)
              PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
*
              CALL RZCDIR (PAT2CT, ' ')
              IF (IQUEST(1).NE.0)  THEN
                IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG :'//
     +          ' Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
                GO TO 999
              ENDIF
              LCDRCD = IQUEST(11)
              IKDRCD = IQUEST(13)
              NKEYCK = IQUEST(7)
              IF (NKEYCK.EQ.0)                    GO TO 115
*
              DO 95 IK = 1, NKEYCK
                IPURCK(IK) = 2
   95         CONTINUE
*
*             - loop over objects in the master directory
              DO 112 II=1,NKEY2
                IF(INDKCK(II).EQ.2) THEN
                  CALL RZCDIR (PAT3CT, ' ')
                  IF (IQUEST(1).NE.0)  THEN
                    IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
                    IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' DBPU'//
     +              'RG : Illegal Path Name '//PAT3CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
                    GO TO 999
                  ENDIF
                  LCDRCD = IQUEST(11)
                  IKDRCD = IQUEST(13)
                  CALL CDKEYR (II, NWKYCK, KEYVCK)
                  IF (KEY7.GT.0) THEN
                    IF (KEYVCK(IDHINS).GT.KEY7)   GO TO 112
                  ENDIF
*
                  CALL RZCDIR (PAT2CT, ' ')
                  IF (IQUEST(1).NE.0)  THEN
                    IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
                    IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' DBPU'//
     +              'RG : Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
                    GO TO 999
                  ENDIF
                  LCDRCD = IQUEST(11)
                  IKDRCD = IQUEST(13)
*
*  *              Check all key elements in the slave subdirectory
*
                  DO 110 IK = 1, NKEYCK
                    CALL CDKEYR (IK, NWKYCK, KEYNCK)
                    KEY1CK(IK) = KEYNCK(IDHKSN)
                    IF (KEY7.GT.0) THEN
                      IF (KEYNCK(IDHINS).GT.KEY7) GO TO 110
                    ENDIF
                    DO 105 K = 1, NWKYCK
                      IF (IOKYCA(K).NE.0.AND.KEYVCK(K).NE.KEYNCK(K))
     +                                            GO TO 110
  105               CONTINUE
                    IF (KEYNCK(NKLAST).LE.KEYVCK(NKLAST)) THEN
                      IPURCK(IK) = 0
                    ELSE
                      INDKCK(II) = 0
                    ENDIF
*
  110             CONTINUE
                ENDIF
*             v-- end of loop over objects in the master directory
  112         CONTINUE
*             - delete flagged objects in the slave directory
              CALL CDDELK (IRC)
              NDEL   = NDEL + IQUEST(2)
*
*           v-- end of loop over slave subdirectories
  115       CONTINUE
*           - delete flagged objects in the master directory
            NKEYCK=NKEY2
            CALL UCOPY(INDKCK,IPURCK,NKEYCK)
            CALL RZCDIR (PAT3CT, ' ')
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            CALL CDDELK (IRC)
            NDEL   = NDEL + IQUEST(2)
C\\\            GO TO 75
C\\\          ENDIF
  118     CONTINUE
*
        ENDIF
*
      ELSE IF (IOPKCA.NE.0)  THEN
*
*  **   Delete all keys for which KEY(KYDAT).le.KYTIM
*
        IF (IOPTP.EQ.0)  THEN
*
          CALL VZERO (IPURCK, NKEYCK)
*
*  **     Label by '2' the objects for which KEY(KYDAT).le.KYTIM
*
          DO 120 IK = 1, NKEYCK
            CALL CDKEYR (IK, NWKYCK, KEYNCK)
            KEY1CK(IK) = KEYNCK(IDHKSN)
            IF (KEY7.LE.0) THEN
              IF (KEYNCK(KYDAT).GE.KYTIM) IPURCK(IK) = 2
            ELSE
              IF (KEYNCK(IDHINS).GT.KEY7) THEN
                IPURCK(IK) = 2
              ELSE
                IF (KEYNCK(KYDAT).GE.KYTIM) IPURCK(IK) = 2
              ENDIF
            ENDIF
  120     CONTINUE
          CALL CDDELK (IRC)
          NDEL   = IQUEST(2)
*
        ELSE
*
          NKEYS  = NKEYCK
          DO 130 JK1 = 1, NKEYS
            JK     = NKEYS + 1 - JK1
            CALL CDPATH (TOP1CT, JK)
            PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            NKEYCK = IQUEST(7)
            IF (NKEYCK.LE.0)                      GO TO 130
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            CALL CDKEYT
*
            CALL VZERO (IPURCK, NKEYCK)
*
*  **       Label by '2' the object for which KEY(KYDAT).le.KYTIM
*
            DO 125 IK = 1, NKEYCK
              CALL CDKEYR (IK, NWKYCK, KEYNCK)
              KEY1CK(IK) = KEYNCK(IDHKSN)
              IF (KEY7.LE.0) THEN
                IF (KEYNCK(KYDAT).GE.KYTIM) IPURCK(IK) = 2
              ELSE
                IF (KEYNCK(IDHINS).GT.KEY7) THEN
                  IPURCK(IK) = 2
                ELSE
                  IF (KEYNCK(KYDAT).GE.KYTIM) IPURCK(IK) = 2
                ENDIF
              ENDIF
  125       CONTINUE
            CALL CDDELK (IRC)
            NDEL   = NDEL + IQUEST(2)
  130     CONTINUE
*
        ENDIF
*
      ELSE IF (IOPSCA.NE.0)  THEN
*
*  **   Delete all keys for with KEY(IDHKSN) in the range KYDAT-KYTIM
*
        IF (IOPTP.EQ.0)  THEN
*
          IPNT   = KOFSCD + LCDRCD + IKDRCD
          ISTP   = NWKYCK + 1
*
*  **     Label by '2' objects for which KEY(1) not in range KYDAT-KYTIM
*
          DO 140 IK = 1, NKEYCK
            KEY1CK(IK) = IQ(IPNT+(IK-1)*ISTP+IDHKSN)
            IF (KEY7.LE.0) THEN
              IF (KEY1CK(IK).GE.KYDAT.AND.KEY1CK(IK).LE.KYTIM) THEN
                IPURCK(IK) = 0
              ELSE
                IPURCK(IK) = 2
              ENDIF
            ELSE
              IF (IQ(IPNT+(IK-1)*ISTP+IDHINS).GT.KEY7) THEN
                IPURCK(IK) = 2
              ELSE
                IF (KEY1CK(IK).GE.KYDAT.AND.KEY1CK(IK).LE.KYTIM) THEN
                  IPURCK(IK) = 0
                ELSE
                  IPURCK(IK) = 2
                ENDIF
              ENDIF
            ENDIF
  140     CONTINUE
          CALL CDDELK (IRC)
          NDEL   = IQUEST(2)
*
        ELSE
*
          NKEYS  = NKEYCK
          KST    = NWKYCK + 1
          MAXKY  = -1
          DO 160 JK1 = 1, NKEYS
            JK     = NKEYS + 1 - JK1
            IPNT   = KOFSCD + LCDRCD + IKDRCD
            KPNT   = IUHUNT (JK, IQ(IPNT+MPSRCD), NKEYS*KST, KST)
            IF (KPNT.GT.0) THEN
              KPNT   = KPNT + IPNT - MPSRCD
            ELSE
              KPNT   = IPNT + (JK - 1) * KST
            ENDIF
            MINKY  = IQ(KPNT+MOBJCD) + 1
            IF (KYTIM.LT.MINKY)                   GO TO 155
            IF (MAXKY.GT.0.AND.KYDAT.GT.MAXKY)    GO TO 155
            CALL CDPATH (TOP1CT, JK)
            PAT2CT = PATHY(1:NCHAR)//'/'//TOP1CT
            CALL RZCDIR (PAT2CT, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PAT2CT//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            NKEYCK = IQUEST(7)
            IF (NKEYCK.LE.0)                      GO TO 150
            NWKYCK = IQUEST(8)
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
            CALL CDKEYT
            IPNT   = KOFSCD + LCDRCD + IKDRCD
            ISTP   = NWKYCK + 1
*
*  **       Label by '2' objects with KEY(1) not in range KYDAT-KYTIM
*
            DO 145 IK = 1, NKEYCK
              KEY1CK(IK) = IQ(IPNT+(IK-1)*ISTP+IDHKSN)
              IF (KEY7.LE.0) THEN
                IF (KEY1CK(IK).GE.KYDAT.AND.KEY1CK(IK).LE.KYTIM) THEN
                  IPURCK(IK) = 0
                ELSE
                  IPURCK(IK) = 2
                ENDIF
              ELSE
                IF (IQ(IPNT+(IK-1)*ISTP+IDHINS).GT.KEY7) THEN
                  IPURCK(IK) = 2
                ELSE
                  IF (KEY1CK(IK).GE.KYDAT.AND.KEY1CK(IK).LE.KYTIM) THEN
                    IPURCK(IK) = 0
                  ELSE
                    IPURCK(IK) = 2
                  ENDIF
                ENDIF
              ENDIF
  145       CONTINUE
            CALL CDDELK (IRC)
            NDEL   = NDEL + IQUEST(2)
*
  150       CALL RZCDIR (PATHY, ' ')
            IF (IQUEST(1).NE.0)  THEN
              IRC    = 111
#endif
#if (defined(CERNLIB__DEBUG))&&(!defined(CERNLIB__P3CHILD))
              IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDPURG : '//
     +        'Illegal Path Name '//PATHY//''')', IARGCD, 0)
#endif
#if !defined(CERNLIB__P3CHILD)
              GO TO 999
            ENDIF
            LCDRCD = IQUEST(11)
            IKDRCD = IQUEST(13)
  155       MAXKY  = MINKY - 1
  160     CONTINUE
*
        ENDIF
*
      ENDIF
#endif
*
      IQUEST(2) = NDEL
*                                                             END CDPURG
  999 END
