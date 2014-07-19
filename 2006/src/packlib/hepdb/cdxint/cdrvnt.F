*
* $Id: cdrvnt.F,v 1.1.1.1 1996/02/28 16:24:52 mclareni Exp $
*
* $Log: cdrvnt.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRVNT (NPATH, PATHS, NMASK,MASK,KEYS, NVAR, NKST,NDST,
     +                     NKEYX, KEYXS, NOBJS, KOBJS, CTAG, NOBJM, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRVNT (NPATH, PATHS*, NMASK, MASK*, KEYS*, NVAR*,      *
*                      NKST*, NDST*, NKEYX*, KEYXS*, NOBJS*, KOBJS*,   *
*                      CTAG*, NOBJM, IRC*)                             *
*                                                                      *
*   Reads variables for CDAUXI from command line or external file      *
*                                                                      *
*   Argements :                                                        *
*                                                                      *
*     NPATH    Number of pathnames for the directories                 *
*     PATHS    Names of the directory paths                            *
*     NMASK    Number of key elements to be selected upon              *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection                               *
*     KEYS     Vector of keys for character option                     *
*     NVAR     Total number of variables                               *
*     NKST     Number of key  elements selected                        *
*     NDST     Number of data elements selected                        *
*     NKEYX    Number of key  elements for each paths                  *
*     KEYXS    Key  element indices                                    *
*     NOBJS    Number of data elements for each paths                  *
*     KOBJS    Data element indices                                    *
*     CTAG     Tag for each of the elements                            *
*     NOBJM    Maximum number of variables                             *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDAUXI                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               =167 : Error in reading from the file                  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/ctpath.inc"
#include "hepdb/cxlink.inc"
      DIMENSION       KEYS(9), NKEYX(9), KEYXS(9), KOBJS(9), NOBJS(9)
      DIMENSION       MASK(9)
      CHARACTER       CFNAM*80, CHPRO*32, PATHN*80, CTEMP*5
      CHARACTER*(*)   PATHS(*), CTAG(*)
*
*     ------------------------------------------------------------------
*
      CALL KUGETI (LUNI)
      CALL KUGETC (CFNAM, NCF)
      NVAR   = 0
      NKST   = 0
      NDST   = 0
*
      IF (LUNI.GT.0.AND.CFNAM.NE.' ') THEN
*
* ***   Read the information from an external file
*
#if defined(CERNLIB_UNIX)
        CALL CUTOL  (CFNAM)
#endif
        CALL CDOPFL (LUNI, CFNAM, 'OLD', ISTAT)
        IF (ISTAT.NE.0) THEN
          CALL CDPRNT (L3PRCX, '(/,'' CDRVNT : Error '',I12,'' in ope'//
     +         'ning file '//CFNAM(1:NCF)//''')', ISTAT, 1)
          IRC    = 167
          GO TO 999
        ENDIF
*
*  **   Read in the path names first
*
        DO 5 I = 1, NPATH
          READ (LUNI, *, ERR=30, END=30) PATHS(I)
          PATHN = PATHS(I)
          CALL CDLDUP (PATHN, 0, IRC)
          IF (IRC.NE.0) THEN
            NCH   = LENOCC (PATHN)
            CALL CDPRNT (L3PRCX, '('' DB-path '//PATHN(1:NCH)//' is '//
     +           'illegal'')', IARGCD, 0)
            GO TO 30
          ENDIF
          PATHS(I) = PAT1CT
          IF (I.EQ.1) NWKY1 = NWKYCK
    5   CONTINUE
*
*  **   Now read the key values
*
        DO 10 II = 1, NMASK
          READ (LUNI, *, ERR=30, END=30) I
          IF (I.LT.1.OR.I.GT.NWKY1) GO TO 30
          MASK(I) = 1
          IF (I.EQ.IDHINS.OR.(I.GT.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD.AND.
     +        IOPXCA.NE.0)) THEN
            READ (LUNI, *, ERR=30, END=30) IDATE, ITIME
            IF (I.EQ.IDHINS) THEN
              CALL CDPKTM (IDATE, ITIME, KEYS(I), IRC)
            ELSE
              CALL CDPKTS (IDATE, ITIME, KEYS(I), IRC)
            ENDIF
          ELSE
            READ (LUNI, *, ERR=30, END=30) KEYS(I)
          ENDIF
   10   CONTINUE
*
*  **   Now read the items to be stored
*
        DO 25 I = 1, NPATH
          PATHN  = PATHS(I)
          CALL RZCDIR (PATHN, ' ')
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          CALL CDKYTG
          READ (LUNI, *, ERR=30, END=30) NKEYX(I)
          IF (NKEYX(I).LT.1) THEN
            NKEYX(I) = 0
          ELSE IF (NKEYX(I).GT.NWKYCK) THEN
            NKEYX(I) = NWKYCK
          ENDIF
          IF (NVAR+NKEYX(I).GT.NOBJM) THEN
            CALL CDPRNT (L3PRCX, '('' Too many variables for N-tuple'')'
     +,          IARGCD, 0)
            IRC    = 167
            GO TO 30
          ENDIF
          DO 15 J = 1, NKEYX(I)
            READ (LUNI, *, ERR=30, END=30) KEYXS(NKST+J)
            IF (KEYXS(NKST+J).LT.1.OR.KEYXS(NKST+J).GT.NWKYCK) THEN
              CALL CDPRNT (L3PRCX, '('' Invalid Key index'',I12)',
     +             KEYXS(NKST+J), 1)
              IRC    = 167
              GO TO 30
            ENDIF
            CTAG(NVAR+J) = CTAGCK(KEYXS(NKST+J))
   15     CONTINUE
          NVAR   = NVAR + NKEYX(I)
          NKST   = NKST + NKEYX(I)
          READ (LUNI, *, ERR=30, END=30) NOBJS(I)
          IF (NOBJS(I).LT.1) NOBJS(I) = 0
          IF (NVAR+NOBJS(I).GT.NOBJM) THEN
            CALL CDPRNT (L3PRCX, '('' Too many variables for N-tuple'')'
     +,          IARGCD, 0)
            IRC    = 167
            GO TO 30
          ENDIF
          DO 20 J = 1, NOBJS(I)
            READ (LUNI, *, ERR=30, END=30) KOBJS(NDST+J), CTAG(NVAR+J)
            IF (KOBJS(NDST+J).LT.1) KOBJS(NDST+J) = 1
   20     CONTINUE
          NVAR   = NVAR + NOBJS(I)
          NDST   = NDST + NOBJS(I)
   25   CONTINUE
        IRC    = 0
        GO TO 35
*
   30   CALL CDPRNT (L3PRCX, '(/,'' CDRVNT : Error in reading file '//
     +       CFNAM(1:NCF)//''')', ISTAT, 0)
        IRC   = 167
   35   CALL CDCLFL (LUNI)
*
      ELSE
*
* ***   Read the information from the command line
*
        DO 40 I = 1, NPATH
          WRITE (CHPRO, 1001) I
          CALL KUPROC (CHPRO, PATHN, NCH)
          CALL CDLDUP (PATHN, 0, IRC)
          IF (IRC.NE.0) THEN
            NCH   = LENOCC (PATHN)
            CALL CDPRNT (L3PRCX, '('' DB-path '//PATHN(1:NCH)//' is '//
     +           'illegal'')', IARGCD, 0)
            GO TO 999
          ENDIF
          PATHS(I) = PAT1CT
          IF (I.EQ.1) NWKY1 = NWKYCK
   40   CONTINUE
*
*  **   Now read the key values
*
        DO 50 II = 1, NMASK
   45     CHPRO  = 'Next key index to be selected upon'
          CALL KUPROI (CHPRO, I)
          IF (I.LT.1.OR.I.GT.NWKY1) THEN
            CALL CDPRNT (L3PRCX, '(/,'' CDRVNT : Incorrect key index '//
     +           'given '',I3,'' try once more'')', I, 1)
            GO TO 45
          ENDIF
          MASK(I) = 1
          IF (I.GT.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD.AND.IOPXCA.NE.0) THEN
            IF (I.LE.NOF1CK+NPARCD) THEN
              CTEMP  = 'Begin'
            ELSE
              CTEMP  = 'End'
            ENDIF
            WRITE (CHPRO, 1002) 'YYMMDD', CTEMP
            CALL KUPROI (CHPRO, IDATE)
            WRITE (CHPRO, 1002) 'HHMMSS', CTEMP
            CALL KUPROI (CHPRO, ITIME)
            CALL CDPKTS (IDATE, ITIME, KEYS(I), IRC)
          ELSE IF (I.EQ.IDHINS) THEN
            WRITE (CHPRO, 1003) 'YYMMDD'
            CALL KUPROI (CHPRO, IDATE)
            WRITE (CHPRO, 1003) 'HHMM'
            CALL KUPROI (CHPRO, ITIME)
            CALL CDPKTM (IDATE, ITIME, KEYS(I), IRC)
          ELSE
            WRITE (CHPRO, 1004) I
            CALL KUPROI (CHPRO, KEYS(I))
          ENDIF
   50   CONTINUE
*
*  **   Now read the items to be stored
*
        DO 70 I = 1, NPATH
          PATHN  = PATHS(I)
          CALL RZCDIR (PATHN, ' ')
          NKEYCK = IQUEST(7)
          NWKYCK = IQUEST(8)
          LCDRCD = IQUEST(11)
          IKDRCD = IQUEST(13)
          CALL CDKYTG
          WRITE (CHPRO, 1005) I
          CALL KUPROI (CHPRO, NKEYX(I))
          IF (NKEYX(I).LT.1) THEN
            NKEYX(I) = 0
          ELSE IF (NKEYX(I).GT.NWKYCK) THEN
            NKEYX(I) = NWKYCK
          ENDIF
          IF (NVAR+NKEYX(I).GT.NOBJM) THEN
            CALL CDPRNT (L3PRCX, '('' Too many variables for N-tuple'')'
     +,          IARGCD, 0)
            IRC    = 167
            GO TO 999
          ENDIF
          DO 60 J = 1, NKEYX(I)
            WRITE (CHPRO, 1006) J, I
   55       CALL KUPROI (CHPRO, KEYXS(NKST+J))
            IF (KEYXS(NKST+J).LT.1.OR.KEYXS(NKST+J).GT.NWKYCK) GO TO 55
            CTAG(NVAR+J) = CTAGCK(KEYXS(NKST+J))
   60     CONTINUE
          NVAR   = NVAR + NKEYX(I)
          NKST   = NKST + NKEYX(I)
          WRITE (CHPRO, 1007) I
          CALL KUPROI (CHPRO, NOBJS(I))
          IF (NOBJS(I).LT.1) NOBJS(I) = 0
          IF (NVAR+NOBJS(I).GT.NOBJM) THEN
            CALL CDPRNT (L3PRCX, '('' Too many variables for N-tuple'')'
     +,          IARGCD, 0)
            IRC    = 167
            GO TO 999
          ENDIF
          DO 65 J = 1, NOBJS(I)
            WRITE (CHPRO, 1008) J, I
            CALL KUPROI (CHPRO, KOBJS(NDST+J))
            IF (KOBJS(NDST+J).LT.1) KOBJS(NDST+J) = 1
            WRITE (CHPRO, 1009) KOBJS(NDST+J)
            CALL CDPROC (CHPRO, CTAG(NVAR+J), NCH)
   65     CONTINUE
          NVAR   = NVAR + NOBJS(I)
          NDST   = NDST + NOBJS(I)
   70   CONTINUE
        IRC    = 0
*
      ENDIF
*
 1001 FORMAT (I2,'th Path name')
 1002 FORMAT (A,' of ',A,' Range')
 1003 FORMAT (A,' of cut in insertion time')
 1004 FORMAT ('Key(',I2,') ?')
 1005 FORMAT ('No. of keys to store for path ',I2)
 1006 FORMAT (I2,'th Key element for path ',I2)
 1007 FORMAT ('No. of data to store for path ',I2)
 1008 FORMAT (I2,'th data element for path ',I2)
 1009 FORMAT ('Tag for',I3,'th data element')
*                                                             END CDRVNT
  999 END
