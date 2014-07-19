*
* $Id: cdrvpl.F,v 1.1.1.1 1996/02/28 16:24:52 mclareni Exp $
*
* $Log: cdrvpl.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:52  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDRVPL (INDX, NOBJ, NMASK, KOBJ1,KOBJ2, MASK,KEYS, IRC)
*     ==================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDRVPL (INDX,NOBJ,NMASK,KOBJ1*,KOBJ2*,MASK*,KEYS*,IRC*) *
*                                                                      *
*   Reads variables for CDACPL from command line or external file      *
*                                                                      *
*   Argements :                                                        *
*                                                                      *
*     INDX     Type of input (1 for CDPLOB; 2 for CDPLOV; 0 otherwise) *
*     NOBJ     Number of objects                                       *
*     NMASK    Number of key elements to be selected upon              *
*     KOBJ1    Array for object indices for CDPLOB                     *
*     KOBJ2    Array for pair of object indices for CDPLOV             *
*     MASK     Integer vector indicating which elements of KEYS are    *
*              significant for selection                               *
*     KEYS     Key indices                                             *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDACPL                                                   *
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
#include "hepdb/cxlink.inc"
      DIMENSION       KEYS(9), MASK(9), KOBJ2(2,9), KOBJ1(9)
      CHARACTER       CFNAM*80, CHPRO*32, CTEMP*5
*
*     ------------------------------------------------------------------
*
      CALL KUGETI (LUNI)
      CALL KUGETC (CFNAM, NCF)
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
          CALL CDPRNT (L3PRCX, '(/,'' CDRVPL : Error '',I12,'' in ope'//
     +         'ning file '//CFNAM(1:NCF)//''')', ISTAT, 1)
          IRC    = 167
          GO TO 999
        ENDIF
*
*  **   Read the object indices first
*
        IF (INDX.EQ.1) THEN
          DO 10 I = 1, NOBJ
            READ (LUNI, *, ERR=25, END=25) KOBJ1(I)
   10     CONTINUE
        ELSE IF (INDX.EQ.2) THEN
          DO 15 I = 1, NOBJ
            READ (LUNI, *, ERR=25, END=25) KOBJ2(1,I), KOBJ2(2,I)
   15     CONTINUE
        ENDIF
*
*  **   Now read the key values
*
        DO 20 II = 1, NMASK
          READ (LUNI, *, ERR=25, END=25) I
          IF (I.LT.1.OR.I.GT.NWKYCK) GO TO 25
          MASK(I) = 1
          IF (I.EQ.IDHINS.OR.(I.GT.NOF1CK.AND.I.LE.NOF1CK+2*NPARCD.AND.
     +        IOPXCA.NE.0)) THEN
            READ (LUNI, *, ERR=25, END=25) IDATE, ITIME
            IF (I.EQ.IDHINS) THEN
              CALL CDPKTM (IDATE, ITIME, KEYS(I), IRC)
            ELSE
              CALL CDPKTS (IDATE, ITIME, KEYS(I), IRC)
            ENDIF
          ELSE
            READ (LUNI, *, ERR=25, END=25) KEYS(I)
          ENDIF
   20   CONTINUE
        IRC    = 0
        GO TO 30
*
   25   CALL CDPRNT (L3PRCX, '(/,'' CDRVPL : Error in reading file '//
     +       CFNAM(1:NCF)//''')', ISTAT, 0)
        IRC    = 167
   30   CALL CDCLFL (LUNI)
*
      ELSE
*
* ***   Read the information from the command line
*
        IF (INDX.EQ.1) THEN
          DO 35 I = 1, NOBJ
            WRITE (CHPRO, 1001) I
            CALL KUPROI (CHPRO, KOBJ1(I))
   35     CONTINUE
        ELSE IF (INDX.EQ.2) THEN
          DO 40 I = 1, NOBJ
            WRITE (CHPRO, 1002) 'First ',I
            CALL KUPROI (CHPRO, KOBJ2(1,I))
            WRITE (CHPRO, 1002) 'Second',I
            CALL KUPROI (CHPRO, KOBJ2(2,I))
   40     CONTINUE
        ENDIF
*
*  **   Now read the key values
*
        DO 50 II = 1, NMASK
   45     CHPRO  = 'Next key index to be selected upon'
          CALL KUPROI (CHPRO, I)
          IF (I.LT.1.OR.I.GT.NWKYCK) THEN
            CALL CDPRNT (L3PRCX, '(/,'' CDRVPL : Incorrect key index '//
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
            WRITE (CHPRO, 1003) 'YYMMDD', CTEMP
            CALL KUPROI (CHPRO, IDATE)
            WRITE (CHPRO, 1003) 'HHMMSS', CTEMP
            CALL KUPROI (CHPRO, ITIME)
            CALL CDPKTS (IDATE, ITIME, KEYS(I), IRC)
          ELSE IF (I.EQ.IDHINS) THEN
            WRITE (CHPRO, 1004) 'YYMMDD'
            CALL KUPROI (CHPRO, IDATE)
            WRITE (CHPRO, 1004) 'HHMM'
            CALL KUPROI (CHPRO, ITIME)
            CALL CDPKTM (IDATE, ITIME, KEYS(I), IRC)
          ELSE
            WRITE (CHPRO, 1005) I
            CALL KUPROI (CHPRO, KEYS(I))
          ENDIF
   50   CONTINUE
        IRC    = 0
      ENDIF
*
 1001 FORMAT ('Index for Object',I2,' ?')
 1002 FORMAT (A,' index for Object',I2,' ?')
 1003 FORMAT (A,' of ',A,' Range')
 1004 FORMAT (A,' of cut in insertion time')
 1005 FORMAT ('Key(',I2,') ?')
*                                                             END CDRVPL
  999 END
