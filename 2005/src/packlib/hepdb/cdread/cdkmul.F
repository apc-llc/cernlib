*
* $Id: cdkmul.F,v 1.1.1.1 1996/02/28 16:24:26 mclareni Exp $
*
* $Log: cdkmul.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:26  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKMUL (KEYS, NKYMX, IRC)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKMUL (KEYS, NKYMX*, IRC*)                             *
*                                                                      *
*   Computes total number of objects to be retrieved in the context    *
*   of M option                                                        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     KEYS     Vector of keys                                          *
*     NKYMX    Total number of banks to be retrieved                   *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDFREE, CDKYDB                                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 21 : Too many keys with option M                     *
*               = 22 : Illegal key option                              *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/cmulop.inc"
      PARAMETER       (NZ=0)
      DIMENSION       KEYS(9)
*
*     ------------------------------------------------------------------
*
      IRC    = 0
      NKYMX  = 1
*
* *** Get number of key banks needed
*
      NMULCM = 0
      ISTKY  = NWKYCK
      IF (IOPMCA.NE.0)  THEN
        IF (NWKYCK.GT.NSYSCK)  THEN
          DO 10 I = NSYSCK+1, NWKYCK
            IF (IOKYCA(I).NE.0) THEN
              IF (NMULCM.GE.NMLMCM)  THEN
*
*  **           Too many keys with option M
*
                IRC        = 21
                IQUEST(11) = NMLMCM + 1
                IQUEST(12) = I
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDKMUL :'//
     +          ' Too many keys '',I3,'' with M option - Last Key'',I3)'
     +,         IQUEST(11), 2)
#endif
                GO TO 999
              ENDIF
*
              IF (KEYS(I).LE.0)  THEN
*
*  **           Illegal key option
*
                IRC        = 22
                IQUEST(11) = I
                IQUEST(12) = KEYS(I)
#if defined(CERNLIB__DEBUG)
                IF (IDEBCD.GT.0) THEN
                  IARGCD(1) = IQUEST(12)
                  IARGCD(2) = IQUEST(11)
                  CALL CDPRNT (LPRTCD, '(/,'' CDKMUL : Illegal key op'//
     +                 'tion '',I3,'' for key '',I3,'' with option M'')'
     +,                IARGCD, 2)
                ENDIF
#endif
                GO TO 999
              ENDIF
*
*  **         Build up the array of keys used for option M
*
              NMULCM = NMULCM + 1
              NOCCCM(NMULCM) = KEYS(I)
              LFKYCM(NMULCM) = ISTKY
              KTYPCM(NMULCM) = I
              ISTKY  = ISTKY + KEYS(I)
              NKYMX  = NKYMX * KEYS(I)
            ENDIF
   10     CONTINUE
        ENDIF
      ENDIF
*                                                             END CDKMUL
  999 END
