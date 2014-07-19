*
* $Id: cdbank.F,v 1.1.1.1 1996/02/28 16:24:28 mclareni Exp $
*
* $Log: cdbank.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBANK (IDIV,LAD,LSUP,JBIAS,CHID,NL,NS,ND,NIO,NZ,IRC)
*     ================================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDBANK (IDIV,LAD*,LSUP,JBIAS,CHID,NL,NS,ND,NIO,NZ,IRC*) *
*                                                                      *
*   Creates a bank taking care of total ammount of space available in  *
*   the division. When not enough space is available after garbage     *
*   collection, the banks freed by DBFREE are dropped.                 *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDIV     Division number where the object is to be created       *
*     LAD      L-address of the bank created                           *
*     LSUP     L-address of the supporting bank                        *
*     JBIAS    Link bias as described in ZEBRA manual                  *
*     CHID     Character string to specify the bank name               *
*     NL,NS,   Total number of links, number of structural links,      *
*     ND,NIO,  number of data words, I/O characteristics flag          *
*     NZ       for initialising the bank (MZBOOK standard)             *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by various routines in the HEPDB package                    *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 99 : No space in memory for creating the bank        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      CHARACTER       CHID*(*), CHID0*4
      DIMENSION       NL(9), NS(9), ND(9), NIO(9), LSUP(9), LAD(9)
*
*     ------------------------------------------------------------------
*
* *** See if enough space is available in memory
*
      NEEDW  = NL(1) + ND(1) + 20
      CALL MZNEED (IDIV, NEEDW, ' ')
      IF (IQUEST(11).LT.0)  THEN
        CALL MZNEED (IDIV, NEEDW, 'G')
        IF (IQUEST(11).LT.0)  THEN
          LGO    = 0
   10     LBFYCD = LZFIDH (IDIV, IHKYCD, LGO)
          IF (LBFYCD.NE.0)  THEN
            ND0    = IQ(KOFUCD+LBFYCD-1)
            IF (IQ(KOFUCD+LBFYCD+ND0+MKYFRI).GT.0)  THEN
              LDAT   = LQ(KOFUCD+LBFYCD-KLDACD)
              IF (LDAT.GT.0) CALL MZDROP (IDIV, LDAT, 'L')
            ENDIF
            LGO    = LBFYCD
            GO TO 10
          ENDIF
          CALL MZNEED (IDIV, NEEDW, 'G')
          IF (IQUEST(11).LT.0)  THEN
            IRC       = 99
            IQUEST(1) = 99
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              CHID0  = CHID
              CALL CDPRNT (LPRTCD, '(/,'' CDBANK : No space left for '//
     +             'creating bank '//CHID0//' space needed '',I12)',
     +             IQUEST(11), 1)
            ENDIF
#endif
            GO TO 999
          ENDIF
        ENDIF
      ENDIF
*
* *** Create the bank as desired
*
      CALL MZBOOK (IDIV, LAD(1), LSUP(1), JBIAS, CHID, NL, NS, ND, NIO,
     +             NZ)
      IRC       = 0
      IQUEST(1) = 0
      IQ(KOFUCD+LAD(1)-5) = 0
*                                                             END CDBANK
  999 END
