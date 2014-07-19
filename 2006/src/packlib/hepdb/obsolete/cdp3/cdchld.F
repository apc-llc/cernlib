*
* $Id: cdchld.F,v 1.1.1.1 1996/02/28 16:24:07 mclareni Exp $
*
* $Log: cdchld.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:07  mclareni
* Hepdb, cdlib, etc
*
*
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_IBM)
#include "sys/CERNLIB_machine.h"
#include "_hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDCHLD
*     =================
*
************************************************************************
*                                                                      *
*        SUBR. CDCHLD                                                  *
*                                                                      *
*   Prepares the interrupt message vector and halts execution          *
*                                                                      *
*   Called by CDAFRI, CDATOI, CDDDIR, CDEALI, CDENTB, CDINIT, CDKOUT,  *
*             CDPART, CDPURP, CDRENK, CDSDIR, CDSNAM, CDSPUR, CDUSP3   *
*                                                                      *
************************************************************************
*
#include "cdcblk.inc"
#include "p3dbl3.inc"
*     ------------------------------------------------------------------
*
      IF (RNDBP3.EQ.'CDRZIN  ')  GO TO 100
*
*  ** Encode name of calling routine
*
      CALL UCTOH (RNDBP3, IWDBP3(NWDBP3+1), 4, 8)
      NWDBP3 = NWDBP3 + 2
*
*  ** Interrupt host
*
      CALL APPAUS ('CDCHLD', NWDBP3, IWDBP3)
*
*  ** Extract Host return code
*
      IQDBP3 = IWDBP3(NWDBP3)
      NWDBP3 = NWDBP3 - 1
*
      GO TO 999
*
* *** Called in conjunction with DBRZIN
*
  100 IF (NBKDP3.GT.0) INDXP3 = INDXP3+NBKDP3+21
      IF (INDXP3.LE.1) THEN
        IQDBP3=99
        GO TO 999
      ENDIF
      IQ(KOFUCD+LNK1P3+1) = NDIRP3
*
*  ** Interrupt host
*
      IWDBP3(3) = INDXP3
      CALL UCTOH (RNDBP3, IWDBP3(4), 4, 8)
      NWDBP3 = 5
      CALL APSEND (IQDBP3, 'CDCHLD', NWDBP3, IWDBP3,
     +                          IQ(KOFUCD+LNK1P3+1), INDXP3)
*
      NDIRP3=0
      NBKYP3=0
      NBKDP3=0
      INDXP3=1
      IF (IQDBP3.EQ.0) IQDBP3=IWDBP3(1)
      IF (IQDBP3.NE.0)           GO TO 999
*
*  ** Read the objects now
*
      IF (LNK3P3.NE.0) THEN
        CALL MZDROP (IXDBP3, LNK3P3, 'BVL.')
        LNK3P3 = 0
      ENDIF
      NWDBP3 = 32
      CALL APFZIN (IDDBP3, IXDBP3, LNK3P3, 2, '....', NWDBP3, IWDBP3)
      IQDBP3 = IQUEST(1)
*                                                             END CDCHLD
  999 END
#endif
#endif
