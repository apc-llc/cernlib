*
* $Id: cdfzwr.F,v 1.1.1.1 1996/02/28 16:24:11 mclareni Exp $
*
* $Log: cdfzwr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:11  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
#if defined(CERNLIB__P3CHILD)
* Ignoring t=dummy
#endif
      SUBROUTINE CDFZWR (IPREC, CHOPT, IRC)
*     =====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFZWR (IPREC, CHOPT, IRC*)                             *
*                                                                      *
*   Copies the current data structure in memory into the journal file  *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IPREC    Precision word used in the packing algorithm            *
*     CHOPT    Character string to be written in the journal file      *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDRTFZ                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 76 : Cannot form the IO descriptor for the FZ header *
*               = 77 : FZOUT fails to write on to the sequential file  *
*                                                                      *
************************************************************************
*
#include "hepdb/caopts.inc"
#include "hepdb/cdcblk.inc"
#include "hepdb/cfzlun.inc"
#include "hepdb/ckkeys.inc"
#include "hepdb/csavbk.inc"
#include "hepdb/czpack.inc"
      PARAMETER       (NLEVM=20)
      INTEGER         NLCUR(NLEVM)
      CHARACTER       CHCUR(NLEVM)*1, CHFRM*100, CFORM(6)*1, CHOP0*4
      CHARACTER       CHOPT*(*)
      DATA            CFORM /'B', 'I', 'F', 'D', 'H', 'A'/
*
*     ------------------------------------------------------------------
*
* *** Write the sequential output if needed
*
      IF (IOPFCA.EQ.0) THEN
        CHOP0  = CHOPT
      ELSE
        CHOP0  = 'F'//CHOPT
      ENDIF
*
*  ** Find the IO descriptor of the header
*
      NLEV   = 1
      NCUR   = 5
      IFORO  = 2
      CHCUR(NLEV) = CFORM(IFORO)
      IF (INDEX(CHOP0,'Z').NE.0) THEN
        NLCUR(NLEV) = 4
        IFORO  = 3
        NCUR   = 1
        NLEV   = NLEV + 1
        CHCUR(NLEV) = CFORM(IFORO)
        CALL UCOPY (PRECCZ, IHEDCF(MPRECF), 1)
      ELSE
        IHEDCF(MPRECF) = IPREC
      ENDIF
      DO 10 I = 1, NWKYCK
        IFORM  = IOTYCK(I)
        IF (IFORM.EQ.6) IFORM = 5
        IF (IFORM.EQ.IFORO) THEN
          NCUR   = NCUR + 1
        ELSE
          NLCUR(NLEV) = NCUR
          IF (NLEV.GE.NLEVM) THEN
            IRC    = 76
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZWR : Can'//
     +      'not get IO descriptor '')', IARGCD, 0)
#endif
            GO TO 999
          ENDIF
          NLEV   = NLEV + 1
          CHCUR(NLEV) = CFORM(IFORM)
          NCUR   = 1
          IFORO  = IFORM
        ENDIF
   10 CONTINUE
      NLCUR(NLEV) = NCUR
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
      WRITE (CHFRM, 2001) (NLCUR(I), CHCUR(I), I = 1, NLEV)
#endif
#if (defined(CERNLIB_IBM))&&(defined(CERNLIB__P3CHILD))
      CHFRM = ' '
      II    = 1
      DO 15 I = 1, NLEV
        CALL UTWRIT (CHFRM(II:II+1), '(I2)', NLCUR(I))
        II     = II + 2
        CHFRM(II:II) = CHCUR(I)
        II     = II + 2
   15 CONTINUE
#endif
      II     = 4 *NLEV
      CHFRM  = CHFRM(1:II)//' -H'
      CALL MZIOCH (IOFMCF, NWFMCF, CHFRM(1:II+3))
*
*  ** Write on the FZ file
*
      CALL UCTOH (CHOP0, IHEDCF(MPRECF+NWKYCK+1), 4, 4)
      CALL UCOPY (KEYSCS(1,1), IHEDCF(MPRECF+1), NWKYCK)
      CALL FZOUT (LUFZCF, IDISCD, LOBJCS(1), 1, 'L', IOFMCF,
     +            NWHDCF, IHEDCF)
      IF (IQUEST(1).NE.0) THEN
        IRC        = 77
        IQUEST(11) = 1
        IQUEST(12) = 1
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFZWR : Error '//
     +  'in FZOUT while writing Data'')', IARGCD, 0)
#endif
        GO TO 999
      ENDIF
#if !defined(CERNLIB_IBM)||!defined(CERNLIB__P3CHILD)
*
 2001 FORMAT (20(I2,A1,1X))
#endif
*                                                             END CDFZWR
  999 END
