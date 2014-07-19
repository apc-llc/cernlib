*
* $Id: cdfrus.F,v 1.1.1.1 1996/02/28 16:24:20 mclareni Exp $
*
* $Log: cdfrus.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:20  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDFRUS (LC, LU, IPREC, IRC)
*     ======================================
*
************************************************************************
*                                                                      *
*        SUBR. CDFRUS (LC, LU*, IPREC, IRC*)                           *
*                                                                      *
*   Copies bank of LC from the user system to bank at LU of the Data   *
*   Base internal system (where 1st word = 0, 2nd word = IPREC)        *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LC       Address of the input bank                               *
*     LU       Address of the output bank                              *
*     IPREC    Signed precision word; the data are truncated after     *
*              having multiplied by 10**IPREC                          *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDENFZ, CDENTB, CDKOUT, CDPART                           *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*               = 92 : Nonstandard IO descriptor                       *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/czpack.inc"
      PARAMETER       (JBIAS=2)
      DIMENSION       LC(9), LU(9)
*
*     ------------------------------------------------------------------
*
      LREFCD(4) = LC(1)
      NDU    = IQ(KOFUCD+LREFCD(4)-1) + 2
      ITU    = ICDTYP (LREFCD(4))
      IF (ITU.NE.2 .AND. ITU.NE.3 .AND. ITU.NE.5) THEN
        IRC        = 92
        IQUEST(11) = ITU
#if defined(CERNLIB__DEBUG)
        IF (IDEBCD.GT.0) CALL CDPRNT (LPRTCD, '(/,'' CDFRUS : Illegal'//
     +  ' Data-type '',I5,/)', IQUEST(11), 1)
#endif
        GO TO 999
      ENDIF
      CALL CDBANK (IDISCD, LU(1), LU(1), JBIAS, 'BASE', 0, 0, NDU, ITU,
     +             0, IRC)
      IF (IRC.NE.0) GO TO 999
      IF (ITU.EQ.3) THEN
        CALL UCOPY (Q(KOFUCD+LREFCD(4)+1), Q(KOFUCD+LU(1)+3), NDU-2)
        Q (KOFUCD+LU(1)+1) = 0.
        IF (PACKCZ) THEN
          Q (KOFUCD+LU(1)+2) = 0.
        ELSE
          Q (KOFUCD+LU(1)+2) = IPREC
        ENDIF
      ELSE IF (ITU.EQ.2) THEN
        CALL UCOPY (IQ(KOFUCD+LREFCD(4)+1), IQ(KOFUCD+LU(1)+3), NDU-2)
        IQ(KOFUCD+LU(1)+1) = 0
        IF (PACKCZ) THEN
          IQ(KOFUCD+LU(1)+2) = 0
        ELSE
          IQ(KOFUCD+LU(1)+2) = IPREC
        ENDIF
      ELSE IF (ITU.EQ.5) THEN
        CALL UCOPY (IQ(KOFUCD+LREFCD(4)+1), IQ(KOFUCD+LU(1)+3), NDU-2)
        IQ(KOFUCD+LU(1)+1) = 0
        IQ(KOFUCD+LU(1)+2) = 0
      ENDIF
*                                                             END CDFRUS
  999 END
