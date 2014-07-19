*
* $Id: cdtous.F,v 1.1.1.1 1996/02/28 16:24:35 mclareni Exp $
*
* $Log: cdtous.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:35  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDTOUS (LC, IUDIV, LU, LSUP, JBIAS, IPREC, IRC)
*     ==========================================================
*
************************************************************************
*                                                                      *
*        SUBR. CDTOUS (LC, IUDIV, LU*, LSUP, JBIAS, IPREC*, IRC*)      *
*                                                                      *
*   Copies Single Bank at LC from the Data Base system into Bank at LU *
*   in the user division in the user system                            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     LC       Address of the bank to be copied                        *
*     IUDIV    Division index of the user division                     *
*     LU       Address of the copied bank                              *
*     LSUP     Address of the supporting bank (see MZBOOK)             *
*     JBIAS    Link Bias (see MZBOOK)                                  *
*     IPREC    Signed precision word; the data are truncated after     *
*              having multiplied by 10**IPREC                          *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by CDKXIN                                                   *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      DIMENSION       LC(9), LSUP(9), LU(9)
*
*     ------------------------------------------------------------------
*
      LREFCD(4) = LC(1)
      NDU    = IQ(KOFUCD+LREFCD(4)-1) - 2
      ITU    = ICDTYP (LREFCD(4))
      CALL CDBANK (IUDIV, LU(1), LSUP(1), JBIAS, 'USER', 0, 0, NDU, ITU,
     +             0, IRC)
      IF (IRC.NE.0) GO TO 999
      IF (ITU.EQ.3) THEN
        CALL UCOPY (Q(KOFUCD+LREFCD(4)+3), Q(KOFUCD+LU(1)+1), NDU)
        IPREC = Q(KOFUCD+LREFCD(4)+2)
      ELSE
        CALL UCOPY (IQ(KOFUCD+LREFCD(4)+3), IQ(KOFUCD+LU(1)+1), NDU)
        IPREC = IQ(KOFUCD+LREFCD(4)+2)
      ENDIF
*                                                             END CDTOUS
  999 END
