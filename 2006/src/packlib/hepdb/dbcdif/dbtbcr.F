*
* $Id: dbtbcr.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbtbcr.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBTBCR (IUDIV, LAD, LSUP, JBIAS)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. DBTBCR (IUDIV, LAD*, LSUP, JBIAS)                       *
*                                                                      *
*   Creates DBTB bank with information of the data base objects used   *
*   for this event (till the last call to DBTBCR). It stores 2 words   *
*   per object used, a unique identifier corresponding to the path     *
*   name and the serial number of the object (KEY(1) value)            *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IUDIV    User division where the DBTB bank has to be created     *
*     LAD(*)   Address of the DBTB bank (should be in the same store   *
*              as all DB objects)                                      *
*     LSUP     Address of the supporting bank                          *
*     JBIAS    Link bias as described in ZEBRA manual                  *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IQUEST(1) =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
      DIMENSION       LSUP(9), LAD(9)
*
*     ------------------------------------------------------------------
*
      CALL CDINFO (IUDIV, LAD, LSUP, JBIAS, IRC)
      IQUEST(1) = IRC
*                                                             END DBTBCR
  999 END
