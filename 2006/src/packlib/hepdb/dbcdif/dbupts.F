*
* $Id: dbupts.F,v 1.1.1.1 1996/02/28 16:25:02 mclareni Exp $
*
* $Log: dbupts.F,v $
* Revision 1.1.1.1  1996/02/28 16:25:02  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBUPTS (IDATE, ITIME, IDATM)
*     =======================================
*
************************************************************************
*                                                                      *
*        SUBR. DBUPTS (IDATE*, ITIME*, IDATM)                          *
*                                                                      *
*   Unpacks date and time from one word                                *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 6 Decimal integer : HHMMSS                       *
*     IDATM    Packed date-time                                        *
*                                                                      *
*   Called by user,   DBPLOB, DBPLOV, DBPLTI, DBAUXI, DBDISD, DBDKYH,  *
*             DBDKYV                                                   *
*                                                                      *
************************************************************************
*
      CALL CDUPTS (IDATE, ITIME, IDATM, IRC)
*                                                             END DBUPTS
      END
