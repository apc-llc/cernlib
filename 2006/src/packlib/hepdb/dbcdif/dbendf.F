*
* $Id: dbendf.F,v 1.1.1.1 1996/02/28 16:24:55 mclareni Exp $
*
* $Log: dbendf.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:55  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE DBENDF (TOPN)
*     ========================
*
************************************************************************
*                                                                      *
*        SUBR. DBENDF (TOPN)                                           *
*                                                                      *
*   Closes one data base file given by the top directory name          *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     TOPN     Name of the top directory                               *
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
      CHARACTER       TOPN*(*)
*
*     ------------------------------------------------------------------
*
      CALL CDEND (TOPN, ' ', IRC)
      IQUEST(1) = IRC
*                                                             END DBENDF
      END
