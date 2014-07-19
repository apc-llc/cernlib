*
* $Id: cdbfor.F,v 1.1.1.1 1996/02/28 16:24:41 mclareni Exp $
*
* $Log: cdbfor.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:41  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDBFOR (TOPN, IDATE, ITIME, IRC)
*     ===========================================
*
************************************************************************
*                                                                      *
*        SUBR. CDBFOR (TOPN, IDATE, ITIME, IRC*)                       *
*                                                                      *
*   Set the maximum insertion time for retrieval of all subsequent     *
*   data objects for a given top directory                             *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     TOPN     Name of the top directory ('*' means all)               *
*     IDATE    Date : 6 Decimal integer : YYMMDD                       *
*     ITIME    Time : 4 Decimal integer : HHMM                         *
*     IRC      Return code (see below)                                 *
*                                                                      *
*   Called by user                                                     *
*                                                                      *
*   Error Condition :                                                  *
*                                                                      *
*     IRC       =  0 : No error                                        *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/cinitl.inc"
      CHARACTER       TOPN*(*), TOP*16
*
*     ------------------------------------------------------------------
*
      IRC = 0
      CALL CDPKTM (IDATE, ITIME, KY7M, IRC)
      CALL CDSBLC (TOPN, TOPNCI, NCHR)
*
* *** Insert the KY7M value for the appropriate
*
      LFIXCD = LTOPCD
   10 IF (LFIXCD.GT.0) THEN
        NCH    = IQ(KOFUCD+LFIXCD+MUPNCH)
        IF (NCH.EQ.NCHR) THEN
          CALL UHTOC (IQ(KOFUCD+LFIXCD+MUPNAM), 4, TOP, NCH)
          IF (TOPNCI.EQ.'*'.OR.TOP(1:NCH).EQ.TOPNCI) THEN
            IQ(KOFUCD+LFIXCD+MUPKY7) = KY7M
#if defined(CERNLIB__DEBUG)
            IF (IDEBCD.GT.0) THEN
              IARGCD(1) = IDATE
              IARGCD(2) = ITIME
              CALL CDPRNT (LPRTCD, '(/,'' CDBFOR : All subsequent ret'//
     +             'rievals of data objects for '//TOP(1:NCH)//' will'//
     +             ' test on insertion time before '',2I7)', IARGCD, 2)
            ENDIF
#endif
          ENDIF
          LFIXCD = LQ(KOFUCD+LFIXCD)
          GO TO 10
        ENDIF
      ENDIF
*                                                             END CDBFOR
      END
