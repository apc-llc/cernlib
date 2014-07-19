*
* $Id: cdkeyr.F,v 1.1.1.1 1996/02/28 16:24:28 mclareni Exp $
*
* $Log: cdkeyr.F,v $
* Revision 1.1.1.1  1996/02/28 16:24:28  mclareni
* Hepdb, cdlib, etc
*
*
#include "hepdb/pilot.h"
      SUBROUTINE CDKEYR (IKEY, NWKEY, KEY)
*     ====================================
*
************************************************************************
*                                                                      *
*        SUBR. CDKEYR (IKEY, NWKEY, KEY*)                              *
*                                                                      *
*   Loads the Key vector in the array KEY                              *
*                                                                      *
*   Arguments :                                                        *
*                                                                      *
*     IKEY     Index of the key                                        *
*     NWKEY    Number of key elements to be fetched                    *
*     KEY      The Key vector which will contain the Key elements      *
*              for key IKEY on output                                  *
*                                                                      *
*   Called by CDCDIC, CDCHCK, CDCHKY, CDCOMP, CDDDIR, CDDELK, CDDONT,  *
*             CDEALI, CDENFZ, CDENTB, CDGETDB,CDHUNT, CDKOUT, CDKXIN,  *
*             CDKYSE, CDPART, CDPRDT, CDPURG, CDPURP, CDRENK, CDSEKY,  *
*             CDSNAM, CDUDIC, CDUNCP                                   *
*                                                                      *
************************************************************************
*
#include "hepdb/cdcblk.inc"
#include "hepdb/ckkeys.inc"
      DIMENSION       KEY(9)
*
*     ------------------------------------------------------------------
* *** Retrieve the keys
*
      ISTP   = NWKYCK + 1
      IPNT   = KOFSCD + LCDRCD + IKDRCD + (IKEY - 1) * ISTP
      DO 10 I = 1, NWKEY
        IF (IOTYCK(I).EQ.5 .OR. IOTYCK(I).EQ.6)  THEN
          CALL ZITOH (IQ(IPNT+I), KEY(I), 1)
        ELSE
          KEY(I) = IQ(IPNT+I)
        ENDIF
   10 CONTINUE
*                                                             END CDKEYR
  999 END
