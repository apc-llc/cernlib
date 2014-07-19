*
* $Id: pjptfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: pjptfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE PJPTFL (ISAQID,POINTR,MAX_ISAQ,NMISAQ)
C----------------------------------------------------------------------
C-
C-   Purpose and Methods : For every PJET bank book and fill the pointer
C-   bank PJPT. Fill reference links in PJPT with ISAQ addresses.
C-
C-   Inputs  : ISAQID(*)        [I]     List of ISAQ bank ids
C-             POINTR(MAX_ISAQ) [I]     Reference pointer array
C-             MAX_ISAQ         [I]     Maximum array dimension
C-             NMISAQ           [I]     number of ISAQ parton in this PJET
C-   Outputs :
C-   Controls:
C-
C-   Created  19-JAN-1990   Boaz Klima
C-
C----------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
      INTEGER MAX_ISAQ
      INTEGER ISAQID(*),POINTR(MAX_ISAQ),NMISAQ
      INTEGER ID,IP,GZISAQ,GZPJET,RFLINK,LZFIND,JET
#include "zebcom.inc"
#include "lkpjet.inc"
C----------------------------------------------------------------------
C
C ****  LPJET address should be in LKPJET common
C
      IF ( LPJET .LE. 0 ) GOTO 999
C
C ****  Get address of first ISAQ bank
C
      LISAQ=GZISAQ()
      IF ( LISAQ.LE.0 ) GOTO 999
C
C ****  BOOK AND FILL JET POINTER BANK FOR THIS JET
C
      CALL BKPJPT(LPJET,NMISAQ,LPJPT)
      DO IP = 1, NMISAQ
        ID = ISAQID ( POINTR(IP) ) ! Get ID of ISAQ bank
C
C ****  Given bank ID find address
C
        RFLINK = LZFIND(IXMAIN,LISAQ,ID,-5)     ! Get ISAQ address
        IF ( RFLINK .GT. 0 ) THEN
          LQ(LPJPT-IP-1) = RFLINK
        ELSE
          LQ(LPJPT-IP-1) = 0
        ENDIF
      ENDDO
  999 RETURN
      END
