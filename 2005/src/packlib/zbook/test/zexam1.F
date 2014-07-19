*
* $Id: zexam1.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: zexam1.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEXAM1
C
C   EXAMPLE  1:  - INITIALISATION OF A DYNAMIC STRUCTURE
C                - BOOKING AND FILLING OF A BANK
C                - ACCESS TO DATA STORED IN A BANK
C
      COMMON/ZCOMM/Z(1),ID1,ID2,ID3,WS(1)
      DIMENSION IZ(1000),IWS(1)
      EQUIVALENCE (IZ(1),Z(1)), (IWS(1),WS(1))
C
C   INITIALISATION
C
      CALL ZINIT (Z,WS,1000)
C
C   BOOKING
C
      NWORDS = 23
      CALL ZBOOK (Z,ID1,NWORDS)
C
C   CHECK IF BANK EXISTS
C
      IF (ID1.EQ.0)                              GO TO 999
C
C   FILLING
C
      DO  10 I = 1,NWORDS
   10 IZ(ID1 + I) = I
C
C   ACCESS TO DATA STORED IN THE BANK
C
      ILAST  = IZ(ID1 + NWORDS)
C
  999 RETURN
      END
