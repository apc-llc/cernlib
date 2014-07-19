*
* $Id: zexam3.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: zexam3.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEXAM3
C
C   EXAMPLE  3:  - INITIALISATION OF A DYNAMIC STRUCTURE
C                - BOOKING AND FILLING OF A BANK
C                - USAGE OF ZPUSH, ZNAME, ZPRINT, ZDROP
C
      COMMON/ZCOMM/Z(1),ID1,ID2,ID3,WS(1)
      DIMENSION IZ(1000),IWS(1)
      EQUIVALENCE (IZ(1),Z(1)), (IWS(1),WS(1))
C
      LOGICAL ZBUG
C
C   INITIALISATION
C
      CALL ZINIT (Z,WS,1000)
C
C   BOOKING AND FILLING
C
      NWORDS = 23
      CALL ZBOOK (Z,ID3,NWORDS)
C
      IF (ID3.EQ.0)                              GO TO 999
C
      DO  10 I = 1,NWORDS
   10 IZ(ID3 + I) = I
C
C   CHANGE THE BANK LENGTH
C
      NMORE  = 3
      CALL ZPUSH (Z,ID3,NMORE)
      IF (ZBUG (Z,100))                          GO TO 999
C
      DO  20 I = 1,NMORE
      J      = ID3 + NWORDS + I
   20 Z(J)   = FLOAT (I)
C
C   NAME AND NUMBER
C
      CALL ZNAME (Z,ID3,'TEST',1)
C
C   PRINT THE BANK
C
      CALL ZPRINT (Z,ID3,18HBANK OF EXAMPLE 3$,1HF,0,0)
C
C   DROP IT
C
      CALL ZDROP (Z,ID3)
C
  999 RETURN
      END
