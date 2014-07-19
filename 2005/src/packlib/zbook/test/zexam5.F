*
* $Id: zexam5.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: zexam5.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZEXAM5
C
C   EXAMPLE  5:  - INITIALISATION OF A DYNAMIC STRUCTURE
C                - CREATION OF A TREE STRUCTURE (AS IN EXAMPLE 4).
C                - A NEW BRANCH AND NEW DATA CHANGE THE STRUCTURE
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
C   CREATION OF A TREE (AS IN EXAMPLE 4)
C
      NDATA     = 5
      NLINKS = 2
C
      CALL ZBOOKN (Z,ID2,NDATA,NLINKS,'ZEX5',1)
      IF (ID2.EQ.0)                              GO TO 999
C
      DO  10 I = 1,NDATA
   10 IZ(ID2 + I) = I
C
      CALL ZBOOKN (Z,IZ(ID2-1),NDATA,0,'ZEX5',11)
      CALL ZBOOKN (Z,IZ(ID2-2),NDATA,0,'ZEX5',12)
C
      J1     = IZ(ID2 - 1)
      J2     = IZ(ID2 - 2)
      IF (J1.EQ.0.OR.J2.EQ.0)                    GO TO 999
C
      CALL VFILL (Z(J1+1),NDATA,5.)
      CALL VFILL (Z(J2+1),NDATA,6.)
C
C   MODIFY THE MOTHER BANK
C
      MDATA     = 2
      MLINK  = 1
C
      CALL ZPUSHS (Z,ID2,MDATA,MLINK)
      IF (ZBUG (Z,100))                          GO TO 999
C
C   NEW BRANCH
C
      CALL ZBOOKN (Z,IZ(ID2-NLINKS-1),NDATA+MDATA,0,3HNEW,1)
      IF (ZBUG (Z,100))                          GO TO 999
C
      CALL ZPTREE (Z,ID2,5HTREE$,1HF)
C
  999 RETURN
      END
