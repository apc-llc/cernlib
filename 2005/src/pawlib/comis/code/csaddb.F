*
* $Id: csaddb.F,v 1.1.1.1 1996/02/26 17:16:23 mclareni Exp $
*
* $Log: csaddb.F,v $
* Revision 1.1.1.1  1996/02/26 17:16:23  mclareni
* Comis
*
*
#include "comis/pilot.h"
*CMZ :  1.11/01 24/01/91  11.36.56  by  Rene Brun
*-- Author : V.Berezhnoi
      SUBROUTINE CSADDB(NAME)
         CHARACTER*(*)NAME
#include "comis/csrec.inc"
#include "comis/cslun.inc"
         COMMON/CSCHBP/NP,NF
      L=LEN(NAME)
      I1=1
 1    IF(NF+L.GE.81)THEN
        IF(NF.EQ.NP)THEN
          REC1(NF:)=NAME(I1:)
          L1=81-NF
          NF=81
          I1=I1+L1
          L=L-L1
        ENDIF
        CALL CSSOUT(REC1(:NF-1))
        IF(ISTLOG.NE.0)THEN
          WRITE(LUNLOG,77)REC1(1:NF-1)
  77       FORMAT(A)
        ENDIF
        NF=NP
        GO TO 1
      ELSE
         REC1(NF:)=NAME(I1:)
         NF=NF+L
      ENDIF
      END
