*
* $Id: znamsr.F,v 1.1.1.1 1996/03/08 12:01:13 mclareni Exp $
*
* $Log: znamsr.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:13  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
      SUBROUTINE ZNAMSR(IZ,NAME,IST,LAST)
*--- PERFORMS BINARY SEARCH IN TABLE WITH NAMES IN
*--- ASCENDING ORDER.
*--- INPUT
*   NAME  ELEMENT TO LOOK FOR
*--- OUTPUT
*   IST  = -1 NO BANK FOR NAMES FOUND
*        = 0  NAME NOT FOUND IN TABLE
*             OTHERWISE PLACE OF NAME IN LIST
*  LAST  = 0  NAME FOUND
*             OTHERWISE NAME IN LIST BEHIND WHICH NAME BELONGS
      PARAMETER (MLINK=6,MFBANK=4)
      DIMENSION IZ(*)
      CHARACTER *4 SNAME,SLIST
      LOGICAL ZIDOK
      IST=-1
      LAST=0
      J=IZ(IZ(1)-MLINK)
      IF (.NOT.ZIDOK(IZ,J)) GOTO 999
      N=IZ(J+1)
      JN=IZ(J-1)
      IF (.NOT.ZIDOK(IZ,JN)) GOTO 999
      IST=0
      IF(N.GT.0)  THEN
         KPOS=0
         CALL UHTOC(NAME,4,SNAME,4)
   10    M=(N+1)/2
         LAST=KPOS+M
         CALL UHTOC(IZ(JN+LAST),4,SLIST,4)
         IF (SNAME.LT.SLIST)  THEN
            N=M
            LAST=LAST-1
            IF (N.GT.1) GOTO 10
         ELSEIF (SNAME.GT.SLIST)  THEN
            KPOS=LAST
            N=N-M
            IF (N.GT.0) GOTO 10
         ELSE
            IST=LAST
         ENDIF
      ENDIF
  999 END
