*
* $Id: kuvdel.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kuvdel.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.02/02 05/05/93  09.36.54  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KUVDEL(VNAME)
*
********************************************************************************
*
* Delete vector VNAME.
* If VNAME='*' all vectors are deleted.
*
*     Input :
*
      CHARACTER*(*) VNAME
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
#include "kuip/kcvect.inc"
      CHARACTER*32 VN
*
      IS=INDEX(VNAME,'*')
      IF (IS.NE.0) THEN
        LVECN=LQ(LVEC-1)
        LVECN=LQ(LVECN)
        DO 5 I=2,NUMVEC
          CALL UHTOC(IQ(LVECN+1),4,VN,32)
          IF (IS.GT.1) THEN
            IF (VN(1:IS-1).NE.VNAME(1:IS-1)) GO TO 10
          ENDIF
          CALL MZDROP(IXKUIP,LVECN,' ')
          IF (IQUEST(1).NE.0) GO TO 900
*
* inform other applications (COMIS) that the vector has been deleted
*
          CALL KICOMV(VN)
          NUMVEC=NUMVEC-1
10        LVECN=LQ(LVECN)
5       CONTINUE
        GO TO 999
      ENDIF
      IF (NUMVEC.LE.1) THEN
        CALL KUALFA
        PRINT *,'*** No vector defined'
        GO TO 999
      ENDIF
      CALL KUVECT(VNAME,LLOW,LHIGH)
      IF (LLOW.EQ.0) THEN
        CALL KUALFA
        PRINT *,'*** Unknown vector ',VNAME
        GO TO 999
      ENDIF
      IF (VNAME.EQ.'?') THEN
        CALL KUALFA
        PRINT *,'*** Cannot delete this vector'
        GO TO 999
      ENDIF
      CALL MZDROP(IXKUIP,LVECN,' ')
      CALL KICOMV(VNAME)
      NUMVEC=NUMVEC-1
      IF (IQUEST(1).NE.0) GO TO 900
      GO TO 999
900   CALL KUALFA
      PRINT *,'*** Error in deleting vector'
999   END



