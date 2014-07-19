*
* $Id: zexam.F,v 1.1.1.1 1996/03/08 12:01:14 mclareni Exp $
*
* $Log: zexam.F,v $
* Revision 1.1.1.1  1996/03/08 12:01:14  mclareni
* Zbook
*
*
#include "zbook/pilot.h"
#if defined(CERNLIB_CDC)
      PROGRAM ZEXAM(OUTPUT,TAPE1,TAPE2,TAPE6=OUTPUT)
#endif
#if !defined(CERNLIB_CDC)
      PROGRAM ZEXAM
#endif
C
      I      = 1
      WRITE(6,1000)
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
      WRITE(6,1040)
C
      CALL ZEXAM1
C
      I      = 2
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
      WRITE(6,1040)
C
      CALL ZEXAM2
C
      I      = 3
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
C
      CALL ZEXAM3
C
      I      = 4
      WRITE(6,1040)
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
C
      CALL ZEXAM4
C
      I      = 5
      WRITE(6,1040)
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
C
      CALL ZEXAM5
C
      I      = 6
      WRITE(6,1040)
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
C
      CALL ZEXAM6
C
      I      = 7
      WRITE(6,1040)
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
C
      CALL ZEXAM7
C
      I      = 8
      WRITE(6,1040)
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1010)
      WRITE(6,1040)
C
      CALL ZEXAM8
C
      I      = 9
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1030) I
      WRITE(6,1020)
      WRITE(6,1060)
      WRITE(6,1070)
      WRITE(6,1020)
      WRITE(6,1010)
      WRITE(6,1040)
C
      CALL ZEXAM9
C
#if defined(CERNLIB_IBM)||defined(CERNLIB_VAXVMS)
      I      = 1
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1050) I
      WRITE(6,1020)
      WRITE(6,1010)
      WRITE(6,1040)
C
      CALL ZEPEX1
C
      CALL EPRWND (2,IBUF,IERR)
C
      I      = 2
      WRITE(6,1010)
      WRITE(6,1020)
      WRITE(6,1050) I
      WRITE(6,1020)
      WRITE(6,1010)
C
      CALL ZEPEX2
#endif
C
      STOP
 1000 FORMAT ('1')
 1010 FORMAT (1X,80('*'))
 1020 FORMAT (1X,20('*'),40X,20('*'))
 1030 FORMAT (1X,20('*'),10X,'SUBROUTINE ZEXAM',I1,13X,20('*'))
 1040 FORMAT ('0')
 1050 FORMAT (1X,20('*'),10X,'SUBROUTINE ZEPEX',I1,13X,20('*'))
 1060 FORMAT (1X,20('*'),' ZBOOK ERRORS ARE GENERATED ON PURPOSE  '
     *,20('*'))
 1070 FORMAT (1X,20('*'),' TO TEST RECOVERY ROUTINES ',13X,20('*'))
      END
