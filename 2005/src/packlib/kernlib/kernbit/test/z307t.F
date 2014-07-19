*
* $Id: z307t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: z307t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_IBMVM)
      SUBROUTINE Z307T
      DIMENSION SPACE(32)
      LOGICAL BATCH ,JOB$VM
      CHARACTER*8 INFO(10),MACHTP(3)
      EQUIVALENCE (INFO(1),SPACE( 1))
      EQUIVALENCE (TIME   ,SPACE(21))
      EQUIVALENCE (TNORM  ,SPACE(23))
      EQUIVALENCE (MACH   ,SPACE(25))
      DATA MACHTP/'Unknown','370','XA'/
C
      CALL KBHDR('Z307 JOB$VM')
      BATCH = JOB$VM(SPACE)
      PRINT 1000,MACHTP(MACH+1),BATCH
      PRINT 1001,TIME,TNORM
      PRINT 1002,(INFO(N),N=1,10)
1000  FORMAT(6X,'Running on ',A,' machine in Batch? ',I2)
1001  FORMAT(6X,'CPU time: ',F11.2,' sc. Normalization factor: ',F7.2)
1002  FORMAT((4X,5(2X,A8)))
      RETURN
      END
#endif
