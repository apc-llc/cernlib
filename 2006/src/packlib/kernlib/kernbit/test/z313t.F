*
* $Id: z313t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: z313t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_IBMVM)||defined(CERNLIB_VAXVMS)
      SUBROUTINE Z313T
      COMMON/Z313IN/JINFO(20,2)
#if defined(CERNLIB_IBMVM)
      CHARACTER*8 JOBINFO,INFO,JINFO
#endif
#if defined(CERNLIB_VAXVMS)
      CHARACTER*15 JOBINFO,INFO,JINFO
#endif
C
      CALL KBHDR('Z313 JOBINFO')
C
      INFO =JOBINFO('CPUTIME')
      TIME =ICHARN(INFO)
      INFO =JOBINFO('CPUFACT')
      TNORM=ICHARN(INFO)/100
      PRINT 1001,TIME,TNORM
      PRINT 1002,JINFO
1001  FORMAT(6X,'CPU time: ',F11.2,' sc. Normalization factor: ',F7.2)
1002  FORMAT((4X,5(2X,A)))
      RETURN
      END
#endif
