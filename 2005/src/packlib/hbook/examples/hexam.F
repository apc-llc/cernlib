*
* $Id: hexam.F,v 1.2 1996/06/06 09:52:36 cernlib Exp $
*
* $Log: hexam.F,v $
* Revision 1.2  1996/06/06 09:52:36  cernlib
* Remove #ifdef (CERNLIB_EXAMPLES) and pilot.h where not used
*
* Revision 1.1.1.1  1996/01/16 17:07:49  mclareni
* First import
*
*
#include "pilot.h"
*CMZ :  4.17/02 18/12/92  12.44.27  by  Unknown
*-- Author :
      PROGRAM HTEST
*.==========>
*.           HBOOK GENERAL TEST PROGRAM
*..=========> ( R.Brun )
      PARAMETER (NWPAW=300000)
      COMMON/PAWC/H(NWPAW)
*.___________________________________________
*
#if (defined(CERNLIB_IBM))&&(!defined(CERNLIB_IBMMVS))
      CALL VMCMS('FILEDEF 31 DISK HEXAM OUT A (RECFM F LRECL 133',IST)
      IF (IST.EQ.0)OPEN(UNIT=31,STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_IBMMVS)
*     File is allocated via JCL
#endif
#if defined(CERNLIB_UNIX)
      OPEN(UNIT=31,FILE='hexam.out',STATUS='UNKNOWN')
#endif
#if defined(CERNLIB_CRAY)
      OPEN(UNIT=31,FILE='hexam.out',STATUS='UNKNOWN')
#endif
      CALL HLIMIT(NWPAW)
      CALL HOUTPU(31)
*     CALL HPAGSZ(45)
      CALL TIMED(T0)
      CALL HEXAM1
      CALL HDELET(0)
      CALL TIMED(T1)
      WRITE(31,*) 'TIME FOR EXAMPLE 1 =',T1,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 1 =',T1,'  SECONDS'
      CALL HEXAM2
      CALL HDELET(0)
      CALL TIMED(T2)
      WRITE(31,*) 'TIME FOR EXAMPLE 2 =',T2,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 2 =',T2,'  SECONDS'
      CALL HEXAM3
      CALL HDELET(0)
      CALL TIMED(T3)
      WRITE(31,*) 'TIME FOR EXAMPLE 3 =',T3,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 3 =',T3,'  SECONDS'
      CALL HEXAM4
      CALL HDELET(0)
      CALL TIMED(T4)
      WRITE(31,*) 'TIME FOR EXAMPLE 4 =',T4,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 4 =',T4,'  SECONDS'
      CALL HEXAM5
      CALL TIMED(T5)
      WRITE(31,*) 'TIME FOR EXAMPLE 5 =',T5,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 5 =',T5,'  SECONDS'
      CALL HDELET(0)
      CALL HEXAM6
      CALL TIMED(T6)
      WRITE(31,*) 'TIME FOR EXAMPLE 6 =',T6,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 6 =',T6,'  SECONDS'
      CALL HDELET(0)
      CALL HEXAM7
      CALL TIMED(T7)
      WRITE(31,*) 'TIME FOR EXAMPLE 7 =',T7,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 7 =',T7,'  SECONDS'
      CALL HDELET(0)
      CALL HEXAM8
      CALL TIMED(T8)
      WRITE(31,*) 'TIME FOR EXAMPLE 8 =',T8,'  SECONDS'
      WRITE( 6,*) 'TIME FOR EXAMPLE 8 =',T8,'  SECONDS'
      TTOT=T1+T2+T3+T4+T5+T6+T7+T8
      WRITE(31,*) 'TIME FOR HEXAM =',TTOT,'  SECONDS'
      WRITE( 6,*) 'TIME FOR HEXAM =',TTOT,'  SECONDS'
      END
