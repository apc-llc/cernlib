*
* $Id: m444t.F,v 1.1.1.1 1996/02/15 17:47:55 mclareni Exp $
*
* $Log: m444t.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:55  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
#if defined(CERNLIB_IBMVM)
      SUBROUTINE M444T
      CHARACTER*80 UXFILE
      CHARACTER*21 VMFILE,UXTOVM
      CALL KBHDR('M444 UXTOVM')
      UXFILE='/cern/new/src/source.f'
      VMFILE=UXTOVM(UXFILE)
      PRINT 1000, UXFILE,VMFILE
      UXFILE='/cern/new/src/source.o'
      VMFILE=UXTOVM(UXFILE)
      PRINT 1000, UXFILE,VMFILE
      UXFILE='/cern/new/lib/libkernlib.a'
      VMFILE=UXTOVM(UXFILE)
      PRINT 1000, UXFILE,VMFILE
      UXFILE='kernlib.txtlib'
      VMFILE=UXTOVM(UXFILE)
      PRINT 1000, UXFILE,VMFILE
      UXFILE='profile.exec.p'
      VMFILE=UXTOVM(UXFILE)
      PRINT 1000, UXFILE,VMFILE
*
1000  FORMAT(' Unix filename : ',A/' VM translation: ',A)
      RETURN
      END
#endif
