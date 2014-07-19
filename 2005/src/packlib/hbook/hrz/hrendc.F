*
* $Id: hrendc.F,v 1.3 1998/03/18 17:02:52 couet Exp $
*
* $Log: hrendc.F,v $
* Revision 1.3  1998/03/18 17:02:52  couet
* - FORTRAN I/O default for VMS
*
* Revision 1.2  1997/05/13 15:21:00  couet
* - default is now C/IO for all machines. To have fortran IO the option F should
* be given to HROPEN and the file should be close with HRENDF. HREND is now
* equivalent to HRENDC.
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
      SUBROUTINE HRENDC(CHDIR)
#if !defined(CERNLIB_VAXVMS)
      ENTRY HREND(CHDIR)
#endif
*.==========>
*.           Close an HBOOK/RZ file. Via RZCLOS the unit is also correctly
*.           closed in case of C I/O.
*.           This routine is a copy of HRENDF, make changes in both at the
*.           same time.
*.           C/IO is now the default (except VMS). Calling HREND is the same 
*.           as calling HREND. To have FORTRAN IO, HROPEN must be called with 
*.           option F and HRENDF must be called instead of HREND/HRENDC. 
*..=========>
#include "hbook/hcdire.inc"
      CHARACTER*(*) CHDIR
*.___________________________________________
*
*
      NCHMAX=NCHTOP
      DO 20 I=2,NCHMAX
         IF(CHTOP(I).EQ.CHDIR)THEN
            IF(ICHTOP(I).GT.0.AND.ICHTOP(I).LT.1000)THEN
               CALL RZCLOS(CHDIR,' ')
               CALL HBFREE(ICHTOP(I))
            ENDIF
            CALL HNTDEL(CHDIR)
*
            DO 10 J=I+1,NCHTOP
               ICHTOP(J-1)=ICHTOP(J)
               ICHLUN(J-1)=ICHLUN(J)
               ICHTYP(J-1)=ICHTYP(J)
               CHTOP(J-1)=CHTOP(J)
               HFNAME(J-1)=HFNAME(J)
  10        CONTINUE
            NCHTOP=NCHTOP-1
         ENDIF
  20  CONTINUE
*
      CALL HCDIR('//PAWC',' ')
*
      END
