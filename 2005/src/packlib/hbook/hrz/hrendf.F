*
* $Id:
*
* $Log:
*
#include "hbook/pilot.h"
      SUBROUTINE HRENDF(CHDIR)
#if defined(CERNLIB_VAXVMS)
      ENTRY HREND(CHDIR)
#endif
*.==========>
*.           Close an HBOOK/RZ file.
*.           This routine is a copy of HRENDC, make changes in both at the
*.           same time.
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
               CALL RZEND(CHDIR)
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
