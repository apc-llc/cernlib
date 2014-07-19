*
* $Id: hropen.F,v 1.5 1998/03/18 16:59:41 couet Exp $
*
* $Log: hropen.F,v $
* Revision 1.5  1998/03/18 16:59:41  couet
* - FORTRAN I/O is the default for VMS
*
* Revision 1.4  1997/05/13 15:21:01  couet
* - default is now C/IO for all machines. To have fortran IO the option F should
* be given to HROPEN and the file should be close with HRENDF. HREND is now
* equivalent to HRENDC.
*
* Revision 1.3  1997/01/24 17:00:11  gunter
* add passing on of eXchange format
*
* Revision 1.2  1996/08/28 09:49:17  couet
* - Option C to call rzopen
*
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/11 04/03/94  19.35.51  by  Fons Rademakers
*-- Author :
      SUBROUTINE HROPEN(LUN,CHDIR,CFNAME,CHOPTT,LRECL,ISTAT)
*.==========>
*.           Open an HBOOK/RZ file
*..=========> ( R.Brun )
#include "hbook/hcdire.inc"
      COMMON/QUEST/IQUEST(100)
      CHARACTER*(*) CFNAME,CHDIR,CHOPTT
      CHARACTER*8 CHOPT
*.___________________________________________
*
      CHOPT=CHOPTT
      CALL CLTOU(CHOPT)
*
      DO 10 I=1,NCHTOP
         IF(CFNAME.EQ.HFNAME(I))THEN
            CALL HBUG('File already connected','HROPEN',0)
            GO TO 99
         ENDIF
  10  CONTINUE
*
      IQ10=IQUEST(10)
#if !defined(CERNLIB_VAXVMS)
* C/IO is the default. Option F set FORTRAN IO.
      IF (INDEX(CHOPT,'F').EQ.0) THEN
         IC = MIN(LENOCC(CHOPT)+1,8)
         CHOPT(IC:IC) = 'C'
      ENDIF
#endif
*
      CALL RZOPEN(LUN,CHDIR,CFNAME,CHOPT,LRECL,ISTAT)
  90  IF(ISTAT.NE.0)THEN
         CALL HBUG('Cannot open file','HROPEN',0)
         GO TO 99
      ENDIF

      IF (IQUEST(12).NE.0 ) THEN
*          file is in exchange mode, this to hrfile/rzfile
         IC = MIN(LENOCC(CHOPT)+1,8)
         CHOPT(IC:IC) = 'X'
      ENDIF

      LRE=IQUEST(10)
*
      IQUEST(10)=IQ10
      IQUEST(99)=LRE
      CALL HRFILE(LUN,CHDIR,CHOPT)
      IF(IQUEST(1).NE.0)THEN
         ISTAT=IQUEST(1)
         CALL RZEND(CHDIR)
         CLOSE(LUN)
         GO TO 90
      ENDIF
      IF(ICDIR.GT.0)HFNAME(ICDIR)=CFNAME
      IF(INDEX(CHOPT,'Q').EQ.0)IQUEST(10)=LRE
*
99    RETURN
      END
