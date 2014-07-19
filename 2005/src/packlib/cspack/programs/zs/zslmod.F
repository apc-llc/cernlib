*
* $Id: zslmod.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zslmod.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE ZSLMOD
*
*            List HBOOK directory of an OS9 process
*
#include "cspack/hos9c.inc"
#include "cspack/czdir.inc"
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
      TEMPLATE PAWM
#include "cspack/pawc.inc"
      DIMENSION IQ(1),Q(1),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(Q(1),LQ(9)),(IQ(1),Q(1))
*
      CHARACTER*100 LOUT
      CHARACTER*32 CHDIR
      CHARACTER*1 HTYPE
*
      lcpu=lenocc(cpu)
      chdir=cpu(1:lcpu)//'/'//module
      WRITE(LOUT,1000)CHDIR
      CALL CZPUTA(LOUT,ISTAT)
*           Remote CPU
      IF(LUNRZ(IDIR).LT.0)THEN
         call zslmod2
         go to 95
*
      ENDIF
*
*           Module on server
      IF(LMAIN.LE.0)GO TO 95
      LCID =LQ(LMAIN-2)
  10  IF(LCID.EQ.0)GO TO 95
*
      ID=IQ(LCID-5)
      LOUT=' '
      IF(JBIT(IQ(LCID+KBITS),1).NE.0)THEN
         HTYPE='1'
         NWTITL=IQ(LCID-1)-KTIT1+1
         NCX=IQ(LCID+KNCX)
         WRITE(LOUT,2000)ID,HTYPE,NCX,(IQ(LCID+KTIT1+I-1),I=1,NWTITL)
      ELSE
         HTYPE='2'
         NWTITL=IQ(LCID-1)-KTIT2+1
         NCX=IQ(LCID+KNCX)*IQ(LCID+KNCY)
         WRITE(LOUT,2000)ID,HTYPE,NCX,(IQ(LCID+KTIT2+I-1),I=1,NWTITL)
      ENDIF
      CALL CZPUTA(LOUT,ISTAT)
      IF(ISTAT.NE.0)RETURN
      LCID=LQ(LCID)
      GO TO 10
*
  95  CALL CZPUTA('1 ===> End of remote directory',ISTAT)
*
 1000 FORMAT('2 ==> Remote Directory : ',A)
 2000 FORMAT('2',I10,1X,'(',A,')',3X,I10,3X,20A4)
      END
#endif
