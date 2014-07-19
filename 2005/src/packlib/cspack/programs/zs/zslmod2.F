*
* $Id: zslmod2.F,v 1.1.1.1 1996/03/08 15:44:21 mclareni Exp $
*
* $Log: zslmod2.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:21  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
#if defined(CERNLIB_OS9)
      SUBROUTINE ZSLMOD2
*
*            Prints ID and histogram title
*
      PARAMETER(KNCX=3,KXMIN=4,KXMAX=5,KBWIDX=6,KMIN=7,KMAX=8,KNORM=9,
     +          KTIT1=10,KNCY=7,KYMIN=8,KYMAX=9,KBWIDY=10,KSCAL2=11,
     +          KTIT2=12,KNBIT=1,KNOENT=2,KSTAT1=3,KNSDIR=5,KNRH=6,
     +          KCON1=9,KCON2=3,KBITS=1,KNTOT=2)
*
#include "cspack/hos9c.inc"
*
      CHARACTER*100 LOUT
      CHARACTER*1 HTYPE
*
      do 10 ihist=1,nhist
         id=ihead(kid,ihist)
         lout=' '
         IF(JBIT(ihead(kbits,ihist),1).NE.0)THEN
            HTYPE='1'
            NWTITL=ihead(knwid,ihist)-KTIT1+1
            WRITE(LOUT,2000)ID,HTYPE,(ihead(KTIT1+I-1,ihist),I=1,NWTITL)
         ELSE
            HTYPE='2'
            NWTITL=ihead(knwid,ihist)-KTIT2+1
            WRITE(LOUT,2000)ID,HTYPE,(ihead(KTIT2+I-1,ihist),I=1,NWTITL)
         ENDIF
         CALL CZPUTA(LOUT,ISTAT)
         IF(ISTAT.NE.0)RETURN
  10  continue
*
 1100 FORMAT('2 ',5I6)
 2000 FORMAT('2',I10,1X,'(',A,')',3X,20A4)
      END
 
#endif
