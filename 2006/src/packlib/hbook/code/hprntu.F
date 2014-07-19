*
* $Id: hprntu.F,v 1.1.1.1 1996/01/16 17:07:46 mclareni Exp $
*
* $Log: hprntu.F,v $
* Revision 1.1.1.1  1996/01/16 17:07:46  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.17/01 17/08/92  16.04.10  by  Fons Rademakers
*-- Author :
      SUBROUTINE HPRNTU(ID1)
*.==========>
*.           To print statistics about a N-tuple
*.
*..=========> ( R.Brun )
#include "hbook/hcflag.inc"
#include "hbook/hcbook.inc"
#include "hbook/hcbits.inc"
#include "hbook/hcunit.inc"
      DIMENSION NAME(2)
*.___________________________________________
*
      ID=ID1
      IDPOS=LOCATI(IQ(LTAB+1),IQ(LCDIR+KNRH),ID)
      IF(IDPOS.LE.0)RETURN
      LCID=LQ(LTAB-IDPOS)
      LCONT=LQ(LCID-3)
      I4=JBIT(IQ(LCID+KBITS),4)
      IF(I4.EQ.0)RETURN
      IF (IQ(LCID-2) .NE. 2) THEN
         CALL HPRNT(ID1)
         RETURN
      ENDIF
*
      NDIM=IQ(LCID+2)
      LLIMS=LQ(LCID-2)
      NOENT=IQ(LCID+3)
      ITAG1=IQ(LCID+10)
      ITIT1=IQ(LCID+9)
      NWTIT=IQ(LCID+8)
*
      WRITE(LOUT,1000)ID1,NOENT,(IQ(LCID+ITIT1+I-1),I=1,NWTIT)
      WRITE(LOUT,2000)
      DO 10 I=1,NDIM
         CALL UCOPY(IQ(LCID+ITAG1+2*(I-1)),NAME,2)
         WRITE(LOUT,3000)I,NAME,Q(LLIMS+2*I-1),Q(LLIMS+2*I)
  10  CONTINUE
      WRITE(LOUT,4000)
*
 1000 FORMAT(//,
     + ' ********************************************************',/,
     + ' * NTUPLE ID=',I5,'  ENTRIES=',I7,3X,20A4)
 2000 FORMAT(
     + ' ********************************************************',/,
     + ' *  Var numb  *   Name    *    Lower     *    Upper     *',/,
     + ' ********************************************************')
 3000 FORMAT(
     + ' *',4X,I3,5X,'* ',2A4,2X,'* ',E12.6,' * ',E12.6,' *')
 4000 FORMAT(
     + ' ********************************************************',/)
*
  99  RETURN
      END
