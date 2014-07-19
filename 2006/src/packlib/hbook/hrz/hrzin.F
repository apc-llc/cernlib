*
* $Id: hrzin.F,v 1.1.1.1 1996/01/16 17:08:08 mclareni Exp $
*
* $Log: hrzin.F,v $
* Revision 1.1.1.1  1996/01/16 17:08:08  mclareni
* First import
*
*
#include "hbook/pilot.h"
*CMZ :  4.21/08 13/02/94  17.37.14  by  Fons Rademakers
*-- Author :
      SUBROUTINE HRZIN(IXDIV,LBANK,JBIAS,KEYS,ICYCLE,CHOPT)
*.==========>
*.        to read a record from a RZ file
*..=========> ( R.Brun )
#include "hbook/hcbook.inc"
#include "hbook/hcdire.inc"
#include "hbook/hcmail.inc"
      COMMON/QUEST/IQUEST(100)
      DIMENSION LBANK(1),JBIAS(1)
#if defined(CERNLIB_CZ)
      DIMENSION IQSAVE(100)
#endif
      INTEGER      KEYS(2)
      CHARACTER*(*)CHOPT
      CHARACTER*1 FCHOPT
      CHARACTER*8 CHOPT1
*.___________________________________________
*
      IF(ICHTOP(ICDIR).GT.1000)THEN
#if defined(CERNLIB_CZ)
*
*          Send message to the host
*
         IF(LBANK(1).EQ.0.AND.JBIAS(1).LE.0)THEN
            FCHOPT='S'
         ELSE
            FCHOPT=' '
         ENDIF
         CHOPT1=CHOPT
         IF(CHOPT(1:1).EQ.'R')THEN
            CALL MZDROP(IXDIV,LQ(LBANK(1)+JBIAS(1)),' ')
            LQ(LBANK(1)+JBIAS(1))=0
            CHOPT1=' '
         ENDIF
         WRITE(CHMAIL,1000)KEYS(1),KEYS(2),ICYCLE,CHOPT1
 1000    FORMAT('CZIN  ',2I10,I10,2X,A)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF (ISTAT .NE. 0) THEN
            IQUEST(1) = ISTAT
            RETURN
         ENDIF
*
*          Read the ZEBRA data structure from host
*
         NUH=100
         CALL FZIN(999,IXDIV,LBANK,JBIAS,FCHOPT,NUH,IQSAVE)
         CALL UCOPY(IQSAVE,IQUEST,100)
#endif
#if !defined(CERNLIB_CZ)
         CALL HBUG('CZ option not active','HRZIN',0)
#endif
         RETURN
      ENDIF
*
      CALL RZIN(IXDIV,LBANK,JBIAS,KEYS,ICYCLE,CHOPT)
*
      END
