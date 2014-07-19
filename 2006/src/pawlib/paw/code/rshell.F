*
* $Id: rshell.F,v 1.1.1.1 1996/03/01 11:38:47 mclareni Exp $
*
* $Log: rshell.F,v $
* Revision 1.1.1.1  1996/03/01 11:38:47  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.02/02 24/05/93  10.33.34  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE RSHELL
*
*           To send a message to a remote machine
*     Method:
*         The message is sent
*         The ackowledgement is received in CHMESS
*         If CHMESS(1:1)='0' Return
*         If CHMESS(1:1)='1' Print CHMESS and Return
*         If CHMESS(1:1)='2' Print CHMESS and read next CHMESS
*         If CHMESS(1:1)='9' Close connection with remote machine
*
#if defined(CERNLIB_CZ)
#include "hbook/hcdire.inc"
#include "hbook/hcunit.inc"
#include "hbook/czsock.inc"
#include "paw/quest.inc"
      CHARACTER*80 CHMESS
      DIMENSION IHMESS(22),ICONTR(2)

*
      IF(ICHTOP(ICDIR).LT.1000)THEN
         PRINT *,' Cannot send message to ',CHTOP(ICDIR)
         GO TO 99
      ENDIF
*
      CALL KUGETE(CHMESS,NCH)
      IF(NCH.LE.0)RETURN
*
      CALL JUMPST(IADTCP)
      ICONTR(1)=3
      ICONTR(2)=20
      CALL VBLANK(IHMESS,22)
      CALL UCTOH('MESS :'//CHMESS,IHMESS,4,NCH+6)
      CALL JUMPX2(IHMESS,ICONTR)
      IF(ICONTR(1).NE.0)GO TO 99
*
  10  ICONTR(1)=2
      ICONTR(2)=20
      IQUEST(1)=0
      CALL VBLANK(IHMESS,22)
      CALL JUMPX2(IHMESS,ICONTR)
      IF(ICONTR(1).NE.0)GO TO 99
      CALL UHTOC(IHMESS,4,CHMESS,80)
      IF(CHMESS(1:1).EQ.'0')GO TO 99
      IF(CHMESS(1:1).EQ.'9')THEN
         CALL CZCLOS(ISTAT)
         PRINT *,' Communication closed with: ',CHTOP(ICDIR)
         DO 20 I=ICDIR+1,NCHTOP
            ICHTOP(I-1)=ICHTOP(I)
            ICHLUN(I-1)=ICHLUN(I)
            CHTOP(I-1)=CHTOP(I)
            HFNAME(I-1)=HFNAME(I)
  20     CONTINUE
         NCHTOP=NCHTOP-1
         CALL HCDIR('//PAWC',' ')
         GO TO 99
      ENDIF
      NCH=LENOCC(CHMESS)
      IF(NCH.GT.1)THEN
         WRITE(LOUT,'(A)')CHMESS(2:NCH)
      ENDIF
      IF(CHMESS(1:1).EQ.'3')IQUEST(1)=1
      IF(CHMESS(1:1).EQ.'2')GO TO 10
#endif
#if !defined(CERNLIB_CZ)
      PRINT *,' PAW not compiled with communication option'
#endif
*
  99  END
