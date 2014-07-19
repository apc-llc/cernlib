*
* $Id: n001ch.F,v 1.1.1.1 1996/02/15 17:48:46 mclareni Exp $
*
* $Log: n001ch.F,v $
* Revision 1.1.1.1  1996/02/15 17:48:46  mclareni
* Kernlib
*
*
#include "kernnumt/pilot.h"
          SUBROUTINE N001CH(OK)
          LOGICAL   OK
          EXTERNAL  C205R3,   C205R4
          CHARACTER ERCODE*6
#include "kernnumt/sysdat.inc"
          OK  =  .TRUE.
          ERCODE  =  'C205'
          CALL KERSET(ERCODE,  NULL,  1,  2)
          CALL KERSET('C205.2',IOUNIT,100,0)
          ISAVE   =  LGFILE
          LGFILE  =  IOUNIT
          WRITE(*,1000) IOUNIT
          WRITE(IOUNIT,1001)
          EPSI  =  RELPR * 1000.
          DO 10     MESS  =  1, 3
             CALL RZERO(1.,2.,X,R,EPSI,30,C205R3)
             CALL RZERO(-1./ASIN(.1),+1./ASIN(.5),X,R,EPSI,5,C205R4)
  10         CONTINUE
          LGFILE  =  ISAVE
          RETURN
1000      FORMAT(5X,'THIS CHECK IS SUCCESSFUL IF FIVE MESSAGES ',
     +           'ARE WRITTEN TO UNIT',I2,' AS SPECIFIED THERE')
1001      FORMAT(/ '1N001.' //
     +           ' FIVE MESSAGES FROM PACKAGE C205, NAMELY ...' //
     +     10X, ' (I)    A WARNING     FROM C205.1     (2 LINES)' /
     +     10X, ' (II)   A FATAL ERROR FROM C205.2     (3 LINES)' /
     +     10X, ' (III)  A FATAL ERROR FROM C205.2     (3 LINES)' /
     +     10X, ' (IV)   A FATAL ERROR FROM C205.1     (2 LINES)' /
     +     10X, ' (V)    A FATAL ERROR FROM C205.2     (3 LINES)' /
     +     ' SHOULD NOW FOLLOW. THE LAST LINE FOR EACH FATAL ERROR ',
     +     'MUST SAY ...' // ' ABEND ROUTINE HAS BEEN CALLED.' ///)
          END
