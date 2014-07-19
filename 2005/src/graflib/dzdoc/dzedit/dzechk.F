*
* $Id: dzechk.F,v 1.1.1.1 1996/03/04 16:13:23 mclareni Exp $
*
* $Log: dzechk.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:23  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZECHK
#include "dzeditkeep.inc"
      INTEGER KEYVEC(2)
*     compare contents of RZfile with contents of CHCOMP
 
      NFKEY = 1
      NONCON = 0
      NENTRY=0
      WRITE(LUNLST,'(A)')' Diagnostics:'
5     CALL DZDCRD(LUNCOM,CHKEYC,IRC)
      IF(IRC.NE.0)GOTO 6
*   5     READ(LUNCOM,'(A)',END=6)CHKEYC
      NENTRY=NENTRY+1
      GOTO 5
6     CONTINUE
      REWIND LUNCOM
      CALL MZBOOK(0,LTAUX,LTAUX,1,'DZEC',0,0,2*NENTRY,2,-1)
      IP=-1
      DO 7 I=1,NENTRY
      CALL DZDCRD(LUNCOM,CHKEYC,IRC)
*      READ(LUNCOM,'(A)')CHKEYC
      CALL CLTOU(CHKEYC)
      IP=IP+2
      CALL UCTOH(CHKEYC,IQ(LTAUX+IP),4,8)
7     CONTINUE
 
   10 CONTINUE
      CALL DZDWCS('********',KEYVEC,NFKEY)
      IF(KEYVEC(1) .EQ. 0)THEN
         IF(NONCON .EQ. 0)WRITE(*,*)' None'
         GOTO 888
      ENDIF
*      CALL UHTOC(KEYVEC(1),4,CHKEY(1:4),4)
*      CALL UHTOC(KEYVEC(2),4,CHKEY(5:8),4)
      IFOUND = 0
      IP=-1
*   20 READ(LUNCOM,'(A)',END=30  )CHKEYC
*      CALL CLTOU(CHKEYC)
*      CALL CLTOU(CHKEY)
20    IP=IP+2
      IF(IP .GE. 2*NENTRY)GOTO 30
      IF(KEYVEC(1).NE.IQ(LTAUX+IP))GOTO 20
      IF(KEYVEC(2).NE.IQ(LTAUX+IP+1))GOTO 20
*      IF(CHKEY .NE. CHKEYC)GOTO 20
      IF(IFOUND .NE. 0)THEN
         WRITE(LUNLST,'(A,A4,A,A4,A)')' ',KEYVEC(1),'/',KEYVEC(2),
     +   ' Multiply used:'
         NONCON = NONCON+1
      ENDIF
      IFOUND = 1
      GOTO 20
   30 CONTINUE
      IF(IFOUND .EQ. 0)THEN
         WRITE(LUNLST,'(A,A4,A,A4,A)')' ',KEYVEC(1),'/',KEYVEC(2),
     +   '  Not connected: '
         NONCON=NONCON+1
      ENDIF
*      REWIND LUNCOM
      GOTO 10
 888  CONTINUE
      IF(LTAUX.NE.0)CALL MZDROP(0,LTAUX,' ')
      LTAUX=0
  999 END
