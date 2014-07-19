*
* $Id: dzdbok.F,v 1.1.1.1 1996/03/04 16:13:17 mclareni Exp $
*
* $Log: dzdbok.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:17  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBOK(IXSTOR,MAXK,CHOPTD,L,JB,LRET)
      INTEGER IXSTOR,MAXK,JB, L(9),LRET(9)
      CHARACTER*(*) CHOPTD
 
*     book a bank for the directory CDIR
*     bank format for a directory:
*     IFLAG (=1), OFFSET-TO-NAME, NKEYS, NWKEY,NDIR,NQUOTA
*     KEYFORM((NWKEY+3)/4), KEYTAG (2*NWKEY), NAME
 
#include "zebra/zunit.inc"
#include "zebra/rzcl.inc"
#include "zebra/rzk.inc"
#include "dzdchv.inc"
      PARAMETER (MAXKEY=100)
      CHARACTER*8 CHTAG(MAXKEY)
      CHARACTER*80 CURDIR
      INTEGER MMBK(7)
 
      CALL RZKEYD(NWKEY,CLINE,CHTAG)
*      get full name of current directory
      CALL RZCDIR(CURDIR,'R')
 
      IF(LCDIR.EQ.0)GO TO 999
      LK   = IQ(KQS+LCDIR+KLK)
      NK   = IQ(KQS+LCDIR+KNKEYS)
      NWKEY= IQ(KQS+LCDIR+KNWKEY)
*     get total number of key entries (all cycles)
      NCTOT=0
      DO 5 I=1,NK
          LKC=LK+(NWKEY+1)*(I-1)
          LCYC=IQ(KQS+LCDIR+LKC)
  6       NCTOT=NCTOT+1
           LCOLD = JBYT(IQ(KQS+LCDIR+LCYC ), 1,16)
           IF(LCOLD.NE.0)THEN
              LCYC=LCOLD
              GO TO 6
          ENDIF
  5   CONTINUE
 
      NDIRS = IQUEST(9)
      ILC = INDXBC(CURDIR,' ')
      NDNAME =  (ILC+3) / 4
      NEXTRA = 6 + (NWKEY+3)/4 + 2*NWKEY
      ND = NDNAME+NEXTRA
      CALL UCTOH('RDIR',MMBK(1),4,4)
      MMBK(2) = NDIRS+NCTOT+1
      MMBK(3) = MMBK(2)
      MMBK(4) = ND
      CALL MZIOBK(MMBK,7,'6I -H')
      IF(JB .GT. 0)THEN
          JB1 = JB
      ELSE
          JB1 = JB-IQ(KQS+L(1)+3)
      ENDIF
      CALL MZLIFT(IXSTOR,LRET(1),L(1),JB1,MMBK,0)
#include "zebra/qstore.inc"
      IQ(KQS+LRET(1)+1) = 1
      IQ(KQS+LRET(1)+2) = NEXTRA
      CALL UCOPY(IQUEST(7),IQ(KQS+LRET(1)+3),4)
      CALL UCTOH(CLINE,IQ(KQS+LRET(1)+7),4,NWKEY)
      KP = 7 + (NWKEY+3)/4
      DO 10 K=1,NWKEY
          CALL UCTOH(CHTAG(K),IQ(KQS+LRET(1)+KP),4,8)
          KP = KP+2
   10 CONTINUE
*     store full name
      CALL UCTOH(CURDIR,IQ(KQS+LRET(1)+NEXTRA+1),4,NDNAME*4)
 
      IF(NK.LE.0)GOTO 999
      IF(NK.GT.MAXK)THEN
          IF(IQPRNT.EQ.6)CALL IGTERM
          IF(INDEX(CHOPTD,'W').NE.0)
     &    WRITE(IQPRNT,*)' Dir: ',CURDIR(1:32),' has',
     &    NK,' keys, will show only last',MAXK
          NK1=NK-MAXK+1
      ELSE
          NK1=1
      ENDIF
 
*     taken from  RZKEYS
 
      CALL DZDSTK(IXSTOR,NK1,NK,LRET(1))
  999 RETURN
      END
 
