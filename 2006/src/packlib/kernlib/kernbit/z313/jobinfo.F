*
* $Id: jobinfo.F,v 1.1.1.1 1996/02/15 17:47:53 mclareni Exp $
*
* $Log: jobinfo.F,v $
* Revision 1.1.1.1  1996/02/15 17:47:53  mclareni
* Kernlib
*
*
#include "kernbit/pilot.h"
      CHARACTER*(*) FUNCTION JOBINFO(PAR)
C
C CERN PROGLIB#    Z313 JOBINFO         .VERSION KERNBIT  1.08  911216
C ORIG. 16/12/91, Miguel Marquina
C Extraction of Job Information for Vax/VMS systems
C
      CHARACTER*(*) PAR
      PARAMETER (JBDATA=20,JBL=15)
      COMMON/Z313IN/JINFO(JBDATA,2)
      CHARACTER*(JBL) JREQ(JBDATA), INFO(JBDATA), JINFO
      CHARACTER*(JBL) USERID,HOSTID

      DATA JREQ/'SENDERID','ACCOUNT ','DISTCODE','USERID  ','JOBID   ',
     +          'STORAGE ','JOBSTAT ','HOSTID  ','BMONID  ','MBMONID ',
     +          'CPUTIME ','CPUFACT ','MACHINE ','BATCHSYS','GID     ',
     +          '        ','        ','        ','        ','        '/
      DATA INFO/'userid  ','account ','distcode','userid  ','INTERACT',
     +          'storage ','        ','nodeid  ','        ','        ',
     +          '2592000 ','100     ','Unknown ','VMSBATCH','        ',
     +          '        ','        ','        ','        ','        '/

      IF(JFILL.EQ.1) GOTO 100
      JFILL  =1
C
      INFO(4)=USERID()
C     INFO(6)=STORSZ()
      INFO(8)=HOSTID()
      INFO(1)=INFO(4)
C
      DO 10 N=1,JBDATA
         JINFO(N,1)=JREQ(N)
         JINFO(N,2)=INFO(N)
10    CONTINUE
100    CONTINUE
C--    Fill Job request
         JOBINFO=' '
         LPAR=LEN(PAR)
         IF(LPAR.LT.1) GOTO 120
         LINF=LEN(JOBINFO)
       DO 110 N=1,JBDATA
         IF(PAR.NE.JREQ(N)(1:LPAR)) GOTO 110
         JOBINFO=INFO(N)(1:MIN(JBL,LINF))
         GOTO 120
110    CONTINUE
120    CONTINUE
       RETURN
       END
