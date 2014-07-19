*
* $Id: jobinfo.F,v 1.1.1.1 1996/02/15 17:51:58 mclareni Exp $
*
* $Log: jobinfo.F,v $
* Revision 1.1.1.1  1996/02/15 17:51:58  mclareni
* Kernlib
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      CHARACTER*(*) FUNCTION JOBINFO(PAR)
C
C CERN PROGLIB#    Z313 JOBINFO         .VERSION KERNCMS  1.06  910721
C ORIG. 16/07/91, Miguel Marquina
C Extraction of Job Information for SLAC-Batch and VMBatch systems
C
      CHARACTER*(*) PAR
      PARAMETER (JBDATA=20)
      COMMON/Z313IN/JINFO(JBDATA,2)
      CHARACTER*8 JREQ(JBDATA), INFO(JBDATA), JINFO
      CHARACTER*8 USERID,HOSTID,STORSZ
      INTEGER     SYSTEMF
 
      CHARACTER*80 LINE
 
      DATA JREQ/'SENDERID','ACCOUNT ','DISTCODE','USERID  ','JOBID   ',
     +          'STORAGE ','JOBSTAT ','HOSTID  ','BMONID  ','MBMONID ',
     +          'CPUTIME ','CPUFACT ','MACHINE ','BATCHSYS','GID     ',
     +          '        ','        ','        ','        ','        '/
      DATA INFO/'userid  ','account ','distcode','userid  ','INTERACT',
     +          'storage ','        ','nodeid  ','        ','        ',
     +          '9999.   ','100     ','Unknown ','SLACBATC','        ',
     +          '        ','        ','        ','        ','        '/
 
      IF(JFILL.EQ.1) GOTO 100
      JFILL  =1
C
      INFO(4)=USERID()
      INFO(6)=STORSZ()
      INFO(8)=HOSTID()
      INFO(1)=INFO(4)
C
      IF(INFO(4)(1:5).EQ.'BATCH') INFO(14)='SLACBATC'
      IF(INFO(4)(1:5).EQ.'VMBAT') INFO(14)='VMBATCH '
      IF(INFO(4)(1:5).EQ.'?????') INFO(14)='CMSBATCH'
C
      IF(INFO(14).EQ.'SLACBATC') THEN
C***  SLAC-Batch
        NLINE=0
       OPEN(7,FILE='/BATCHSUB NAMES A',FORM='UNFORMATTED',ERR=50)
 10     CONTINUE
         LINE=' '
         READ(7,NUM=NBYT,END=40) LINE
         NLINE=NLINE+1
         IF(LINE(1: 6).NE.':NICK.') GOTO 10
         NC=7
         DO 20 N=7,NBYT
           NC=NC+1
           IF(LINE(N:N).EQ.':') GOTO 30
 20      CONTINUE
 30      CONTINUE
         IF(LINE(NC-1:NC+5).NE.':VALUE.') GOTO 10
         NC=NC+6
         IF(LINE(7:14).EQ.'USERID  ') INFO( 1)=LINE(NC:NC+7)
         IF(LINE(7:14).EQ.'ACCOUNT ') INFO( 2)=LINE(NC:NC+7)
         IF(LINE(7: 9).EQ.'BIN'     ) INFO( 3)=LINE(NC:NC+7)
         IF(LINE(7:14).EQ.'JOBNAME ') INFO( 5)=LINE(NC:NC+7)
         IF(LINE(7:14).EQ.'STORAGE ') INFO( 6)=LINE(NC:NC+7)
         IF(LINE(7:14).EQ.'START_TY') THEN
                                      INFO( 7)=LINE(NC:NC+7)
          IF(INFO(7)(1:1).EQ.'s')     INFO( 7)='START   '
          IF(INFO(7)(1:1).EQ.'r')     INFO( 7)='RESTART '
         ENDIF
         IF(LINE(7:14).EQ.'ORIGNODE') INFO( 8)=LINE(NC:NC+7)
         IF(LINE(7:10).EQ.'BMON'    ) INFO( 9)=LINE(NC:NC+7)
         IF(LINE(7:10).EQ.'BMON'    ) INFO(10)=LINE(NC:NC+7)
         IF(LINE(7:14).EQ.'TIME    ') INFO(11)=LINE(NC:NC+7)
         IF(LINE(7:14).EQ.'CPU_FACT') INFO(12)=LINE(NC:NC+7)
         GOTO 10
 40     CONTINUE
        CLOSE(7)
 50     CONTINUE
        IF(INFO(6)(5:5).NE.'M') THEN
           MSTOR=ICHARN(INFO(6))/1024
           MS1=MSTOR/100
           MS2=MSTOR/10 - 10*MS1  +ICHAR('0')
           MS1=MSTOR/100          +ICHAR('0')
           MS3=MOD(MSTOR,10)      +ICHAR('0')
           INFO(6)='0'//CHAR(MS1)//CHAR(MS2)//CHAR(MS3)//'M'
        ENDIF
      ELSEIF(INFO(14).EQ.'VMBATCH ') THEN
        IRC=SYSTEMF('VMBATCH SERVICE STACK $USER')
        IF(IRC.EQ.0) THEN
          CALL VMRTRM(LINE,LENS)
          INFO(1)=LINE(1:MIN(8,LENS))
        ENDIF
        IRC=SYSTEMF('VMBATCH SERVICE STACK NAME ')
        IF(IRC.EQ.0) THEN
          CALL VMRTRM(LINE,LENS)
          INFO(5)=LINE(1:MIN(8,LENS))
        ENDIF
      ELSEIF(INFO(14).EQ.'CMSBATCH') THEN
C***  CMS-Batch system from IBM
      ENDIF
      CALL UCOPY(JREQ,JINFO(1,1),2*JBDATA)
      CALL UCOPY(INFO,JINFO(1,2),2*JBDATA)
100    CONTINUE
C--    Fill Job request
         JOBINFO=' '
         LPAR=LEN(PAR)
         IF(LPAR.LT.1) GOTO 120
         LINF=LEN(JOBINFO)
       DO 110 N=1,JBDATA
         IF(PAR.NE.JREQ(N)(1:LPAR)) GOTO 110
         JOBINFO=INFO(N)(1:MIN(8,LINF))
         GOTO 120
110    CONTINUE
120    CONTINUE
       RETURN
       END
#ifdef CERNLIB_TCGEN_JOBINFO
#undef CERNLIB_TCGEN_JOBINFO
#endif
