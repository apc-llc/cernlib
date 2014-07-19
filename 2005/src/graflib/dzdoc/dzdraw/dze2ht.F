*
* $Id: dze2ht.F,v 1.1.1.1 1996/03/04 16:13:19 mclareni Exp $
*
* $Log: dze2ht.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:19  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZE2HT(LUNIN,LUNUSE, FLAG, UPUP1)
      IMPLICIT NONE
 
      INTEGER LUNIN,LUNUSE
      CHARACTER*80 CFOUT
      CHARACTER*4 UPUP1
      INTEGER FLAG
*
*     convert DZEDIT SGML to HTML, plug in links
*
*     flag:       Steering flag:
*                 0 make all hyperlinks internal only
*                 1 add file name in first up bank
*                 2 add file names in all links
*                 3 as 2 and make 1 file/bank
*     upup1:      Id of Upbank of Upbank of top bank
*
*     output file name(s): BankId//UpId.html
*
*     Author: O.Schaile
*
      INTEGER ML, MS
      PARAMETER (ML=2000, MS=5000)
      CHARACTER*132 CB(ML), CL
      CHARACTER*8 CSTACK(MS), CLIST(MS)
      CHARACTER*4 BANKID, UPID, UPUPID, DOWNID
      CHARACTER*13 CF
      CHARACTER*4 DZE2US
      INTEGER IPBKID,IPUP,IPLSTA,IPLLAS, ISTAT, IP, IPSTAC,I,J,K,NC
     +        ,INDEXA,INDEXN,INDEXC,LNBLNK, IS, NUP,NCUPID,NCCF,IPLIST
     +        ,LUNOUT, NBANKS, NFILES
      LOGICAL EOF, IGNORE
*---
      IF(UPUP1.EQ.' ')THEN
         UPUP1='NONE'
      ELSE
         UPUP1=DZE2US(UPUP1)
      ENDIF
      EOF=.FALSE.
      IGNORE=.FALSE.
      NUP = 0
      NBANKS=0
      NFILES=0
      LUNOUT=0
 
*     Run through files and find all banks
      IPBKID=0
      IPUP=0
      IPLIST=0
*  10    READ(LUNIN,'(A)',END=50)CL
10    CALL DZDCRD(LUNIN,CL,ISTAT)
      IF(ISTAT.NE.0)GOTO 50
      IF(CL(2:9).EQ.'Bank IDH')THEN
         IPBKID=1
         CB(1)=CL
         BANKID=CL(12:15)
      ELSE IF(CL(2:5).EQ.'Up  ')THEN
         IF(IPBKID.EQ.0)THEN
            WRITE(*,*)'Prescan: Upbank appears before bank'
            GOTO 999
         ENDIF
         UPID=CL(12:15)
         BANKID=DZE2US(BANKID)
         UPID  =DZE2US(UPID)
         IPLIST=IPLIST+1
         IF(IPLIST.GT.MS)THEN
            WRITE(*,*)'Too many banks',IPLIST
            GOTO 999
         ENDIF
         CLIST(IPLIST)=BANKID//UPID
         IPBKID=0
      ENDIF
      GOTO 10
 
 50   CONTINUE
 
      REWIND(LUNIN)
*     LOOP ON BANKS
 
      IPSTAC=0
 100  CONTINUE
      IP=0
      IPBKID=0
      IPUP=0
      IPLSTA=0
      IPLLAS=0
*   110  READ(LUNIN,'(A)',END=150)CL
 110    CALL DZDCRD(LUNIN,CL,ISTAT)
      IF(ISTAT.NE.0)GOTO 150
*     IGNORE ALL CONTROL LINES
      IF(CL(1:4).EQ.'<FIG')IGNORE=.TRUE.
      IF(CL(1:5).EQ.'</FIG')IGNORE=.FALSE.
      IF(IGNORE)GOTO 110
      IF(CL(1:1).EQ.'<')GOTO 110
      IF(CL(1:2).EQ.' |')GOTO 110
      IF(CL(1:11).EQ.' ----------')GOTO 110
*     GET HERE FOR START OF A NEW BANK
 115  CONTINUE
      IF(CL(2:9).EQ.'Bank IDH')THEN
*        ONE FILE/BANK
         IF(FLAG.GE.3)THEN
            IF(LUNOUT.NE.0)THEN
               CLOSE(LUNOUT)
               LUNOUT=0
            ENDIF
         ENDIF
         IF(IPBKID.NE.0)THEN
            NBANKS=NBANKS+1
            IF(IPLSTA.NE.0 .AND. IPLLAS.EQ.0)IPLLAS=IP
            IPSTAC=IPSTAC+1
            IF(IPSTAC.GT.MS)THEN
               WRITE(*,*)'Too many banks'
               GOTO 999
            ENDIF
*
            NCUPID=LNBLNK(UPID)
            BANKID=DZE2US(BANKID)
            UPID  =DZE2US(UPID)
            IF(LUNOUT.LE.0)THEN
               LUNOUT=LUNUSE
               CALL CUTOL(CFOUT)
#if !defined(CERNLIB_QMIBM)
               CFOUT=BANKID//UPID//'.html'
               CALL KUOPEN(LUNOUT,CFOUT(1:LNBLNK(CFOUT)),
     +                    'UNKNOWN',ISTAT)
#endif
#if defined(CERNLIB_QMIBM)
               CFOUT=BANKID//UPID//' html'
               CALL FILEINF(ISTAT,'LRECL',256,'RECFM','V')
               OPEN(LUNOUT,FILE='/'//CFOUT(1:LNBLNK(CFOUT)),
     +         ACCESS='SEQUENTIAL',
     +          STATUS='UNKNOWN',FORM='FORMATTED',IOSTAT=ISTAT)
#endif
               NFILES=NFILES+1
               WRITE(LUNOUT,'(A)')'<html>'
               WRITE(LUNOUT,'(A)')'<head>'
               WRITE(LUNOUT,'(A)')'<body>'
               WRITE(LUNOUT,'(A)')'<isindex>'
               WRITE(LUNOUT,'(A)')'</head>'
               WRITE(LUNOUT,'(A)')'<body>'
               WRITE(LUNOUT,'(A)')'<pre>'
            ENDIF
            CSTACK(IPSTAC)=BANKID//UPID
            DO I=1,IP
               NC = LNBLNK(CB(I))
               IF(I.EQ.IPBKID)THEN
                  WRITE(LUNOUT,'(A)')'<hr>'
                  WRITE(LUNOUT,'(A)')'<a name="'
     +            //BANKID//UPID//'"><b>'//
     +            CB(I)(1:NC)//'</b></a>'
               ELSE IF(I.EQ.IPUP .AND. UPID.NE.'NONE')THEN
*                 FIRST FIND UP BANK OF IT
                  IF(IPSTAC.EQ.1 .AND. UPUP1.NE.' ')THEN
                     UPUPID=UPUP1
                  ELSE
                     UPUPID='NONE'
                     IF(IPSTAC.GT.1)THEN
                        DO K=IPSTAC-1,1,-1
                           IF(CSTACK(K)(1:4).EQ.UPID)THEN
                              UPUPID=CSTACK(K)(5:8)
                              GOTO 120
                           ENDIF
                        ENDDO
 120                    CONTINUE
                     ENDIF
                  ENDIF
                  IF((FLAG.GE.1 .AND. NUP .EQ.0) .OR.
     +                FLAG.GE.2) THEN
                     CF=UPID//UPUPID//'.html'
                     CALL CUTOL(CF)
                     NCCF=LNBLNK(CF)
                  ELSE
                     NCCF=0
                  ENDIF
                  NUP=NUP+1
                  NC=MAX(NC,16)
                  IF(NCCF.GT.0)THEN
                     WRITE(LUNOUT,'(A)')' Up        <a href="'//
     +                           CF//'#'//UPID//UPUPID//
     +                      '"><b>'//CB(I)(12:15)//'</b></a>'//
     +                        CB(I)(16:NC)
                  ELSE
                     WRITE(LUNOUT,'(A)')' Up        <a href="'//
     +                                       '#'//upid//upupid//
     +                      '"><b>'//CB(I)(12:15)//'</b></a>'//
     +                        CB(I)(16:NC)
                  ENDIF
               ELSEIF(IPLSTA.NE.0.AND.I.GE.IPLSTA.AND.I.LE.IPLLAS)THEN
                  IS=INDEXC(CB(I),' ')
                  IF(IS.LE.0)GOTO 145
*                 is it an integer, i.e. only numbers or space?
                  DO 126 J=IS,IS+4
                     IF(CB(I)(J:J).NE.' ')THEN
                        IF(INDEXN(CB(I)(J:J)).NE.1)GOTO 130
                     ENDIF
126               CONTINUE  
*                  READ(CB(I)(IS:IS+4),'(I5)',ERR=130)K
*                 yes, where does name start?
                  IS = INDEXA(CB(I))
                  IF(IS.LE.0)THEN
                     WRITE (*,*)' Down link has no name ', BANKID, UPID
                     WRITE(*,*) CB(I)
                     IS=10
                  ENDIF
                  DOWNID=CB(I)(IS:IS+3)
                  DOWNID=DZE2US(DOWNID)
                  IF(FLAG.GE.2)THEN
                     CF=DOWNID//BANKID//'.html'
                     CALL CUTOL(CF)
                     NCCF=13
                  ELSE
                     NCCF=0
                  ENDIF
                  IF(NCCF.GT.0)THEN
                     NC=MAX(NC,IS+4)
                     WRITE(LUNOUT,'(A)')CB(I)(1:IS-1)//
     +                           '<a href="'//CF//'#'//
     +                           DOWNID//BANKID//'"><b>'//
     +                           CB(I)(IS:IS+3)//'</b></a>'//
     +                           CB(I)(IS+4:NC)
                  ELSE
*                    is there doc?
                     DO K=1,IPLIST
                        IF(CLIST(K).EQ.DOWNID//BANKID)THEN
                           WRITE(LUNOUT,'(A)')CB(I)(1:IS-1)//
     +                           '<a href="#'//
     +                           DOWNID//BANKID//'"><b>'//
     +                           CB(I)(IS:IS+3)//'</b></a>'//
     +                           CB(I)(IS+4:NC)
                            GOTO 125
                         ENDIF
                     ENDDO
*                    fallen through do loop, no doc there
                     WRITE(LUNOUT,'(A)')CB(I)(1:IS-1)//
     +                           '<a href="nodoc.html"><b>'//
     +                           CB(I)(IS:IS+3)//'</b></a>'//
     +                           CB(I)(IS+4:NC)
                     WRITE(*,'(A)')' No doc for: '
     +               //DOWNID//' '//BANKID//
     +               ' will make a reference to: "nodoc.html"'
 125                 CONTINUE
                  ENDIF
                  GOTO 140
 130              CONTINUE
                  NC=MAX(NC,1)
                  WRITE(LUNOUT,'(A)')CB(I)(1:NC)
 140              CONTINUE
               ELSE
                  NC=MAX(NC,1)
                  WRITE(LUNOUT,'(A)')CB(I)(1:NC)
               ENDIF
 145           CONTINUE
            ENDDO
            IF(EOF) GOTO 900
            IPBKID=0
            IP=0
            IPBKID=0
            IPUP=0
            IPLSTA=0
            IPLLAS=0
         ENDIF
         IP=IP+1
         CB(IP)=CL
         IPBKID=IP
         BANKID=CL(12:15)
         CALL CLTOU(BANKID)
      ELSE IF(CL(2:5).EQ.'Up  ')THEN
         IF(IPUP.NE.0)THEN
            WRITE(*,*)'More then 1 Up bank'
            GOTO 999
         ENDIF
         IP=IP+1
         CB(IP)=CL
         IPUP=IP
         UPID=CL(12:15)
         CALL CLTOU(UPID)
      ELSE IF(INDEX(CL,'Description of the links') .NE. 0)THEN
         IF(IPLSTA.NE.0)THEN
            WRITE(*,*)'More then 1 Link block start'
            GOTO 999
         ENDIF
         IP=IP+1
         CB(IP)=CL
         IPLSTA=IP+1
      ELSE IF(INDEX(CL,'Description of the data') .NE. 0)THEN
         IF(IPLLAS.NE.0)THEN
            WRITE(*,*)'More then 1 Link block end'
            GOTO 999
         ENDIF
         IP=IP+1
         CB(IP)=CL
         IPLLAS=IP-1
      ELSE
         IP=IP+1
         CB(IP)=CL
      ENDIF
      GOTO 110
 150  EOF=.TRUE.
*     DO THE LAST BANK
      CL(2:9)='Bank IDH'
      GOTO 115
 
900   CONTINUE
      WRITE(LUNOUT,'(A)')'</pre>'
      WRITE(LUNOUT,'(A)')'</body>'
      WRITE(LUNOUT,'(A)')'</html>'
      CLOSE(LUNIN)
      CLOSE(LUNOUT)
999   END
 
      CHARACTER*(*) FUNCTION DZE2US(CH)
      CHARACTER*(*) CH
      DO I=1,LEN(CH)
         IF(CH(I:I).EQ.' ')THEN
            DZE2US(I:I)='_'
         ELSE
            DZE2US(I:I)=CH(I:I)
         ENDIF
      ENDDO
      RETURN
      END
************************************************************************
 
