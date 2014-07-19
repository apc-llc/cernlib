*
* $Id: dzdbk1.F,v 1.1.1.1 1996/03/04 16:12:56 mclareni Exp $
*
* $Log: dzdbk1.F,v $
* Revision 1.1.1.1  1996/03/04 16:12:56  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDBK1(CHBANK,LUNL,IFLAG)
      CHARACTER*8 CHBANK
 
      CHARACTER*16  CTEMP
      CHARACTER*60 CGET, SPACES
      EXTERNAL SPACES
      CHARACTER*1 CC
      CHARACTER*16 CNL,CNS,CND,CNZ
      CHARACTER*6 CIOC
      CHARACTER*4 CFLINK,CLLINK
      SAVE CFLINK,CLLINK
      INTEGER KEYVEC(2)
#include "dzdoc/bkwrp.inc"
#include "dzdoc/tapes.inc"
*         From DZEBRA
#include "zebra/zbcdk.inc"
 
      CALL UCTOH(CHBANK(1:4),KEYVEC(1),4,4)
      IDHBK = KEYVEC(1)
      CALL UCTOH(CHBANK(5:8),KEYVEC(2),4,4)
      ICYCLE = 100000
      LQBKD1=0
      CALL RZIN(0,LQBKD1,2,KEYVEC,ICYCLE,' ')
      IF(LQBKD1.EQ.0 .OR. IQUEST(1) .NE. 0)THEN
          WRITE(LUNL,*)'* no doc for ',CHBANK
          GOTO 999
      ENDIF
*      WRITE(*,'(A,A4)')' Bank ID= ',IQ(KQSP+LQBKD1+1)
      NWGEN=IQ(KQSP+LQBKD1+11)
*     WRITE(*,*)' NWGEN',NWGEN
      IP0 =LQBKD1+KQSP+1
*     keep seq for declarations
      ILCB=INDXBC(CHBANK(1:4),' ')
      IF(IFLAG.EQ. 1)THEN
          WRITE(LUNL,'(A)')'+KEEP,DECL'//CHBANK(1:ILCB)
          CALL DZDPLN(LUNL,'INTEGER L'//CHBANK(1:ILCB),2)
          CFLINK=CHBANK(1:ILCB)
          CLLINK=CFLINK
          GOTO 999
      ENDIF
      IF(IFLAG.EQ. 5)THEN
          CALL DZDPLN(LUNL,
     &    'COMMON/LK'//CHBANK(1:ILCB)//'/L'//CHBANK(1:ILCB),2)
          CFLINK=CHBANK(1:ILCB)
          CLLINK=CFLINK
          GOTO 999
      ENDIF
      IF(IFLAG.EQ.2 .OR. IFLAG.EQ.6)THEN
          CALL DZDPLN(LUNL,',L'//CHBANK(1:ILCB),1)
          IF(IFLAG.EQ.6)CLLINK=CHBANK(1:ILCB)
          GOTO 999
      ENDIF
 
      IF(IFLAG.EQ. 7)THEN
          WRITE(LUNL,'(A)')'+KEEP,DECLMM'//CHBANK(1:ILCB)
          CALL DZDPLN(LUNL,
     &    'INTEGER MM'//CHBANK(1:ILCB)//'(5)',2)
          GOTO 999
      ENDIF
      IF(IFLAG.EQ. 9)THEN
          CALL DZDPLN(LUNL,
     &    'COMMON/MM'//CHBANK(1:ILCB)//'/MM'//CHBANK(1:ILCB),2)
          GOTO 999
      ENDIF
      IF(IFLAG.EQ.8)THEN
          CALL DZDPLN(LUNL,
     &    ',MM'//CHBANK(1:ILCB)//'(5)',1)
          GOTO 999
      ENDIF
      IF(IFLAG.EQ.10)THEN
          CALL DZDPLN(LUNL,
     &    ',MM'//CHBANK(1:ILCB),1)
          GOTO 999
      ENDIF
*
      IF(IFLAG.EQ.11)THEN
          WRITE(LUNL,'(A)')'+KEEP,FILLMM'//CHBANK(1:ILCB)
      ENDIF
 
      IF(IFLAG.EQ.3 .OR. IFLAG .EQ. 13)THEN
          IF(IFLAG.EQ.3)THEN
              WRITE(LUNL,'(A)')'+KEEP,BOOK'//CHBANK(1:ILCB)
          ELSE
              WRITE(LUNL,'(A)')'+KEEP,LIFT'//CHBANK(1:ILCB)
          ENDIF
*         div index
          CALL DZDGGI(IQ(IP0),'DV',' ',CGET,IFC,ILC)
*          WRITE(*,*)CGET(IFC:ILC)
           IF(IFC .EQ.0)THEN
              IFC=1
             ILC=1
              CGET(IFC:IFC)='0'
          ENDIF
          CALL DZDPLN(LUNL,'CALL MZLINK('//CGET(IFC:ILC)//','
     &    //''''//CHBANK(1:ILCB)//''''
     &    //',L'//CFLINK//',L'//CFLINK//',L'//CLLINK//')',2)
          CALL DZDPLN(LUNL,' ',99)
      ENDIF
      IF(   IFLAG.EQ. 3 .OR. IFLAG.EQ. 4
     & .OR. IFLAG.EQ.13 .OR. IFLAG.EQ.14)THEN
*         write a comment line
          CALL DZDGGI(IQ(IP0),'CL',' ',CGET,IFC,ILC)
*         skip HID
          IFC=IFC+4
          IF(IFC.LE.ILC)WRITE(LUNL,'(A)')'*    '//CGET(IFC:ILC)
      ENDIF
 
      CALL DZDGGI(IQ(IP0),'ND',' ',CND,IFC,ILC)
      IF(IFC .EQ.0 .OR. CND(1:1).EQ.'0')THEN
          CND='0'
          CIOC='2'
      ELSE
*         IO char
          CALL DZDGGI(IQ(IP0),'IO',' ',CGET,IFC,ILC)
          IF(ILC.LE.0)THEN
             CALL DZDDOC(0,0,CHBANK,'I',1,10000,6,CGET,NKEEPS)
             IFC=INDXNC(CGET)
             IF(IFC.LE.0)GOTO 991
             ILC=LNBLNK(CGET)
             IF(ILC.GT.IFC)IFC=1
          ENDIF
          IF(IFC.EQ.ILC)THEN
*            just one character given (0,1,2,3,4,5,B,I,F,D,H)
             CC=CGET(IFC:IFC)
             IF(CC.EQ.'B')THEN
                 CC='1'
             ELSEIF(CC.EQ.'I')THEN
                 CC='2'
             ELSEIF(CC.EQ.'F')THEN
                 CC='3'
             ELSEIF(CC.EQ.'D')THEN
                 CC='4'
             ELSEIF(CC.EQ.'H')THEN
                 CC='5'
             ENDIF
*            is it a number from 0-5?
             IF(INDEXN(CC).EQ.1)THEN
                 READ(CC,'(I1)')IIOC
                 IF(IIOC.GT.5)GOTO 991
             ELSE
                 GOTO 991
             ENDIF
             CIOC=CC
          ELSE
*            more then one char given call MZFORM
             CIOC='IO'//CHBANK(1:ILCB)
             IF(IFLAG.LE.2)CALL DZDPLN(LUNL,','//CIOC,1)
          ENDIF
          ILCIO=INDXBC(CIOC,' ')
      ENDIF
 
*     come back to MZFORM
      IF(  IFLAG.EQ.3 .OR. IFLAG.EQ.4
     & .OR.IFLAG.EQ.11 .OR. IFLAG.EQ.12)THEN
          IF(IFC.LT.ILC)THEN
*            remove ' '
             II=INDEX(CGET(IFC:ILC),'''')
             IF(II.GT.0)THEN
                CGET(IFC+II-1:IFC+II-1)=' '
                II=INDEX(CGET(IFC:ILC),'''')
                IF(II.GT.0)CGET(IFC+II-1:IFC+II-1)=' '
             ENDIF
             CGET(IFC:ILC)=SPACES(CGET(IFC:ILC),1)
             ILC=MAX(1,LNBLNK(CGET(IFC:ILC))+IFC-1)
             CALL DZDPLN(LUNL,
     &       'CALL MZFORM('''//CHBANK(1:ILCB)
     &       //''','''//CGET(IFC:ILC)
     &       //''','//CIOC//')',2)
          ENDIF
      ENDIF
 
*     get and remember parameters needed after JBIAS
 
      CALL DZDGGI(IQ(IP0),'NL',' ',CNL,IFCNL,ILCNL)
      IF(IFCNL.EQ.0)THEN
         IFCNL=1
         ILCNL=1
         CNL='0'
      ENDIF
      CALL DZDGGI(IQ(IP0),'NS',' ',CNS,IFCNS,ILCNS)
      IF(IFCNS.EQ.0)THEN
         IFCNS=1
         ILCNS=1
         CNS='0'
      ENDIF
 
      CALL DZDGGI(IQ(IP0),'ND',' ',CND,IFCND,ILCND)
      IF(IFCND .EQ.0)THEN
         IFCND=1
         ILCND=1
         CND='0'
      ENDIF
 
*    fill bank descriptor vector if MZLIFTs req
 
      IF(IFLAG.EQ.11 .OR. IFLAG.EQ.12)THEN
          CALL DZDPLN(LUNL,
     &    'CALL UCTOH('//''''//CHBANK(1:ILCB)//''''//',MM'
     &    //CHBANK(1:ILCB)//',4,4)',2)
          CALL DZDPLN(LUNL,
     &    'MM'//CHBANK(1:ILCB)//'(2) = '//CNL(IFCNL:ILCNL),2)
          CALL DZDPLN(LUNL,
     &    'MM'//CHBANK(1:ILCB)//'(3) = '//CNS(IFCNS:ILCNS),2)
          CALL DZDPLN(LUNL,
     &    'MM'//CHBANK(1:ILCB)//'(4) = '//CND(IFCND:ILCND),2)
          CALL DZDPLN(LUNL,
     &    'MM'//CHBANK(1:ILCB)//'(5) = '//CIOC(1:ILCIO),2)
 
           GOTO 999
      ENDIF
 
      CALL DZDGGI(IQ(IP0),'NZ',' ',CNZ,IFC,ILC)
      IF(IFC .EQ.0)CNZ='0'
*     div index
      CALL DZDGGI(IQ(IP0),'DV',' ',CGET,IFC,ILC)
*      WRITE(*,*)CGET(IFC:ILC)
      IF(IFC .EQ.0)THEN
          IFC=1
          ILC=1
          CGET(IFC:IFC)='0'
      ENDIF
 
      IF(IFLAG.EQ.3 .OR. IFLAG.EQ.4)
     & CALL DZDPLN(LUNL,'CALL MZBOOK(',2)
 
      IF(IFLAG.EQ.13 .OR. IFLAG.EQ.14)
     & CALL DZDPLN(LUNL,'CALL MZLIFT(',2)
      CALL DZDPLN(LUNL,CGET(IFC:ILC),1)
*     link to lifted bank
      CALL DZDPLN(LUNL,',L'//CHBANK(1:ILCB),1)
*     up bank
      CALL DZDGGI(IQ(IP0),'UP',' ',CGET,IFC,ILC)
*      WRITE(*,*)CGET(IFC:ILC)
      IF(IFC.LE.0)THEN
           WRITE(LUNL,*)' No Up-bank given'
           GOTO 999
      ENDIF
      ILCU1=INDXBC(CGET(IFC:IFC+3),' ')-1
      CALL CLTOU(CGET(IFC:IFC+ILCU1))
      CALL DZDPLN(LUNL,',L'//CGET(IFC:IFC+ILCU1),1)
 
*     Jbias
      IF(ILC.LE.4)THEN
*         no JBIAS given
          IF(CGET(IFC:IFC+ILCU1).EQ.'NONE')THEN
*             is it top bank?
              ILC=IFC+4
              CGET(ILC:ILC)='1'
          ELSE
 
*           find it in up bank
            CTEMP(1:4) = CHBANK(5:8)
            CTEMP(5:8) = '****'
            NFKEY = 0
5           CONTINUE
            CALL DZDWCS(CTEMP,KEYVEC,NFKEY)
            IF(KEYVEC(1) .EQ. 0)GOTO 100
            CALL DZDPIN(KEYVEC,'RBG')
            IF(IQUEST(1) .NE. 0)GOTO 100
            LUPD = IQUEST(11)
 
*     find link offset in up-bank
 
            NSUP = IQ(KQSP+LUPD+5)
            DO 10 I=1,NSUP
            IF(IQ(KQSP+LUPD+10+I) .EQ. IDHBK)THEN
                  CGET(1:4)=' '
                  WRITE(CGET(1:10),'(I10)')-I
                  IFC=INDEXC(CGET,' ')-4
                  ILC=10
                  GOTO 20
            ENDIF
10          CONTINUE
*           try next
            GOTO 5
100         WRITE(LUNL,*)'JBIAS not found'
          ENDIF
20        CONTINUE
          IF(LUPD.NE.0)CALL MZDROP(0,LUPD,' ')
          LUPD=0
      ENDIF
      CALL DZDPLN(LUNL,','//CGET(IFC+4:ILC),1)
 
      IF(IFLAG.EQ.3 .OR. IFLAG.EQ.4)THEN
*         CHID
          CALL DZDPLN(LUNL,','''//CHBANK(1:ILCB)//'''',1)
*         NL,NS,ND
          CALL DZDPLN(LUNL,','//CNL,1)
          CALL DZDPLN(LUNL,','//CNS,1)
          CALL DZDPLN(LUNL,','//CND,1)
*         IOC
          CALL DZDPLN(LUNL,','//CIOC(1:ILCIO),1)
      ENDIF
      IF(IFLAG.EQ.13 .OR. IFLAG.EQ.14)THEN
          CALL DZDPLN(LUNL,',MM'//CHBANK(1:ILCB),1)
      ENDIF
*     NZ
      CALL DZDPLN(LUNL,','//CNZ,1)
      CALL DZDPLN(LUNL,')',1)
      CALL DZDPLN(LUNL,' ',99)
      GOTO 999
991   WRITE(LUNL,*)'Illegal or no IO Char given', CGET(IFC:ILC)
999   CONTINUE
      IF(LQBKD1 .NE. 0)CALL MZDROP(0,LQBKD1,' ')
      LQBKD1=0
      END
 
***********************************************************************
