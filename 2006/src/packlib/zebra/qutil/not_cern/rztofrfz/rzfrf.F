*
* $Id: rzfrf.F,v 1.1.1.1 1996/03/06 10:47:19 mclareni Exp $
*
* $Log: rzfrf.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:19  mclareni
* Zebra
*
*
#include "sys/CERNLIB_machine.h"
#include "_zebra/pilot.h"
#if defined(CERNLIB_RFRX)
      PROGRAM RFRX
#endif
#if defined(CERNLIB_RFRA)
      PROGRAM RFRA
#endif
C
C
#include "rzpawc.inc"
#include "rzlenf.inc"
      COMMON / QUEST / IQUEST(100)
      COMMON/PAWC/NWPAW,IXPAWC,IHDIV,IXHIGZ,IXKU,FENC(5),LMAIN,
     +           HCV(MEMOR)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
      CHARACTER*2  CHOPT
      CHARACTER*8  CHTAG(100)
      CHARACTER*99 CHFORM
      CHARACTER*12 CHORG
#include "rzclun.inc"
      CHARACTER*(LENF) FIN, FOUT
#if (defined(CERNLIB_QMVAX))&&(defined(CERNLIB_RFRX))
      CHARACTER*10 RECORDTYPE
#endif
#if defined(CERNLIB_QMIBM)
      INTEGER      SYSTEMF
#endif
#if (defined(CERNLIB_QMIBM))&&(defined(CERNLIB_RFRX))
      CHARACTER*6  CHRECL
#endif
      INTEGER OURECL
      DIMENSION IHTAG(2),ICDIR(400),IHEAD(500)
      EQUIVALENCE (ICDIR(1),IHEAD(4))
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))
      LOGICAL      GET_COMMAND_LINE
      CHARACTER*4  CHVERB
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RFRX))
      EXTERNAL     RFRX_CLD
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RFRA))
      EXTERNAL     RFRA_CLD
#endif
#include "rzmode.inc"
C
#if defined(CERNLIB_QMIBM)
      CALL ERRSET(151,0,-1,1,1)
#endif
      CHOPT='F'//MODE
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RFRA))
      CHVERB = 'RFRA'
      IF ( .NOT. GET_COMMAND_LINE( RFRA_CLD , CHVERB ) ) THEN
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))&&(defined(CERNLIB_RFRX))
      CHVERB = 'RFRX'
      IF ( .NOT. GET_COMMAND_LINE( RFRX_CLD , CHVERB ) ) THEN
#endif
#if (defined(CERNLIB_QMVAX))&&(!defined(CERNLIB_DCLTABLES))
        PRINT 9001,CHVERB
9001  FORMAT(1X,A,'. invalid command line')
        CALL EXIT
      ENDIF
#endif
 
      CALL GETF(FIN,FOUT,CHOPT,'RZ')
C
#if (defined(CERNLIB_QMIBM))&&(!defined(CERNLIB_IOPACK)||defined(CERNLIB_RFRA))
      IRET=SYSTEMF('FILEDEF 1 CLEAR')
#endif
#if (defined(CERNLIB_QMIBM))&&(defined(CERNLIB_IOPACK))&&(defined(CERNLIB_RFRX))
      IRET=SYSTEMF('FILEDEF IOFILE01 CLEAR')
#endif
#if defined(CERNLIB_QMIBM)
      IRET=SYSTEMF('FILEDEF 2 CLEAR')
#endif
*
*     Figure out the record length of the input file
*
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMALT))
      INRECL =  3600
#endif
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMIBM)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX))
      INRECL = 32760
#endif
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMAPO))
      OPEN(1,FILE=FIN,FORM=FORM,STATUS='READONLY')
      INQUIRE(1,RECL=INRECL)
#endif
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMVMI)||defined(CERNLIB_QMSGI))
      INQUIRE(1,RECL=INRECL)
#endif
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMVAX))
      OPEN(1,FILE=FIN,FORM=FORM,STATUS='OLD',SHARED,READONLY)
      INQUIRE(1,RECL=INRECL,RECORDTYPE=RECORDTYPE)
      CLOSE(1)
#endif
#if (defined(CERNLIB_RFRA))&&(defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX))
      INRECL = 80
#endif
*
*     Open the input file
*
#if (defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMUIX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX))&&(!defined(CERNLIB_QMALT))
      OPEN(1,FILE=FIN,FORM=FORM,STATUS='OLD')
#endif
#if defined(CERNLIB_QMALT)
      OPEN(1,FILE=FIN,FORM=FORM,STATUS='OLD',READONLY
#endif
#if defined(CERNLIB_QMAPO)
      OPEN(1,FILE=FIN,FORM=FORM,STATUS='READONLY'
#endif
#if defined(CERNLIB_QMVAX)
      OPEN(1,FILE=FIN,FORM=FORM,STATUS='OLD',SHARED,READONLY
#endif
#if (defined(CERNLIB_QMALT)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMVAX))&&(defined(CERNLIB_RFRX))
     +     ,RECL=INRECL
#endif
#if (defined(CERNLIB_QMVAX))&&(defined(CERNLIB_RFRX))
     +     ,RECORDTYPE=RECORDTYPE
#endif
#if defined(CERNLIB_QMALT)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMVAX)
     +     )
#endif
#if (defined(CERNLIB_QMIBM))&&(defined(CERNLIB_RFRX))&&(defined(CERNLIB_IOPACK))
      WRITE(CHRECL,'(I6)') INRECL
      IRC=SYSTEMF('FILEDEF IOFILE01 DISK '//FIN//
     +           ' (RECFM U BLOCK '//CHRECL)
#endif
#if (defined(CERNLIB_QMIBM))&&(defined(CERNLIB_RFRX))&&(!defined(CERNLIB_IOPACK))
      CALL FILEINF(ISTAT,'RECFM','U','BLKSIZE',INRECL)
#endif
#if (defined(CERNLIB_QMIBM))&&(!defined(CERNLIB_IOPACK)||defined(CERNLIB_RFRA))
      OPEN(1,FILE='/'//FIN,FORM=FORM,STATUS='OLD')
#endif
C
      CALL MZEBRA(-3)
      CALL MZPAW(MEMOR,' ')
      CHOPT='I'//MODE
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMVAX)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMVMI))
      CALL FZFILE(1,INRECL,CHOPT)
#endif
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMALT)||defined(CERNLIB_QMAPO))
      CALL FZFILE(1,INRECL/4,CHOPT)
#endif
#if (defined(CERNLIB_RFRX))&&(defined(CERNLIB_QMIBM)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIBX)||defined(CERNLIB_QMNXT)||defined(CERNLIB_QMLNX))
      CALL FZFILE(1,0,CHOPT)
#endif
#if defined(CERNLIB_RFRA)
*     Defaults ok on all machines for RFRA
      CALL FZFILE(1,0,CHOPT)
#endif
C
   5  NH=KNSIZE
      CALL FZIN(1,0,0,0,'S',NH,IHEAD)
      IF(IQUEST(1).NE.0) THEN
        WRITE(6,'('' Error in reading the file'')')
        GO TO 99
      END IF
      IF(NH.NE.2)           GO TO 5
      IF(IHEAD(1).NE.12345) GO TO 5
   7  NH=KNSIZE
      CALL FZIN(1,0,0,0,'S',NH,IHEAD)
      IF(IQUEST(1).NE.0) THEN
        WRITE(6,'('' Error in reading the file'')')
        GO TO 99
      END IF
      IF(NH.NE.KNSIZE)  GO TO 7
      IF(IHEAD(1).NE.1) GO TO 7
C
      NREC   = ICDIR(KQUOTA)
      NWKEY  = ICDIR(KNWKEY)
      KTAGS  = KKDES+(NWKEY-1)/10+1
      CHFORM = ' '
      LB     = ICDIR(KLB)
      OURECL = ICDIR(LB+1)
C
#if defined(CERNLIB_QMIBM)
      CALL FILEINF(ISTAT,'MAXREC',20)
      IF(ISTAT.NE.0) THEN
        WRITE(6,'('' Error in FILEINF 20'')')
        GO TO 99
      END IF
C
      FOUT(1:1) = '/'
      OPEN(2,FILE=FOUT,
     +     ACCESS='DIRECT',
     +     FORM='UNFORMATTED',
     +     STATUS='UNKNOWN',
     +     RECL=4*OURECL)
      CLOSE(2)
C
      CALL FILEINF(ISTAT,'MAXREC',2**24-1)
      IF(ISTAT.NE.0) THEN
        WRITE(6,'('' Error in FILEINF 2**24-1'')')
        GO TO 99
      END IF
#endif
#if defined(CERNLIB_QMVAX)
      CHORG = 'SEQUENTIAL'
      IF(IRELAT.NE.0) CHORG = 'RELATIVE'
#endif
      OPEN(2,FILE=FOUT,
     +     ACCESS='DIRECT',
     +     FORM='UNFORMATTED',
#if defined(CERNLIB_QMVAX)
     +     STATUS='NEW',ORGANIZATION=CHORG,
#endif
#if defined(CERNLIB_QMIBM)||defined(CERNLIB_QMAPO)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMUIX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)
     +     STATUS='UNKNOWN',
#endif
#if defined(CERNLIB_QMVAX)||defined(CERNLIB_QMSGI)||defined(CERNLIB_QMVMI)
     +     RECL=OURECL
#endif
#if defined(CERNLIB_QMAPO)||defined(CERNLIB_QMIBM)||defined(CERNLIB_QMIRT)||defined(CERNLIB_QMSUN)||defined(CERNLIB_QMHPX)||defined(CERNLIB_QMIBX)||defined(CERNLIB_QMALT)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)||defined(CERNLIB_QMNXT)
     +     RECL=4*OURECL
#endif
#if defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCV64)
     +     RECL=8*OURECL
#endif
#if defined(CERNLIB_QMCV32)
     +     RECL=4*OURECL
#endif
     +     )
C
      DO 10 I=1,NWKEY
         CALL ZITOH(ICDIR(KTAGS+2*I-2),IHTAG,2)
         CALL UHTOC(IHTAG,4,CHTAG(I),8)
         IKDES=(I-1)/10
         IKBIT1=3*I-30*IKDES-2
         IFORM=JBYT(ICDIR(KKDES+IKDES),IKBIT1,3)
         IF(IFORM.EQ.3)THEN
            CHFORM(I:I)='H'
         ELSEIF(IFORM.EQ.4)THEN
            CHFORM(I:I)='A'
         ELSEIF(IFORM.EQ.1)THEN
            CHFORM(I:I)='B'
         ELSE
            CHFORM(I:I)='I'
         ENDIF
  10  CONTINUE
C
      CALL FZENDI(1,'IQ')
C
*     N option did not exist prior to 94B, but now means
*     NEW RZ format
*
      CALL RZMAKE(2,'RZ',NWKEY,CHFORM,CHTAG,NREC,' ')
*     CALL RZMAKE(2,'RZ',NWKEY,CHFORM,CHTAG,NREC,'N')
      IQ(LTOP+KDATEC)=ICDIR(KDATEC)
      IQ(LTOP+KDATEM)=ICDIR(KDATEM)
      CALL RZFRFZ(1,' ')
      CALL RZLDIR(' ',' ')
      CALL FZENDI(1,' ')
      CALL RZEND('RZ')
C
  99  CONTINUE
#if defined(CERNLIB_QMAPO)||defined(CERNLIB_QMVAX)||defined(CERNLIB_QMCRY)||defined(CERNLIB_QMCVX)||defined(CERNLIB_QMUIX)||defined(CERNLIB_QMDOS)||defined(CERNLIB_QMLNX)
      CLOSE(1)
#endif
#if (defined(CERNLIB_QMIBM))&&(defined(CERNLIB_IOPACK))&&(defined(CERNLIB_RFRX))
      IRET=SYSTEMF('FILEDEF IOFILE01 CLEAR')
#endif
#if (defined(CERNLIB_QMIBM))&&(!defined(CERNLIB_IOPACK)||defined(CERNLIB_RFRA))
      CLOSE(1)
#endif
      CLOSE(2)
C
      END
