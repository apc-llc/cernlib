*
* $Id: dzdedd.F,v 1.1.1.1 1996/03/04 16:13:02 mclareni Exp $
*
* $Log: dzdedd.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:02  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDEDD
#include "dzdzbrinc.inc"
      INTEGER IQUEST(100)
      COMMON/QUEST/IQUEST
#include "zebra/zebq.inc"
#include "zebra/mzca.inc"
#include "zebra/mzcb.inc"
#include "zebra/mzcc.inc"
#include "zebra/mzcn.inc"
#include "zebra/mzbits.inc"
      INTEGER IHID(2),NCH,NCHB,NCHU,NCHO,NCHF, IDIV, ISTORE, LGO
      INTEGER IDIVIN,LTMP, LIN, IXDIV, LL, LUP
      LOGICAL EXST
      CHARACTER*8 CHKEY
      CHARACTER*6 CHOPT, CHEXT
      CHARACTER*1 CHOTMP
      CHARACTER*80 CHFILE, CHFULL, SPACES
*--
      CALL DZDIBR
      CALL RZCDIR(CWDSAV,'R')
      IXDIV=0
      LTMP=0
      CHKEY=' '
      CALL KUGETC(CHKEY(1:4),NCHB)
      CALL KUGETC(CHKEY(5:8),NCHU)
      IF(INDEX(CHKEY(5:8),'*').NE.0)NCHU=0
      IF(NCHU.LE.0)CHKEY(5:8)='****'
      CALL KUGETS(CHOPT,NCHO)
      CALL KUGETS(CHFILE,NCHF)
      CALL KUGETI(ISTORE)
      CALL KUGETI(IDIV)
      CHOTMP='T'
      GOTO 5
 
*     entry if called from browser
 
      ENTRY DZDED1(IDIVIN,LIN)
 
      CHOTMP=' '
      CALL DZDIBR
      CALL RZCDIR(CWDSAV,'R')
      LTMP=LIN
      IXDIV=IDIVIN
      ISTORE=JBYT(IXDIV,27,6)
      NCHF=0
      NCHB=4
      CALL MZSDIV(IXDIV,1)
*     get HID of bank and up bank
      IHID(1)=IQ(LIN+KQS-4)
      CALL UHTOC(IHID(1),4,CHKEY,4)
      IF(LQ(LIN+KQS+1).EQ.0)THEN
         CHKEY(5:8)='NONE'
         CALL UCTOH(CHKEY(5:8),IHID(2),4,4)
      ELSE
         IHID(2)=IQ(LQ(LIN+KQS+1)+KQS-4)
         CALL UHTOC(IHID(2),4,CHKEY(5:8),4)
      ENDIF
*     look if doc is in Rzfile
      CHOPT='Memory'
      IF(CTDDOC.NE.' ')THEN
         CALL RZCDIR(CTDDOC,' ')
         ICYCLE=1000000
         CALL RZIN(0,LL,2,IHID,ICYCLE,'C')
         IF(IQUEST(1).EQ.0)THEN
            CHOPT='Rz'
         ENDIF
      ENDIF
5     CONTINUE
 
      IF(NCHF.LE.0)THEN
         IF(NCHB.LE.0)THEN
            WRITE(*,*)'No bank nore file name given'
            GOTO 999
         ENDIF
         CHFILE(1:4)=CHKEY(1:4)
         IF(NCHU.GT.0)THEN
            CHFILE(5:9)='_'//CHKEY(5:8)
         ELSE
            CHFILE(5:9)=' '
         ENDIF
         CHFILE(10:)='.dzdoc'
      ENDIF
      CHFILE=SPACES(CHFILE,0)
      CALL CUTOL(CHFILE)
      LUNTMP=LUPTMP
      IF(CHOPT.NE.'Ascii')THEN
         CALL KUOPEN(LUNTMP,CHFILE,'UNKNOWN',ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(*,'(A)')'Cannot open '//CHFILE
            LUNTMP=6
            GOTO 999
         ENDIF
      ENDIF
      IF     (CHOPT.EQ.'Rz')THEN
         CALL DZDSDO(0,0,LUNTMP,CHKEY,'IR')
 
      ELSE IF(CHOPT.EQ.'Ascii')THEN
 
         IF(INDEX(CHFILE,'.').NE.0)THEN
            CHEXT=' '
         ELSE
            CHEXT='.dzdoc'
         ENDIF
         NCH=LENOCC(CHFILE)
         DO 10 I=0,NDIADD
*           look on CWD
            IF(I.EQ.0)THEN
               CHFULL=CHFILE(1:NCH)//CHEXT
            ELSE
               NCH1=LENOCC(CDIADD(I))
               CHFULL=CDIADD(I)(1:NCH1)//CHFILE(1:NCH)//CHEXT
            ENDIF
            INQUIRE(FILE=CHFULL, EXIST=EXST)
            IF(EXST)THEN
               CHFILE=CHFULL
               GOTO 30
            ENDIF
10       CONTINUE
         WRITE(*,*)'File not found: ', CHFILE
 
      ELSE IF(CHOPT.EQ.'Memory')THEN
         IF(LTMP.NE.0)THEN
            LZEBLK(3,ISTORE)=LTMP
            GOTO 25
         ENDIF
         CALL UCTOH(CHKEY(1:4),IHID(1),4,4)
         CALL UCTOH(CHKEY(5:8),IHID(2),4,4)
         IF(IXDIV.EQ.0)THEN
            IF(ISTORE.LT.0 .OR. ISTORE.GT.16)ISTORE=0
            IF(IDIV.LE.0 .OR. IDIV.GT.20)IDIV=2
            IXDIV=IDIV
            CALL SBYT(ISTORE,IXDIV,27,6)
            CALL MZSDIV(IXDIV,1)
         ENDIF
*        init link area for this store
         IF(LZEFLG(ISTORE).EQ.0)THEN
            CALL MZLINK(IXDIV,'DZDZEB',
     &      LZEBLK(1,ISTORE),LZEBLK(3,ISTORE),LZEBLK(3,ISTORE))
            LZEFLG(ISTORE)=1
            IIST=0
            CALL SBYT(ISTORE,IIST,27,6)
            DO 6 K=21,24
6           CALL MZXREF(IXDIV,K+IIST,'A')
         ENDIF
 
*        find the bank
 
         LGO=0
20       CONTINUE
         LZEBLK(3,ISTORE)=LZFIDH(IXDIV,IHID(1),LGO)
         IF(LZEBLK(3,ISTORE).EQ.0)THEN
            WRITE(*,'(A,I3,A,I3)')
     &      'Bank not found in Store',ISTORE,' Division',IDIV
            GOTO 999
         ENDIF
*        look for Upbank
         IF(NCHU.NE.0)THEN
            LGO=LZEBLK(3,ISTORE)
            LUP= LQ(LZEBLK(3,ISTORE)+1+KQS)
            IF(CHKEY(5:8).EQ.'NONE' .AND. LUP .NE. 0)GOTO 20
            IF(LUP.EQ.0)THEN
               WRITE(*,'(A,A,I3,A,I3)')
     &         'Bank  not found in Store',ISTORE,' Division',IDIV
               GOTO 999
            ENDIF
            IF(IQ(LUP-4+KQS).NE.IHID(2))GOTO 20
         ENDIF
25       CALL DZDTMP(IXDIV,LZEBLK(3,ISTORE),LUNTMP,CHOTMP//'AVDS')
      ELSE
         WRITE(*,*)'Unknown option ',CHOPT
         GOTO 999
      ENDIF
 
30    CONTINUE
      CLOSE(LUNTMP)
      LUNTMP=0
      IF(EDITSV)THEN
         CALL KUESVR(CHFILE, ISTAT)
      ELSE
         CALL KUEDIT(CHFILE, ISTAT)
         IF(ISTAT.NE.0)THEN
            WRITE(*,*)'No mods made, dont put into Rz-file'
            GOTO 999
         ENDIF
         CALL DZDUDD(CHFILE)
      ENDIF
999   CALL RZCDIR(CWDSAV,' ')
      END
 
*********************************************************************
