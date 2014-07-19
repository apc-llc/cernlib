*
* $Id: xzrzcp.F,v 1.1.1.1 1996/03/08 15:44:32 mclareni Exp $
*
* $Log: xzrzcp.F,v $
* Revision 1.1.1.1  1996/03/08 15:44:32  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE XZRZCP(CHIN,CHOUT,IRECL,CHOPT,IRC)
 
      CHARACTER*(*) CHIN,CHOUT
      CHARACTER*4   CHOPO,CHOPM
#include "cspack/slate.inc"
#include "cspack/hcmail.inc"
#include "cspack/czunit.inc"
#include "cspack/czsock.inc"
#include "cspack/zmach.inc"
#include "cspack/pawc.inc"
      DIMENSION KEYI(100),KEYO(100)
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
      CHARACTER*8  CHTAG(100)
      CHARACTER*90 CHFORM
      DIMENSION    IHTAG(2)
#include "cspack/rzclun.inc"
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
#include "cspack/quest.inc"
#include "cspack/czopts.inc"
 
      IRC    = 0
 
      WRITE(6,9001)
 9001 FORMAT(' XZRZCP. this routine used the RZ routine RZCOPY.',/,
     +       '         Unfortunately, permits neither data nor',
     +       ' record length conversion.')
      WRITE(6,9002)
 9002 FORMAT(' XZRZCP. data (e.g. native to exchange) and/or',
     +       ' record length conversion can',/,8X,
     +       ' be accomplished using the RTOF/RFRF ZFTP commands.')
      RETURN
 
#if defined(CERNLIB_ONEDAY)
      NRECS  = IS(1)
      LCHIN  = LENOCC(CHIN)
      LCHOUT = LENOCC(CHOUT)
      LCHOPT = LENOCC(CHOPT)
 
      IF(IDEBXZ.GE.1) PRINT *,'XZRZCP. enter for ',
     +   CHIN(1:LCHIN),' ',CHOUT(1:LCHOUT),' ',
     +   IRECL,' ',CHOPT
 
      IF(LCHIN.EQ.0.OR.LCHOUT.EQ.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRZCP. error - input or ',
     +      'output file name missing'
         IRC = -1
         GOTO 40
      ENDIF
*
*     Open input file
*
      IF(IOPTC.NE.0) THEN
         CALL RZOPEN(LUNXZI,'RZIN',CHIN(1:LCHIN),'P',JRECL,IRC)
      ELSE
         CALL RZOPEN(LUNXZI,'RZIN',CHIN(1:LCHIN),' ',JRECL,IRC)
      ENDIF
 
      IF(IRC.NE.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRZCP. cannot open input file'
         GOTO 40
      ELSEIF(IDEBXZ.GE.0) THEN
         PRINT *,'XZRZCP. input file opened with LRECL = ',JRECL
      ENDIF
 
      CALL RZFILE(LUNXZI,'RZIN',' ')
      IF(IQUEST(1).NE.0) THEN
         IRC = IQUEST(1)
         CLOSE(LUNXZI)
         GO TO 40
      ENDIF
 
      NREC   = IQ(LCDIR+KQUOTA)
      IF(NRECS.GT.0) NREC = NRECS
      NWKEY  = IQ(LCDIR+KNWKEY)
      KTAGS  = KKDES+(NWKEY-1)/10+1
      LB     = IQ(LCDIR+KLB)
      LRECL  = IQ(LCDIR+LB+1)
      IDATEC = IQ(LCDIR+KDATEC)
      IDATEM = IQ(LCDIR+KDATEM)
 
      DO 10 I=1,NWKEY
         CALL ZITOH(IQ(LCDIR+KTAGS+2*I-2),IHTAG,2)
         CALL UHTOC(IHTAG,4,CHTAG(I),8)
         IKDES=(I-1)/10
         IKBIT1=3*I-30*IKDES-2
         IFORM=JBYT(IQ(LCDIR+KKDES+IKDES),IKBIT1,3)
         IF(IFORM.EQ.3)THEN
            CHFORM(I:I)='H'
         ELSEIF(IFORM.EQ.4) THEN
            CHFORM(I:I)='A'
         ELSEIF(IFORM.EQ.1) THEN
            CHFORM(I:I)='B'
         ELSE
            CHFORM(I:I)='I'
         ENDIF
   10 CONTINUE
*
*     Open output file
*
      IF(IRECL.NE.0) LRECL = IRECL
 
      IF(IOPTN.NE.0) THEN
         CHOPM = ' '
         CHOPO = 'N'
      ELSEIF(IOPTX.NE.0) THEN
         CHOPM = 'X'
         CHOPO = 'NX'
      ENDIF
 
      LCHOPO = LENOCC(CHOPO)
 
      IF(IOPTC.NE.0) THEN
         LCHOPO = LCHOPO + 1
         CHOPO(LCHOPO:LCHOPO) = 'P'
      ENDIF
*
*     Create output file
*
      IF(IDEBXZ.GE.0) PRINT *,'XZRZCP. opening output file, LRECL = ',
     +   LRECL
      CALL RZOPEN(LUNXZO,'RZOUT',CHOUT(1:LCHOUT),CHOPO(1:LCHOPO),
     +            LRECL,IRC)
      IF(IRC.NE.0) THEN
         IF(IDEBXZ.GE.0) PRINT *,'XZRZCP. cannot open output file'
         GOTO 30
      ENDIF
 
      NQUO = MIN(NREC,65000)
 
      IF(IDEBXZ.GE.0) PRINT *,'XZRZCP. making output file, NQUO = ',
     +   NQUO
 
      CALL RZMAKE(LUNXZO,'RZOUT',NWKEY,CHFORM,CHTAG,NQUO,CHOPM)
      IF(IQUEST(1).NE.0) THEN
         IRC = IQUEST(1)
         IF(IDEBXZ.GE.0) PRINT *,'XZRZCP. cannot make output file'
         CLOSE(LUNXZO)
         GOTO 30
      ENDIF
*
*     Copy the data
*
      CALL RZCOPY('//RZIN',KEYI,ICYCLE,KEYO,'CKT')
 
   20 CONTINUE
      CALL RZCLOS('RZOUT',' ')
 
   30 CONTINUE
      CALL RZCLOS('RZIN',' ')
 
   40 CONTINUE
#endif
 
      END
