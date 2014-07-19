*
* $Id: sgetrz.F,v 1.2 1996/04/11 14:54:13 cernlib Exp $
*
* $Log: sgetrz.F,v $
* Revision 1.2  1996/04/11 14:54:13  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SGETRZ(CHF)
*
*          Transfer a RZ file to the client
*
#include "hbook/hcmail.inc"
#include "cspack/czsock.inc"
#include "cspack/czunit.inc"
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
C
#include "zebra/rzclun.inc"
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
      COMMON/QUEST/IQUEST(100)
      CHARACTER*12  CHDATE
      CHARACTER*(*) CHF
      CHARACTER*8  CHTAG(100)
      CHARACTER*80 CHFILE
      CHARACTER*90 CHFORM
      CHARACTER*4  CHOPT,CHOPE,CHOPR
      DIMENSION IHTAG(2)
      DIMENSION ITEST(50)
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
*
*_______________________________________
*
*
*        Open RZ file
*
      IQUEST(1)=0
      IOPEN=0
*
*     Get file name and options
*
      LF = LENOCC(CHF)
      LB = INDEX(CHF(1:LF),' ')
      IF(LB.NE.0) THEN
         CHFILE=CHF(1:LB-1)
         CHOPT = CHF(LB+1:LF)
      ELSE
         CHFILE=CHF(1:LF)
         CHOPT = ' '
      ENDIF
      NREC   = 0
      NWKEY  = 0
      CHFORM = ' '
      LRECL  = 128
      LRECL  = 0
      IDATEC = 0
      IDATEM = 0
      CHOPE  = ' '
      CHOPR  = ' '
      LCHOPE = 1
 
      IF(INDEX(CHOPT,'C').NE.0) THEN
         CHOPE  = 'P'
         LCHOPE = LCHOPE + 1
      ENDIF
*
*     Existing file is in exchange mode (force)
*
      IF(INDEX(CHOPT,'E').NE.0) THEN
         CHOPE(LCHOPE:LCHOPE)  = 'X'
         CHOPR = 'X'
      ENDIF
 
      CALL RZOPEN(LUNXZI,'RZ',CHFILE,CHOPE,LRECL,ISTAT)
*
*     Cannot open file - send NWKEY = 0 to client to indicate trouble
*
      IF(ISTAT.NE.0) THEN
         CHMAIL=' '
         WRITE(CHMAIL,1000)NWKEY,NREC,LRECL,IDATEC,IDATEM
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         CALL CZPUTA(CHFORM,ISTAT)
         GO TO 90
      ENDIF
 
      CALL RZFILE(LUNXZI,'RZ',CHOPR)
      IF(IQUEST(1).NE.0)THEN
*
*     Cannot issue RZFILE - send NWKEY = 0 to client to indicate trouble
*
         CLOSE(LUNXZI)
         CHMAIL=' '
         WRITE(CHMAIL,1000)NWKEY,NREC,LRECL,IDATEC,IDATEM
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         CALL CZPUTA(CHFORM,ISTAT)
         GO TO 90
      ENDIF
 
      IOPEN  = 1
      NREC   = IQ(LCDIR+KQUOTA)
      NWKEY  = IQ(LCDIR+KNWKEY)
      KTAGS  = KKDES+(NWKEY-1)/10+1
      LB     = IQ(LCDIR+KLB)
      LRECL  = IQ(LCDIR+LB+1)
      IDATEC = IQ(LCDIR+KDATEC)
      IDATEM = IQ(LCDIR+KDATEM)
      CHMAIL=' '
*
*     Test if this file is in exchange mode
*
*     CALL RZIODO(LUNXZI,50,2,ITEST,1)
*SELF,IF=VAX,DECS.
*     IMODEX = IOR(ITEST(6),0)
*SELF,IF=-VAX,IF=-DECS.
*     IMODEX = ITEST(6)
*SELF.
*
#if defined(CERNLIB_UNIX)
      IF(INDEX(CHOPT,'X').NE.0) THEN
#endif
#if !defined(CERNLIB_UNIX)
      IF((IMODEX.NE.0).AND.(INDEX(CHOPT,'N').EQ.0)) THEN
#endif
*
*     Set NWKEY to indicate that this is an exchange mode transfer
*
         NWKEY = -1
         WRITE(CHMAIL,1000)NWKEY,NREC,LRECL,IDATEC,IDATEM
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
*
*     Now transfer the file - the transfer will be driven by the
*     client using XZGETD.
*
      ELSE
 
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
   10    CONTINUE
   20    CHMAIL=' '
         WRITE(CHMAIL,1000)NWKEY,NREC,LRECL,IDATEC,IDATEM
 1000 FORMAT(5I10)
         CALL CZPUTA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         CALL CZPUTA(CHFORM,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         IF(NWKEY.EQ.0)GO TO 90
         DO 30 I=1,NWKEY
            CHMAIL=CHTAG(I)
            CALL CZPUTA(CHMAIL,ISTAT)
            IF(ISTAT.NE.0)GO TO 90
   30    CONTINUE
*
*          Verify that RZ file has been opened by client
*
         CALL CZGETA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 90
         IF(CHMAIL(1:2).NE.'OK')GO TO 90
*
*          Now transfer the file
*
         CALL RZTOFZ(LUNFZO,'C')
 
      ENDIF
*
   90 IF(IOPEN.EQ.0)GO TO 99
      CALL RZEND('RZ')
      CLOSE(LUNXZI)
*
   99 END
