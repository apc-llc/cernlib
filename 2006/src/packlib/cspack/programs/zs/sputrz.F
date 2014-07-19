*
* $Id: sputrz.F,v 1.2 1996/04/11 14:54:17 cernlib Exp $
*
* $Log: sputrz.F,v $
* Revision 1.2  1996/04/11 14:54:17  cernlib
* Zserv/pawserv used to be build from two patches; these were put both into this
* directory.
* The #includes in all files copied from the other directory had to updated.
*
* Revision 1.1.1.1  1996/03/08 15:44:20  mclareni
* Cspack
*
*
#include "cspack/pilot.h"
      SUBROUTINE SPUTRZ(CHF)
*
*          Transfer a RZ file from the client
*
#include "hbook/hcmail.inc"
#include "cspack/czunit.inc"
      COMMON/QUEST/IQUEST(100)
#include "cspack/pawc.inc"
      DIMENSION IQ(2),Q(2),LQ(8000)
      EQUIVALENCE (LQ(1),LMAIN),(IQ(1),LQ(9)),(Q(1),IQ(1))
#include "zebra/rzclun.inc"
      COMMON /RZCL/  LTOP,LRZ0,LCDIR,LRIN,LROUT,LFREE,LUSED,LPURG
     +,              LTEMP,LCORD,LFROM
      PARAMETER (KUP=5,KPW1=7,KNCH=9,KDATEC=10,KDATEM=11,KQUOTA=12,
     +           KRUSED=13,KWUSED=14,KMEGA=15,KIRIN=17,KIROUT=18,
     +           KRLOUT=19,KIP1=20,KNFREE=22,KNSD=23,KLD=24,KLB=25,
     +           KLS=26,KLK=27,KLF=28,KLC=29,KLE=30,KNKEYS=31,
     +           KNWKEY=32,KKDES=33,KNSIZE=253,KEX=6,KNMAX=100)
      CHARACTER*(*) CHF
      CHARACTER*8  CHTAG(100)
      CHARACTER*8  CHRZ,CHOPE
      CHARACTER*80 LOCAL
      CHARACTER*90 CHFORM
*
*_______________________________________
*
*           Get file parameters from client
*
      LEND = LENOCC(CHF)
      LBLA = INDEX(CHF(1:LEND),' ')
      IF(LBLA.EQ.0) THEN
         LCHF = LEND
         CHRZ = ' '
      ELSE
         CHRZ = CHF(LBLA+1:LEND)
         LCHF = LBLA - 1
      ENDIF
 
      LOCAL=CHF(1:LCHF)
 
      CALL CZGETA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 95
      READ(CHMAIL,1000,ERR=95)NWKEY,NREC,LRECL,IDATEC,IDATEM
      NREC = MIN(NREC,65000)
*
*     Exchange mode transfer - will be performed by XZPUTD from client
*
      IF(NWKEY.EQ.-1) GOTO 99
      CALL CZGETA(CHFORM,ISTAT)
      IF(ISTAT.NE.0)GO TO 95
 1000 FORMAT(5I10)
      IF(NWKEY.EQ.0)GO TO 95
      DO 10 I=1,NWKEY
         CALL CZGETA(CHMAIL,ISTAT)
         IF(ISTAT.NE.0)GO TO 95
         CHTAG(I)=CHMAIL
  10  CONTINUE
*
*           Create local file
*
      CHMAIL='OK'
 
      IF(INDEX(CHRZ,'L').NE.0) THEN
         CHOPE = 'LN'
      ELSE
         CHOPE = 'N'
      ENDIF
 
      IF(INDEX(CHRZ,'C').NE.0) THEN
         LCHOPE = LENOCC(CHOPE) + 1
         CHOPE(LCHOPE:LCHOPE) = 'P'
      ENDIF
 
      CALL RZOPEN(LUNXZO,'RZ',LOCAL,CHOPE,LRECL,ISTAT)
 
      IF(ISTAT.NE.0) GOTO 95
      IF(INDEX(CHRZ,'X').NE.0) THEN
         CALL RZMAKE(LUNXZO,'RZ',NWKEY,CHFORM,CHTAG,NREC,'X')
      ELSE
         CALL RZMAKE(LUNXZO,'RZ',NWKEY,CHFORM,CHTAG,NREC,' ')
      ENDIF
      IF(IQUEST(1).NE.0)GO TO 95
      IQ(LTOP+KDATEC)=IDATEC
      IQ(LTOP+KDATEM)=IDATEM
*
*           Inform client if file opened on local node
*
  50  CALL CZPUTA(CHMAIL,ISTAT)
      IF(ISTAT.NE.0)GO TO 99
      IF(CHMAIL.EQ.'KO')GO TO 99
*
*           Transfer data in RZ file
*
      CALL RZFRFZ(LUNFZI,' ')
*
*     Reset modification date
*
      IQ(LTOP+KDATEC)=IDATEC
      IQ(LTOP+KDATEM)=IDATEM
      ISAVE = 2
      CALL RZSAVE
      CALL RZEND('RZ')
      CLOSE(LUNXZO)
      GO TO 99
*
  95  PRINT *,' Cannot open local file'
      CHMAIL='KO'
      GO TO 50
*
  99  END
 
