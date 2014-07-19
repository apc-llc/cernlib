*
* $Id: rzt.F,v 1.1.1.1 1996/03/06 10:47:04 mclareni Exp $
*
* $Log: rzt.F,v $
* Revision 1.1.1.1  1996/03/06 10:47:04  mclareni
* Zebra
*
*
#if defined(CERNLIB_INTER)
#include "test_include/pilot.h"
      SUBROUTINE RZT(NCOM,NPAR)
*
********************************************************************************
*                                                                              *
*        Dispatching routine for the RZ interactive test program               *
*             All RZ routines can be called interactively                      *
*                                                                              *
*  Author  : R.Brun                                                            *
*  Written : 02.04.86                                                          *
*  Last mod: 22.04.86                                                          *
*                                                                              *
********************************************************************************
*
      COMMON/RZMEM/IRZMEM(400000)
C
      COMMON/QUEST/IQUEST(100)
C
      COMMON/CRZT/IXSTOR,IXDIV,IFENCE(2),LEV,LEVIN,BLVECT(20000)
      DIMENSION LQ(999),IQ(999),Q(999)
      EQUIVALENCE (IQ(1),Q(1),LQ(9)),(LQ(1),LEV)
C
C
      COMMON/RZDIR/NLCDIR,NLNDIR,NLPAT,CHCDIR(20),CHNDIR(20),CHPAT(20)
      CHARACTER*64 CHNDIR,CHCDIR,CHPAT
C
      CHARACTER*64 CHTOP,CHDIR,CHPATH,CHOPT,CHFORM
      CHARACTER*20 CHPROM
      CHARACTER*8  CHLOCK, CHTAG(10),CHPASS
      DIMENSION KEY(10),KEYN(10),IHPROM(10)
      COMMON/RZUSER/NVIN,NVOUT,IV(10000)
*
*     --------------------------------------------------------------------
*
      CALL ZCBRON
C
      GO TO (10,20,30,40,50,60,70,80,90,100,110,120,130,140,
     +       150,160,170,180,190,200,210,220,40,240,250,260,
     +       270,280,290,300,310,320,330,340),NCOM
C
C          RZMAKE
C
  10  CONTINUE
      LUN=1
      CHTOP='RZTEST'
      NREC=1000
      CHOPT=' '
      NWKEY=1
      CALL ZCGETI(' Give logical unit  $',LUN)
      CALL ZCGETC(' Give CHTOP $',CHTOP,NCH)
      CALL ZCGETI(' Give NWKEY $',NWKEY)
      IF(NWKEY.LE.0)GO TO 10
      IF(NWKEY.GT.5)GO TO 10
      CALL ZCGETC(' Give CHFORM $',CHFORM,NCH)
      DO 11 I=1,NWKEY
         NCH=-8
         CALL ZCGETC(' Give CHTAG $',CHTAG(I),NCH)
  11  CONTINUE
      IF(NPAR.GT.NWKEY+4)CALL ZCGETI(' Give NREC $',NREC)
      IF(NPAR.GT.NWKEY+5)CALL ZCGETC(' Give CHOPT $',CHOPT,NCH)
      IF(CHOPT.NE.'M')THEN
         CALL RZMAKE(LUN,CHTOP,NWKEY,CHFORM,CHTAG,NREC,CHOPT)
      ELSE
         IRZMEM(1)=LUN
         CALL RZMAKE(IRZMEM,CHTOP,NWKEY,CHFORM,CHTAG,NREC,CHOPT)
      ENDIF
      GO TO 99
C
C          RZFILE
C
  20  CONTINUE
      LUN=1
      CHTOP='RZTEST'
      CHOPT=' '
      IF(NPAR.GT.0)CALL ZCGETI(' Give logical unit  $',LUN)
      IF(NPAR.GT.1)CALL ZCGETC(' Give CHTOP $',CHTOP,NCH)
      IF(NPAR.GT.2)CALL ZCGETC(' Give CHOPT $',CHOPT,NCH)
      IMEM=0
      DO 25 I=1,NCH
         IF(CHOPT(I:I).EQ.'M')IMEM=1
  25  CONTINUE
      IF(IMEM.NE.0)THEN
         CALL RZFILE(IRZMEM,CHTOP,CHOPT)
      ELSE
         CALL RZFILE(LUN,CHTOP,CHOPT)
      ENDIF
      GO TO 99
C
C          RZNDIR
C
  30  CONTINUE
      IF(NPAR.EQ.0)THEN
         CALL RZNDIR(' ','P')
      ELSE
         CALL ZCGETC(' Give CHPATH $',CHPATH,NCH)
         CALL RZNDIR(CHPATH,' ')
      ENDIF
      GO TO 99
C
C          RZCDIR
C
  40  CONTINUE
      IF(NPAR.EQ.0)THEN
         CALL RZCDIR(' ','P')
      ELSE
         CHOPT=' '
         CALL ZCGETC(' Give CHPATH $',CHPATH,NCH)
         NCH=-4
         IF(NPAR.GT.1)CALL ZCGETC('$',CHOPT,NCH)
         CALL RZCDIR(CHPATH,CHOPT)
      ENDIF
      GO TO 99
C
C          RZMDIR
C
  50  CONTINUE
      CALL ZCGETC(' Give CHDIR $',CHDIR,NCH)
      CALL ZCGETI(' Give NWKEY $',NWKEY)
      IF(NWKEY.LE.0)GO TO 50
      IF(NWKEY.GT.5)GO TO 50
      CALL ZCGETC(' Give CHFORM $',CHFORM,NCH)
      DO 51 I=1,NWKEY
         CALL ZCGETC(' Give CHTAG $',CHTAG(I),NCH)
  51  CONTINUE
      CALL RZMDIR(CHDIR,NWKEY,CHFORM,CHTAG)
      GO TO 99
C
C          RZDELT
C
  60  CONTINUE
      CALL ZCGETC(' Give CHDIR $',CHDIR,NCH)
      CALL RZDELT(CHDIR)
      GO TO 99
C
C          RZQUOT
C
  70  CONTINUE
      CALL ZCGETI(' Give NQUOTA $',NQUOTA)
      CALL RZQUOT(NQUOTA)
      GO TO 99
C
C          RZLOCK
C
  80  CONTINUE
      CALL ZCGETC(' Give CHLOCK $',CHLOCK,NCH)
      CALL RZLOCK(CHLOCK)
      GO TO 99
C
C          RZFREE
C
  90  CONTINUE
      CALL ZCGETC(' Give CHLOCK $',CHLOCK,NCH)
      CALL RZFREE(CHLOCK)
      GO TO 99
C
C          RZLDIR
C
  100 CONTINUE
      CHOPT=' '
      CHPATH=' '
      IF(NPAR.GT.0)CALL ZCGETC(' Give CHPATH $',CHPATH,NCH)
      IF(NPAR.GT.1)CALL ZCGETC(' Give CHOPT $',CHOPT,NCH)
      CALL RZLDIR(CHPATH,CHOPT)
      GO TO 99
C
C          RZPURG
C
  110 CONTINUE
      NKEEP=1
      IF(NPAR.GT.0)CALL ZCGETI(' Give NKEEP $',NKEEP)
      CALL RZPURG(NKEEP)
      GO TO 99
C
C          RZDELK
C
  120 CONTINUE
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEY)
      ICYCLE=9999
      IF(NPAR.GT.NWKEY)CALL ZCGETI('$',ICYCLE)
      CHOPT=' '
      IF(NPAR.GT.NWKEY+1)CALL ZCGETC(' Give CHOPT $ ',CHOPT,NCH)
      CALL RZDELK(KEY,ICYCLE,CHOPT)
      GO TO 99
C
C          RZCOPY (COPY)
C
  130 CONTINUE
      CHPATH=' '
      CHOPT=' '
      CALL ZCGETC(' Give CHPATH $',CHPATH,NCH)
      CALL RZPATH(CHPATH)
      CALL RZFDIR('TEST',LT,LFROM)
      IF(LFROM.EQ.0)GO TO 99
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEY)
      CALL UCOPY(KEY,KEYN,NWKEY)
      ICYCLE=9999
      IF(NPAR.GT.NWKEY+1)CALL ZCGETI('$',ICYCLE)
      IF(NPAR.GE.2*NWKEY+2)THEN
         CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEYN)
      ENDIF
      IF(NPAR.GT.2*NWKEY+2)CALL ZCGETA('$',CHOPT,NCH)
      CALL RZCOPY(CHPATH,KEY,ICYCLE,KEYN,CHOPT)
      GO TO 99
C
C          RZCOPY (CPT)
C
  140 CONTINUE
      CHPATH=' '
      CHOPT='TC'
      CALL ZCGETC(' Give CHPATH $ ',CHPATH,NCH)
      IF(NPAR.GT.1) CALL ZCGETC('$',CHOPT,NCH)
      CALL RZCOPY(CHPATH,0,0,0,CHOPT)
      GO TO 99
C
C          RZEND
C
  150 CONTINUE
      CALL ZCGETC(' Give CHDIR $',CHDIR,NCH)
      CALL RZEND (CHDIR)
      GO TO 99
C
C          RZPATH
C
 160  CONTINUE
      CALL ZCGETC(' Give path $ ',CHPATH,NCH)
      CALL RZPATH(CHPATH)
      PRINT 1000,NLPAT,CHPATH,(CHPAT(I),I=1,NLPAT)
 1000 FORMAT(' NLPAT=',I3,' for Pathname, ',A,/,
     +(A))
      GO TO 99
C
C          RZLOGL
C
 170  CONTINUE
      LUN=1
      LOGLEV=0
      CALL ZCGETI(' Give logical unit $',LUN)
      CALL ZCGETI(' Give LOGLEV $',LOGLEV)
      CALL RZLOGL(LUN,LOGLEV)
      GO TO 99
C
C          RZOUT
C
 180  CONTINUE
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEY)
      CHOPT=' '
      IF(NPAR.GT.NWKEY)CALL ZCGETC(' Give CHOPT $ ',CHOPT,NCH)
      CALL RZOUT(IXSTOR,LEV,KEY,ICYCLE,CHOPT)
      GO TO 99
C
C          RZIN
C
 190  CONTINUE
      IF(LEVIN.NE.0)CALL MZDROP(IXSTOR,LEVIN,' ')
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEY)
      ICYCLE=9999
      IF(NPAR.GT.NWKEY)CALL ZCGETI(' Give ICYCLE $ ',ICYCLE)
      CHOPT=' '
      IF(NPAR.GT.NWKEY+1)CALL ZCGETC(' Give CHOPT $ ',CHOPT,NCH)
      CALL RZIN(IXSTOR,LEVIN,1,KEY,ICYCLE,CHOPT)
      IF(LEVIN.NE.0)CALL UCTOH('LEVI',IQ(LEVIN-4),4,4)
      GO TO 99
C
C          RZSAVE
C
 200  CALL RZSAVE
      GO TO 99
C
C          PQUEST
C
 210  IQ1=1
      IQ2=20
      IF(NPAR.GT.0)THEN
         CALL ZCGETI('$',IQ1)
         IQ2=IQ1
      ENDIF
      IF(NPAR.GT.1)CALL ZCGETI('$',IQ2)
      DO 211 I=IQ1,IQ2
         PRINT 212,I,IQUEST(I),IQUEST(I)
 211  CONTINUE
 212  FORMAT(' IQUEST(',I2,')=',I10,2X,Z8)
      GO TO 99
C
C          PASSWORD
C
 220  IF(NPAR.EQ.0)THEN
         CALL RZPASS(' ',' ')
      ELSE
         NCH=-8
         IF(NPAR.GT.0)CALL ZCGETC('$',CHPASS,NCH)
         CHOPT=' '
         IF(NPAR.GT.1)CALL ZCGETC('$',CHOPT,NCH)
         CALL RZPASS(CHPASS,CHOPT)
      ENDIF
      GO TO 99
C
C          RZSTAT
C
 240  CONTINUE
      NLEVEL=0
      CHOPT=' '
      CHPATH=' '
      NCH=-16
      IF(NPAR.GT.0)CALL ZCGETC(' Give CHPATH $',CHPATH,NCH)
      IF(NPAR.GT.1)CALL ZCGETI(' Give NLEVEL $',NLEVEL)
      IF(NPAR.GT.2)CALL ZCGETC(' Give CHOPT $',CHOPT,NCH)
      CALL RZSTAT(CHPATH,NLEVEL,CHOPT)
      GO TO 99
C
C          LLOK
C
 250  CONTINUE
      CALL RZLLOK
      GO TO 99
C
C          VOUT
C
 260  CONTINUE
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEY)
      CHOPT=' '
      IF(NPAR.GT.NWKEY)CALL ZCGETC(' Give CHOPT $ ',CHOPT,NCH)
      CALL RZVOUT(IV,NVOUT,KEY,ICYCLE,CHOPT)
      GO TO 99
C
C          VIN
C
 270  CONTINUE
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give ',NWKEY,CHFORM,CHTAG,KEY)
      ICYCLE=9999
      IF(NPAR.GT.NWKEY)CALL ZCGETI(' Give ICYCLE $ ',ICYCLE)
      CHOPT=' '
      IF(NPAR.GT.NWKEY+1)CALL ZCGETC(' Give CHOPT $ ',CHOPT,NCH)
      CALL RZVIN(IV,10000,NVIN,KEY,ICYCLE,CHOPT)
      GO TO 99
C
C           VFILL
C
 280  CONTINUE
      NVOUT=100
      CALL ZCGETI(' Give number of words $ ',NVOUT)
      IF(NVOUT.GT.10000)NVOUT=10000
      DO 281 I=1,NVOUT
         IV(I)=I
 281  CONTINUE
      NVIN=NVOUT
      GO TO 99
C
C           PVECT
C
 290  CONTINUE
      DO 291 I=1,NVIN,10
         I2=I+9
         IF(I2.GT.NVIN)I2=NVIN
         PRINT 292,I,(IV(J),J=I,I2)
 291  CONTINUE
 292  FORMAT(I10,2X,10I10)
      GO TO 99
C
C          TOFZ
C
 300  CONTINUE
      CHOPT =' '
      LUNFZ =3
      IF(NPAR.GT.0)CALL ZCGETC('$',CHOPT ,NCH)
      CALL RZTOFZ(LUNFZ,CHOPT)
      GO TO 99
C
C          FRFZ
C
 310  CONTINUE
      CHOPT =' '
      LUNFZ =3
      IF(NPAR.GT.0)CALL ZCGETC('$',CHOPT ,NCH)
      CALL RZFRFZ(LUNFZ,CHOPT)
      GO TO 99
C
C          FZOPEN
C
 320  CONTINUE
      CHOPT='O'
      IF(NPAR.GT.0)CALL ZCGETC('$',CHOPT,NCH)
      CALL FZOPEN(3,0,CHOPT)
      GO TO 99
C
C          FZEND
C
 330  CONTINUE
      CALL FZENDT(3,'EI')
      GO TO 99
C
C          RENK
C
 340  CONTINUE
      CALL RZKEYD(NWKEY,CHFORM,CHTAG)
      CALL RZCGET('Give old ',NWKEY,CHFORM,CHTAG,KEY)
      CALL RZCGET('Give new ',NWKEY,CHFORM,CHTAG,KEYN)
C
      CALL RZRENK(KEY,KEYN)
      GO TO 99
C
  99  CALL ZCBROF
      END
#endif
