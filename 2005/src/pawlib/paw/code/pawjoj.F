*
* $Id: pawjoj.F,v 1.2 1996/09/11 14:59:34 couet Exp $
*
* $Log: pawjoj.F,v $
* Revision 1.2  1996/09/11 14:59:34  couet
* - Hgetnt and Hgetn2 (old qp routines) are now replaced by hntld
*
* Revision 1.1.1.1  1996/03/01 11:38:43  mclareni
* Paw
*
*
#include "paw/pilot.h"
*CMZ :  2.05/17 02/09/94  18.31.05  by  Rene Brun
*-- Author :    Joseph Le Foll 27/03/90
      SUBROUTINE PAWJOJ(IVARS,NVARS)
*
*        generates the PRINCIPAL COMPONENTS calculation routine
*
#include "hbook/hcbook.inc"
#include "paw/pawcom.inc"
#include "paw/pclint.inc"
#include "paw/pcchar.inc"
#include "paw/quest.inc"
#include "paw/pcslas.inc"
*
      DIMENSION A(4)
      CHARACTER*1 IDIGIT(4),ISEPAR(16)
      DIMENSION IVARS(1)
      DIMENSION IOPT(3)
      EQUIVALENCE (IOPTE,IOPT(1)),(IOPTP,IOPT(2)),(IOPTT,IOPT(3))
      CHARACTER*11 CHTAGS(6)
      CHARACTER*9 CHVAR(6)
      CHARACTER*8 CHVART
      CHARACTER*8 CHEQVA
      CHARACTER*1 CHS,QUOTE
      CHARACTER*1 ICOMMA,ISLASH
      SAVE QUOTE
      SAVE ICOMMA
      SAVE ISLASH
      DATA QUOTE/''''/
      DATA NSUIT/72/
      DATA ICOMMA,ISLASH/',','/'/
*
      NCH=6
#if defined(CERNLIB_UNIX)
      CHFILE='XTOXSI.F'
#endif
#if defined(CERNLIB_APOLLO)
      CHFILE='XTOXSI.FTN'
#endif
#if defined(CERNLIB_VAX)
      CHFILE='XTOXSI.FOR'
#endif
#if defined(CERNLIB_IBM)
      CHFILE='XTOXSI.FORTRAN'
#endif
      CHTEMP='XTOXSI(XDUMMY)'
      NCHF=NCH
*
*        get parameters for NTUPLE 'CHID'
*
      CALL HNTLD(CHID)
      IF(IQUEST(1).LT.0)GO TO 999
      IF(ID.NE.0.AND.LCID.LE.0)GO TO 999
*
*        number of data cards needed to transfer array 'EVEC'
*
      NVAR=IQ(LCID+2)
      NV =NVARS*NVARS
      NAR1=(NV+NSUIT-1)/NSUIT
      NAR2=(NVARS+3)/4
      NIVAR=(NVAR+15)/16
*
      KEVEC=MIN0(NSUIT,NV)
*
      CALL PALUNF(60,3,LUN)
      IF(LUN.EQ.0)GO TO 999
#if defined(CERNLIB_APOLLO)||defined(CERNLIB_UNIX)
      CALL CUTOL(CHFILE)
#endif
      CALL KUOPEN(LUN,CHFILE,'UNKNOWN',ISTAT)
      IF(ISTAT.NE.0)GO TO 999
*
      NCH=LENOCC(CHTEMP)
      WRITE(LUN,10000)CHTEMP(1:NCH)
*
      ITAG1=IQ(LCID+10)
      KLOOP=3
*
      DO 120 K=1,KLOOP
         DO 30 I=1,NVAR,6
            IF(I+5.GT.NVAR)THEN
               JMAX=MOD(NVAR,6)
            ELSE
               JMAX=6
            ENDIF
            DO 20 J=1,JMAX
               IVAR=I+J-1
               J1=2*I+2*J-4
               CALL UHTOC(IQ(LCID+ITAG1+J1),4,CHVART,8)
               IF(CHVART.EQ.' ')THEN
                  IF(IVAR.LT.10)THEN
                     WRITE(CHVART,10700)IVAR
                  ELSEIF(IVAR.LT.100)THEN
                     WRITE(CHVART,10800)IVAR
                  ELSE
                     WRITE(CHVART,10900)IVAR
                  ENDIF
               ENDIF
               IF(I.EQ.1.AND.J.EQ.1) CHEQVA=CHVART
               IF(K.EQ.3)THEN
                  IF(IVAR.LT.NVAR)THEN
                     CHTAGS(J)=QUOTE//CHVART//QUOTE//','
                  ELSE
                     CHTAGS(J)=QUOTE//CHVART//QUOTE//'/'
                  ENDIF
               ENDIF
               IF(IVAR.LT.NVAR)THEN
                  CHVAR(J)=CHVART//','
               ELSE
                  CHVAR(J)=CHVART
               ENDIF
               DO 10 L=1,8
                  CHS=CHVAR(J)(L:L)
                  IF(    CHS.EQ.'('.OR.CHS.EQ.')'
     +               .OR.CHS.EQ.'/'.OR.CHS.EQ.BSLASH
     +               .OR.CHS.EQ.'+'.OR.CHS.EQ.'-'
     +               .OR.CHS.EQ.'*'.OR.CHS.EQ.'.')THEN
                     IF(L.EQ.8)THEN
                        CHVAR(J)(L:L)=' '
                     ELSE
                        CHVAR(J)(L:L)='x'
                     ENDIF
                  ENDIF
   10          CONTINUE
   20       CONTINUE
*
            IF(K.NE.3)THEN
               WRITE(LUN,10100)(CHVAR(L),L=1,JMAX)
            ELSE
               WRITE(LUN,10100)(CHTAGS(L),L=1,JMAX)
            ENDIF
   30    CONTINUE
         IF(K.EQ.1) WRITE(LUN,10200)
         IF(K.EQ.2.AND.KLOOP.EQ.3)THEN
            WRITE(LUN,10300)NVAR,NVAR,NVARS,NVAR
*
            WRITE (LUN,11000)NVARS,NVARS,NVARS,NV
            DO 40 I=1,NAR1
               IF(I.EQ.NAR1) KEVEC=NV-(NAR1-1)*NSUIT
               J=NSUIT*(I-1)+1
               IF(I.LT.10) WRITE(LUN,11100)I,KEVEC,I,J
               IF(I.GE.10.AND.I.LT.100) WRITE(LUN,11200)I,KEVEC,I,J
               IF(I.GT.100) WRITE(LUN,11300)I,KEVEC,I,J
   40       CONTINUE
*
            WRITE(LUN,10400) CHEQVA,NVAR,NVARS
*
         ENDIF
         IF(K.EQ.3)THEN
*
*             punch array of used variables
*
            WRITE(LUN,11800)
*
            DO 50  I=1,16
               ISEPAR(I)=ICOMMA
   50       CONTINUE
*
            DO 60  I=1,NIVAR
               I2=16*I
               I1=I2-15
               IF(I2.GT.NVARS)I2=NVARS
               N=I2-I1+1
               IF(I.EQ.NIVAR)ISEPAR(N)=ISLASH
               WRITE(LUN,11500)(IVARS(I1-1+KA),ISEPAR(KA) ,KA=1,N)
   60       CONTINUE
*
*             punch array of mean-values
*
            WRITE(LUN,11600)
*
            DO 70  I=1,4
               IDIGIT(I)=ICOMMA
   70       CONTINUE
*
            DO 80 I=1,NAR2
               I2=4*I
               I1=I2-3
               IF(I2.GT.NVARS)I2=NVARS
               N=I2-I1+1
               CALL UCOPY(XA( I1),A,N)
               IF(I.EQ.NAR2)IDIGIT(N)=ISLASH
               WRITE(LUN,11400)( A(KA),IDIGIT(KA) ,KA=1,N)
   80       CONTINUE
C
C             punch array of standard deviations
C
            WRITE(LUN,11700)
C
            IDIGIT(N)=ICOMMA
            DO 90 I=1,NAR2
               I2=4*I
               I1=I2-3
               IF(I2.GT.NVARS)I2=NVARS
               N=I2-I1+1
               CALL UCOPY(S( I1),A,N)
               IF(I.EQ.NAR2)IDIGIT(N)=ISLASH
               WRITE(LUN,11400)( A(KA),IDIGIT(KA) ,KA=1,N)
   90       CONTINUE
C
C             punch array of eigenvectors
C
            NAR2=NSUIT/4
            DO 110 I=1,NAR1
               IF(I.LT.10) WRITE(LUN,12000)I
               IF(I.GE.10.AND.I.LT.100) WRITE(LUN,12100)I
               IF(I.GT.100) WRITE(LUN,12200)I
               IDIGIT(N)=ICOMMA
               DO 100 J=1,NAR2
                  I1=(I-1)*NSUIT+4*(J-1)+1
                  I2=I1+3
                  IF(I2.GT.NV)I2=NV
                  N=I2-I1+1
                  CALL UCOPY(EIGVEC(I1),A,N)
                  IF(I2.EQ.NV)IDIGIT(N)=ISLASH
                  IF(J.EQ.NAR2)IDIGIT(N)=ISLASH
                  WRITE(LUN,11400)( A(KA),IDIGIT(KA) ,KA=1,N)
                  IF(I2.EQ.NV)GO TO 110
  100          CONTINUE
  110       CONTINUE
         ENDIF
  120 CONTINUE
*
*             punch code of linear transformation
*
      WRITE(LUN,11900)
*
      WRITE(LUN,10600)CHTEMP(1:NCHF)
*
      CALL PACLOS(LUN)
*
10000 FORMAT(6X,'REAL FUNCTION ',A,/,6X,'REAL')
10100 FORMAT(5X,'+',6(A))
10200 FORMAT(6X,'COMMON/PAWIDN/IDNEVT,VIDN1,VIDN2,VIDN3,VIDN(10),')
10300 FORMAT(6X,'DIMENSION XDUMMY(',I3,'),XVAR(',I3,')',/,
     +6X,'DIMENSION IVARS(',I3,')',/,
     +6X,'CHARACTER*8 CHTAGS(',I3,')')
10400 FORMAT(6X,'EQUIVALENCE (',A,',XVAR(1))',/,
     +       6X,'DATA NVAR/',I3,'/',/,
     +       6X,'DATA NVARS/',I3,'/',/,
     +       6X,'DATA CHTAGS/')
10500 FORMAT('*',/,6X,A,'=1.',/,
     +6X,'PRINT 1000,IDNEVT',/,
     +6X,'DO 10 I=1,',I3,/,
     +9X,'PRINT 2000,I,CHTAGS(I),XDUMMY(I)',/,
     +'  10  CONTINUE',/,'*',/,
     +' 1000 FORMAT(8H IDNEVT=,I5)',/,
     +' 2000 FORMAT(5X,I3,5X,A,1H=,G14.7)',/,
     +6X,'END')
10600 FORMAT('*',/,6X,A,'=XSI(K)',/,6X,'END')
10700 FORMAT('V_',I1,5X)
10800 FORMAT('V_',I2,4X)
10900 FORMAT('V_',I3,3X)
11000 FORMAT(                                           1H*/
     +6X,'DIMENSION XSI(',I3,'),XA(',I3,'),SQ(',I3,'),EVEC(',I5,')')
11100 FORMAT(6X,'DIMENSION C',I1,'(',I3,')',/
     +6X,'EQUIVALENCE (C',I1,'(1),EVEC(',I5,'))')
11200 FORMAT(6X,'DIMENSION C',I2,'(',I3,')',/
     +6X,'EQUIVALENCE (C',I2,'(1),EVEC(',I5,'))')
11300 FORMAT(6X,'DIMENSION C',I3,'(',I3,')',/
     +6X,'EQUIVALENCE (C',I3,'(1),EVEC(',I5,'))')
11400 FORMAT(5X,'+',4(E15.8,A1))
11500 FORMAT(5X,'+',16(I3,A1))
11600 FORMAT('*'/6X,'DATA XA /')
11700 FORMAT('*'/6X,'DATA SQ /')
11800 FORMAT('*'/6X,'DATA IVARS /')
11900 FORMAT('*'/'*',6X,'TRANSFORMS   ORIGINAL VECTOR X IN THE BASE OF
     + EIGENVECTORS'/'*'/,6X,'IDUMMY=XDUMMY(1)',/,'*',/,
     +6X,'K =IDUMMY'/6X,'IF(K.LE.0.OR.K.GT.NVARS)K=1'/
     +6X,'XSI(K)=0.'/6X,'DO 10 I=1,NVARS'/
     +6X,'L=(K-1)*NVARS+I'/
     +6X,'XSI(K)=XSI(K)+(XVAR(IVARS(I))-XA(I))*EVEC(L)/SQ(I)'/
     +2X,'10',2X,'CONTINUE'/'*')
*    +6X,'IF( IDUMMY.GT.NVARS ) THEN',/
*    +12X,'PRINT 1000,IDUMMY',/
*    +1X,'1000       FORMAT(26H ---> THE VARIABLE XTOXSI(,I3,16H) IS NOT
*    + DEFINED)',/,
*    +6X,'RETURN',/,6X,'ENDIF',/
*    +  ,'*',/,
12000 FORMAT('*'/6X,'DATA C',I1,'/')
12100 FORMAT('*'/6X,'DATA C',I2,'/')
12200 FORMAT('*'/6X,'DATA C',I3,'/')
  999 END
