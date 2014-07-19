*
* $Id: iztabl.F,v 1.1.1.1 1996/02/14 13:11:14 mclareni Exp $
*
* $Log: iztabl.F,v $
* Revision 1.1.1.1  1996/02/14 13:11:14  mclareni
* Higz
*
*
#include "higz/pilot.h"
#if defined(CERNLIB_ZEBRA)||defined(CERNLIB_MAIL)
*CMZ :  1.22/08 27/02/95  17.41.08  by  O.Couet
*-- Author :    O.Couet   18/09/89
      SUBROUTINE IZTABL(NX,NY,V,NPARI,PAR,IOPT)
*.===========>
*.
*.   This routine stores in the current picture the necessary data to
*. to draw a table .
*.
*. _Input parameters:
*.
*.  INTEGER NX x NY : Number of points .
*.  REAL V(NX,NY) : Table contain .
*.  INTEGER NPAR : Number of additional parameters .
*.  REAL PAR(NPAR) : additional parameters .
*.  INTEGER IOPT : Option .
*.
*..==========> (O.Couet)
#include "higz/hicode.inc"
#include "higz/hiflag.inc"
#if defined(CERNLIB_ZEBRA)
#include "higz/hipaw.inc"
#endif
#if defined(CERNLIB_ZEBRA)
#include "higz/hipack.inc"
#endif
#if defined(CERNLIB_MAIL)
#include "higz/himail.inc"
#endif
      DIMENSION IOPT(*)
      DIMENSION PAR(*),V(NX,1)
#if defined(CERNLIB_MAIL)
      CHARACTER*16 CHTEMP
#endif
      REAL IGCELL
*.______________________________________
*
#include "higz/hibit.inc"
*
      NYY  = MAX(NY,1)
      NPAR = NPARI
#if defined(CERNLIB_ZEBRA)
      IF(ZFLAG)THEN
         IF(LPICT.LT.0)RETURN
         IF(PAR(9).LT.0..OR.PAR(10).LT.0.)NPAR=NPAR+2
         IF(IPACK.NE.0)LWV=INT(V(1,1))
         IF(IZPUSH(5,NX*NYY+NPAR,0,'IGTABL').NE.0)RETURN
         IF(IPACK.NE.0)V(1,1)=FLOAT(LWV)
         CALL IZCFA(IBOFCO,1)
         CALL IZCFA(IBWDCO,1)
         CALL IZCFA(ILNCO,1)
         CALL IZCFA(ILWSCO,1)
         CALL IZCFA(IPLCCO,1)
         CALL IZCFA(IFAICO,1)
         CALL IZCFA(IFASCO,1)
         CALL IZCFA(IFACCO,1)
         CALL IZCFA(ITMSCO,1)
         CALL IZCFA(IALHCO,1)
         CALL IZCFA(IALDCO,1)
         CALL IZCFA(ITXCCO,1)
         CALL IZCFA(ITXFCO,1)
         CALL IZCFA(ICHHCO,1)
         CALL IZCFA(IGTSCO,1)
         CALL IZCFA(IGTWCO,1)
         CALL IZCFA(IAWLCO,1)
         CALL IZCFA(IMKCO,1)
         CALL IZCFA(IMKSCO,1)
         CALL IZCFA(IPMCCO,1)
*
         CALL IZSTCC(ITABCO,INTPTR)
         IQ(LHI+INTPTR)  =NX
         IQ(LHI+INTPTR+1)=NY
         IQ(LHI+INTPTR+2)=IFLPTR
         IQ(LHI+INTPTR+3)=NPARI
         DO 10 I=1,30
            IF(IOPT(I).NE.0)CALL SBIT1(IQ(LHI+INTPTR+4),I)
   10    CONTINUE
         CALL IZINCI(5)
         DO 20 I=1,NPARI
            Q(LHF+IFLPTR+I-1)=PAR(I)
   20    CONTINUE
         IF(PAR(9).LT.0.) Q(LHF+IFLPTR+NPARI)=IQUEST(60)
         IF(PAR(10).LT.0.)Q(LHF+IFLPTR+NPARI+1)=IQUEST(61)
         CALL IZINCF(NPAR)
         DO 40 J=1,NYY
            DO 30 I=1,NX
               Q(LHF+IFLPTR+I-1+(J-1)*NX)=IGCELL(NX,NY,V,I,J,1)
   30       CONTINUE
   40    CONTINUE
         CALL IZINCF(NX*NYY)
      ENDIF
#endif
#if defined(CERNLIB_MAIL)
*
      IF(MFLAG)THEN
         IF(IOPT(11).NE.0)THEN
            CALL IGERR('Stacked LEGO not implemented with telnetg'
     +,                'IGTABL')
            RETURN
         ENDIF
         IOP=INDEX(CHOPTM,'H')
         IF(IOP.GT.0)CHOPTM(IOP:IOP)=' '
         IF(PAR(9).LT.0.) PAR(9) =1000.*ABS(PAR(9))+IQUEST(60)
         IF(PAR(10).LT.0.)PAR(10)=1000.*ABS(PAR(10))+IQUEST(61)
         WRITE (CHMAIL,'(I3,A20,3I5)') ITABCO,CHOPTM,NX,NY,NPARI
         CALL IMWRIT(1)
         CALL IMFOUT(NPARI,PAR)
         JJ=0
         CHMAIL=' '
         DO 70 J=1,NYY
            DO 60 I=1,NX
               WRITE (CHTEMP,'(G13.7)')IGCELL(NX,NY,V,I,J,1)
               K=(13*JJ)+1
               CHMAIL(K:K+12)=CHTEMP(1:13)
               JJ=JJ+1
               IF(JJ.EQ.6)THEN
                  JJ=0
                  CALL IMWRIT(2)
               ENDIF
   60       CONTINUE
   70    CONTINUE
         IF(JJ.NE.0)CALL IMWRIT(2)
         CALL IMWRIT(5)
      ENDIF
#endif
*
      END
#endif
