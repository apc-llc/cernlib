*
* $Id: dzdmnt.F,v 1.1.1.1 1996/03/04 16:13:05 mclareni Exp $
*
* $Log: dzdmnt.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:05  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDMNT(IXDIV,LIN)
      INTEGER IXDIV,LIN
#include "zebra/mqsys.inc"
*--
      CHARACTER CQSTAK*13,CQINFO*40
      PARAMETER (NLICHQ=130,NSTCHQ=8,NDVCHQ=8,NBKCHQ=4 )
      CHARACTER CQLINE*(NLICHQ),CQMAP(10)*(NLICHQ)
      CHARACTER CQSTOR*(NSTCHQ),CQDIV*(NDVCHQ),CQID*(NBKCHQ)
      COMMON /DZC1CH/ CQSTOR,CQDIV,CQID,CQMAP,CQSTAK,CQINFO
      EQUIVALENCE (CQLINE,CQMAP)
*--
#include "dzdzbrinc.inc"
      INTEGER MAXS, MAXV
      PARAMETER (MAXS=10, MAXV=100, MAXAN=5)
      COMMON/DZDNTS/ ISVAR(MAXS), IVVAR(MAXV+1,MAXAN)
 
      REAL XNTVAL(MAXS)
      EQUIVALENCE(ISVAR,XNTVAL)
*--
*      INTEGER IAFLAG(MAXS)
      INTEGER NCOLV, NVAL, NP
      SAVE NCOLV
*--
      INTEGER IWORDF, IWORDL, NCH, NSTEPS, LUP, LORIG, L, LL
     &       ,IOCH, INIFLG, NSU, ITYPE
      SAVE  INIFLG,  IOCH
      CHARACTER*8 CNAME, SPACES, VTYPE
      CHARACTER*4 CHOPT
      DATA  INIFLG/0/
*---
      IF(INIFLG.EQ.0)THEN
          INIFLG=1
          CALL MZIOCH(IOCH,1,'4H -I')
          NCOLV=0
      ENDIF
      CALL MZSDIV(IXDIV,-7)
      ISTORE = JBYT(IXDIV,27,6)
 
*     init link area for store
 
      IF(LZEFLG(ISTORE).EQ.0)THEN
         CALL MZLINK(IXDIV,'DZDZEB',
     &   LZEBLK(1,ISTORE),LZEBLK(3,ISTORE),LZEBLK(3,ISTORE))
         LZEFLG(ISTORE)=1
      ENDIF
 
      CALL KUGETC(VTYPE,NCH)
      CALL CLTOU(VTYPE)
      CALL KUGETI(IWORDF)
      CALL KUGETI(IWORDL)
      CALL KUGETC(CNAME,NCH)
*      CALL KUGETC(CHLOOP,NCH)
      CALL KUGETC(CHOPT,NCH)
 
      IF(IWORDF.EQ.0 .AND. CNAME.EQ.' ')THEN
         WRITE(*,*)'No variable defined'
         GOTO 999
      ENDIF
*     new set of ntuples?
      IF(INDEX(CHOPT,'N').NE.0)THEN
         IF(LZEBLK(2,ISTORE).NE.0)THEN
            CALL MZDROP(IXDIV,LZEBLK(2,ISTORE),'L')
            LZEBLK(2,ISTORE)=0
            ISTNTU=0
         ENDIF
         NCOLV=0
         CALL KUPVAL('MARK_NT','COPT',0,0.,' ','D')
      ENDIF
*     find data type
      IF(IWORDF.GT.0)THEN
         IF(IQ(KQS+LIN-1).LT.IWORDF .OR.IQ(KQS+LIN-1).LT.IWORDL)THEN
            WRITE(*,*)'Bank has only',IQ(KQS+LIN-1),' data words'
            GOTO 999
         ENDIF
         CALL DZSHOW(' ',IXDIV, LIN,'Q',1,0,IWORDF,IWORDF)
         IF     (INDEX(CQLINE(13:24),'"').NE.0)THEN
            ITYPE=5
         ELSE IF(INDEX(CQLINE(13:24),'.').NE.0)THEN
            ITYPE=3
         ELSE
            ITYPE=2
         ENDIF
         NVAL=0
      ELSE
         IF(NCOLV.LT.MAXAN)THEN
            NCOLV=NCOLV+1
            NVAL=MAXV
            CALL DZDGVA(IXDIV,LIN,'VQP',CNAME,NVAL,IVVAR(2,NCOLV),ITYPE)
            IF(ITYPE.LE.0)THEN
               WRITE(*,'(A,A)')CNAME,' not found'
               GOTO 999
            ENDIF
         ELSE
            WRITE(*,*)'Max number of array type Ntuple reached:',NCOLV
            GOTO 999
         ENDIF
      ENDIF
      IF(LZEBLK(2,ISTORE).EQ.0)THEN
         ISTNTU=ISTORE
      ELSE
         IF(ISTNTU.NE.ISTORE)THEN
            WRITE(*,*)'Variable is not in store',ISTNTU
            GOTO 999
         ENDIF
      ENDIF
 
*     get number of steps back
 
      L=LIN
      NSTEPS=0
10    CONTINUE
      LUP=LQ(KQS+L+1)
      IF(LUP.NE.0)THEN
         NSTEPS=NSTEPS+1
         LORIG=LQ(KQS+L+2)
         L=LUP
         IF(LORIG .GT. 2)THEN
            NSU = IQ(KQS+LUP-2)
            IF(LORIG .GT. LUP .OR. LORIG .LT. LUP-NSU) L=LORIG
         ENDIF
        GOTO 10
      ENDIF
 
*     book a bank to store the path etc
 
      NP=ABS(NVAL)+1
      CALL MZBOOK(IXDIV,LL,LZEBLK(2,ISTORE),1,'PATH',
     &            0,0,NP+NSTEPS+8,IOCH,0)
      IF(CNAME.NE.' ')THEN
         CALL UCTOH(CNAME,IQ(KQS+LL+1),4,8)
      ELSE
         WRITE(CNAME,'(I8)')IWORDF
         CALL UHTOC(IQ(KQS+LIN-4),4,CNAME,4)
         CNAME=SPACES(CNAME,0)
         CALL UCTOH(CNAME,IQ(KQS+LL+1),4,8)
      ENDIF
      IQ(KQS+LL+3)=IQ(KQS+LIN-4)
      IF(LQ(KQS+LIN+1).NE.0)THEN
         IQ(KQS+LL+4)=IQ(KQS+LQ(KQS+LIN+1)-4)
      ELSE
         CALL UCTOH('NONE',IQ(KQS+LL+4),4,4)
      ENDIF
      IQ(KQS+LL+5)=NSTEPS
 
*     fill the path
 
      L=LIN
      NSTEPS=6
20    CONTINUE
      LUP=LQ(KQS+L+1)
      IF(LUP.NE.0)THEN
         LORIG=LQ(KQS+L+2)
         IF(LORIG .GT. 2)THEN
            NSU = IQ(KQS+LUP-2)
            IF(LORIG .GT. LUP .OR. LORIG .LT. LUP-NSU)THEN
               JB = 0
            ELSE
               JB = LUP-LORIG
            ENDIF
            IQ(KQS+LL+NSTEPS)=JB
            NSTEPS=NSTEPS+1
        ENDIF
        IF(JB.EQ.0)THEN
           L=LORIG
        ELSE
           L=LUP
        ENDIF
        GOTO 20
      ENDIF
      IQ(KQS+LL+NSTEPS)=IWORDF
*      IQ(KQS+LL+NSTEPS+1)=IWORDL
      IF(INDEX(VTYPE,'ARR').NE.0 .OR.
     &   INDEX(VTYPE,'MUL').NE.0)ITYPE=-ITYPE
      IQ(KQS+LL+NSTEPS+2)=ITYPE
      IQ(KQS+LL+NSTEPS+3)=NVAL
      IF(NVAL.GT.0)CALL UCOPY(IVVAR(2,NCOLV),IQ(KQS+LL+NSTEPS+4),NVAL)
999   END
 
***********************************************************************
