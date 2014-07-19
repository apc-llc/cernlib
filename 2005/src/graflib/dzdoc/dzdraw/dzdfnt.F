*
* $Id: dzdfnt.F,v 1.1.1.1 1996/03/04 16:13:04 mclareni Exp $
*
* $Log: dzdfnt.F,v $
* Revision 1.1.1.1  1996/03/04 16:13:04  mclareni
* Dzdoc/Zebpack
*
*
#include "dzdoc/pilot.h"
      SUBROUTINE DZDFNT
#include "zebra/mqsys.inc"
#include "dzdzbrinc.inc"
      INTEGER MAXS, MAXV
      PARAMETER (MAXS=10, MAXV=100, MAXAN=5)
      COMMON/DZDNTS/ ISVAR(MAXS), IVVAR(MAXV+1,MAXAN)
 
      REAL XNTVAL(MAXS)
      EQUIVALENCE(ISVAR,XNTVAL)
*--
      INTEGER IAFLAG(MAXS)
      INTEGER NTUPID,MAXEV,NEVENT, NCOLS, NCOLV
     &        , LL, IXDIV,ISTORE,I,LD,NSTEPS, ITYPE
     &        , IWORDF, IWORDL, NVAL, IP, ISTEER
      CHARACTER*8 CNAME, CTAG(MAXS)
      CHARACTER*4 CHOPT
      CHARACTER*2 CTYPE
      CHARACTER*50 CPATH
      LOGICAL HEXIST
*--
      ISTEER=0
      GOTO 5
 
      ENTRY DZDPNT
      ISTEER=1
 
 5    IXDIV=0
      ISTORE=ISTNTU
      CALL SBYT(ISTORE,IXDIV,27,6)
      CALL MZSDIV(IXDIV,-7)
      LL=LZEBLK(2,ISTORE)
      IF(LL.EQ.0)THEN
         WRITE(*,*)'Nothing marked for Ntuple'
         GOTO 999
      ENDIF
      NCOLS=0
      NCOLV=0
 
      IF(ISTEER.EQ.1)GOTO 10
 
      CALL KUGETI(LUN)
      CALL KUGETI(NTUPID)
      IF(HEXIST(NTUPID))THEN
         WRITE(*,*)'Id exists already',NTUPID
         GOTO 999
      ENDIF
      CALL KUGETC(CNAME,NCH)
      CALL KUGETC(CPATH,NCH)
      CALL KUGETI(MAXEV)
      IF(MAXEV.LE.0)MAXEV=100
      CALL KUGETC(CHOPT,NCH)
      IF(INDEX(CHOPT,'R').NE.0)CALL FZENDI(LUN,'I')
      IF(CTDDOC.NE.' ')CALL RZCDIR(CWDSAV,'R')
 
*       CALL HBNT(NTUPID,CNAME,CPATH)
 
*     get tags
 
10    CONTINUE
      NSTEPS=IQ(KQS+LL+5)
*     variable type (if <0 its array)
      ITYPE= IQ(KQS+LL+NSTEPS+8)
      IF     (ABS(ITYPE).EQ.3)THEN
         CTYPE=':R'
      ELSE IF(ABS(ITYPE).EQ.5)THEN
         CTYPE=':C'
      ELSE
         CTYPE=':I'
      ENDIF
*      CALL UHTOC(IQ(KQS+LL+1),4,CTAG,8)
 
*     currently only single variables
 
*      IF(ITYPE.GT.0)THEN
         IF(NCOLS.GE.MAXS)THEN
            WRITE(*,*)'No more space for single variable'
            GOTO 15
         ENDIF
         NCOLS=NCOLS+1
         IF(ITYPE.GT.0)THEN
            IAFLAG(NCOLS)=0
         ELSE
            IAFLAG(NCOLS)=1
         ENDIF
         CALL UHTOC(IQ(KQS+LL+1),4,CTAG(NCOLS),8)
*         WRITE(CVAR,'(A,(I2.2))')'VA',NCOLS
*         CALL HBNAME(NTUPID,CVAR,ISVAR(NCOLS),CTAG//CTYPE)
*      ELSE
*         IF(NCOLV.GE.MAXAN)THEN
*            WRITE(*,*)'No more space for array variable'
*            GOTO 15
*         ENDIF
*         NCOLV=NCOLV+1
*         WRITE(CVAR,'(A,(I2.2))')'VA',NCOLV
*         CHFORM= 'N_'//CTAG//'[1,100],'
*     &           //CTAG//'('//'N_'//CTAG//')'//CTYPE
*         CHFORM=SPACES( CHFORM,0)
*         NCH=LENOCC(CHFORM)
*         CALL HBNAME(NTUPID,CVAR,IVVAR(1,NCOLV),CHFORM(1:NCH))
*      ENDIF
      IF(ISTEER.EQ.1)THEN
         WRITE(*,*)'Name: '//CTAG(NCOLS)//' type: '//CTYPE
      ENDIF
15    LL=LQ(KQS+LL)
      IF(LL.NE.0)GOTO 10
 
      IF(ISTEER.EQ.1)GOTO 999
 
      CALL HBOOKN(NTUPID,CNAME,NCOLS,CPATH,100*NCOLS,CTAG)
      NEVENT=0
20    CONTINUE
 
      IF(LZEBLK(1,ISTORE).NE.0)CALL MZDROP(IXDIV,LZEBLK(1,ISTORE),' ')
      LZEBLK(1,ISTORE)=0
      NUH=NUHMAX
      CALL FZIN(LUN,IXDIV,LZEBLK(1,ISTORE),1,' ',NUH,IUHEAD)
      IF(IQUEST(1).NE.0)THEN
         IF(IQUEST(1).GE.3 .AND. IQUEST(1).LE.6)THEN
            WRITE(*,*)'FZIN: End of file reached, rewind it'
            CALL FZENDI(LUN,'I')
            GOTO 999
         ELSE
            WRITE(*,*)'Error from FZIN', IQUEST(1)
            GOTO 20
         ENDIF
      ENDIF
      IF(LZEBLK(1,ISTORE).EQ.0)THEN
         WRITE(*,*)'FZIN: No d/s found'
         GOTO 20
      ENDIF
 
      LL=LZEBLK(2,ISTORE)
      NCOLS=0
      NCOLV=0
 
*     currently only single variables, RESET VECTORS
 
      DO 26 I=1,MAXAN
         DO 26 J=1,MAXV+1
26         IVVAR(J,I)=0
 
*     loop on columns
 
25    CONTINUE
      NSTEPS=IQ(KQS+LL+5)
      ITYPE= IQ(KQS+LL+NSTEPS+8)
 
      CALL UHTOC(IQ(KQS+LL+1),4,CTAG,8)
*      IF(ITYPE.GT.0)THEN
         IF(NCOLS.GE.MAXS)GOTO 30
         NCOLS=NCOLS+1
*      ELSE
       IF(ITYPE.LT.0)THEN
         IF(NCOLV.GE.MAXAN)GOTO 30
         NCOLV=NCOLV+1
      ENDIF
      LD=LZEBLK(1,ISTORE)
 
*     find the bank
      IF(NSTEPS.LE.0)GOTO 36
      DO 35 K=NSTEPS+5,6,-1
         IF(IQ(KQS+LL+K).EQ.0)THEN
            LD=LQ(KQS+LD)
         ELSE
            LD=LQ(KQS+LD-IQ(KQS+LL+K))
         ENDIF
         IF(LD.EQ.0)THEN
            WRITE(*,*)'Requested bank not found'
            GOTO 30
         ENDIF
35    CONTINUE
 
*     is offset given explicitly?
36    IWORDF=IQ(KQS+LL+NSTEPS+6)
      IWORDL=IQ(KQS+LL+NSTEPS+7)
      IF(ITYPE.GT.0)THEN
*        scalar case
         IF(IWORDF.GT.0)THEN
            ISVAR(NCOLS) = IQ(KQS+LD+IWORDF)
         ELSE
            NVAL=1
            IF(CTDDOC.NE.' ')CALL RZCDIR(CTDDOC,' ')
            CALL DZDGVA(IXDIV,LD,'VQ',CTAG(NCOLS),NVAL,ISVAR(NCOLS),ITY)
         ENDIF
      ELSE
*        array
         IF(IWORDF.GT.0)THEN
            IF(IWORDL.LT.IWORDF)IWORDL=IWORDF
            IF(IWORDL-IWORDF.GT.MAXV)IWORDL=IWORDF+MAXV-1
            IVVAR(1,NCOLV)=IWORDL-IWORDF+1
            IP=1
            DO 37 I=IWORDF,IWORDL
              IP=IP+1
37            IVVAR(IP,NCOLV) = IQ(KQS+LD+I)
         ELSE
            NVAL=MAXV
            IF(CTDDOC.NE.' ')CALL RZCDIR(CTDDOC,' ')
            CALL DZDGVA(IXDIV,LD,'VQ',CTAG(NCOLS)
     &      ,NVAL,IVVAR(2,NCOLV),ITY)
            IVVAR(1,NCOLV)=NVAL
         ENDIF
      ENDIF
30    LL=LQ(KQS+LL)
      IF(LL.NE.0)GOTO 25
 
40    CONTINUE
 
*     currently only single variables
      MVAL=1
      IF(NCOLV.GT.0)THEN
         DO 45 I=1,NCOLV
         IF(IVVAR(1,I).GT.MVAL)MVAL=IVVAR(1,I)
45       CONTINUE
      ENDIF
      DO 60 I=2,MVAL+1
      NVAL=0
      DO 50 J=1,NCOLS
        IF(IAFLAG(J).NE.0)THEN
           NVAL=NVAL+1
           IF(ABS(ITYPE).EQ.2)THEN
              XNTVAL(J)=IVVAR(I,NVAL)
           ELSE
              ISVAR(J)=IVVAR(I,NVAL)
           ENDIF
        ELSE
           IF(ABS(ITYPE).EQ.2)THEN
              XNTVAL(J)=ISVAR(J)
           ENDIF
        ENDIF
50    CONTINUE
60    CALL HFN(NTUPID,XNTVAL)
 
*      CALL HFNT(NTUPID)
      NEVENT=NEVENT+1
      IF(NEVENT.LT.MAXEV)GOTO 20
      IF(CTDDOC.NE.' ')CALL RZCDIR(CWDSAV,' ')
 
999   END
***********************************************************************
