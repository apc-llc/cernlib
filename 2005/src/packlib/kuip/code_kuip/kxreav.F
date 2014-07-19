*
* $Id: kxreav.F,v 1.5 2000/01/10 10:35:43 couet Exp $
*
* $Log: kxreav.F,v $
* Revision 1.5  2000/01/10 10:35:43  couet
* - FNAME is now taken from the common block kcvrwf
*
* Revision 1.4  1997/09/25 14:02:28  cremel
* VECTOR WRITE (kxwriv.F): increase limitation of number of vectors in the
* list VLIST from 10 to 30 (same value as in kxreav.F for VECTOR/READ).
* Update HELP for VECTOR/WRITE and VECTOR/READ and mention explicitely this
* limitation. Improve error message in kxwriv.F and kxreav.F by
* adding explicitely VECTOR/WRITE and VECTOR/READ.
* + Increase KUIP version number to 2.07/16  (25/09/97).
*
* Revision 1.3  1997/07/08 09:48:24  cremel
* New version 2.07/14 :
* bug correction in VECTOR/READ (for free FORMAT suppress the limitation of
* 80 characters and protect the code when number of tokens > 50) -
* see example: vec2.kumac + vec2.dat + vec2bad.dat
*
* Revision 1.2  1996/10/01 07:35:17  gunter
* Correct reading of 1-D vectors with matching pattern and creation of
* vector. This did normally create/leave a vector too big. Now this case is
* handled the same as n-D vectors-- this can be done as with matching there
* can only be one number per line.
*
* ----------------------------------------------------------------------
*
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.05/15 01/08/94  10.35.04  by  Alfred Nathaniel
*-- Author :
      SUBROUTINE KXREAV
*
********************************************************************************
*
* Execution routine for command '/VECTOR/READ'
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
#include "kuip/kcvect.inc"
#include "kuip/kcmac.inc"
#include "kuip/kcunit.inc"
#include "kuip/kcvrwf.inc"
      CHARACTER*80  LINE,MATCH
      CHARACTER*512 LLINE
      CHARACTER*64 VNAME,VNAMES
      PARAMETER    (MAXNV=30)
      DIMENSION    VNAMES(MAXNV),LLOWS(MAXNV),LHIGHS(MAXNV)
      DIMENSION    LLOWC(MAXNV),LHIGHC(MAXNV)
      DIMENSION    IEXIST(MAXNV),V(MAXNV),INTV(MAXNV)
      DIMENSION    NELMAX(MAXNV),NEL(MAXNV)
      EQUIVALENCE  (V(1),INTV(1))
      CHARACTER*64 FORMAT,FORTMP
      CHARACTER*4  CHOPT
      DIMENSION    LENFR(3),LENTO(3),NDIM(3)
      LOGICAL      AEXIST
      PARAMETER    (MAXTOK=50)
      INTEGER      TOK(2,MAXTOK)
      PARAMETER    (MAXINT=2147483647)
      LOGICAL      MATSIG,IMSTAR,IMOK
      SAVE         VNAME
      SAVE         NTKSAV,NVSIZS
*
      NUMREC=0
      TVECFL=.FALSE.
      CALL KUGETC(VNAME,NCHNAM)
*
      IF (NCHNAM.EQ.0) THEN
        CALL KUALFA
        PRINT *,'*** VECTOR/READ: Vector not specified'
        IQUEST(1)=1
        GO TO 9999
      ENDIF
*
      NV=0
      LENMIN=1000000
   10 CONTINUE
      IF (NV.EQ.MAXNV) THEN
        PRINT *,
     +   '*** VECTOR/READ: Sive of Vector list is limited to: ',
     +   MAXNV
        PRINT *,'*** VECTOR/READ: Last vector to be processed: ',
     +   VNAME
      ENDIF
      CALL KUGETL(VNAME,N)
      IF (N.GT.0.AND.NV.LT.MAXNV) THEN
        NV=NV+1
        CALL KUVECT(VNAME,LLOW,LHIGH)
        IF (IQUEST(1).NE.0) GO TO 999
        NEXIST=0

        NCHNAM=IQUEST(10)
        NCHSUB=LENOCC(VNAME)-NCHNAM-2
        IF (NCHSUB.GT.0) THEN
          DO 15 I=1,3
            LENFR(I)=IQUEST(20+I)
            LENTO(I)=IQUEST(30+I)
   15     CONTINUE
          CALL KIVIND(VNAME(NCHNAM+2:),NCHSUB,LENFR,LENTO)
          L=LENTO(1)
        ELSE
          L=0
        ENDIF

        IF (LLOW.EQ.0) THEN
          IEXIST(NV)=0
          IF (NCHSUB.GT.0) THEN
            L=LENTO(1)
            IF (L.LT.LENMIN) LENMIN=L
          ENDIF
        ELSE
          IF (IQUEST(11).LT.L) THEN
*
            NL=0
            ND=L-IQUEST(11)
            CALL MZPUSH(IXKUIP,LVECN,NL,ND,' ')
            IQUEST(1)=0
            IQ(LVECN+10)=L
            IQ(LVECN+10+1)=L
            DO 20 J=1,NV
              CALL KUVECT(VNAMES(J),LLOWS(J),LHIGHS(J))
              IF (IQUEST(1).NE.0) GO TO 999
   20       CONTINUE
*
          ENDIF
          IF (IQUEST(11).LT.LENMIN) LENMIN=IQUEST(11)
          IEXIST(NV)=1
          NEXIST=NEXIST+1
        ENDIF
        VNAMES(NV)=VNAME
        LLOWS(NV)=LLOW
        LHIGHS(NV)=LHIGH
        GO TO 10
      ENDIF
      TVECFL=.TRUE.
*
      AEXIST=.FALSE.
      DO 30 I=1,NV
        IF (IEXIST(I).EQ.0) GO TO 40
   30 CONTINUE
      AEXIST=.TRUE.
   40 CONTINUE
*
      CALL KUGETF(FNAME,NCH)
      CALL KUGETC(FORMAT,NCH)
*      IF (FORMAT.EQ.' '.AND.NV.GT.1) THEN
*        CALL KUALFA
*        PRINT *,'*** Free format on multiple vectors is not allowed:'
*        PRINT *,'    you should add a format in the VECTOR/READ command'
*        GO TO 999
*      ENDIF
      IF (FORMAT.NE.' ') THEN
        FORTMP=' '
        FORTMP(1:1)='('
        FORTMP(2:)=FORMAT
        FORTMP(NCH+2:NCH+2)=')'
        FORMAT=FORTMP
      ENDIF
      CALL KUGETC(CHOPT,NCH)
      CALL KUGETS(MATCH,NCH)
      IF (MATCH.NE.' '.AND.CHOPT.NE.'OC') THEN
        CALL KUALFA
        PRINT *,
     +   '*** VECTOR/READ: MATCH and CHOPT options are incompatible'
        IQUEST(1)=1
        GO TO 999
      ENDIF
      IF (FNAME.EQ.' ') THEN
        LUNVR=5
        CALL KUALFA
        PRINT *,
     +   '*** VECTOR/READ: A non-blank file name must be specified'
        IQUEST(1)=1
        GO TO 999
      ELSE
        LUNVR=LUIVR
        IF (INDEX(CHOPT,'O').GT.0) THEN
          CALL KUOPEN(LUNVR,FNAME,'VERYOLD',ISTAT)
          IF (ISTAT.NE.0) THEN
            CALL KUALFA
            PRINT *,'*** VECTOR/READ: Error in opening file ',
     +              FNAME(1:LENOCC(FNAME))
            IQUEST(1)=1
            GO TO 999
          ENDIF
*
* Read the first line of the file to get the vector horizontal size
* (but only if one single vector is given as argument)
*
          IF (NV.EQ.1) THEN
            NTKSAV=1
            DO 50  I=1,1000000
              READ (LUNVR,'(A)',ERR=280,END=60) LLINE
              IF (LLINE.NE.' ') THEN
                CALL KILEXP(LLINE,' ','''','''',MAXTOK,TOK,NTKSAV,' ')
                GO TO 60
              ENDIF
   50       CONTINUE
   60       REWIND (LUNVR)
            IF (NTKSAV.EQ.0) THEN
              CALL KUALFA
              PRINT *,'*** VECTOR/READ: Change data file FORMAT: ',
     +                FNAME(1:LENOCC(FNAME)) 
              GO TO 999
            ENDIF
          ENDIF
*
* Read the whole file to get the vector vertical size
*
*===>>>   IF (MATCH.EQ.' ') THEN
            NVSIZS=0
            DO 70  I=1,1000000
              READ (LUNVR,'(A)',ERR=280,END=80) LINE
              NVSIZS=NVSIZS+1
   70       CONTINUE
   80       REWIND (LUNVR)
*===>>>   ENDIF
*
        ENDIF
      ENDIF
*
      NVSIZ=NVSIZS
*
      IF (INDEX(CHOPT,'O').EQ.0.AND.AEXIST) THEN
*
        NVSIZ=LENMIN
*
      ELSE
*
        IF (NV.EQ.1) THEN
          IF (FORMAT.EQ.' ') THEN
            NTOK=NTKSAV
          ELSE
*
* Count items in format:
*   search for all E,F,G,I specifiers and add up (optional) repeat counts
*   multiply by (optional) global repeat count
*   repeat count can be 0 (==> count=1), 1, or 2 digits
*   mixing of I and float specifiers does not work
*   only one level of global repeat count implemented
*   no checks for validity of format done
* Examples:
*   10F8.2                 : 10 item per line
*   5F4.0,5E12.6           : 10   "   "   "
*   4(2F3.0,1X,G11.5)      : 12   "   "   "
* Won't work:
*   100I1                  : more than 2 digits in repeat count
*   F8.2,I6                : mixing of float and integer
*   2(F2.0,3(F3.0,F4.1))   : nested repeat counts
*
            NTOK=0
            FORTMP=FORMAT
 82         CONTINUE
            LE=INDEX(FORTMP,'E')
            LF=INDEX(FORTMP,'F')
            LG=INDEX(FORTMP,'G')
            LI=INDEX(FORTMP,'I')
            IF(LE.EQ.0) LE=LEN(FORTMP)
            IF(LF.EQ.0) LF=LEN(FORTMP)
            IF(LG.EQ.0) LG=LEN(FORTMP)
            IF(LI.EQ.0) LI=LEN(FORTMP)
            L=MIN(LE,LF,LG,LI)
            IF(L.LT.LEN(FORTMP)) THEN
* repeat count of specifier
              N=0
              I=ICHAR(FORTMP(L-1:L-1))-ICHAR('0')
              IF(I.GE.0 .AND. I.LE.9) THEN
                N=I
                I=ICHAR(FORTMP(L-2:L-2))-ICHAR('0')
                IF(I.GE.0 .AND. I.LE.9) THEN
                   N=I*10+N
                ENDIF
                NTOK=NTOK+N
              ELSE
                NTOK=NTOK+1
              ENDIF
              FORTMP=FORTMP(L+1:)
              GOTO 82
            ENDIF
* global repeat count
            I=ICHAR(FORMAT(2:2))-ICHAR('0')
            IF(I.GE.0 .AND. I.LE.9) THEN
              N=I
              I=ICHAR(FORMAT(3:3))-ICHAR('0')
              IF(I.GE.0 .AND. I.LE.9) THEN
                IF(FORMAT(4:4).NE.'(') THEN
                  N=1
                ELSE
                  N=N*10+I
                ENDIF
              ELSEIF(FORMAT(3:3).NE.'(') THEN
                N=1
              ENDIF
              NTOK=NTOK*N
            ENDIF
          ENDIF
        ELSE
          NTOK=1
        ENDIF
*
* Create the vector used as index for matching pattern
*
        NNN=0
        IF (MATCH.NE.' ') THEN
*===>>>   NNN=10000
          NNN=NVSIZS
          J1=INDEX(VNAMES(1),'(')
          IF (J1.EQ.0) J1=LENOCC(VNAMES(1))+1
          CALL KUVEC(VNAMES(1)(1:J1-1)//'#',0,NNN,'CI')
          IF (IQUEST(1).NE.0) GO TO 310
          LLOWX=IQUEST(12)
          LHIGHX=IQUEST(13)
          LMATCH=LENOCC(MATCH)
          LM2=LMATCH-1
          IF (MATCH(1:1).EQ.'-') THEN
            MATSIG=.FALSE.
            LM1=3
          ELSE
            MATSIG=.TRUE.
            LM1=2
          ENDIF
          IMBEG=1
          IF(MATCH(LM1-1:LM1-1).NE.'/') GO TO 320
          IF(MATCH(LM2+1:LM2+1).EQ.')') THEN
            LM2=INDEX(MATCH(LM1:),'/')
            IF (LM2.EQ.0) GO TO 320
            LM2=LM2+LM1-1
            IF (MATCH(LM2+1:LM2+1).NE.'(') GO TO 320
            LM3=LM2+2
            LM4=LMATCH-1
            LM2=LM2-1
            IF (LM3.EQ.LM4.AND.MATCH(LM3:LM4).EQ.'*') THEN
              IMSTAR=.TRUE.
            ELSE
              IMSTAR=.FALSE.
              CALL KICTOI(MATCH(LM3:LM4),IMBEG)
              IF (IQUEST(1).NE.0) GO TO 320
            ENDIF
          ELSE IF (MATCH(LM2+1:LM2+1).NE.'/') THEN
            GO TO 320
          ENDIF
          IMEND=LM2-LM1+1
          IMEND=IMBEG+IMEND-1
        ENDIF
*
* Initialize the index vector
*
        IF (NNN.GT.0) THEN
          IF (IMSTAR) THEN
            IMBEG=1
            IMEND=1
          ENDIF
          NVSIZ=0
          DO 77  I=1,1000000
            READ (LUNVR,'(A)',ERR=280,END=88) LINE
            IMOK=.FALSE.
            IF (IMSTAR) THEN
              IF (INDEX(LINE,MATCH(LM1:LM2)).GT.0) IMOK=.TRUE.
            ENDIF
            IF (IMOK.OR.LINE(IMBEG:IMEND).EQ.MATCH(LM1:LM2)) THEN
              IF (MATSIG) THEN
                IQ(LLOWX+NVSIZ)=1
              ELSE
                IQ(LLOWX+NVSIZ)=0
              ENDIF
            ELSE
              IF (MATSIG) THEN
                IQ(LLOWX+NVSIZ)=0
              ELSE
                IQ(LLOWX+NVSIZ)=1
              ENDIF
            ENDIF
            NVSIZ=NVSIZ+1
   77     CONTINUE
   88     REWIND (LUNVR)
        ENDIF
*
        IF (MATCH.NE.' ') THEN
          NUMREC=NVSIZ
          NTOK=1
        ELSE
          NVSIZ=NVSIZ*NTOK
        ENDIF
*
* Check if there is space for automatically created vectors
*
        LENTOT=NVSIZ*NV
        LLL=50+LENTOT
        CALL MZNEED(IXKUIP,LLL,'G')
        IQUEST(1)=0
        IF (IQUEST(11).LT.0) THEN
          CALL KUALFA
          PRINT *,
     +     '*** VECTOR/READ: Not enough memory - Vector not created'
          IQUEST(1)=1
          GO TO 999
        ENDIF
*
      ENDIF
*
      IF (LENMIN.EQ.1000000) THEN
*
      ELSE
        NVSIZ=MIN(LENMIN,NVSIZ)
      ENDIF
      IF (NVSIZ.LE.0) GO TO 290

      NVSAV=NVSIZ

      DO 100 IV=1,NV
        LLOWC(IV)=1000000
        LHIGHC(IV)=1000000
        IF (IEXIST(IV).EQ.1) GO TO 100
        J1=INDEX(VNAMES(IV),'(')
        IF (J1.EQ.0) J1=LENOCC(VNAMES(IV))+1
        CALL KUVEC(VNAMES(IV)(1:J1-1),0,NVSIZ,'C')
        IF (IQUEST(1).NE.0) GO TO 310
        LLOW=IQUEST(12)
        LHIGH=IQUEST(13)
        DO 90 I=LLOW,LHIGH
          IQ(I)=MAXINT
   90   CONTINUE
        LLOWC(IV)=LLOW
        LHIGHC(IV)=LHIGH
  100 CONTINUE
*
* Re-compute vector addresses
*
      DO 660 IV=1,NV
        CALL KUVECT(VNAMES(IV),LLOWC(IV),LHIGHC(IV))
        IF (IQUEST(1).NE.0) GO TO 310
  660 CONTINUE

      J1=INDEX(VNAMES(1),'(')
      IF (J1.EQ.0) J1=LENOCC(VNAMES(1))+1
      CALL KUVECT(VNAMES(1)(1:J1-1)//'#',LLOWX,LHIGHX)
      IF (IQUEST(1).NE.0) GO TO 310

      DO 110 IV=1,NV
        CALL KUVECT(VNAMES(IV),LLOWS(IV),LHIGHS(IV))
        IF (IQUEST(1).NE.0) GO TO 310
        IF (IEXIST(IV).EQ.1) GO TO 110
        IF (LLOWS(IV).NE.LLOWC(IV)) GO TO 300
  110 CONTINUE

      LLOW=LLOWS(1)
      LHIGH=LHIGHS(1)

      ITYPE=IQUEST(14)
*
* Temporary vector used ?
*
      IF (IQUEST(20).EQ.1) THEN
*
        DO 120 I=1,3
          LENFR(I)=IQUEST(20+I)
          LENTO(I)=IQUEST(30+I)
          NDIM(I)=IQ(LVECN+10+I)
  120   CONTINUE
        LLLBEG=LVECN+14
        DO 130 K=LENFR(3),LENTO(3)
          DO 130 J=LENFR(2),LENTO(2)
            DO 130 I=LENFR(1),LENTO(1)
              JJJ=I+NDIM(1)*(J-1)+NDIM(1)*NDIM(2)*(K-1)
              LLL=LLLBEG+JJJ
              IF (FORMAT.EQ.' ') THEN
                IF (ITYPE.EQ.1) THEN
                  READ (LUNVR,*,END=330,ERR=280) Q(LLL)
                ELSE IF (ITYPE.EQ.2) THEN
                  READ (LUNVR,*,END=330,ERR=280) IQ(LLL)
                ENDIF
              ELSE
                IF (ITYPE.EQ.1) THEN
                  READ (LUNVR,FORMAT,END=330,ERR=280) Q(LLL)
                ELSE IF (ITYPE.EQ.2) THEN
                  READ (LUNVR,FORMAT,END=330,ERR=280) IQ(LLL)
                ENDIF
              ENDIF
  130   CONTINUE
*
      ELSE
*
        DO 140 I=1,NV
          NEL(I)=0
          NELMAX(I)=1000000
          IF (LHIGHS(I)-LLOWS(I)+1.LT.NELMAX(I))
     +      NELMAX(I)=LHIGHS(I)-LLOWS(I)+1
  140   CONTINUE

        NLIN=1000000
        IF (NV.GT.1) THEN
          NCOL=NV
        ELSE
          NCOL=NTOK
        ENDIF

        IF (INDEX(CHOPT,'O').EQ.0) THEN
          READ (LUNVR,'(A)',ERR=280,END=280) LINE
          CALL KILEXP(LINE,' ','''','''',MAXTOK,TOK,NTOK,' ')
          NCOL=MIN(NTOK,MAXNV)
          BACKSPACE (LUNVR)
        ENDIF

        IEOF=1
        IOKLIN=0

        DO 180 ILIN=1,NLIN

          IF (NUMREC.GT.0) THEN
            IF (IQ(LLOWX+ILIN-1).EQ.0) THEN
              READ (LUNVR,'(A)',ERR=280,END=190) LINE
              GO TO 180
            ENDIF
          ENDIF

          IOKLIN=IOKLIN+1
          LINSAV=IOKLIN

          DO 150 I=1,NCOL
            INTV(I)=MAXINT
  150     CONTINUE

*----------------------------------------------------------------------v
          IF (FORMAT.EQ.' ') THEN
            IF (ITYPE.EQ.1) THEN
              IF (NV.GT.1 .OR. MATCH.NE.' ') THEN
                READ (LUNVR,*,END=190,ERR=280) (V(I),I=1,NCOL)
              ELSE
                READ (LUNVR,*,END=190,ERR=280)
     +               (Q(LLOWS(1)+I),I=0,NVSAV-1)
                GO TO 330
              ENDIF
            ELSE IF (ITYPE.EQ.2) THEN
              IF (NV.GT.1 .OR. MATCH.NE.' ') THEN
                READ (LUNVR,*,END=190,ERR=280) (INTV(I),I=1,NCOL)
              ELSE
                READ (LUNVR,*,END=190,ERR=280)
     +               (IQ(LLOWS(1)+I),I=0,NVSAV-1)
                GO TO 330
              ENDIF
            ENDIF
*----------------------------------------------------------------------^
          ELSE
            IF (ITYPE.EQ.1) THEN
              READ (LUNVR,FORMAT,END=190,ERR=280) (V(I),I=1,NCOL)
            ELSE IF (ITYPE.EQ.2) THEN
              READ (LUNVR,FORMAT,END=190,ERR=280) (INTV(I),I=1,NCOL)
            ENDIF
          ENDIF

          IF (NV.GT.1 .OR. MATCH.NE.' ') THEN
            DO 160 IV=1,NCOL
              NEL(IV)=NEL(IV)+1
              IF (NEL(IV).GT.NELMAX(IV)) GO TO 190
              Q(LLOWS(IV)+IOKLIN-1)=V(IV)
  160       CONTINUE
          ELSE
            DO 170 ICOL=1,NCOL
              NEL(1)=NEL(1)+1
              IF (NEL(1).GT.NELMAX(1)) GO TO 190
              Q(LLOWS(1)+NTOK*(IOKLIN-1)+ICOL-1)=V(ICOL)
  170       CONTINUE
          ENDIF
          IF (INDEX(CHOPT,'C').EQ.0.AND.NEL(1).GE.NELMAX(1)) GO TO 185
  180   CONTINUE

  185   CONTINUE

        IEOF=0
        GO TO 191

  190   CONTINUE
        DO 192 I=1,NCOL
          INTV(I)=MAXINT
  192   CONTINUE

  191   CONTINUE

*----------------------------------------------------------------------v
        IF (FORMAT.EQ.' ' .AND. NV.EQ.1 .AND. MATCH.EQ.' ') GO TO 330
*----------------------------------------------------------------------^

        IF (INTV(NCOL).EQ.MAXINT) THEN
          DO 200 INVC=NCOL,1,-1
            IF (INTV(INVC).NE.MAXINT) GO TO 210
  200     CONTINUE
          INVC=0
          IOKLIN=IOKLIN-1
  210     CONTINUE
          IF (NV.GT.1 .OR. MATCH.NE.' ') THEN
            DO 220 IV=1,INVC
              Q(LLOWS(IV)+LINSAV-1)=V(IV)
  220       CONTINUE
          ELSE
            DO 230 ICOL=1,INVC
              Q(LLOWS(1)+NTOK*(LINSAV-1)+ICOL-1)=V(ICOL)
  230       CONTINUE
          ENDIF
        ENDIF

        IF (NV.EQ.1 .OR. MATCH.NE.' ') THEN
          DO 240 J=LHIGH,LLOW,-1
            IF (IQ(J).NE.MAXINT) THEN
              I=J+1
              GO TO 250
            ENDIF
  240     CONTINUE
          I=LLOW
  250     CONTINUE
          IF (IEXIST(1).EQ.1) THEN
            NWV=I-LLOWS(1)
          ELSE
            NWV=I-LLOWC(1)
          ENDIF
        ELSE
          NWV=IOKLIN
        ENDIF

        DO 270 IV=1,NV

          IF (IEXIST(IV).EQ.0) THEN
            IF (IOKLIN.EQ.0) THEN
              J1=INDEX(VNAMES(IV),'(')
              IF (J1.EQ.0) J1=LENOCC(VNAMES(IV))+1
              CALL KUVDEL(VNAMES(IV)(1:J1-1))
              GO TO 270
            ENDIF
            LINE=VNAMES(IV)
            CALL CLTOU(LINE)
            CALL KUALFA
            PRINT *,'*** VECTOR/READ: Unknown vector ',
     +        LINE(:LENOCC(LINE)),
     +        ' is created with length',NWV
*           IF (IEOF.EQ.0) THEN
*             IF (NV.EQ.1.OR.NV.EQ.NEXIST) THEN
*               PRINT *,'    but whole file did not fit into vector'
*               LINE=FNAME
*               CALL CLTOU(LINE)
*               PRINT *,'*** EOF on file ',LINE(:LENOCC(LINE))
*             ENDIF
*           ENDIF
            NL=0
            ND=-(NVSIZ-NWV)
            CALL KUVECT(VNAMES(IV),LLOWS(IV),LHIGHS(IV))
            IF (IQUEST(1).NE.0) GO TO 310
            CALL MZPUSH(IXKUIP,LVECN,NL,ND,' ')
            IQUEST(1)=0
            IQ(LVECN+10)=NWV
            IQ(LVECN+10+1)=NWV
            DO 260 J=1,NV
              CALL KUVECT(VNAMES(J),LLOWS(J),LHIGHS(J))
              IF (IQUEST(1).NE.0) GO TO 310
  260       CONTINUE
          ELSE
*           IF (IEOF.EQ.1) THEN
*             LINE=FNAME
*             CALL CLTOU(LINE)
*             PRINT *,'*** EOF on file ',LINE(:LENOCC(LINE))
*             GO TO 250
*           ENDIF
          ENDIF

  270   CONTINUE
*
      ENDIF
*
      GO TO 330

  280 CALL KUALFA
      PRINT *,'*** VECTOR/READ: Error in reading from file ',
     +        FNAME(1:LENOCC(FNAME))
      IF (LUNVR.NE.5) CALL KUCLOS(LUNVR,' ',ISTAT)
      IQUEST(1)=1
      GO TO 999

  290 CALL KUALFA
      IF (LUNVR.NE.5) CALL KUCLOS(LUNVR,' ',ISTAT)
      PRINT *,
     + '*** VECTOR/READ: Not enough memory - Cannot create the vector'
      IQUEST(1)=1
      GO TO 999

  300 CALL KUALFA
      PRINT *,'*** VECTOR/READ: Subscripts must start at 1'
      IF (LUNVR.NE.5) CALL KUCLOS(LUNVR,' ',ISTAT)
      IQUEST(1)=1
      GO TO 999

  310 IF (LUNVR.NE.5) CALL KUCLOS(LUNVR,' ',ISTAT)
      GO TO 999

  320 CALL KUALFA
      IF (LUNVR.NE.5) CALL KUCLOS(LUNVR,' ',ISTAT)
      PRINT *,
     + '*** VECTOR/READ: Error in definition of matching pattern'
      IQUEST(1)=1
      GO TO 999

  330 CONTINUE

      IF (FORMAT.EQ.' ' .AND. NV.EQ.1 .AND. MATCH.EQ.' ') THEN
        IF (IEXIST(1).EQ.0) THEN
          LINE=VNAMES(1)
          CALL CLTOU(LINE)
          CALL KUALFA
          PRINT *,'*** VECTOR/READ: Unknown vector ',
     +            LINE(:LENOCC(LINE)),
     +      ' is created with length',NVSAV
        ENDIF
      ENDIF

      IF ((LUNVR.NE.5).AND.(INDEX(CHOPT,'C').GT.0))
     +                                 CALL KUCLOS(LUNVR,' ',ISTAT)

      IF (INDEX(CHOPT,'Z').NE.0) THEN
*
* Temporary vector used ?
*
        IF (IQUEST(20).EQ.1) THEN
*
          DO 340 K=LENFR(3),LENTO(3)
            DO 340 J=LENFR(2),LENTO(2)
              DO 340 I=LENFR(1),LENTO(1)
                JJJ=I+NDIM(1)*(J-1)+NDIM(1)*NDIM(2)*(K-1)
                LLL=LLLBEG+JJJ
                LLLP1=LLL+1
                IF (ITYPE.EQ.1) THEN
                  IF (Q(LLLP1).EQ.0) Q(LLLP1)=Q(LLL)
                ELSE IF (ITYPE.EQ.2) THEN
                  IF (IQ(LLLP1).EQ.0) IQ(LLLP1)=IQ(LLL)
                ENDIF
  340     CONTINUE
*
        ELSE
*
          DO 350 I=LLOW+1,LHIGH
            IF (ITYPE.EQ.1) THEN
              IF (Q(I).EQ.0) Q(I)=Q(I-1)
            ELSE IF (ITYPE.EQ.2) THEN
              IF (IQ(I).EQ.0) IQ(I)=IQ(I-1)
            ENDIF
  350   CONTINUE
*
        ENDIF
*
      ENDIF
*
  999 CONTINUE

      IF (NUMREC.GT.0) THEN
        J1=INDEX(VNAMES(1),'(')
        IF (J1.EQ.0) J1=LENOCC(VNAMES(1))+1
        CALL KUVDEL(VNAMES(1)(1:J1-1)//'#')
        CALL KUALFA
        IF (IOKLIN.EQ.0) THEN
          PRINT *,'*** VECTOR/READ: No match'
        ELSE
          IF (IEXIST(1).EQ.1) PRINT *,'*** VECTOR/READ: ',
     +                        IOKLIN,' record(s) matched'
        ENDIF
      ENDIF

 9999 END
