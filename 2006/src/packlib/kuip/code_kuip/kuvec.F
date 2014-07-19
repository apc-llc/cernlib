*
* $Id: kuvec.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kuvec.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.07/03 02/05/95  16.01.25  by  Gunter Folger
*-- Author :
      SUBROUTINE KUVEC(VECN,X,NELEMS,CHOPT)
*
********************************************************************************
*
* Routine for vector handling.
*
* Note: in the following description ILOW stands for ILOW(IDIM),
*       IHIGH for IHIGH(IDIM) and LENTOT for LENGTH(IDIM), where IDIM is
*       the dimension (1, 2 or 3) affected.
*
* Only one dimension can be handled by this routine, ex. VECN can be
* VEC(3), VEC(2,3:5), etc. if VEC is two-dim, but cannot be VEC(2:3,3:5)
*
* CHOPT='R' read in the array X the content of vector VECN (eg. VECN ---> X)
*           starting by the ILOW-th vector element (ILOW=1 is the first one).
*           NELEMS elements are read, eg. VECN(ILOW:ILOW+NELEMS-1), or all
*           if NELEMS<=0 or NELEMS>=LENTOT.
*           If the vector is not existing then IQUEST(1)=1.
*
* CHOPT=' ' same as CHOPT='R'.
*
* CHOPT='W' write the array X into the vector VECN (eg. X ---> VECN)
*           starting by the ILOW-th vector element (ILOW=1 is the first one).
*           NELEMS elements are written, eg. X(1:NELEMS).
*           If the vector is not large enough, it is automatically extended.
*           If the vector is not existing it is created.
*
* CHOPT='-' (used together with option 'W') same as CHOPT='W' and in addition
*           the vector is shrinked to the actual size.
*
* CHOPT='C' just create the vector if not existing.
*
* CHOPT='I' (used together with option 'W' or 'C') vector is of type Integer
*           (default case is Real)
*
* CHOPT='X' (used together with option 'W' or 'C') vector is of type compleX
*           (default case is Real)
*
* It returns also in IQUEST :
*
* IQUEST(10) : NCHNAM (number of chars of VECN)
* IQUEST(11) : LENTOT (total number of elements of vector)
* IQUEST(12) : LLOW   (low address)
* IQUEST(13) : LHIGH  (high address)
* IQUEST(14) : ITYPE  (type: 1=real, 2=integer, 3=hollerith)
*
* IQUEST(20) : ICOPY  (if <> 0 a copy on temporary vectory was done,
*                      with LENFR and LENTO addresses defined as follow)
* IQUEST(21) : LENFR(1)
* IQUEST(22) : LENFR(2)
* IQUEST(23) : LENFR(3)
* IQUEST(31) : LENTO(1)
* IQUEST(32) : LENTO(2)
* IQUEST(33) : LENTO(3)
*
* The vector elements can be addressed individually, if the common block /PAWC/
* is present, by Q(LLOW+I) or IQ(LLOW+I), with I ranging from 0 to LENTOT-1.
*
* The array X should be defined in the calling routine of the right type,
* INTEGER or REAL, corresponding to the vector type.
*
*     Input :
*
      CHARACTER*(*) VECN
*
*     Input/Output :
*
      INTEGER X(*)
*
*     Input :
*
      INTEGER NELEMS
      CHARACTER*(*) CHOPT
*
********************************************************************************
*
#include "kuip/kcgen.inc"
#include "kuip/kcques.inc"
#include "kuip/kcvect.inc"
      CHARACTER*1  VECTYP
      DIMENSION LENFR(3),LENTO(3),ILOW(3),LENGTH(3),IHIGH(3),ISTEPS(3)
      DIMENSION NDIM(3)
      CHARACTER*64 VNAME,VECNAM
      DIMENSION IOPT(7)
      EQUIVALENCE (IOPT(1),IOPTR),(IOPT(2),IOPTBL),(IOPT(3),IOPTW)
      EQUIVALENCE (IOPT(4),IOPTMI),(IOPT(5),IOPTC),(IOPT(6),IOPTI)
      EQUIVALENCE (IOPT(7),IOPTX)

*
      IQUEST(1)=0
      NELEM=NELEMS

      VECNAM=VECN
      CALL CLTOU(VECNAM)

      CALL UOPTC(CHOPT,'R W-CIX',IOPT)

      IF (IOPTR.GT.0.AND.CHOPT.NE.'R') THEN
         CALL KUALFA
         PRINT *,'*** KUVEC: Option R cannot coexist with other '
     +   //'options'
         IQUEST(1)=1
         GO TO 999
      ENDIF

      TVECFL=.FALSE.
      CALL KUVECT(VECNAM,LLOW,LHIGH)
      TVECFL=.TRUE.

      NCHNAM=IQUEST(10)
      IF (LLOW.GT.0) THEN
         ICOPY=IQUEST(20)
         LENFR(1)=IQUEST(21)
         LENFR(2)=IQUEST(22)
         LENFR(3)=IQUEST(23)
         LENTO(1)=IQUEST(31)
         LENTO(2)=IQUEST(32)
         LENTO(3)=IQUEST(33)
      ELSE
         ICOPY=0
         I1=INDEX(VECNAM,'(')
         I2=INDEX(VECNAM,')')
         NCHSUB=(I2-1)-(I1+1)+1
         DO 5 I=1,3
            LENFR(I)=1
            LENTO(I)=1
    5    CONTINUE
         IF (NCHSUB.GT.0) THEN
            CALL KIVIND(VECNAM(I1+1:I2-1),NCHSUB,LENFR,LENTO)
            IF (IQUEST(1).NE.0) GO TO 999
         ENDIF
         DO 6 I=1,3
            IF (LENFR(I).GT.LENTO(I)) LENTO(I)=LENFR(I)
    6    CONTINUE
      ENDIF

      DO 8 I=1,3
         ILOW(I)=LENFR(I)
         LENGTH(I)=LENTO(I)-LENFR(I)+1
    8 CONTINUE
*
* Multi-dim vectors accepted only if large enough
* (i.e. only one-dim vectors can be expanded)
* and with non-mixed dimensions
*
      IDIM=1
      III=0
      DO 9 I=1,3
         IF (LENGTH(I).GT.1) THEN
            III=III+1
            IDIM=I
         ENDIF
    9 CONTINUE
      IF (III.GT.1) THEN
         PRINT *,'*** KUVEC: Cannot handle mixed dimensions for vector '
     +   , VECNAM(:NCHNAM)
         IQUEST(1)=1
         GO TO 999
      ENDIF
      IF (NELEM.GT.LENGTH(IDIM)) THEN
         IF ( LLOW.GT.0) THEN
*               upper limit given is smaller/= than real length of vector
            IF ( LENTO(IDIM).LT.IQ(LVECN+10+IDIM) ) THEN
               NELEM=LENGTH(IDIM)
            ELSE
*                need to expand vector, check if 1-d
               IF ( IQ(LVECN+14).NE.1 ) THEN
                  PRINT *,'*** KUVEC: Multi-dim vector ',
     +                    VECNAM(:NCHNAM),' is not large enough'
                  IQUEST(1)=1
                  GO TO 999
               ENDIF
            ENDIF

         ENDIF
      ENDIF
*
      IF (IOPTC.GT.0.AND.IDIM.EQ.1) LENGTH(1)=NELEM
*
      IF (LLOW.EQ.0) THEN
*
* Vector not existing
*
         IF (IOPTBL.GT.0.OR.IOPTR.GT.0) THEN
            IQUEST(1)=1
            GO TO 999
         ELSE
*
* Create the vector
*
            VNAME=' '
            WRITE (VNAME,1000) VECNAM(1:NCHNAM),(LENGTH(I),I=1,3)
 1000     FORMAT (A,'(',I6,',',I6,',',I6,')')
            IF (IOPTI.GT.0) THEN
               VECTYP='I'
               IQUEST(14)=2
            ELSE IF (IOPTX.GT.0) THEN
               VECTYP='C'
               IQUEST(14)=3
            ELSE
               VECTYP='R'
               IQUEST(14)=1
            ENDIF
            CALL KXCRV1(VNAME,VECTYP)
            IF (IQUEST(1).NE.0) GO TO 999
*
         ENDIF
*
      ENDIF

      IF (ILOW(IDIM).LT.1) THEN
         IQUEST(1)=1
         GO TO 999
      ENDIF
*
      IF (IOPTBL.GT.0.OR.IOPTR.GT.0) THEN
         IF (NELEM.LT.1.OR.NELEM.GT.LENGTH(IDIM)) NELEM=LENGTH(IDIM)
      ENDIF
*
      IHIGH(IDIM)=ILOW(IDIM)+NELEM-1
      IF (IHIGH(IDIM).LT.1.OR.IHIGH(IDIM).LT.ILOW(IDIM)) THEN
         IQUEST(1)=1
         GO TO 999
      ENDIF
*             cannot read beyond limits of vector
      IF (IOPTBL.GT.0.OR.IOPTR.GT.0) THEN
         IF (IHIGH(IDIM).GT.IQ(LVECN+10+IDIM)) THEN
            IQUEST(1)=1
            GO TO 999
         ENDIF
      ENDIF
*
      JJJ=ILOW(1)+IQ(LVECN+11)*(ILOW(2)-1)+
     +    IQ(LVECN+11)*IQ(LVECN+12)*(ILOW(3)-1)
      LLOW=LVECN+14+JJJ
      LHIGH=LVECN+14+JJJ+NELEM-1
*
* CHOPT='R' - Copy from the vector to the array
*
      IF (IOPTBL.GT.0.OR.IOPTR.GT.0) THEN
         DO 40 I=1,NELEM
            X(I)=IQ(LLOW+I-1)
   40    CONTINUE
         GO TO 999
      ENDIF
*
* CHOPT='C' - Just create the vector
*
      IF (IOPTC.GT.0) THEN
         IQUEST(10+IDIM)=LENGTH(IDIM)
         IQUEST(12)=LLOW
         IQUEST(13)=LHIGH
         GO TO 999
      ENDIF
*
      IF (IOPTW.EQ.0) THEN
         CALL KUALFA
         PRINT *,'*** KUVEC: Unknown option ',CHOPT
         IQUEST(1)=1
         GO TO 999
      ENDIF
*
* CHOPT='W'
*
      IF (IOPTW.GT.0) THEN
*           only shrink/expand 1-d vectors
         IF (IDIM.EQ.1 .AND. IQ(LVECN+14).EQ.1 ) THEN
*
            ND=ILOW(IDIM)+ NELEM-1 -IQ(LVECN+10+IDIM)
*
* If there is a subscript in the vector name,
* then do not shrink even if options '-' is present
*
            IF (ND.LT.0 .AND.  ILOW(IDIM).GT.1
     +         .OR.LENTO(IDIM).LT.IQ(LVECN+10+IDIM) )  ND=0
*
* The one-dim vector must be enlarged or shrinked ?
*
            IF ((ND.GT.0).OR.(ND.LT.0.AND.IOPTMI.GT.0)) THEN
               NL=0
               CALL MZPUSH(IXKUIP,LVECN,NL,ND,' ')
               IQUEST(1)=0
               LENGTH(IDIM)=LENGTH(IDIM) + ND
               IQ(LVECN+10)=IQ(LVECN+10) + ND
               IQ(LVECN+10+IDIM)=IQ(LVECN+10+IDIM) + ND
               LLOW=LVECN+14+ILOW(IDIM)
               LHIGH=LVECN+14+IHIGH(IDIM)
            ENDIF
*
* Copy from the array to the one-dim vector
*
            DO 50 I=1,NELEM
               IQ(LLOW+I-1)=X(I)
   50       CONTINUE
*
            IQUEST(10+IDIM)=LENGTH(IDIM)
            IQUEST(12)=LLOW
            IQUEST(13)=LHIGH
*
         ELSE
*
* Copy from the array to the multi-dim vector
*
            IF (ICOPY.EQ.0) THEN
*
               DO 70 I=1,NELEM
                  IQ(LLOW+I-1)=X(I)
   70          CONTINUE
*
            ELSE
*
               DO 80 I=1,3
                  NDIM(I)=IQ(LVECN+10+I)
                  IF (LENFR(I).LE.LENTO(I)) THEN
                     ISTEPS(I)=1
                  ELSE
                     ISTEPS(I)=-1
                  ENDIF
   80          CONTINUE
               III=1
               DO 90 K=LENFR(3),LENTO(3),ISTEPS(3)
                  DO 90 J=LENFR(2),LENTO(2),ISTEPS(2)
                     DO 90 I=LENFR(1),LENTO(1),ISTEPS(1)
                        JJJ=I+NDIM(1)*(J-1)+NDIM(1)*NDIM(2)*(K-1)
                        LLL=LVECN+14+JJJ
                        IQ(LLL)=X(III)
                        III=III+1
   90          CONTINUE
*
            ENDIF
*
         ENDIF
      ENDIF
*
  999 END
