*
* $Id: kuvect.F,v 1.1.1.1 1996/03/08 15:32:54 mclareni Exp $
*
* $Log: kuvect.F,v $
* Revision 1.1.1.1  1996/03/08 15:32:54  mclareni
* Kuip
*
*
#include "kuip/pilot.h"
*CMZ :  2.02/00 15/04/93  18.27.05  by  Fons Rademakers
*-- Author :
      SUBROUTINE KUVECT(VECNAM,LLOW,LHIGH)
*
********************************************************************************
*
* Get address of vector VECNAM
*
* The vector VECNAM can be accessed by Q(LLOW:LHIGH) if ITYPE=1,
* or IQ(LLOW:LHIGH) if ITYPE=2
*
* If the vector is not existing then LLOW=LHIGH=0
*
* It returns also in IQUEST :
*
* IQUEST(10) : NCHNAM (number of chars of VECNAM)
* IQUEST(11) : LENTOT (total number of elements of vector)
* IQUEST(12) : ILOW   (low index)
* IQUEST(13) : IHIGH  (high index)
* IQUEST(14) : ITYPE  (type: 1=real, 2=integer, 3=complex)
* IQUEST(15) : IVEC   (vector index, 1<=IVEC<=NUMVEC)
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
*     Input :
*
      CHARACTER*(*) VECNAM
*
*     Output :
*
      INTEGER LLOW
      INTEGER LHIGH
*
********************************************************************************
*
#include "kuip/kcmcmd.inc"
#include "kuip/kcgen.inc"
#include "kuip/kcgen1.inc"
#include "kuip/kcgen2.inc"
#include "kuip/kcques.inc"
#include "kuip/kcvect.inc"
#include "kuip/kcwork.inc"
      CHARACTER*32 VNAME,VNAM
      CHARACTER*1  TYPE
      DIMENSION LENFR(3),LENTO(3),NDIM(3),NLEN(3)
*
      VNAME=VECNAM
      CALL CLTOU(VNAME)
      NCHNAM=LENOCC(VNAME)
      I1=INDEX(VNAME,'(')
      IF (I1.GT.1) NCHNAM=I1-1
      IQUEST(10)=NCHNAM
      LLOW=0
      LHIGH=0

      IF(NUMVEC.GT.0) THEN
         LVECN=LQ(LVEC-1)
         DO 10 I=1,NUMVEC
            CALL UHTOC(IQ(LVECN+1),4,VNAM,32)
            IF(LENOCC(VNAM).EQ.NCHNAM .AND.
     +         VNAM.EQ.VNAME(1:NCHNAM)) GOTO 20
            LVECN=LQ(LVECN)
 10      CONTINUE
      ENDIF
*
*--- on the Piaf server get the vector from the client
      CALL KIVECT(VNAME(1:NCHNAM),LVECN)
      IF(LVECN.EQ.0) GOTO 999
*
20    CONTINUE
      IVEC=I
      LENTOT=IQ(LVECN+10)
      ILOW=1
      IHIGH=LENTOT

      CALL UHTOC(IQ(LVECN+9),4,TYPE,1)
      IF (TYPE.EQ.'R') THEN
        ITYPE=1
      ELSE IF (TYPE.EQ.'I') THEN
        ITYPE=2
      ELSE IF (TYPE.EQ.'C') THEN
        ITYPE=3
      ELSE
        ITYPE=0
      ENDIF

      LENFR(1)=1
      LENFR(2)=1
      LENFR(3)=1
      LENTO(1)=IQ(LVECN+11)
      LENTO(2)=IQ(LVECN+12)
      LENTO(3)=IQ(LVECN+13)
      NLEN(1)=LENTO(1)
      NLEN(2)=LENTO(2)
      NLEN(3)=LENTO(3)

      ICOPY=0
*
      IF (I1.GT.0) THEN
*
        I3=INDEX(VNAME,')')
        IF (I3.NE.LENOCC(VNAME)) GO TO 999
        NCHSUB=I3-I1-1
        IF (NCHSUB.GT.0) THEN
          CALL KIVIND(VNAME(I1+1:I3-1),NCHSUB,LENFR,LENTO)
          IF (IQUEST(1).NE.0) GO TO 999
*
* Special logic for VEC(I)   ---> VEC(I,:)   if VEC is two-dim
* Special logic for VEC(I)   ---> VEC(I,:,:) if VEC is three-dim
* Special logic for VEC(I,J) ---> VEC(I,J,:) if VEC is three-dim
*
          ICOMM1=INDEX(VNAME(I1+1:I3-1),',')
          ICOMM2=0
          IF (ICOMM1.GT.0) ICOMM2=INDEX(VNAME(I1+1+ICOMM1:I3-1),',')
          IF (IQ(LVECN+12).GT.1.AND.ICOMM1.EQ.0) LENTO(2)=IQ(LVECN+12)
          IF (IQ(LVECN+13).GT.1.AND.ICOMM2.EQ.0) LENTO(3)=IQ(LVECN+13)
*
        ENDIF
*
* Check for index out of dimensions
*
        IF (ITYPE.EQ.3) THEN
          IQ(LVECN+11)=IQ(LVECN+11)*2
          IQ(LVECN+12)=IQ(LVECN+12)*2
          IQ(LVECN+13)=IQ(LVECN+13)*2
        ENDIF
*
        DO 30 I=1,3
          IF (LENFR(I).EQ.LENTO(I)) THEN
            IF (LENFR(I).LT.1) THEN
              LENFR(I)=1
              LENTO(I)=1
              CALL KUALFA
              PRINT *,'*** Warning: Index < 1'
            ELSE IF (LENFR(I).GT.IQ(LVECN+10+I)) THEN
              CALL KUALFA
              PRINT *,'*** Warning: Index > Dimension_length'
              GO TO 999
            ENDIF
          ELSE
            IF (LENFR(I).LT.1) THEN
              LENFR(I)=1
              CALL KUALFA
              PRINT *,'*** Warning: Low_index < 1'
            ENDIF
            IF (LENFR(I).GT.IQ(LVECN+10+I)) THEN
              CALL KUALFA
              PRINT *,'*** Warning: Index > Dimension_length'
              GO TO 999
            ENDIF
            IF (LENTO(I).GT.IQ(LVECN+10+I)) THEN
              LENTO(I)=IQ(LVECN+10+I)
              CALL KUALFA
              PRINT *,'*** Warning: High_index > Dimension_length'
            ENDIF
          ENDIF
          NDIM(I)=LENTO(I)-LENFR(I)+1
30      CONTINUE
*
        IF (ITYPE.EQ.3) THEN
          IQ(LVECN+11)=IQ(LVECN+11)/2
          IQ(LVECN+12)=IQ(LVECN+12)/2
          IQ(LVECN+13)=IQ(LVECN+13)/2
        ENDIF
*
* TVECFL is set to .FALSE. by routines that has to write
* on the vector, like KXINPV, KXREAV, etc.
*
        IF (NDIM(3).NE.1) THEN
          IF (NLEN(2).NE.NDIM(2) .OR. NLEN(1).NE.NDIM(1)) THEN
            ICOPY=1
          ENDIF
        ELSEIF (NDIM(2).NE.1) THEN
          IF (NLEN(1).NE.NDIM(1)) THEN
            ICOPY=1
          ENDIF
        ENDIF
        IF (ICOPY.EQ.0) THEN
          ILOW = LENFR(1) + NLEN(1)*(LENFR(2)-1)
     +                    + NLEN(1)*NLEN(2)*(LENFR(3)-1)
          IHIGH= LENTO(1) + NLEN(1)*(LENTO(2)-1)
     +                    + NLEN(1)*NLEN(2)*(LENTO(3)-1)
          LLOW=LVECN+14+ILOW
          LHIGH=LVECN+14+IHIGH
        ELSE
*
* If vector elements are not consecutive in memory
* create a temporary bank with all elements consecutively copied:
* check first if there are free vector addresses
*
          NVADD=NVADD+1
          IF (NVADD.GT.MAXLVA) THEN
            PRINT *,'*** Too many temporary vectors'
            IQUEST(1)=1
            GO TO 999
          ENDIF
*
* Check if there is enough space
*
          ND=NDIM(1)*NDIM(2)*NDIM(3)
          LLL=50+ND
          IX=IXPAWC+1
          CALL MZNEED(IX,LLL,'G')
          IQUEST(1)=0
          IF (IQUEST(11).LT.0) THEN
            PRINT *,'*** Not enough memory to store temporary vector'
            IQUEST(1)=1
            GO TO 999
          ENDIF
*
* Book the bank in the division IXPAWC+1
*
          NL=0
          NS=0
          IOTEMP=0
          IX=IXPAWC+1
          CALL MZBOOK(IX,LVADDR(NVADD),LVADDR(NVADD),1,'TEMP',
     +                NL,NS,ND,IOTEMP,0)
*
* Copy from the vector in division IXKUIP to the temporary one in IXPAWC+1
*
          III=0
          DO 40 K=LENFR(3),LENTO(3)
            DO 40 J=LENFR(2),LENTO(2)
              DO 40 I=LENFR(1),LENTO(1)
                III=III+1
                JJJ=I+IQ(LVECN+11)*(J-1)+IQ(LVECN+11)*IQ(LVECN+12)*(K-1)
                IQ(LVADDR(NVADD)+III)=IQ(LVECN+14+JJJ)
40        CONTINUE
          ILOW=1
          IHIGH=III
          LLOW=LVADDR(NVADD)+1
          LHIGH=LLOW+III-1
        ENDIF
*
      ELSE
*
        IF (ILOW.GT.LENTOT) THEN
          ILOW=LENTOT
          CALL KUALFA
          PRINT *,'*** Warning: Low_index > vector_length'
        ENDIF
        IF (IHIGH.LT.1) THEN
          IHIGH=1
          CALL KUALFA
          PRINT *,'*** Warning: High_index < 1'
        ENDIF
        IF (IHIGH.GT.LENTOT) THEN
          IHIGH=LENTOT
          CALL KUALFA
          PRINT *,'*** Warning: High_index > vector_length'
        ENDIF

        LLOW=LVECN+14+ILOW
        LHIGH=LVECN+14+IHIGH

      ENDIF

      IQUEST(10)=NCHNAM
      IQUEST(11)=LENTOT
      IQUEST(12)=ILOW
      IQUEST(13)=IHIGH
      IQUEST(14)=ITYPE
      IQUEST(15)=IVEC

      IQUEST(20)=ICOPY
      IQUEST(21)=LENFR(1)
      IQUEST(22)=LENFR(2)
      IQUEST(23)=LENFR(3)
      IQUEST(31)=LENTO(1)
      IQUEST(32)=LENTO(2)
      IQUEST(33)=LENTO(3)

      IF (VNAME(1:NCHNAM).EQ.'?') THEN
        I1=LOCF(VECTOR(ILOW))
        I2=LOCF(Q(LLOW))
        LLOW=LLOW+I1-I2
***        LHIGH=LLOW+LENTOT-1
        LHIGH=LLOW+IHIGH-ILOW
      ENDIF

999   END



