*
* $Id: isapfl.F,v 1.11 2001/10/08 14:03:17 mclareni Exp $
*
* $Log: isapfl.F,v $
* Revision 1.11  2001/10/08 14:03:17  mclareni
* Version 7.58 from author
*
*
#include "sys/CERNLIB_machine.h"
#include "pilot.h"
      SUBROUTINE ISAPFL(NPART,NVERTX)
C-----------------------------------------------------------------------
C-                                                                     -
C-                                                                     -
C-      Search for stable and unstable particles                       -
C-      fill particle Zebra banks ISP1                                 -
C_      generate short lived vertices                                  -
C-      fill vertex Zebra banks ISV1                                   -
C-      NOTE: if number of decay vertices exceeds 100 results can      -
C-            be incorrect                                             -
C-                                                                     -
C-       OUTPUT:                                                       -
C-       NPART  = number of stable particles                           -
C-       NVERTX = number of vertices                                   -
C-                                                                     -
C-       WRITTEN BY SDP Dec.,1985, rev. Dec,1986                       -
C-   Updated  13-DEC-1989   Serban D. Protopopescu
C-                                                                     -
C-----------------------------------------------------------------------
C
C
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
C
#include "jetset.inc"
#include "partcl.inc"
#include "primar.inc"
#include "izisv1.inc"
#include "izisp1.inc"
#include "zebcom.inc"
#include "isalnk.inc"
C
      INTEGER ITAG(4000)
      EQUIVALENCE(ITAG,ZSTOR)
      INTEGER NVERTX,NPART
      REAL XYZB(3),XYZ(3,NVD),MOM(5)
      INTEGER I,ID,IV,LVP,IVT1,IVT2,IPREV
      INTEGER IOISP1,IOISV1
      INTEGER JET,LISV1,LISP1,LISAQ,LISAJ
      INTEGER JSPIN,K,IFL,JOR,JP,IWPOS,IWNEG
      INTEGER NJT,NQS
      REAL PHI1,THETA,ETA,PTOT
      LOGICAL FIRST,PEDIT,ADDQ,BOTDIQ
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C
      NPART=0
      NVERTX=0
      IF(NPTCL.NE.0) THEN
C
C  First make a Zebra bank for primary vertex
        CALL INTVTX(XYZ)       ! generate a primary vertex
        CALL VZERO(MOM,3)
        MOM(4)=ECM
        MOM(5)=ECM
        ID=-1
        NVERTX=1
        CALL BKISV1(NVERTX,LISV1)
        IQ(LISV1-5)=1
        IQ(LISV1+1)=ID
        CALL UCOPY(MOM,Q(LISV1+2),5)
        CALL UCOPY(XYZ,Q(LISV1+7),3)
        LQ(LISV1-2)=0                !  no parent vertex
C
C   loop over all particles to generate short lived vertices
C   and flag which particles belong to each vertex
C
        CALL MZWORK(IXCOM,ITAG,ITAG(NPTCL),0)  ! create array for flags
        DO 30 I=1,NPTCL
   30   ITAG(I)=1
C
        DO 50 I=1,NPTCL
C
          IF(IDCAY(I).EQ.0) GOTO 50
          IVT1=IDCAY(I)/IPACK
          IVT2=MOD(IDCAY(I),IPACK)
          IF(ITAG(I).NE.1) THEN     ! tag granddaughters
            DO 41 K=IVT1,IVT2
   41       ITAG(K)=ITAG(I)
          ENDIF
          IF(IDENT(I).EQ.IDENT(IVT1)) GOTO 50  ! ISAJET idiosyncracy
C
          ID=IABS(IDENT(I))
          IF(IDENT(I).EQ. 80) IWPOS=I    ! tag W+
          IF(IDENT(I).EQ.-80) IWNEG=I    ! tag W-
          JSPIN=MOD(ID,10)
          IFL=MOD(ID,100)/10
C
C   check for ground state heavy flavors or taus
          IF(((JSPIN.EQ.0.AND.IFL.GT.3).OR.(ID.EQ.16))
     &      .AND.(ID.NE.80)) THEN
            IV=ITAG(I)
            LVP=LVD(IV)
            NVERTX=NVERTX+1
            CALL UCOPY(XYZ(1,IV),XYZB,3)
C                  generate decay vertex
            CALL GENVTX(PPTCL(1,I),IDENT(I),XYZB,XYZ(1,NVERTX))
            CALL BKISV1(NVERTX,LISV1)
            IQ(LISV1+1)=IDENT(I)
            CALL UCOPY(PPTCL(1,I),Q(LISV1+2),5)
            CALL UCOPY(XYZ(1,NVERTX),Q(LISV1+7),3)
            LQ(LISV1-2)=LVP     !  reference link to parent vertex
C
            DO 42 K=IVT1,IVT2
   42       ITAG(K)=NVERTX      ! tag daughter particles
          ENDIF
C
   50   CONTINUE
C
C  fill now all ISP1 banks
        IF(FIRST) CALL MZFORM('ISP1','1I-F',IOISP1)  ! format for ISP1
        CALL ISNUMQ(NJT,NQS)
        NQS=NJSET            ! use total number of partons
C
        DO 100 I=1,NPTCL       ! loop over all particles
C
          ID=IABS(IDENT(I))
C           check for bottom di-quarks
          BOTDIQ=.FALSE.
          IF(MOD(ID/100,10).EQ.5) BOTDIQ=.TRUE.
C
C          move partons to parton banks
          IF((ID.LT.100.AND.ID.NE.10.AND.ID.NE.20).OR.BOTDIQ) THEN
            IF(I.EQ.IWPOS.OR.I.EQ.IWNEG) THEN ! add W to ISAJ
              NJT=NJT+1
              CALL BKISAJ(LISAJ)
              IQ(LISAJ+1)=IDENT(I)                  ! particle id
              CALL UCOPY(PPTCL(1,I),Q(LISAJ+2),5)   ! momenta and mass
C
C   calculate PHI1,THETA and ETA
              CALL ISPETA(PPTCL(1,I),THETA,PHI1,ETA)
              Q(LISAJ+7)=PHI1
              Q(LISAJ+8)=THETA
              Q(LISAJ+9)=ETA
C   set pointers
              PQREF(NJT)=LISAJ
              JOR=IABS(IORIG(I))
              JET=JOR/IPACK
              LQ(LISAJ-1)=PQREF(JET)
              IORIG(I)=-NJT*IPACK          ! remake ORIG to point to ISAJ
            ENDIF
C
            IVT1=IDCAY(I)/IPACK
            IF(IDENT(I).EQ.IDENT(IVT1)) GOTO 100  ! ISAJET idiosyncracy
C
            IF(ID.LT.17.OR.BOTDIQ) THEN
C                  add this parton to ISAQ
              JOR=IABS(IORIG(I))
              JP=MOD(JOR,IPACK)
              ADDQ=.TRUE.
C                check that leptons are from direct W decays
C                and not on parton list already
              IF(ID.GT.10.AND.ID.LT.17) THEN
                ADDQ=.FALSE.
                IF(IDENT(I).EQ.IDENT(JP).AND.IORIG(I).GT.0)
     &            ADDQ=.TRUE.
              ENDIF
              IF(ADDQ) THEN
                IF(IORIG(I).GT.0) THEN   ! trace back ancestry if needed
   81             JOR=IABS(IORIG(JP))
                  IF(IORIG(JP).GT.0) THEN
                    JP=MOD(JOR,IPACK)
                    GOTO 81
                  ENDIF
                  JP=MOD(JOR,IPACK)
                ENDIF
                JET=JOR/IPACK
                CALL BKISAQ(LISAQ)
                NQS=NQS+1
                IQ(LISAQ+1)=IDENT(I)                  ! particle id
                CALL UCOPY(PPTCL(1,I),Q(LISAQ+2),5)   ! momenta and mass
C
C   calculate PHI1,THETA and ETA
                CALL ISPETA(PPTCL(1,I),THETA,PHI1,ETA)
                Q(LISAQ+7)=PHI1
                Q(LISAQ+8)=THETA
                Q(LISAQ+9)=ETA
                LQ(LISAQ-1)=PQREF(JET)
                IORIG(I)=-(JET*IPACK+NQS)   ! remake ORIG to point to ISAQ
                QREF(NQS)=LISAQ
              ENDIF
C
            ENDIF
C
          ENDIF
C
C     put stable particles in particle banks
          IF(IDCAY(I).EQ.0) THEN
            PTOT=SQRT(PPTCL(1,I)**2+PPTCL(2,I)**2+PPTCL(3,I)**2)
C
C           Keep only selected particles with non-0 momentum
            IF(PEDIT(IDENT(I),PPTCL(1,I)).AND.PTOT.GT.0) THEN
              IV=ITAG(I)
              LISV1=LVD(IV)
              CALL MZBOOK(IXMAIN,LISP1,LISV1,-IZISP1,
     $                  'ISP1',5,1,9,IOISP1,-1)
              JOR=IABS(IORIG(I))
C   set reference link to parent parton
              JP=MOD(JOR,IPACK)
              IF(IORIG(I).GT.0) THEN   ! trace back ancestry if needed
   91           JOR=IABS(IORIG(JP))
                IF(IORIG(JP).GT.0) THEN
                  JP=MOD(JOR,IPACK)
                  GOTO 91
                ENDIF
                JP=MOD(JOR,IPACK)
              ENDIF
              IF(JP.GT.0) LQ(LISP1-2)=QREF(JP)
              JET=JOR/IPACK
              LQ(LISP1-3)=PQREF(JET)
C
C   fill Zebra bank
              NPART=NPART+1
              IQ(LISP1-5)=NPART                     ! impose part. no.
              IQ(LISP1+1)=IDENT(I)                  ! particle id
              CALL UCOPY(PPTCL(1,I),Q(LISP1+2),5)   ! momenta and mass
C
C   calculate PHI1,THETA and ETA
              CALL ISPETA(PPTCL(1,I),THETA,PHI1,ETA)
              Q(LISP1+7)=PHI1
              Q(LISP1+8)=THETA
              Q(LISP1+9)=ETA
            ENDIF
C
          ENDIF
  100   CONTINUE
        FIRST=.FALSE.
C
C   renumber vertices in sequence
        NVERTX=0
        LISV1=LVD(1)
  110   NVERTX=NVERTX+1
        IQ(LISV1-5)=NVERTX        ! impose a vertex number
        LISV1=LQ(LISV1)
        IF(LISV1.NE.0) GOTO 110
      ENDIF
C
C
      RETURN
      END
