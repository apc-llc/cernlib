C***********************************************************************
C $Id: arupdj.F,v 1.2 1996/04/10 12:33:42 mclareni Exp $
 
      SUBROUTINE ARUPDJ(I2,I1,I3)
 
C...ARiadne subroutine UPDate Jet entry
 
C...Takes a jet entry I2 and determines its minimum invariant pt wrt.
C...all other jets. I1 and I3 indicates which jets have been changed
C...since last call
 
#include "arimpl.f"
#include "arpart.f"
#include "ardips.f"
#include "ardat1.f"


      IF (QDONE(I2)) RETURN

      BP21=BP(I2,1)
      BP22=BP(I2,2)
      BP23=BP(I2,3)
      BP24=BP(I2,4)
      BP25=BP(I2,5)
      BP252=BX1(I2)
      B0=0.0

      IF (I1.EQ.0.OR.QDONE(IDI(I2)).OR.QDONE(IDO(I2)).OR.I2.EQ.I1.OR.
     $     I2.EQ.I3.OR.IDI(I2).EQ.I1.OR.IDO(I2).EQ.I3) THEN

C...If jet I2 has been changed or if jets wrt. which I2 has minimum pt
C...has been changed then recalculate minimum pt from scratch
        PT2IN(I2)=PARA(40)
        DO 100 J1=1,IPART-1
          IF ((.NOT.QDONE(J1)).AND.J1.NE.I2) THEN
            BP11=BP(J1,1)
            BP12=BP(J1,2)
            BP13=BP(J1,3)
            BP14=BP(J1,4)
            BP15=BP(J1,5)
            BP152=BX1(J1)
            BP1P2=MAX(BP14*BP24-BP13*BP23-BP12*BP22-BP11*BP21,B0)
            BR12=MAX(BP1P2-BP15*BP25,B0)
            DO 110 J3=J1+1,IPART
              IF ((.NOT.QDONE(J3)).AND.J3.NE.I2) THEN
                BP31=BP(J3,1)
                BP32=BP(J3,2)
                BP33=BP(J3,3)
                BP34=BP(J3,4)
                BP2P3=MAX(BP34*BP24-BP33*BP23-BP32*BP22-BP31*BP21,B0)
                BR23=MAX(BP2P3-BP25*BP(J3,5),B0)
                BP1P3=MAX(BP14*BP34-BP13*BP33-BP12*BP32-BP11*BP31,B0)
                PT2I=4.0*BR12*BR23/(BP152+BP252+BX1(J3)+
     $               2.0*(BP1P2+BP2P3+BP1P3))
                IF (PT2I.LT.PT2IN(I2)) THEN
                  PT2IN(I2)=PT2I
                  IDI(I2)=J1
                  IDO(I2)=J3
                ENDIF
              ENDIF
 110        CONTINUE
          ENDIF
 100    CONTINUE
      ELSE

C...Check all combinations involving changed jets to see if new minimum pt
C...is obtained.
        BP11=BP(I1,1)
        BP12=BP(I1,2)
        BP13=BP(I1,3)
        BP14=BP(I1,4)
        BP15=BP(I1,5)
        BP152=BX1(I1)
        BP31=BP(I3,1)
        BP32=BP(I3,2)
        BP33=BP(I3,3)
        BP34=BP(I3,4)
        BP35=BP(I3,5)
        BP352=BX1(I3)
        BP1P2=MAX(BP14*BP24-BP13*BP23-BP12*BP22-BP11*BP21,B0)
        BP2P3=MAX(BP34*BP24-BP33*BP23-BP32*BP22-BP31*BP21,B0)
        BR12=MAX(BP1P2-BP15*BP25,B0)
        BR23=MAX(BP2P3-BP25*BP35,B0)

        DO 200 J=1,IPART
          IF (J.NE.I2.AND.(.NOT.QDONE(J))) THEN
            BPJ1=BP(J,1)
            BPJ2=BP(J,2)
            BPJ3=BP(J,3)
            BPJ4=BP(J,4)
            BPJ52=BX1(J)
            BPJP2=MAX(BPJ4*BP24-BPJ3*BP23-BPJ2*BP22-BPJ1*BP21,B0)
            BRJ2=MAX(BPJP2-BP(J,5)*BP25,B0)
            IF (J.NE.I1) THEN
              BP1PJ=MAX(BP14*BPJ4-BP13*BPJ3-BP12*BPJ2-BP11*BPJ1,B0)
              PT2I=4.0*BR12*BRJ2/(BP152+BP252+BPJ52+
     $             2.0*(BP1P2+BPJP2+BP1PJ))
              IF (PT2I.LT.PT2IN(I2)) THEN
                PT2IN(I2)=PT2I
                IF (J.GT.I1) THEN
                  IDI(I2)=I1
                  IDO(I2)=J
                ELSE
                  IDI(I2)=J
                  IDO(I2)=I1
                ENDIF
              ENDIF
            ENDIF
            IF (J.NE.I3) THEN
              BPJP3=MAX(BPJ4*BP34-BPJ3*BP33-BPJ2*BP32-BPJ1*BP31,B0)
              PT2I=4.0*BRJ2*BR23/(BPJ52+BP252+BP352+
     $             2.0*(BPJP2+BP2P3+BPJP3))
              IF (PT2I.LT.PT2IN(I2)) THEN
                PT2IN(I2)=PT2I
                IF (J.LT.I3) THEN
                  IDI(I2)=J
                  IDO(I2)=I3
                ELSE
                  IDI(I2)=I3
                  IDO(I2)=J
                ENDIF
              ENDIF
            ENDIF
          ENDIF
 200    CONTINUE
      ENDIF

      RETURN
 
C**** END OF ARUPDJ ****************************************************
      END
