C-       previous ISJT banks are dropped
C-
C-       Expects calorimeter information in Zebra banks ISCL
C-
C-       Uses a simple jet-finding algorithm (similar to UA1):
C-        find highest remaining cell with ET>ETSTOP and sum
C-        surrounding cells with--
C-            DELTA(Y)**2+DELTA(PHI)**2<RJET**2
C-            ET>ECCUT.
C-          keep jets with ET>EJCUT.
C-          the UA1 parameters are RJET=1.0 and EJCUT=5.0
C-
C-    INPUT:
C-    RJET  = size of Y, PHI region for jet
C-    EJCUT = minimum jet ET
C-    ECCUT = minimum ET for cell to be added to jet
C-    ETSTOP= minimum ET in a cell for seed
C-    NJMAX= maximum number of jets
C-
C-    OUTPUT:
C-    NCJET= number of jets found
C-
C-                       SDP May,1986
C--------------------------------------------------------------------
#if defined(CERNLIB_IMPNONE)
      IMPLICIT NONE
#endif
#include "isajet/itapes.inc"
      INTEGER GZISJS
      REAL TWOPI
      PARAMETER (TWOPI=6.283185)
#include "isajet/izisac.inc"
#include "isajet/izisae.inc"
#include "isajet/iziscl.inc"
#include "isajet/zebcom.inc"
#include "isajet/zlinka.inc"
      CHARACTER*64 MESG
      REAL RJET,EJCUT
      INTEGER NJMAX,NCJET,NLISJT
      REAL ECCUT,ETSTOP,PHIMAX,ETAMAX,PHI,ETA,MASSQ
      INTEGER LISAC,LISCL,LISJT,GZISJT
      INTEGER IPASS,NCLS,I,GZISAC
      REAL EEM,EHAD,ETMAX,SNTH,CSTH,SNPH,CSPH,E,RSQ,RJSQ
      REAL PCJET(4),ETJET,ET,PHI1,THETA,PTOT
      REAL ETCMAX(50)
      LOGICAL FIRST
      SAVE NLISJT,FIRST
      DATA NLISJT/0/
      DATA FIRST/.TRUE./
C-----------------------------------------------------------------------
C
      IF(FIRST) THEN
        CALL GSLINK('ISZJET',NLISJT)  ! reserve a link for ISJT
        FIRST=.FALSE.
      ENDIF
      LISJT=GZISJT()
      IF(LISJT.NE.0) CALL MZDROP(IXCOM,LISJT,'L')
      RJSQ=RJET**2
      LISJT=0
      NCJET=0
      IPASS=0
      LISAC=GZISAC()
      LISCL=LISAC-IZISCL
  100 LISCL=LQ(LISCL)         ! zero any jet pointers
      IF(LISCL.GT.0.) THEN
C
        LQ(LISCL-2)=0
        GOTO 100
      ENDIF
C
C          find highest cell remaining
C
1     ETMAX=0.
      NCLS=0
      LISCL=LISAC-IZISCL
  200 LISCL=LQ(LISCL)         ! loop over unused cells
      NCLS=NCLS+1
      IF(LISCL.GT.0.) THEN
        IF(LQ(LISCL-2).EQ.0) THEN
          EEM=Q(LISCL+3)
          EHAD=Q(LISCL+4)
          SNTH=Q(LISCL+5)
          ET=(EEM+EHAD)*SNTH
          IF(ET.GT.ETMAX) THEN
            ETMAX=ET
            PHIMAX=Q(LISCL+9)
            ETAMAX=Q(LISCL+10)
          ENDIF
        ENDIF
C
        GOTO 200
      ENDIF
      IF(ETMAX.LT.ETSTOP) GOTO 999 !  no more jets to be found
C
C          errors
C
      IPASS=IPASS+1
      IF(IPASS.GT.NCLS) THEN
        WRITE(ITLIS,88) IPASS
  88    FORMAT(' ERROR IN ISZJET...IPASS > ',I6)
        RETURN
      ENDIF
      NCJET=NCJET+1
      IF(NCJET.GT.NJMAX) THEN
        WRITE(ITLIS,99) NCJET
  99    FORMAT(' ERROR IN ISZJET...NCJET > ',I5)
        RETURN
      ENDIF
C
C        Start a new jet
C
      LSLINK(NLISJT)=LISJT
      ETCMAX(NCJET)=0
      CALL BKISJT(LISJT)
      NCLS=0
      CALL VZERO(PCJET,4)
      LISCL=LISAC-IZISCL
  300 LISCL=LQ(LISCL)         ! loop over unused cells
      NCLS=NCLS+1
      IF(LISCL.GT.0) THEN
C
        IF(LQ(LISCL-2).EQ.0) THEN
          EEM=Q(LISCL+3)
          EHAD=Q(LISCL+4)
          SNTH=Q(LISCL+5)
          ET=(EEM+EHAD)*SNTH
          PHI=Q(LISCL+9)
          ETA=Q(LISCL+10)
          RSQ=(PHI-PHIMAX)**2+(ETA-ETAMAX)**2
C
          IF(ET.GT.ECCUT.AND.RSQ.LT.RJSQ) THEN  ! add cell to jet
            E=EEM+EHAD
            CSTH=Q(LISCL+6)
            SNPH=Q(LISCL+7)
            CSPH=Q(LISCL+8)
            PCJET(1)=PCJET(1)+E*SNTH*CSPH
            PCJET(2)=PCJET(2)+E*SNTH*SNPH
            PCJET(3)=PCJET(3)+E*CSTH
            PCJET(4)=PCJET(4)+E
            LQ(LISCL-2)=LISJT
            IF(ET.GT.ETCMAX(NCJET)) THEN  ! find cell with max. ET contrib.
              ETCMAX(NCJET)=ET
              LQ(LISJT-1)=LQ(LISCL-1)   ! reference to parent parton
            ENDIF
          ENDIF
C
        ENDIF
C
        GOTO 300
      ENDIF
C
C          discard jet if ET < EJCUT, fill bank otherwise.
C
      ETJET=SQRT(PCJET(1)**2+PCJET(2)**2)
      IF(ETJET.LT.EJCUT) THEN
        NCJET=NCJET-1
        CALL MZDROP(IXMAIN,LISJT,' ')
        LISJT=LSLINK(NLISJT)
      ELSE
        IQ(LISJT-5)=NCJET
        Q(LISJT+1)=ETJET                  ! jet ET
        CALL UCOPY(PCJET,Q(LISJT+2),4)    ! jet 4-momenta
        MASSQ=PCJET(4)**2-PCJET(1)**2-PCJET(2)**2-PCJET(3)**2
        Q(LISJT+6)=0
        IF(MASSQ.GT.0) Q(LISJT+6)=SQRT(MASSQ)   ! jet mass
C   calculate PHI1,theta and eta
          PHI1=ATAN2(PCJET(2),PCJET(1))
          IF(PHI1.LT.0) PHI1=PHI1+TWOPI
          PTOT=SQRT(PCJET(1)**2+PCJET(2)**2+PCJET(3)**2)
          THETA=ACOS(PCJET(3)/PTOT)
          ETA=-ALOG(TAN(THETA/2.))
          Q(LISJT+7)=PHI1
          Q(LISJT+8)=THETA
          Q(LISJT+9)=ETA
      ENDIF
      GO TO 1
C
  999 RETURN
      END
