C***********************************************************************

      REAL FUNCTION ARSTRA(KF,KQ,X,XP,XQ2)

C...ARiadne function Structure Function RAtio

C...Return ratio of structure functions for given flavour, x, xp and Q^2

#include "arimpl.f"
#include "ardat1.f"
#include "arhide.f"

      REAL XPQ(-6:6),XPYST(-25:25)


      IF (MSTA(1).EQ.3) THEN
        CALL LNSTRF(X/ABS(XP),XQ2,XPQ)
        ARSTRA=XPQ(0)
        CALL LNSTRF(X,XQ2,XPQ)
        XFQ=MIN(XPQ(KQ),XPQ(-KQ))
        IF (XP.LT.0.0.AND.PARA(19).LT.0.0) THEN
          IF (XFQ.LT.-PARA(19)) THEN
            ARSTRA=-1.0
            RETURN
          ENDIF
        ENDIF
C        IF (MHAR(118).EQ.0) XFQ=MAX(XFQ,ABS(PARA(19)))
        IF (MHAR(118).EQ.0.AND.XFQ.LT.ABS(PARA(19))) THEN
          XFQ=ABS(PARA(19))
        ENDIF
        IF (XFQ.GT.0.0) THEN
          ARSTRA=ARSTRA/XFQ
        ELSE
          ARSTRA=-1.0
        ENDIF
      ELSE
        CALL PYSTFU(KF,X/ABS(XP),XQ2,XPYST)
        ARSTRA=XPYST(0)
        CALL PYSTFU(KF,X,XQ2,XPYST)
        XFQ=MIN(XPYST(KQ),XPYST(-KQ))
        IF (XP.LT.0.0.AND.PARA(19).LT.0.0) THEN
          IF (XFQ.LT.-PARA(19)) THEN
            ARSTRA=-1.0
            RETURN
          ENDIF
        ENDIF
C        IF (MHAR(118).EQ.0) XFQ=MAX(XFQ,ABS(PARA(19)))
        IF (MHAR(118).EQ.0.AND.XFQ.LT.ABS(PARA(19))) THEN
          XFQ=ABS(PARA(19))
        ENDIF
        IF (XFQ.GT.0.0) THEN
          ARSTRA=ARSTRA/XFQ
        ELSE
          ARSTRA=-1.0
        ENDIF
      ENDIF
      IF (MHAR(102).NE.2) ARSTRA=ARSTRA*XP

      RETURN

C**** END OF ARSTRA ****************************************************
      END
