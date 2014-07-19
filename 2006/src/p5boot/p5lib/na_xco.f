CDECK  ID>, NA_XCO.
      FUNCTION NA_XCO (NAMEA,NAMEB,NCHA,NCHB)

C-    Compare NAMEA(1:NCH) with NAMEB(1:NCH),
C-    return zero if NAMEA = NAMEB
C-             -1 if NAMEA < NAMEB
C-             +1 if NAMEA > NAMEB
C-    internal routine called only from NA_DIFF

      CHARACTER*512  NAMEA, NAMEB

      JVAL = 0
      NCH  = MIN (NCHA,NCHB)
      IF (NCH.EQ.0)  THEN
          IF (NCHA.EQ.NCHB)        GO TO 49
          IF (NCHA.LT.NCHB)  THEN
              JVAL = -1
            ELSE
              JVAL =  1
            ENDIF
          GO TO 49
        ENDIF

      IF (NAMEA(1:NCH) .EQ. NAMEB(1:NCH))  THEN
          IF (NCHA.EQ.NCHB)        GO TO 49
          IF (NCHA.LT.NCHB)  THEN
              JVAL = -1
            ELSE
              JVAL =  1
            ENDIF
          GO TO 49
        ENDIF

      IF (LLT(NAMEA(1:NCH),NAMEB(1:NCH)))  THEN
          JVAL = -1
        ELSE
          JVAL =  1
        ENDIF

   49 NA_XCO = JVAL
      RETURN
      END
