CDECK  ID>, DPLMSQ.
      SUBROUTINE DPLMSQ (IXSEQ)

C-    LIST "missing sequence" warning for seq. IXSEQ
C.    started 10-feb-92

      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      COMMON /MUSEBC/ MX_FORC, MU_GLOB, MU_PAT, MU_DECK, MU_INH, MU_FORG
     +,               MX_TRAN, MX_FORG, MX_SINH, MX_SELF, NVEXDK(6)
C--------------    End CDE              --------------------------------
      CHARACTER    MSG*56
      EQUIVALENCE (MSG,SLERRM)


      MSG = 'Missing sequence Z='
C-           _:.=+=.: 1_:.=+=.: 2
      CALL NA_GET (IXSEQ,MSG,20)
      N = NESLAT - 1

      JSEV = 0
      NSP  = 1
      IF (NVEXDK(2).NE.0)  NSP= -1

      CALL FAILMSG (JSEV,NSP, 0,MSG(1:N))
      RETURN
      END
