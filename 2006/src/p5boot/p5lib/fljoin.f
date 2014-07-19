CDECK  ID>, FLJOIN.
      SUBROUTINE FLJOIN (IXDIR,IXFN,IXEXT,IXFILE)

C-    Compose a path name from the 3 parts, return its index
C.    started 17-may-95

      COMMON /SLATE/ NDSLAT,NESLAT,NFSLAT,NGSLAT,NUSLAT(2),DUMMY(34)
      CHARACTER       SLLINE*512, SLERRM*256
      COMMON /SLATLN/ SLLINE, SLERRM
C--------------    End CDE              --------------------------------


      N = 1
      IF (IXDIR.NE.0)  THEN
          CALL NA_GET (IXDIR, SLLINE,1)
          N = NESLAT
        ENDIF
      IF (IXFN.NE.0)  THEN
          CALL NA_GET (IXFN, SLLINE,N)
          N = NESLAT
        ENDIF
      IF (IXEXT.NE.0)  THEN
          CALL NA_GET (IXEXT, SLLINE,N)
          N = NESLAT
        ENDIF
      IXFILE = NA_LONG (SLLINE(1:N-1))
      RETURN
      END
