#if defined(CERNLIB_PARA)
c     Arrays that hold the mommenta and particle-id of initial tracks
      integer     MAXNTR
      parameter  (MAXNTR=100)
      
      integer     ntr
      real        p1s(3,MAXNTR)
      Integer     itypes(MAXNTR)
      common   /momentid/ ntr, p1s, itypes
#endif
