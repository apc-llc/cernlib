#ifndef CERNLIB_FATMEN_DTMPAK_INC
#define CERNLIB_FATMEN_DTMPAK_INC
*
* dtmpak.inc
*
      PARAMETER       (MXSEC=61, MXMIN=61, MXHOU=25, MXDAY=32, MXMON=13)
*          From 1980 to 2010 ==> 31 Years
      PARAMETER       (MXYER=31, ITPLY=1)
      INTEGER         IDAYY(MXYER), IDAYM(12,2)
      SAVE            IDAYY, IDAYM
      DATA            IDAYY / 366,  731, 1096, 1461, 1827, 2192, 2557,
     1                       2922, 3288, 3653, 4018, 4383, 4749, 5114,
     2                       5479, 5844, 6210, 6575, 6940, 7305, 7671,
     3                       8036, 8401, 8766, 9132, 9497, 9682,10227,
     4                      10593,10958,11323/
      DATA            IDAYM / 0,  31,  59,  90, 120, 151, 181, 212, 243,
     1                      273, 304, 334,   0,  31,  60,  91, 121, 152,
     2                      182, 213, 244, 274, 305, 335/
*
#endif
