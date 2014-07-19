*
* $Id: ctmpak.inc,v 1.2 1999/08/17 12:42:50 mclareni Exp $
*
* $Log: ctmpak.inc,v $
* Revision 1.2  1999/08/17 12:42:50  mclareni
* Y2k mods submitted by Rob Komar
*
* Revision 1.1.1.1  1996/02/28 16:24:05  mclareni
* Hepdb, cdlib, etc
*
*
#ifndef CERNLIB_HEPDB_CTMPAK_INC
#define CERNLIB_HEPDB_CTMPAK_INC
*
*
* ctmpak.inc
*
      PARAMETER       (MXSEC=61, MXMIN=61, MXHOU=25, MXDAY=32, MXMON=13)
      PARAMETER       (MXYEA=134)
      INTEGER         IDAYY(55), IDAYM(12,2)      
      SAVE            IDAYY, IDAYM
      DATA            IDAYY / 366,  731, 1096, 1461, 1827, 2192, 2557,
     1                       2922, 3288, 3653, 4018, 4383, 4749, 5114,
     2                       5479, 5844, 6210, 6575, 6940, 7305, 7671,
     3                       8036, 8401, 8766, 9132, 9497, 9862, 10227,
     4                       10593,10958,11323,11688,12054,12419,12784,
     5                       13149,13515,13880,14245,14610,14976,15341,
     6                       15706,16071,16437,16802,17167,17532,17898,
     7                       18263,18628,18993,19359,19724,20089/     
      DATA            IDAYM / 0,  31,  59,  90, 120, 151, 181, 212, 243,
     1                      273, 304, 334,   0,  31,  60,  91, 121, 152,
     2                      182, 213, 244, 274, 305, 335/
*

#endif
