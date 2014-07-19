*
* $Id: hihist.inc,v 1.2 2001/10/17 14:36:53 couet Exp $
*
* $Log: hihist.inc,v $
* Revision 1.2  2001/10/17 14:36:53  couet
* - The WORK buffers were too small and the follwoing macro didn't not
*   work for more that 101 points:
*
*   vec/del *
*   n = [1]
*   vec/cr time([n])
*   vec/cr etime([n]) r [n]*0.0
*   vec/cr y([n]) r
*   vec/cr ey([n]) r [n]*0.7
*   do i = 1, [n]
*     vec/input time([i]) [i]
*     vec/input y([i]) $eval(3.0+0.01*[i])
*   enddo
*   null -10 160 0 10
*   hplot/errors time y etime ey [n] 20 0.1 3
*
* Revision 1.1.1.1  1996/02/14 13:10:15  mclareni
* Higz
*
*
#ifndef CERNLIB_HIGZ_HIHIST_INC
#define CERNLIB_HIGZ_HIHIST_INC
*
*
* hihist.inc
*
*CMZ :  1.18/00 23/04/93  17.27.57  by  O.Couet
*-- Author :
      COMMON /HIHIST/ XWORK(1004),YWORK(1004),XWORKL(1004),YWORKL(1004)
#endif
