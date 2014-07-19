/*
 * $Id: pawlunf.h,v 1.1.1.1 1996/03/01 11:39:13 mclareni Exp $
 *
 * $Log: pawlunf.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:13  mclareni
 * Paw
 *
 */
/*CMZ :  2.02/00 20/04/93  18.20.45  by  Fons Rademakers*/
/*-- Author :*/
/* interface to PAWLUN common block (defined in PAW) */
extern struct {
   long lunit[128];
   long lunchn;
} pawlun;

