/*
 * $Id: kfor.h,v 1.4 1998/12/17 14:20:22 couet Exp $
 *
 * $Log: kfor.h,v $
 * Revision 1.4  1998/12/17 14:20:22  couet
 * - new routine kxdsig to delete the temporary vectors created
 *  by $SIGMA. On NT a direct call to kuvdel didn't work
 *
 * Revision 1.3  1997/09/02 14:49:59  mclareni
 * WINNT corrections
 *
 * Revision 1.2  1997/03/14 17:16:37  mclareni
 * WNT mods
 *
 * Revision 1.1.1.1.2.1  1997/01/21 11:33:02  mclareni
 * All mods for Winnt 96a on winnt branch
 *
 * Revision 1.1.1.1  1996/03/08 15:33:00  mclareni
 * Kuip
 *
 */
/* kfor.h: Fortran-C interface */

/*
 * Fortran data types
 */
typedef  int      INTEGER;
typedef  int      LOGICAL;
typedef  float    REAL;
typedef  double   DBLPREC;
typedef  struct { REAL re; REAL im; } COMPLEX;

typedef  INTEGER        INT_FUNCTION();
typedef  INT_FUNCTION  *INT_FUNCPTR;

#if defined(CERNLIB_WINNT) || defined(WIN32)
#  ifdef CERNLIB_MSSTDCALL
#   ifndef F77_EXTERN_UPPERCASE 
#     define F77_EXTERN_UPPERCASE
#   endif
#   define  type_of_call _stdcall
#  endif
#endif
 
#ifndef type_of_call
#define type_of_call
#endif

typedef  void         (*SUBRPTR)();
typedef  void   type_of_call      SUBROUTINE();

#ifdef   IBM370
#pragma  linkage(SUBROUTINE,FORTRAN)
#pragma  linkage(INT_FUNCTION,FORTRAN)

#pragma  map(__CTOF,"@@CTOF")
extern INTEGER __CTOF( INT_FUNCPTR, ... );
#endif

typedef union _EQUIV_INT_REAL {
  INTEGER i;
  LOGICAL l;
  REAL    r;
} EQUIV_INT_REAL;


/*
 * Mapping of C-routine name for Fortran CALL SUB
 *
 *   #define F77_EXTERN_LOWERCASE   ==>  void sub()
 *   #define F77_EXTERN_UPPERCASE   ==>  void SUB()
 *           otherwise              ==>  void sub_()
 */
#ifdef   F77_EXTERN_UPPERCASE
#  define  F77_NAME(name,NAME) NAME
#else
#  ifdef   F77_EXTERN_LOWERCASE
#    define  F77_NAME(name,NAME) name
#  else
#    define  F77_NAME(name,NAME) ConCat(name,_)
#  endif
#endif

#ifndef  F77_BLOCK
#  define  F77_BLOCK(name,NAME) F77_NAME(name,NAME)
#endif

#ifndef  F77_COMMON
#  define  F77_COMMON(name) name
#endif


/*
 * Routine address in CALL SUB(FUN) ; EXTERNAL FUN
 *
 *   #define F77_EXTERN_INDIRECT    ==>  void (**fun)();
 *           otherwise              ==>  void (*fun)();
 */
#ifdef F77_EXTERN_INDIRECT
#  define F77_EXTERN_ARG(e) ConCat(e,_ptr)
#  define F77_EXTERN_DCL(e) SUBROUTINE **ConCat(e,_ptr);
#  define F77_EXTERN_DEF(e) SUBROUTINE *e = *ConCat(e,_ptr);
#else
#  define F77_EXTERN_ARG(e) e
#  define F77_EXTERN_DCL(e) SUBROUTINE *e;
#  define F77_EXTERN_DEF(e)
#endif

#define F77_EXTERN2ARG(e1,e2) F77_EXTERN_ARG(e1),F77_EXTERN_ARG(e2)
#define F77_EXTERN2DCL(e1,e2) F77_EXTERN_DCL(e1) F77_EXTERN_DCL(e2)
#define F77_EXTERN2DEF(e1,e2) F77_EXTERN_DEF(e1) F77_EXTERN_DEF(e2)

#define F77_EXTERN3ARG(e1,e2,e3) F77_EXTERN_ARG(e1),F77_EXTERN2ARG(e2,e3)
#define F77_EXTERN3DCL(e1,e2,e3) F77_EXTERN_DCL(e1) F77_EXTERN2DCL(e2,e3)
#define F77_EXTERN3DEF(e1,e2,e3) F77_EXTERN_DEF(e1) F77_EXTERN2DEF(e2,e3)

#define F77_EXTERN4ARG(e1,e2,e3,e4) F77_EXTERN_ARG(e1),F77_EXTERN3ARG(e2,e3,e4)
#define F77_EXTERN4DCL(e1,e2,e3,e4) F77_EXTERN_DCL(e1) F77_EXTERN3DCL(e2,e3,e4)
#define F77_EXTERN4DEF(e1,e2,e3,e4) F77_EXTERN_DEF(e1) F77_EXTERN3DEF(e2,e3,e4)


#ifdef F77_ARG_CONSTANT

/*
 * If the Fortran compiler (e.g. VSFORTRAN and Convex fc without -sa option)
 * uses constant argument blocks we have to make a private copy in case the
 * routine uses the arguments as local variables.
 */

#  define F77_XXXX_ARG_PTR(t,x) ConCat(x,_ptr)
#  define F77_XXXX_ARG_DCL(t,x) t *ConCat(x,_ptr);
#  define F77_XXXX_ARG_DEF(t,x) t *x = ConCat(x,_ptr);

#else

#  define F77_XXXX_ARG_PTR(t,x) x
#  define F77_XXXX_ARG_DCL(t,x) t *x;
#  define F77_XXXX_ARG_DEF(t,x)

#endif

#define F77_REAL_ARG_PTR(r) F77_XXXX_ARG_PTR(REAL,r)
#define F77_REAL_ARG_DCL(r) F77_XXXX_ARG_DCL(REAL,r)
#define F77_REAL_ARG_DEF(r) F77_XXXX_ARG_DEF(REAL,r)

#define F77_INTG_ARG_PTR(i) F77_XXXX_ARG_PTR(INTEGER,i)
#define F77_INTG_ARG_DCL(i) F77_XXXX_ARG_DCL(INTEGER,i)
#define F77_INTG_ARG_DEF(i) F77_XXXX_ARG_DEF(INTEGER,i)

#define F77_INTG_ARG2PTR(i1,i2) F77_INTG_ARG_PTR(i1),F77_INTG_ARG_PTR(i2)
#define F77_INTG_ARG2DCL(i1,i2) F77_INTG_ARG_DCL(i1) F77_INTG_ARG_DCL(i2)
#define F77_INTG_ARG2DEF(i1,i2) F77_INTG_ARG_DEF(i1) F77_INTG_ARG_DEF(i2)

#define F77_INTG_ARG3PTR(i1,i2,i3) F77_INTG_ARG_PTR(i1),F77_INTG_ARG2PTR(i2,i3)
#define F77_INTG_ARG3DCL(i1,i2,i3) F77_INTG_ARG_DCL(i1) F77_INTG_ARG2DCL(i2,i3)
#define F77_INTG_ARG3DEF(i1,i2,i3) F77_INTG_ARG_DEF(i1) F77_INTG_ARG2DEF(i2,i3)

#define F77_INTG_ARG4PTR(i1,i2,i3,i4) F77_INTG_ARG_PTR(i1), \
                                      F77_INTG_ARG3PTR(i2,i3,i4)
#define F77_INTG_ARG4DCL(i1,i2,i3,i4) F77_INTG_ARG_DCL(i1) \
                                      F77_INTG_ARG3DCL(i2,i3,i4)
#define F77_INTG_ARG4DEF(i1,i2,i3,i4) F77_INTG_ARG_DEF(i1) \
                                      F77_INTG_ARG3DEF(i2,i3,i4)


/*
 * Access to Fortran CHARACTER arguments
 */

#ifdef vms                      /* VMS string descriptors */


#  define F77_CHAR_ARG_PTR(s) ConCat(s,_ptr)
#  define F77_CHAR_ARG_LEN(s)
#  define F77_CHAR_ARG_DCL(s) struct dsc$descriptor_s *ConCat(s,_ptr);
#  define F77_CHAR_ARG_DEF(s) char *s = ConCat(s,_ptr)->dsc$a_pointer; \
                   int ConCat(len_,s) = ConCat(s,_ptr)->dsc$w_length;

#  define F77_CHAR_DEF_DSC(s,p,l) struct dsc$descriptor_s ConCat(s,_dsc);
#  define F77_CHAR_ASS_DSC(s,p,l) ConCat(s,_dsc).dsc$w_length = l; \
                                  ConCat(s,_dsc).dsc$b_dtype = DSC$K_DTYPE_T;\
                                  ConCat(s,_dsc).dsc$b_class = DSC$K_CLASS_S;\
                                  ConCat(s,_dsc).dsc$a_pointer = (char*)p;
#  define F77_CHAR_USE_PTR(s,p,l) &ConCat(s,_dsc)
#  define F77_CHAR_USE_LEN(s,p,l)


#else
#ifdef CRAY                     /* Cray string descriptors */


#  define F77_CHAR_ARG_PTR(s) ConCat(s,_ptr)
#  define F77_CHAR_ARG_LEN(s)
#  define F77_CHAR_ARG_DCL(s) _fcd ConCat(s,_ptr);
#  define F77_CHAR_ARG_DEF(s) char *s = _fcdtocp(ConCat(s,_ptr)); \
                   int ConCat(len_,s) = _fcdlen(ConCat(s,_ptr));

#  define F77_CHAR_DEF_DSC(s,p,l)
#  define F77_CHAR_ASS_DSC(s,p,l)
#  define F77_CHAR_USE_PTR(s,p,l) _cptofcd(p,l)
#  define F77_CHAR_USE_LEN(s,p,l)


#else                           /* length passed as additional argument */


# ifdef F77_CHAR_LEN_IND        /* length passed by reference */
#  define F77_CHAR_LEN_STAR(len) *len
# else
#  define F77_CHAR_LEN_STAR(len)  len
# endif

# ifndef F77_CHAR_LEN_TYPE
#  define F77_CHAR_LEN_TYPE int
# endif


# ifdef F77_ARG_CONSTANT

#  define F77_CHAR_ARG_PTR(s) ConCat(s,_ptr)
#  define F77_CHAR_ARG_LEN(s) , ConCat(s,_len)
#  define F77_CHAR_ARG_DCL(s) char *ConCat(s,_ptr); \
          F77_CHAR_LEN_TYPE F77_CHAR_LEN_STAR(ConCat(s,_len));
#  define F77_CHAR_ARG_DEF(s) char *s = ConCat(s,_ptr); \
       int ConCat(len_,s) = F77_CHAR_LEN_STAR(ConCat(s,_len));

# else
# ifdef F77_CHAR_LEN_IND

#  define F77_CHAR_ARG_PTR(s) s
#  define F77_CHAR_ARG_LEN(s) , ConCat(s,_len)
#  define F77_CHAR_ARG_DCL(s) char *s; \
          F77_CHAR_LEN_TYPE F77_CHAR_LEN_STAR(ConCat(s,_len));
#  define F77_CHAR_ARG_DEF(s) \
       int ConCat(len_,s) = F77_CHAR_LEN_STAR(ConCat(s,_len));

# else

#  ifdef CERNLIB_MSSTDCALL
#    define F77_CHAR_ARG_PTR(s) s, ConCat(len_,s)
#    define F77_CHAR_ARG_LEN(s)
#  else
#    define F77_CHAR_ARG_PTR(s) s
#    define F77_CHAR_ARG_LEN(s) , ConCat(len_,s)
#  endif

#  define F77_CHAR_ARG_DCL(s) char *s; int ConCat(len_,s);
#  define F77_CHAR_ARG_DEF(s)

# endif
# endif


# if defined(F77_CHAR_LEN_IND)

#  define F77_CHAR_DEF_DSC(s,p,l) F77_CHAR_LEN_TYPE ConCat(s,_dsc) = l;
#  define F77_CHAR_ASS_DSC(s,p,l)
#  define F77_CHAR_USE_PTR(s,p,l) p
#  define F77_CHAR_USE_LEN(s,p,l) , &ConCat(s,_dsc)

# else

#  define F77_CHAR_DEF_DSC(s,p,l)
#  define F77_CHAR_ASS_DSC(s,p,l)

#   ifndef CERNLIB_MSSTDCALL
#     define F77_CHAR_USE_PTR(s,p,l) p
#     define F77_CHAR_USE_LEN(s,p,l) , l
#   else
#     define F77_CHAR_USE_PTR(s,p,l) p, l
#     define F77_CHAR_USE_LEN(s,p,l)
#   endif
 

# endif

#endif
#endif


#ifndef F77_XXXX_ARG_LEN   /* length argument of non-CHARACTER arguments */

#  define F77_XXXX_ARG_LEN(x)   /* nil */
#  define F77_XXXX_USE_LEN(x)   /* nil */

#endif


#define F77_CHAR_ARG2PTR(c1,c2) \
        F77_CHAR_ARG_PTR(c1),F77_CHAR_ARG_PTR(c2)
#define F77_CHAR_ARG2LEN(c1,c2) \
        F77_CHAR_ARG_LEN(c1) F77_CHAR_ARG_LEN(c2)
#define F77_XXXX_ARG2LEN(c1,c2) \
        F77_XXXX_ARG_LEN(c1) F77_XXXX_ARG_LEN(c2)
#define F77_CHAR_ARG2DCL(c1,c2) \
        F77_CHAR_ARG_DCL(c1) F77_CHAR_ARG_DCL(c2)
#define F77_CHAR_ARG2DEF(c1,c2) \
        F77_CHAR_ARG_DEF(c1) F77_CHAR_ARG_DEF(c2)

#define F77_CHAR_ARG3PTR(c1,c2,c3) \
        F77_CHAR_ARG2PTR(c1,c2),F77_CHAR_ARG_PTR(c3)
#define F77_CHAR_ARG3LEN(c1,c2,c3) \
        F77_CHAR_ARG2LEN(c1,c2) F77_CHAR_ARG_LEN(c3)
#define F77_XXXX_ARG3LEN(c1,c2,c3) \
        F77_XXXX_ARG2LEN(c1,c2) F77_XXXX_ARG_LEN(c3)
#define F77_CHAR_ARG3DCL(c1,c2,c3) \
        F77_CHAR_ARG2DCL(c1,c2) F77_CHAR_ARG_DCL(c3)
#define F77_CHAR_ARG3DEF(c1,c2,c3) \
        F77_CHAR_ARG2DEF(c1,c2) F77_CHAR_ARG_DEF(c3)

#define F77_CHAR_ARG4PTR(c1,c2,c3,c4) \
        F77_CHAR_ARG3PTR(c1,c2,c3),F77_CHAR_ARG_PTR(c4)
#define F77_CHAR_ARG4LEN(c1,c2,c3,c4) \
        F77_CHAR_ARG3LEN(c1,c2,c3) F77_CHAR_ARG_LEN(c4)
#define F77_XXXX_ARG4LEN(c1,c2,c3,c4) \
        F77_XXXX_ARG3LEN(c1,c2,c3) F77_XXXX_ARG_LEN(c4)
#define F77_CHAR_ARG4DCL(c1,c2,c3,c4) \
        F77_CHAR_ARG3DCL(c1,c2,c3) F77_CHAR_ARG_DCL(c4)
#define F77_CHAR_ARG4DEF(c1,c2,c3,c4) \
        F77_CHAR_ARG3DEF(c1,c2,c3) F77_CHAR_ARG_DEF(c4)

#define F77_CHAR_ARG5PTR(c1,c2,c3,c4,c5) \
        F77_CHAR_ARG4PTR(c1,c2,c3,c4),F77_CHAR_ARG_PTR(c5)
#define F77_CHAR_ARG5LEN(c1,c2,c3,c4,c5) \
        F77_CHAR_ARG4LEN(c1,c2,c3,c4) F77_CHAR_ARG_LEN(c5)
#define F77_CHAR_ARG5DCL(c1,c2,c3,c4,c5) \
        F77_CHAR_ARG4DCL(c1,c2,c3,c4) F77_CHAR_ARG_DCL(c5)
#define F77_CHAR_ARG5DEF(c1,c2,c3,c4,c5) \
        F77_CHAR_ARG4DEF(c1,c2,c3,c4) F77_CHAR_ARG_DEF(c5)


/*
 * Fortran-calls-C interface
 *
 * To define a C function called by Fortran CALL SUB(A,B,C):
 *
 *   #define Sub  F77_NAME(sub,SUB)
 *   #pragma linkage(SUB,FORTRAN)   // for IBM C/370 compiler
 *
 *   F77_ENTRY_xyz(Sub,a,b,c)       // opening { contained in macro
 *   // body ...
 *   }
 *
 * Each character in xyz declares the type of the corresponding parameter:
 *
 *   C = CHARACTER
 *   E = EXTERNAL
 *   I = INTEGER
 *   R = REAL
 *
 * If a parameter PAR is declared as CHARACTER the macro defines:
 *
 *   char *PAR;     // pointer to string (not terminated by \0 !!!)
 *   int len_PAR;   // length of string as defined by Fortran's LEN(PAR)
 *
 * The names PAR_dsc and PAR_ptr are reserved for internal use.
 *
 * Note: The function body follows the F77_ENTRY_... macro call directly.
 *       The opening { is generated by the macro.
 */

#define F77_ENTRY(name) \
  type_of_call \
  name() {

#define F77_ENTRY_C(name,c1) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1) F77_CHAR_ARG_LEN(c1) ) \
        F77_CHAR_ARG_DCL(c1) \
      { F77_CHAR_ARG_DEF(c1)


#define F77_ENTRY_CC(name,c1,c2) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2) F77_CHAR_ARG2LEN(c1,c2) ) \
        F77_CHAR_ARG2DCL(c1,c2) \
      { F77_CHAR_ARG2DEF(c1,c2)


#define F77_ENTRY_C3(name,c1,c2,c3) \
  type_of_call \
  name( F77_CHAR_ARG3PTR(c1,c2,c3) \
        F77_CHAR_ARG3LEN(c1,c2,c3) ) \
        F77_CHAR_ARG3DCL(c1,c2,c3) \
      { F77_CHAR_ARG3DEF(c1,c2,c3)


#define F77_ENTRY_C5(name,c1,c2,c3,c4,c5) \
  type_of_call \
  name( F77_CHAR_ARG5PTR(c1,c2,c3,c4,c5) \
        F77_CHAR_ARG5LEN(c1,c2,c3,c4,c5) ) \
        F77_CHAR_ARG5DCL(c1,c2,c3,c4,c5) \
      { F77_CHAR_ARG5DEF(c1,c2,c3,c4,c5)


#define F77_ENTRY_C4E(name,c1,c2,c3,c4,e5) \
  type_of_call \
  name( F77_CHAR_ARG4PTR(c1,c2,c3,c4),F77_EXTERN_ARG(e5) \
        F77_CHAR_ARG4LEN(c1,c2,c3,c4) ) \
        F77_CHAR_ARG4DCL(c1,c2,c3,c4) F77_EXTERN_DCL(e5) \
      { F77_CHAR_ARG4DEF(c1,c2,c3,c4) F77_EXTERN_DEF(e5)


#define F77_ENTRY_C4I(name,c1,c2,c3,c4,i5) \
  type_of_call \
  name( F77_CHAR_ARG4PTR(c1,c2,c3,c4),F77_INTG_ARG_PTR(i5) \
        F77_CHAR_ARG4LEN(c1,c2,c3,c4) ) \
        F77_CHAR_ARG4DCL(c1,c2,c3,c4) F77_INTG_ARG_DCL(i5) \
      { F77_CHAR_ARG4DEF(c1,c2,c3,c4) F77_INTG_ARG_DEF(i5)


#define F77_ENTRY_C5E(name,c1,c2,c3,c4,c5,e6) \
  type_of_call \
  name( F77_CHAR_ARG5PTR(c1,c2,c3,c4,c5),F77_EXTERN_ARG(e6) \
        F77_CHAR_ARG5LEN(c1,c2,c3,c4,c5) ) \
        F77_CHAR_ARG5DCL(c1,c2,c3,c4,c5) F77_EXTERN_DCL(e6) \
      { F77_CHAR_ARG5DEF(c1,c2,c3,c4,c5) F77_EXTERN_DEF(e6)


#define F77_ENTRY_CCE(name,c1,c2,e3) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2),F77_EXTERN_ARG(e3) \
        F77_CHAR_ARG2LEN(c1,c2) ) \
        F77_CHAR_ARG2DCL(c1,c2) F77_EXTERN_DCL(e3) \
      { F77_CHAR_ARG2DEF(c1,c2) F77_EXTERN_DEF(e3)


#define F77_ENTRY_CCEE(name,c1,c2,e3,e4) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2),F77_EXTERN2ARG(e3,e4) \
        F77_CHAR_ARG2LEN(c1,c2) ) \
        F77_CHAR_ARG2DCL(c1,c2) F77_EXTERN2DCL(e3,e4) \
      { F77_CHAR_ARG2DEF(c1,c2) F77_EXTERN2DEF(e3,e4)


#define F77_ENTRY_CCI(name,c1,c2,i3) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2),F77_INTG_ARG_PTR(i3) \
        F77_CHAR_ARG2LEN(c1,c2) ) \
        F77_CHAR_ARG2DCL(c1,c2) F77_INTG_ARG_DCL(i3) \
      { F77_CHAR_ARG2DEF(c1,c2) F77_INTG_ARG_DEF(i3)


#define F77_ENTRY_CCIC(name,c1,c2,i3,c4) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2),F77_INTG_ARG_PTR(i3),F77_CHAR_ARG_PTR(c4) \
        F77_CHAR_ARG2LEN(c1,c2) F77_XXXX_ARG_LEN(i3) F77_CHAR_ARG_LEN(c4) ) \
        F77_CHAR_ARG3DCL(c1,c2,c4) F77_INTG_ARG_DCL(i3) \
      { F77_CHAR_ARG3DEF(c1,c2,c4) F77_INTG_ARG_DEF(i3)


#define F77_ENTRY_CCI3(name,c1,c2,i3,i4,i5) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2),F77_INTG_ARG3PTR(i3,i4,i5) \
        F77_CHAR_ARG2LEN(c1,c2) ) \
        F77_CHAR_ARG2DCL(c1,c2) F77_INTG_ARG3DCL(i3,i4,i5) \
      { F77_CHAR_ARG2DEF(c1,c2) F77_INTG_ARG3DEF(i3,i4,i5)


#define F77_ENTRY_CCIRCC(name,c1,c2,i3,r4,c5,c6) \
  type_of_call \
  name( F77_CHAR_ARG2PTR(c1,c2), \
            F77_INTG_ARG_PTR(i3),F77_REAL_ARG_PTR(r4),F77_CHAR_ARG2PTR(c5,c6) \
        F77_CHAR_ARG2LEN(c1,c2) \
            F77_XXXX_ARG2LEN(i3,r4) F77_CHAR_ARG2LEN(c5,c6) ) \
        F77_CHAR_ARG4DCL(c1,c2,c5,c6) \
            F77_INTG_ARG_DCL(i3) F77_REAL_ARG_DCL(r4) \
      { F77_CHAR_ARG4DEF(c1,c2,c5,c6) \
            F77_INTG_ARG_DEF(i3) F77_REAL_ARG_DEF(r4) \


#define F77_ENTRY_CE(name,c1,e2) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1),F77_EXTERN_ARG(e2) \
        F77_CHAR_ARG_LEN(c1) ) \
        F77_CHAR_ARG_DCL(c1) F77_EXTERN_DCL(e2) \
      { F77_CHAR_ARG_DEF(c1) F77_EXTERN_DEF(e2)


#define F77_ENTRY_CI(name,c1,i2) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1),F77_INTG_ARG_PTR(i2) \
        F77_CHAR_ARG_LEN(c1) ) \
        F77_CHAR_ARG_DCL(c1) F77_INTG_ARG_DCL(i2) \
      { F77_CHAR_ARG_DEF(c1) F77_INTG_ARG_DEF(i2)


#define F77_ENTRY_CICI(name,c1,i2,c3,i4) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1),F77_INTG_ARG_PTR(i2), \
            F77_CHAR_ARG_PTR(c3),F77_INTG_ARG_PTR(i4) \
        F77_CHAR_ARG_LEN(c1) F77_XXXX_ARG_LEN(i2) \
            F77_CHAR_ARG_LEN(c3) ) \
        F77_CHAR_ARG2DCL(c1,c3) F77_INTG_ARG2DCL(i2,i4) \
      { F77_CHAR_ARG2DEF(c1,c3) F77_INTG_ARG2DEF(i2,i4)


#define F77_ENTRY_CII(name,c1,i2,i3) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1),F77_INTG_ARG2PTR(i2,i3) \
        F77_CHAR_ARG_LEN(c1) ) \
        F77_CHAR_ARG_DCL(c1) F77_INTG_ARG2DCL(i2,i3) \
      { F77_CHAR_ARG_DEF(c1) F77_INTG_ARG2DEF(i2,i3)


#define F77_ENTRY_CIIC(name,c1,i2,i3,c4) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1),F77_INTG_ARG2PTR(i2,i3),F77_CHAR_ARG_PTR(c4) \
        F77_CHAR_ARG_LEN(c1) F77_XXXX_ARG2LEN(i2,i3) F77_CHAR_ARG_LEN(c4) ) \
        F77_CHAR_ARG2DCL(c1,c4) F77_INTG_ARG2DCL(i2,i3) \
      { F77_CHAR_ARG2DEF(c1,c4) F77_INTG_ARG2DEF(i2,i3)


#define F77_ENTRY_CR(name,c1,r2) \
  type_of_call \
  name( F77_CHAR_ARG_PTR(c1),F77_REAL_ARG_PTR(r2) \
        F77_CHAR_ARG_LEN(c1) ) \
        F77_CHAR_ARG_DCL(c1) F77_REAL_ARG_DCL(r2) \
      { F77_CHAR_ARG_DEF(c1) F77_REAL_ARG_DEF(r2)

#define F77_ENTRY_R(name,r1)   \
  type_of_call                 \
  name( F77_REAL_ARG_PTR(r1) ) \
        F77_REAL_ARG_DCL(r1)   \
      { F77_REAL_ARG_DEF(r1)

#define F77_ENTRY_E(name,e1) \
  type_of_call \
  name( F77_EXTERN_ARG(e1) ) \
        F77_EXTERN_DCL(e1) \
      { F77_EXTERN_DEF(e1)


#define F77_ENTRY_E4(name,e1,e2,e3,e4) \
  type_of_call \
  name( F77_EXTERN4ARG(e1,e2,e3,e4) ) \
        F77_EXTERN4DCL(e1,e2,e3,e4) \
      { F77_EXTERN4DEF(e1,e2,e3,e4)

#define F77_ENTRY_I(name,i1)    \
  type_of_call                  \
  name(  F77_INTG_ARG_PTR(i1) ) \
         F77_INTG_ARG_DCL(i1)   \
      {  F77_INTG_ARG_DEF(i1)
 
#define F77_ENTRY_IC(name,i1,c2) \
  type_of_call \
  name( F77_INTG_ARG_PTR(i1),F77_CHAR_ARG_PTR(c2) \
        F77_XXXX_ARG_LEN(i1) F77_CHAR_ARG_LEN(c2) ) \
        F77_INTG_ARG_DCL(i1) F77_CHAR_ARG_DCL(c2) \
      { F77_INTG_ARG_DEF(i1) F77_CHAR_ARG_DEF(c2)


#define F77_ENTRY_ICI(name,i1,c2,i3) \
  type_of_call \
  name( F77_INTG_ARG_PTR(i1),   F77_CHAR_ARG_PTR(c2),F77_INTG_ARG_PTR(i3) \
        F77_XXXX_ARG_LEN(i1)    F77_CHAR_ARG_LEN(c2) ) \
        F77_INTG_ARG2DCL(i1,i3) F77_CHAR_ARG_DCL(c2) \
      { F77_INTG_ARG2DEF(i1,i3) F77_CHAR_ARG_DEF(c2)


#define F77_ENTRY_IIC(name,i1,i2,c3) \
  type_of_call \
  name( F77_INTG_ARG2PTR(i1,i2),F77_CHAR_ARG_PTR(c3) \
        F77_XXXX_ARG2LEN(i1,i2) F77_CHAR_ARG_LEN(c3) ) \
        F77_INTG_ARG2DCL(i1,i2) F77_CHAR_ARG_DCL(c3) \
      { F77_INTG_ARG2DEF(i1,i2) F77_CHAR_ARG_DEF(c3)


#define F77_ENTRY_I3C(name,i1,i2,i3,c4) \
  type_of_call \
  name( F77_INTG_ARG3PTR(i1,i2,i3),F77_CHAR_ARG_PTR(c4) \
        F77_XXXX_ARG3LEN(i1,i2,i3) F77_CHAR_ARG_LEN(c4) ) \
        F77_INTG_ARG3DCL(i1,i2,i3) F77_CHAR_ARG_DCL(c4) \
      { F77_INTG_ARG3DEF(i1,i2,i3) F77_CHAR_ARG_DEF(c4)


#define F77_ENTRY_I4CCC(name,i1,i2,i3,i4,c5,c6,c7) \
  type_of_call \
  name( F77_INTG_ARG4PTR(i1,i2,i3,i4),F77_CHAR_ARG3PTR(c5,c6,c7) \
        F77_XXXX_ARG4LEN(i1,i2,i3,i4) F77_CHAR_ARG3LEN(c5,c6,c7) ) \
        F77_INTG_ARG4DCL(i1,i2,i3,i4) F77_CHAR_ARG3DCL(c5,c6,c7) \
      { F77_INTG_ARG4DEF(i1,i2,i3,i4) F77_CHAR_ARG3DEF(c5,c6,c7)


#define F77_CHAR_DEF2DSC(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_DEF_DSC(s1,p1,l1) F77_CHAR_DEF_DSC(s2,p2,l2)
#define F77_CHAR_ASS2DSC(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_ASS_DSC(s1,p1,l1) F77_CHAR_ASS_DSC(s2,p2,l2)
#define F77_CHAR_USE2PTR(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_USE_PTR(s1,p1,l1),F77_CHAR_USE_PTR(s2,p2,l2)
#define F77_CHAR_USE2LEN(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_USE_LEN(s1,p1,l1) F77_CHAR_USE_LEN(s2,p2,l2)
#define F77_XXXX_USE2LEN(x1,x2) \
        F77_XXXX_USE_LEN(x1) F77_XXXX_USE_LEN(x2)


#define F77_CHAR_DEF3DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        F77_CHAR_DEF2DSC(s1,p1,l1,s2,p2,l2) F77_CHAR_DEF_DSC(s3,p3,l3)
#define F77_CHAR_ASS3DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        F77_CHAR_ASS2DSC(s1,p1,l1,s2,p2,l2) F77_CHAR_ASS_DSC(s3,p3,l3)
#define F77_CHAR_USE3PTR(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        F77_CHAR_USE2PTR(s1,p1,l1,s2,p2,l2),F77_CHAR_USE_PTR(s3,p3,l3)
#define F77_CHAR_USE3LEN(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        F77_CHAR_USE2LEN(s1,p1,l1,s2,p2,l2) F77_CHAR_USE_LEN(s3,p3,l3)
#define F77_XXXX_USE3LEN(x1,x2,x3) \
        F77_XXXX_USE2LEN(x1,x2) F77_XXXX_USE_LEN(x3)


#define F77_CHAR_DEF4DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_DEF3DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3) F77_CHAR_DEF_DSC(s4,p4,l4)
#define F77_CHAR_ASS4DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_ASS3DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3) F77_CHAR_ASS_DSC(s4,p4,l4)
#define F77_CHAR_USE4PTR(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_USE3PTR(s1,p1,l1,s2,p2,l2,s3,p3,l3),F77_CHAR_USE_PTR(s4,p4,l4)
#define F77_CHAR_USE4LEN(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_USE3LEN(s1,p1,l1,s2,p2,l2,s3,p3,l3) F77_CHAR_USE_LEN(s4,p4,l4)
#define F77_XXXX_USE4LEN(x1,x2,x3,x4) \
        F77_XXXX_USE3LEN(x1,x2,x3) F77_XXXX_USE_LEN(x4)


#define F77_XXXX_USE5LEN(x1,x2,x3,x4,x5) \
        F77_XXXX_USE4LEN(x1,x2,x3,x4) F77_XXXX_USE_LEN(x5)


#define F77_XXXX_USE6LEN(x1,x2,x3,x4,x5,x6) \
        F77_XXXX_USE5LEN(x1,x2,x3,x4,x5) F77_XXXX_USE_LEN(x6)


#define F77_XXXX_USE7LEN(x1,x2,x3,x4,x5,x6,x7) \
        F77_XXXX_USE6LEN(x1,x2,x3,x4,x5,x6) F77_XXXX_USE_LEN(x7)


#define F77_XXXX_USE8LEN(x1,x2,x3,x4,x5,x6,x7,x8) \
        F77_XXXX_USE7LEN(x1,x2,x3,x4,x5,x6,x7) F77_XXXX_USE_LEN(x8)


EXTERN INTEGER F77_i0;          /* dummy */

#ifdef IBM370
#pragma linkage(K77C,FORTRAN)
#define F77_IFUN_C(i0,name,p1,l1) do { \
        i0 = K77C(&name,p1,l1); } while(0)
#else
#define F77_IFUN_C(i0,name,p1,l1) do { \
        F77_CHAR_DEF_DSC(s1,p1,l1) \
        F77_CHAR_ASS_DSC(s1,p1,l1) \
        i0 = (*name)( \
        F77_CHAR_USE_PTR(s1,p1,l1) \
        F77_CHAR_USE_LEN(s1,p1,l1) \
       ); } while(0)
#endif
#define F77_CALL_C(name,p1,l1) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_C(F77_i0,_p0_,p1,l1); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77CC,FORTRAN)
#define F77_IFUN_CC(i0,name,p1,l1,p2,l2) do { \
        i0 = K77CC(&name,p1,l1,p2,l2); } while(0)
#else
#define F77_IFUN_CC(i0,name,p1,l1,p2,l2) do { \
        F77_CHAR_DEF2DSC(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_ASS2DSC(s1,p1,l1,s2,p2,l2) \
        i0 = (*name)( \
        F77_CHAR_USE2PTR(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_USE2LEN(s1,p1,l1,s2,p2,l2) \
       ); } while(0)
#endif
#define F77_CALL_CC(name,p1,l1,p2,l2) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_CC(F77_i0,_p0_,p1,l1,p2,l2); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77C3,FORTRAN)
#define F77_IFUN_C3(i0,name,p1,l1,p2,l2,p3,l3) do { \
        i0 = K77C3(&name,p1,l1,p2,l2,p3,l3); } while(0)
#else
#define F77_IFUN_C3(i0,name,p1,l1,p2,l2,p3,l3) do { \
        F77_CHAR_DEF3DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        F77_CHAR_ASS3DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        i0 = (*name)( \
        F77_CHAR_USE3PTR(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
        F77_CHAR_USE3LEN(s1,p1,l1,s2,p2,l2,s3,p3,l3) \
       ); } while(0)
#endif
#define F77_CALL_C3(name,p1,l1,p2,l2,p3,l3) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_C3(F77_i0,_p0_,p1,l1,p2,l2,p3,l3); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77C7,FORTRAN)
#define F77_IFUN_C7(i0,name,p1,l1,p2,l2,p3,l3,p4,l4,p5,l5,p6,l6,p7,l7) do { \
        i0 = K77C7(&name,p1,l1,p2,l2,p3,l3,p4,l4,p5,l5,p6,l6,p7,l7); } while(0)
#else
#define F77_IFUN_C7(i0,name,p1,l1,p2,l2,p3,l3,p4,l4,p5,l5,p6,l6,p7,l7) do { \
        F77_CHAR_DEF4DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_DEF3DSC(s5,p5,l5,s6,p6,l6,s7,p7,l7) \
        F77_CHAR_ASS4DSC(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_ASS3DSC(s5,p5,l5,s6,p6,l6,s7,p7,l7) \
        i0 = (*name)( \
        F77_CHAR_USE4PTR(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4), \
        F77_CHAR_USE3PTR(s5,p5,l5,s6,p6,l6,s7,p7,l7) \
        F77_CHAR_USE4LEN(s1,p1,l1,s2,p2,l2,s3,p3,l3,s4,p4,l4) \
        F77_CHAR_USE3LEN(s5,p5,l5,s6,p6,l6,s7,p7,l7) \
       ); } while(0)
#endif
#define F77_CALL_C7(name,p1,l1,p2,l2,p3,l3,p4,l4,p5,l5,p6,l6,p7,l7) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_C7(F77_i0,_p0_,p1,l1,p2,l2,p3,l3,p4,l4,p5,l5,p6,l6,p7,l7); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77CCx,FORTRAN)
#define F77_CALL_CCx(name,p1,l1,p2,l2,x3) do { \
        SUBROUTINE *F77 = name; \
        K77CCx(&F77,p1,l1,p2,l2,x3); } while(0)
#else
#define F77_CALL_CCx(name,p1,l1,p2,l2,x3) do { \
        F77_CHAR_DEF2DSC(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_ASS2DSC(s1,p1,l1,s2,p2,l2) \
  name( F77_CHAR_USE2PTR(s1,p1,l1,s2,p2,l2), \
                         x3 \
        F77_CHAR_USE2LEN(s1,p1,l1,s2,p2,l2) \
        F77_XXXX_USE_LEN(x3) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(K77CCxx,FORTRAN)
#define F77_IFUN_CCxx(i0,name,p1,l1,p2,l2,x3,x4) do { \
        i0 = K77CCxx(&name,p1,l1,p2,l2,x3,x4); } while(0)
#else
#define F77_IFUN_CCxx(i0,name,p1,l1,p2,l2,x3,x4) do { \
        F77_CHAR_DEF2DSC(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_ASS2DSC(s1,p1,l1,s2,p2,l2) \
        i0 = (*name)( \
        F77_CHAR_USE2PTR(s1,p1,l1,s2,p2,l2), \
                         x3,x4 \
        F77_CHAR_USE2LEN(s1,p1,l1,s2,p2,l2) \
       ); } while(0)
#endif
#define F77_CALL_CCxx(name,p1,l1,p2,l2,x3,x4) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_CCxx(F77_i0,_p0_,p1,l1,p2,l2,x3,x4); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77CCx3,FORTRAN)
#define F77_IFUN_CCx3(i0,name,p1,l1,p2,l2,x3,x4,x5) do { \
        i0 = K77CCx3(&name,p1,l1,p2,l2,x3,x4,x5); } while(0)
#else
#define F77_IFUN_CCx3(i0,name,p1,l1,p2,l2,x3,x4,x5) do { \
        F77_CHAR_DEF2DSC(s1,p1,l1,s2,p2,l2) \
        F77_CHAR_ASS2DSC(s1,p1,l1,s2,p2,l2) \
        i0 = (*name)( \
        F77_CHAR_USE2PTR(s1,p1,l1,s2,p2,l2), \
                         x3,x4,x5 \
        F77_CHAR_USE2LEN(s1,p1,l1,s2,p2,l2) \
       ); } while(0)
#endif
#define F77_CALL_CCx3(name,p1,l1,p2,l2,x3,x4,x5) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_CCx3(F77_i0,_p0_,p1,l1,p2,l2,x3,x4,x5); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77Cx,FORTRAN)
#define F77_IFUN_Cx(i0,name,p1,l1,x2) do { \
        i0 = K77Cx(&name,p1,l1,x2); } while(0)
#else
#define F77_IFUN_Cx(i0,name,p1,l1,x2) do { \
        F77_CHAR_DEF_DSC(s1,p1,l1) \
        F77_CHAR_ASS_DSC(s1,p1,l1) \
  i0 = (*name)( \
        F77_CHAR_USE_PTR(s1,p1,l1), \
                         x2 \
        F77_CHAR_USE_LEN(s1,p1,l1) \
        F77_XXXX_USE_LEN(x2) \
       ); } while(0)
#endif

#define F77_CALL_Cx(name,p1,l1,x2) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_Cx(F77_i0,_p0_,p1,l1,x2); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77CxC,FORTRAN)
#define F77_CALL_CxC(name,p1,l1,x2,p3,l3) do { \
        SUBROUTINE *F77 = name; \
        K77CxC(&F77,p1,l1,x2,p3,l3); } while(0)
#else
#define F77_CALL_CxC(name,p1,l1,x2,p3,l3) do { \
        F77_CHAR_DEF2DSC(s1,p1,l1,s3,p3,l3) \
        F77_CHAR_ASS2DSC(s1,p1,l1,s3,p3,l3) \
  name( F77_CHAR_USE_PTR(s1,p1,l1), \
                         x2, \
        F77_CHAR_USE_PTR(s3,p3,l3) \
        F77_CHAR_USE_LEN(s1,p1,l1) \
        F77_XXXX_USE_LEN(x2) \
        F77_CHAR_USE_LEN(s3,p3,l3) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(K77Cxx,FORTRAN)
#define F77_CALL_Cxx(name,p1,l1,x2,x3) do { \
        SUBROUTINE *F77 = name; \
        K77Cxx(&F77,p1,l1,x2,x3); } while(0)
#else
#define F77_CALL_Cxx(name,p1,l1,x2,x3) do { \
        F77_CHAR_DEF_DSC(s1,p1,l1) \
        F77_CHAR_ASS_DSC(s1,p1,l1) \
  name( F77_CHAR_USE_PTR(s1,p1,l1), \
                         x2,x3 \
        F77_CHAR_USE_LEN(s1,p1,l1) \
        F77_XXXX_USE2LEN(x2,x3) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(K77xC,FORTRAN)
#define F77_CALL_xC(name,x1,p2,l2) do { \
        SUBROUTINE *F77 = name; \
        K77xC(&F77,x1,p2,l2); } while(0)
#else
#define F77_CALL_xC(name,x1,p2,l2) do { \
        F77_CHAR_DEF_DSC(s2,p2,l2) \
        F77_CHAR_ASS_DSC(s2,p2,l2) \
  name(                  x1, \
        F77_CHAR_USE_PTR(s2,p2,l2) \
        F77_XXXX_USE_LEN(x1) \
        F77_CHAR_USE_LEN(s2,p2,l2) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(K77xCCx,FORTRAN)
#define F77_CALL_xCCx(name,x1,p2,l2,p3,l3,x4) do { \
        SUBROUTINE *F77 = name; \
        K77xCCx(&F77,x1,p2,l2,p3,l3,x4); } while(0)
#else
#define F77_CALL_xCCx(name,x1,p2,l2,p3,l3,x4) do { \
        F77_CHAR_DEF2DSC(s2,p2,l2,s3,p3,l3) \
        F77_CHAR_ASS2DSC(s2,p2,l2,s3,p3,l3) \
  name(                  x1, \
        F77_CHAR_USE2PTR(s2,p2,l2,s3,p3,l3), \
                         x4 \
        F77_XXXX_USE_LEN(x1) \
        F77_CHAR_USE2LEN(s2,p2,l2,s3,p3,l3) \
        F77_XXXX_USE_LEN(x4) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(K77xCC,FORTRAN)
#define F77_IFUN_xCC(i0,name,x1,p2,l2,p3,l3) do { \
        i0 = K77xCC(&name,x1,p2,l2,p3,l3); } while(0)
#else
#define F77_IFUN_xCC(i0,name,x1,p2,l2,p3,l3) do { \
        F77_CHAR_DEF2DSC(s2,p2,l2,s3,p3,l3) \
        F77_CHAR_ASS2DSC(s2,p2,l2,s3,p3,l3) \
  i0 = (*name)(          x1, \
        F77_CHAR_USE2PTR(s2,p2,l2,s3,p3,l3) \
        F77_XXXX_USE_LEN(x1) \
        F77_CHAR_USE2LEN(s2,p2,l2,s3,p3,l3) \
       ); } while(0)
#endif

#define F77_CALL_xCC(name,x1,p2,l2,p3,l3) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_xCC(F77_i0,_p0_,x1,p2,l2,p3,l3); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77xCx,FORTRAN)
#define F77_IFUN_xCx(i0,name,x1,p2,l2,x3) do { \
        i0 = K77xCx(&name,x1,p2,l2,x3); } while(0)
#else
#define F77_IFUN_xCx(i0,name,x1,p2,l2,x3) do { \
        F77_CHAR_DEF_DSC(s2,p2,l2) \
        F77_CHAR_ASS_DSC(s2,p2,l2) \
  i0 = (*name)(          x1, \
        F77_CHAR_USE_PTR(s2,p2,l2), \
                         x3 \
        F77_XXXX_USE_LEN(x1) \
        F77_CHAR_USE_LEN(s2,p2,l2) \
        F77_XXXX_USE_LEN(x3) \
       ); } while(0)
#endif

#define F77_CALL_xCx(name,x1,p2,l2,x3) do { \
        INT_FUNCPTR _p0_ = (INT_FUNCPTR)name; \
        F77_IFUN_xCx(F77_i0,_p0_,x1,p2,l2,x3); \
        } while(0)


#ifdef IBM370
#pragma linkage(K77x4C,FORTRAN)
#define F77_CALL_x4C(name,x1,x2,x3,x4,p5,l5) do { \
        SUBROUTINE *F77 = name; \
        K77x4C(&F77,x1,x2,x3,x4,p5,l5); } while(0)
#else
#define F77_CALL_x4C(name,x1,x2,x3,x4,p5,l5) do { \
        F77_CHAR_DEF_DSC(s5,p5,l5) \
        F77_CHAR_ASS_DSC(s5,p5,l5) \
  name(                  x1,x2,x3,x4, \
        F77_CHAR_USE_PTR(s5,p5,l5) \
        F77_XXXX_USE4LEN(x1,x2,x3,x4) \
        F77_CHAR_USE_LEN(s5,p5,l5) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(K77x4Cxx,FORTRAN)
#define F77_CALL_x4Cxx(name,x1,x2,x3,x4,p5,l5,x6,x7) do { \
        SUBROUTINE *F77 = name; \
        K77x4Cxx(&F77,x1,x2,x3,x4,p5,l5,x6,x7); } while(0)
#else
#define F77_CALL_x4Cxx(name,x1,x2,x3,x4,p5,l5,x6,x7) do { \
        F77_CHAR_DEF_DSC(s5,p5,l5) \
        F77_CHAR_ASS_DSC(s5,p5,l5) \
  name(                  x1,x2,x3,x4, \
        F77_CHAR_USE_PTR(s5,p5,l5), \
                         x6,x7 \
        F77_XXXX_USE4LEN(x1,x2,x3,x4) \
        F77_CHAR_USE_LEN(s5,p5,l5) \
        F77_XXXX_USE2LEN(x6,x7) \
       ); } while(0)
#endif


#ifdef IBM370
#pragma linkage(KIGMENU,FORTRAN)
/*                               1  2  3 4 5 6 7  8  9 10  11  12 13 14 */
#define F77_CALL_xCx5CxC3xC(name,a,b,B,c,d,e,f,g,h,H,i,j,J,k,K,l,L,m,n,N) do {\
        SUBROUTINE *F77 = name; \
        KIGMENU(&F77,a,b,B,c,d,e,f,g,h,H,i,j,J,k,K,l,L,m,n,N); } while(0)
#else
#define F77_CALL_xCx5CxC3xC(name,a,b,B,c,d,e,f,g,h,H,i,j,J,k,K,l,L,m,n,N) do {\
        F77_CHAR_DEF_DSC(sb,b,B) \
        F77_CHAR_DEF_DSC(sh,h,H) \
        F77_CHAR_DEF3DSC(sj,j,J,sk,k,K,sl,l,L) \
        F77_CHAR_DEF_DSC(sn,n,N) \
        F77_CHAR_ASS_DSC(sb,b,B) \
        F77_CHAR_ASS_DSC(sh,h,H) \
        F77_CHAR_ASS3DSC(sj,j,J,sk,k,K,sl,l,L) \
        F77_CHAR_ASS_DSC(sn,n,N) \
  name(                  a, \
        F77_CHAR_USE_PTR(sb,b,B), \
                         c,d,e,f,g, \
        F77_CHAR_USE_PTR(sh,h,H), \
                         i, \
        F77_CHAR_USE3PTR(sj,j,J,sk,k,K,sl,l,L), \
                         m, \
        F77_CHAR_USE_PTR(sn,n,N) \
        F77_XXXX_USE_LEN(a) \
        F77_CHAR_USE_LEN(sb,b,B) \
        F77_XXXX_USE5LEN(c,d,e,f,g) \
        F77_CHAR_USE_LEN(sh,h,H) \
        F77_XXXX_USE_LEN(i) \
        F77_CHAR_USE3LEN(sj,j,J,sk,k,K,sl,l,L) \
        F77_XXXX_USE_LEN(m) \
        F77_CHAR_USE_LEN(sn,n,N) \
       ); } while(0)
#endif


#ifdef IBM370
#define F77_IFUN_x(i0,name,x1) i0 = __CTOF(name,x1)
#else
#define F77_IFUN_x(i0,name,x1) i0 = (*name)(x1)
#endif


#ifdef IBM370
#define F77_IFUN_xx(i0,name,x1,x2) i0 = __CTOF(name,x1,x2)
#else
#define F77_IFUN_xx(i0,name,x1,x2) i0 = (*name)(x1,x2)
#endif


#ifdef IBM370
#pragma linkage(K77xCx8,FORTRAN)
#define F77_IFUN_xCx8(i0,name,x1,p2,l2,x3,x4,x5,x6,x7,x8,x9,x10) do { \
        i0 = K77xCx8(&name,x1,p2,l2,x3,x4,x5,x6,x7,x8,x9,x10); } while(0)
#else
#define F77_IFUN_xCx8(i0,name,x1,p2,l2,x3,x4,x5,x6,x7,x8,x9,x10) do { \
        F77_CHAR_DEF_DSC(s2,p2,l2) \
        F77_CHAR_ASS_DSC(s2,p2,l2) \
  i0 = (*name)(          x1, \
        F77_CHAR_USE_PTR(s2,p2,l2), \
                         x3,x4,x5,x6,x7,x8,x9,x10 \
        F77_XXXX_USE_LEN(x1) \
        F77_CHAR_USE_LEN(s2,p2,l2) \
        F77_XXXX_USE8LEN(x3,x4,x5,x6,x7,x8,x9,x10) \
       ); } while(0)
#endif


extern LOGICAL ku_true( int return_true );


/*
 * routines called by Fortran
 */
#define  Errrun           F77_NAME(errrun,ERRRUN)
#define  Fmemcpy          F77_NAME(fmemcpy,FMEMCPY)

#define  Getarg           F77_NAME(getarg,GETARG)
                 extern SUBROUTINE Getarg;
#define  Goparm           F77_NAME(goparm,GOPARM)
                 extern SUBROUTINE Goparm;

#define  Iclrwk           F77_NAME(iclrwk,ICLRWK)
                 extern SUBROUTINE Iclrwk;
#define  Iginit           F77_NAME(iginit,IGINIT)
                 extern SUBROUTINE Iginit;
#define  Igmenu           F77_NAME(igmenu,IGMENU)
                 extern SUBROUTINE Igmenu;
#define  Igrng            F77_NAME(igrng,IGRNG)
                 extern SUBROUTINE Igrng;
#define  Igsse            F77_NAME(igsse,IGSSE)
                 extern SUBROUTINE Igsse;
#define  Igsrap           F77_NAME(igsrap,IGSRAP)
                 extern SUBROUTINE Igsrap;
#define  Igwkty           F77_NAME(igwkty,IGWKTY)
                 extern SUBROUTINE Igwkty;

#define  Kcexec           F77_NAME(kcexec,KCEXEC)

#define  Kdialo           F77_NAME(kdialo,KDIALO)
                 extern SUBROUTINE Kdialo;

#define  Kgetar           F77_NAME(kgetar,KGETAR)

#define  Kiargc           F77_NAME(kiargc,KIARGC)
               extern INT_FUNCTION Kiargc;
#define  Kibres           F77_NAME(kibres,KIBRES)
                 extern SUBROUTINE Kibres;
#define  Kiclos           F77_NAME(kiclos,KICLOS)
                 extern SUBROUTINE Kiclos;
#define  Kicomv           F77_NAME(kicomv,KICOMV)
#define  Kierrf           F77_NAME(kierrf,KIERRF)
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kierrf
                 (char *, int); /* To make MS Fortran happy */
#else
                  extern SUBROUTINE Kierrf;
#endif

#define  Kiinit           F77_NAME(kiinit,KIINIT)
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kiinit
                 (int *);
#else
                 extern SUBROUTINE Kiinit;
#endif
#define  Kilun            F77_NAME(kilun,KILUN)
                 extern SUBROUTINE Kilun;
#define  Kipawc           F77_NAME(kipawc,KIPAWC)
#define  Kipiaf           F77_NAME(kipiaf,KIPIAF)
#define  Kiprmt           F77_NAME(kiprmt,KIPRMT)
#define  Kirtim           F77_NAME(kirtim,KIRTIM)
#define  Kisigm           F77_NAME(kisigm,KISIGM)
#define  Kivect           F77_NAME(kivect,KIVECT)

#define  Kmbrset          F77_NAME(kmbrset,KMBRSET)
#define  Kmvsed           F77_NAME(kmvsed,KMVSED)
                 extern SUBROUTINE Kmvsed;
#define  Kmvspg           F77_NAME(kmvspg,KMVSPG)
                 extern SUBROUTINE Kmvspg;
#define  Kmvssh           F77_NAME(kmvssh,KMVSSH)
                 extern SUBROUTINE Kmvssh;

#define  Ksvpar           F77_NAME(ksvpar,KSVPAR)

#define  Kuach            F77_NAME(kuach,KUACH)
#define  Kuact            F77_NAME(kuact,KUACT)
#define  Kualfa           F77_NAME(kualfa,KUALFA)
#define  Kuappl           F77_NAME(kuappl,KUAPPL)
#define  Kuargs           F77_NAME(kuargs,KUARGS)
#define  Kubrek           F77_NAME(kubrek,KUBREK)
#define  Kubrof           F77_NAME(kubrof,KUBROF)
#define  Kubron           F77_NAME(kubron,KUBRON)
#define  Kuclos           F77_NAME(kuclos,KUCLOS)
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kuclos
                 (int *, char *, int , int *);
#else
                 extern SUBROUTINE Kuclos;
#endif

#define  Kucmd            F77_NAME(kucmd,KUCMD)
#define  Kucmdl           F77_NAME(kucmdl,KUCMDL)
#define  Kucomv           F77_NAME(kucomv,KUCOMV)
#define  Kuedit           F77_NAME(kuedit,KUEDIT)
#define  Kuesvr           F77_NAME(kuesvr,KUESVR)
#define  Kueusr           F77_NAME(kueusr,KUEUSR)
#define  Kuexec           F77_NAME(kuexec,KUEXEC)
#define  Kuexel           F77_NAME(kuexel,KUEXEL)
#define  Kuexit           F77_NAME(kuexit,KUEXIT)
#define  Kufcas           F77_NAME(kufcas,KUFCAS)
#define  Kufdef           F77_NAME(kufdef,KUFDEF)
#define  Kugetc           F77_NAME(kugetc,KUGETC)
#define  Kugete           F77_NAME(kugete,KUGETE)
#define  Kugetf           F77_NAME(kugetf,KUGETF)
#define  Kugeti           F77_NAME(kugeti,KUGETI)
#define  Kugetl           F77_NAME(kugetl,KUGETL)
#define  Kugetr           F77_NAME(kugetr,KUGETR)
#define  Kugets           F77_NAME(kugets,KUGETS)
#define  Kugrfl           F77_NAME(kugrfl,KUGRFL)
#define  Kuguid           F77_NAME(kuguid,KUGUID)
#define  Kuhelp           F77_NAME(kuhelp,KUHELP)
#define  Kuhome           F77_NAME(kuhome,KUHOME)
#define  Kuidf1           F77_NAME(kuidf1,KUIDF1)
                 extern SUBROUTINE Kuidf1;
#define  Kuidf2           F77_NAME(kuidf2,KUIDF2)
                 extern SUBROUTINE Kuidf2;
#define  Kuidfm           F77_NAME(kuidfm,KUIDFM)
#define  Kuinim           F77_NAME(kuinim,KUINIM)
#define  Kuinit           F77_NAME(kuinit,KUINIT)
#define  Kuinps           F77_NAME(kuinps,KUINPS)
#define  Kuinqf           F77_NAME(kuinqf,KUINQF)
#define  Kxdsig           F77_NAME(kxdsig,KXDSIG)
                 extern SUBROUTINE Kxdsig;
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kuinqf
                 (char *, int , int *);
#else
                 extern SUBROUTINE Kuinqf;
#endif
#define  Kulun            F77_NAME(kulun,KULUN)
#define  Kumloc           F77_NAME(kumloc,KUMLOC)
#define  Kumout           F77_NAME(kumout,KUMOUT)
#define  Kundpv           F77_NAME(kundpv,KUNDPV)
#define  Kunpar           F77_NAME(kunpar,KUNPAR)
#define  Kunwg            F77_NAME(kunwg,KUNWG)
#define  Kuopen           F77_NAME(kuopen,KUOPEN)
                 extern SUBROUTINE Kuopen;
#define  Kupad            F77_NAME(kupad,KUPAD)
#define  Kupar            F77_NAME(kupar,KUPAR)
#define  Kupath           F77_NAME(kupath,KUPATH)
#define  Kupatl           F77_NAME(kupatl,KUPATL)
#define  Kuproc           F77_NAME(kuproc,KUPROC)
#define  Kuprof           F77_NAME(kuprof,KUPROF)
#define  Kuproi           F77_NAME(kuproi,KUPROI)
#define  Kuprop           F77_NAME(kuprop,KUPROP)
#define  Kupror           F77_NAME(kupror,KUPROR)
#define  Kupros           F77_NAME(kupros,KUPROS)
#define  Kumess           F77_NAME(kumess,KUMESS)
#define  Kupval           F77_NAME(kupval,KUPVAL)
#define  Kuqcas           F77_NAME(kuqcas,KUQCAS)
#define  Kuqenv           F77_NAME(kuqenv,KUQENV)
#define  Kuqexe           F77_NAME(kuqexe,KUQEXE)
#define  Kuqkey           F77_NAME(kuqkey,KUQKEY)
#define  Kuqsvr           F77_NAME(kuqsvr,KUQSVR)
#define  Kuquit           F77_NAME(kuquit,KUQUIT)
#define  Kuqvar           F77_NAME(kuqvar,KUQVAR)
#define  Kuread           F77_NAME(kuread,KUREAD)
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kuread
                 (int *, char *, int , int *);
#else
                 extern SUBROUTINE Kuread;
#endif
#define  Kusapp           F77_NAME(kusapp,KUSAPP)
#define  Kuser            F77_NAME(kuser,KUSER)
#define  Kuserid          F77_NAME(kuserid,KUSERID)
#define  Kusibr           F77_NAME(kusibr,KUSIBR)
#define  Kusigm           F77_NAME(kusigm,KUSIGM)
#define  Kuspy            F77_NAME(kuspy,KUSPY)
#define  Kustat           F77_NAME(kustat,KUSTAT)
#define  Kustop           F77_NAME(kustop,KUSTOP)
#define  Kuterm           F77_NAME(kuterm,KUTERM)
#define  Kutime           F77_NAME(kutime,KUTIME)
#define  Kutim0           F77_NAME(kutim0,KUTIM0)
                 extern SUBROUTINE Kutim0;
#define  Kutrue           F77_NAME(kutrue,KUTRUE)
#define  Kuvar            F77_NAME(kuvar,KUVAR)
#define  Kuvcre           F77_NAME(kuvcre,KUVCRE)
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kuvcre
                 (char *, int, char *, int, int *, int *, int *);
#else
                 extern SUBROUTINE Kuvcre;
#endif
#define  Kuvdel           F77_NAME(kuvdel,KUVDEL)
#ifdef CERNLIB_MSSTDCALL
                 extern type_of_call Kuvdel
                 ( char *, int);
#else
                 extern SUBROUTINE Kuvdel;
#endif
#define  Kuvect           F77_NAME(kuvect,KUVECT)
                 extern SUBROUTINE Kuvect;
#define  Kuvnam           F77_NAME(kuvnam,KUVNAM)
#define  Kuwhag           F77_NAME(kuwhag,KUWHAG)
#define  Kuwham           F77_NAME(kuwham,KUWHAM)
#define  Kuwhat           F77_NAME(kuwhat,KUWHAT)
#define  Kuwrit           F77_NAME(kuwrit,KUWRIT)
                 extern SUBROUTINE Kuwrit;

#define  Kxcrv2           F77_NAME(kxcrv2,KXCRV2)
                 extern SUBROUTINE Kxcrv2;

#define  Macdef           F77_NAME(macdef,MACDEF)
                 extern SUBROUTINE Macdef;

#define  Mdmenu           F77_NAME(mdmenu,MDMENU)
#define  Mhi_close        F77_NAME(mhi_close,MHI_CLOSE)
                 extern SUBROUTINE Mhi_close;
#define  Mhi_open         F77_NAME(mhi_open,MHI_OPEN)
                 extern SUBROUTINE Mhi_open;

#define  Mzwipe           F77_NAME(mzwipe,MZWIPE)
                 extern SUBROUTINE Mzwipe;

#define  Traceq           F77_NAME(traceq,TRACEQ)
                 extern SUBROUTINE Traceq;

#define  Xuflow           F77_NAME(xuflow,XUFLOW)
                 extern SUBROUTINE Xuflow;


#ifdef   IBM370

#  pragma linkage(ERRRUN,FORTRAN)

#  pragma linkage(FMEMCPY,FORTRAN)

#  pragma linkage(GOPARM,FORTRAN)

#  pragma linkage(ICLRWK,FORTRAN)
#  pragma linkage(IGINIT,FORTRAN)
#  pragma linkage(IGMENU,FORTRAN)
#  pragma linkage(IGRNG,FORTRAN)
#  pragma linkage(IGSSE,FORTRAN)
#  pragma linkage(IGSRAP,FORTRAN)
#  pragma linkage(IGWKTY,FORTRAN)

#  pragma linkage(KCEXEC,FORTRAN)

#  pragma linkage(KDIALO,FORTRAN)

#  pragma linkage(KGETAR,FORTRAN)

#  pragma linkage(KIBRES,FORTRAN)
#  pragma linkage(KICLOS,FORTRAN)
#  pragma linkage(KICOMV,FORTRAN)
#  pragma linkage(KIERRF,FORTRAN)
#  pragma linkage(KIINIT,FORTRAN)
#  pragma linkage(KILUN,FORTRAN)
#  pragma linkage(KIPAWC,FORTRAN)
#  pragma linkage(KIPIAF,FORTRAN)
#  pragma linkage(KIPRMT,FORTRAN)
#  pragma linkage(KIRTIM,FORTRAN)
#  pragma linkage(KISIGM,FORTRAN)
#  pragma linkage(KIVECT,FORTRAN)

#  pragma linkage(KMBRSET,FORTRAN)
#  pragma linkage(KMVSED,FORTRAN)
#  pragma linkage(KMVSPG,FORTRAN)
#  pragma linkage(KMVSSH,FORTRAN)

#  pragma linkage(KSVPAR,FORTRAN)

#  pragma linkage(KUACH,FORTRAN)
#  pragma linkage(KUACT,FORTRAN)
#  pragma linkage(KUALFA,FORTRAN)
#  pragma linkage(KUAPPL,FORTRAN)
#  pragma linkage(KUARGS,FORTRAN)
#  pragma linkage(KUBREK,FORTRAN)
#  pragma linkage(KUBROF,FORTRAN)
#  pragma linkage(KUBRON,FORTRAN)
#  pragma linkage(KUCLOS,FORTRAN)
#  pragma linkage(KUCMD,FORTRAN)
#  pragma linkage(KUCMDL,FORTRAN)
#  pragma linkage(KUCOMV,FORTRAN)
#  pragma linkage(KUEDIT,FORTRAN)
#  pragma linkage(KUESVR,FORTRAN)
#  pragma linkage(KUEUSR,FORTRAN)
#  pragma linkage(KUEXEC,FORTRAN)
#  pragma linkage(KUEXEL,FORTRAN)
#  pragma linkage(KUEXIT,FORTRAN)
#  pragma linkage(KUFCAS,FORTRAN)
#  pragma linkage(KUFDEF,FORTRAN)
#  pragma linkage(KUGETC,FORTRAN)
#  pragma linkage(KUGETE,FORTRAN)
#  pragma linkage(KUGETF,FORTRAN)
#  pragma linkage(KUGETI,FORTRAN)
#  pragma linkage(KUGETL,FORTRAN)
#  pragma linkage(KUGETR,FORTRAN)
#  pragma linkage(KUGETS,FORTRAN)
#  pragma linkage(KUGRFL,FORTRAN)
#  pragma linkage(KUGUID,FORTRAN)
#  pragma linkage(KUHELP,FORTRAN)
#  pragma linkage(KUHOME,FORTRAN)
#  pragma linkage(KUIDF1,FORTRAN)
#  pragma linkage(KUIDF2,FORTRAN)
#  pragma linkage(KUIDFM,FORTRAN)
#  pragma linkage(KUINIM,FORTRAN)
#  pragma linkage(KUINIT,FORTRAN)
#  pragma linkage(KUINPS,FORTRAN)
#  pragma linkage(KUINQF,FORTRAN)
#  pragma linkage(KULUN,FORTRAN)
#  pragma linkage(KUMLOC,FORTRAN)
#  pragma linkage(KUMOUT,FORTRAN)
#  pragma linkage(KUNDPV,FORTRAN)
#  pragma linkage(KUNPAR,FORTRAN)
#  pragma linkage(KUNWG,FORTRAN)
#  pragma linkage(KUOPEN,FORTRAN)
#  pragma linkage(KUPAD,FORTRAN)
#  pragma linkage(KUPAR,FORTRAN)
#  pragma linkage(KUPATH,FORTRAN)
#  pragma linkage(KUPATL,FORTRAN)
#  pragma linkage(KUPROC,FORTRAN)
#  pragma linkage(KUPROF,FORTRAN)
#  pragma linkage(KUPROI,FORTRAN)
#  pragma linkage(KUPROP,FORTRAN)
#  pragma linkage(KUPROR,FORTRAN)
#  pragma linkage(KUPROS,FORTRAN)
#  pragma linkage(KUPVAL,FORTRAN)
#  pragma linkage(KUQCAS,FORTRAN)
#  pragma linkage(KUQENV,FORTRAN)
#  pragma linkage(KUQEXE,FORTRAN)
#  pragma linkage(KUQKEY,FORTRAN)
#  pragma linkage(KUQSVR,FORTRAN)
#  pragma linkage(KUQUIT,FORTRAN)
#  pragma linkage(KUQVAR,FORTRAN)
#  pragma linkage(KUREAD,FORTRAN)
#  pragma linkage(KUSAPP,FORTRAN)
#  pragma linkage(KUSIBR,FORTRAN)
#  pragma linkage(KUSIGM,FORTRAN)
#  pragma linkage(KUSPY,FORTRAN)
#  pragma linkage(KUSTAT,FORTRAN)
#  pragma linkage(KUSTOP,FORTRAN)
#  pragma linkage(KUTERM,FORTRAN)
#  pragma linkage(KUTIME,FORTRAN)
#  pragma linkage(KUTIM0,FORTRAN)
#  pragma linkage(KUTRUE,FORTRAN)
#  pragma linkage(KUSER,FORTRAN)
#  pragma linkage(KUVAR,FORTRAN)
#  pragma linkage(KUVCRE,FORTRAN)
#  pragma linkage(KUVDEL,FORTRAN)
#  pragma linkage(KUVECT,FORTRAN)
#  pragma linkage(KUVNAM,FORTRAN)
#  pragma linkage(KUWHAG,FORTRAN)
#  pragma linkage(KUWHAM,FORTRAN)
#  pragma linkage(KUWHAT,FORTRAN)
#  pragma linkage(KUWRIT,FORTRAN)
#  pragma linkage(KXDSIG,FORTRAN)

#  pragma linkage(KXCRV2,FORTRAN)

#  pragma linkage(MACDEF,FORTRAN)

#  pragma linkage(MDMENU,FORTRAN)
#  pragma linkage(MHI_CLOSE,FORTRAN)
#  pragma linkage(MHI_OPEN,FORTRAN)

#  pragma linkage(MZWIPE,FORTRAN)

#  pragma linkage(TRACEQ,FORTRAN)

#  pragma linkage(XUFLOW,FORTRAN)

#endif


#define  MAXCMD   512           /* max length of a command line        */
#define  MAXEDT   32            /* max length of names in edit server  */
#define  MAXLEV   10            /* max levels of command name path     */
#define  MAXSVR   20            /* max number of edit server processes */


/*
 * The PAWC common is referenced through a pointer to allow the use of
 * dynamic common blocks on IBM systems.
 */

#define Pawc kc_pawc

EXTERN struct COMMON_PAWC {
  INTEGER NWPAR;
  INTEGER IXPAWC;
  INTEGER IHBOOK;
  INTEGER IXHIGZ;
  INTEGER IXKUIP;
  INTEGER IFENCE[5];
  INTEGER LQ[8];
  INTEGER DATA[999];
} *Pawc;

#define IQ(n)           Pawc->DATA[n-1]
#define  Q(n) (((REAL*)(Pawc->DATA))[n-1])

