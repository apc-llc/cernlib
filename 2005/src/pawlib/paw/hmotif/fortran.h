/*
 * $Id: fortran.h,v 1.1.1.1 1996/03/01 11:39:12 mclareni Exp $
 *
 * $Log: fortran.h,v $
 * Revision 1.1.1.1  1996/03/01 11:39:12  mclareni
 * Paw
 *
 */
/*CMZ :  2.04/15 13/03/94  13.25.11  by  Fons Rademakers*/
/*-- Author :    Fons Rademakers   23/11/92*/

/* Fortran routines and common blocks called from C and/or vice versa */

#ifdef vms

#include <descrip.h>

#ifdef __ALPHA
#pragma extern_model common_block
#endif

extern void getang(float*, float*);

#define Gethplopt(s1) \
        do { extern void GETHPLOPT(struct dsc$descriptor_s*); \
             struct dsc$descriptor_s s1_dsc; \
             s1_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             s1_dsc.dsc$b_class = DSC$K_CLASS_S; \
             s1_dsc.dsc$a_pointer = s1; \
             s1_dsc.dsc$w_length = strlen(s1); \
             GETHPLOPT(&s1_dsc); } while(0)

#define Gettitle(list,obj,lobj,title,ltitle) \
        do { extern void GETTITLE(int*, struct dsc$descriptor_s*, \
                                  struct dsc$descriptor_s*); \
             struct dsc$descriptor_s obj_dsc, tit_dsc; \
             obj_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             obj_dsc.dsc$b_class = DSC$K_CLASS_S; \
             obj_dsc.dsc$a_pointer = obj; \
             obj_dsc.dsc$w_length = lobj; \
             tit_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             tit_dsc.dsc$b_class = DSC$K_CLASS_S; \
             tit_dsc.dsc$a_pointer = title; \
             tit_dsc.dsc$w_length = ltitle; \
             GETTITLE(list,&obj_dsc,&tit_dsc); } while(0)

extern void gettp(float*, float*);

#define Hplglb(i1,i2,s3,l3,i4) \
        do { extern void HPLGLB(int*, int*, struct dsc$descriptor_s*, int*); \
             struct dsc$descriptor_s s3_dsc; \
             s3_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             s3_dsc.dsc$b_class = DSC$K_CLASS_S; \
             s3_dsc.dsc$a_pointer = s3; \
             s3_dsc.dsc$w_length = l3; \
             HPLGLB(i1,i2,&s3_dsc,i4); } while(0)

extern void hplgzo(int*, int*);

#define Hplset(s1,r2) \
        do { extern void HPLSET(struct dsc$descriptor_s*, float*); \
             struct dsc$descriptor_s s1_dsc; \
             s1_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             s1_dsc.dsc$b_class = DSC$K_CLASS_S; \
             s1_dsc.dsc$a_pointer = s1; \
             s1_dsc.dsc$w_length = strlen(s1); \
             HPLSET(&s1_dsc,r2); } while(0)

#define Igq(s1,r2) \
        do { extern void IGQ(struct dsc$descriptor_s*, float*); \
             struct dsc$descriptor_s s1_dsc; \
             s1_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             s1_dsc.dsc$b_class = DSC$K_CLASS_S; \
             s1_dsc.dsc$a_pointer = s1; \
             s1_dsc.dsc$w_length = strlen(s1); \
             IGQ(&s1_dsc,r2); } while(0)

#define Mhlattr(path,lpath,obj,titl,ltitl,nx,xmin,xmax,ny,ymin,ymax,isw,ntup) \
        do { extern void MHLATTR(struct dsc$descriptor_s*, int*, \
                                 struct dsc$descriptor_s*, \
                                 int*, float*, float*, \
                                 int*, float*, float*, int*, int*); \
             struct dsc$descriptor_s path_dsc, titl_dsc; \
             path_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             path_dsc.dsc$b_class = DSC$K_CLASS_S; \
             path_dsc.dsc$a_pointer = path; \
             path_dsc.dsc$w_length = lpath; \
             titl_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             titl_dsc.dsc$b_class = DSC$K_CLASS_S; \
             titl_dsc.dsc$a_pointer = titl; \
             titl_dsc.dsc$w_length = ltitl; \
             MHLATTR(&path_dsc,obj,&titl_dsc,\
                     nx,xmin,xmax,ny,ymin,ymax,isw,ntup); \
           } while(0)

#define Plot_histo(chid,lchid,chopt,lchopt,x1,x2,y1,y2) \
        do { extern void PLOT_HISTO(struct dsc$descriptor_s*, \
                                    struct dsc$descriptor_s*, \
                                    int*, int*, int*, int*); \
             struct dsc$descriptor_s id_dsc, opt_dsc; \
             id_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             id_dsc.dsc$b_class = DSC$K_CLASS_S; \
             id_dsc.dsc$a_pointer = chid; \
             id_dsc.dsc$w_length = lchid; \
             opt_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             opt_dsc.dsc$b_class = DSC$K_CLASS_S; \
             opt_dsc.dsc$a_pointer = chopt; \
             opt_dsc.dsc$w_length = lchopt; \
             PLOT_HISTO(&id_dsc,&opt_dsc,x1,x2,y1,y2); } while(0)

#define Rebin(id,ifirst,ilast,ng,chdir,lchdir,chopt,lchopt) \
        do { extern void REBIN(int*, int*, int*, int*, \
                               struct dsc$descriptor_s*, \
                               struct dsc$descriptor_s*); \
             struct dsc$descriptor_s dir_dsc, opt_dsc; \
             dir_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             dir_dsc.dsc$b_class = DSC$K_CLASS_S; \
             dir_dsc.dsc$a_pointer = chdir; \
             dir_dsc.dsc$w_length = lchdir; \
             opt_dsc.dsc$b_dtype = DSC$K_DTYPE_T; \
             opt_dsc.dsc$b_class = DSC$K_CLASS_S; \
             opt_dsc.dsc$a_pointer = chopt; \
             opt_dsc.dsc$w_length = lchopt; \
             REBIN(id,ifirst,ilast,ng,&dir_dsc,&opt_dsc); } while(0)

#else

#define getang               getang_
#define hplgzo               hplgzo_
#define gettp                gettp_
#define list_add_ntuple_var  list_add_ntuple_var_
#define matrix_label         matrix_label_
#define matrix_row           matrix_row_
#define ntwork_init          ntwork_init_
#define ntwork_check         ntwork_check_
#define fset_histo           fset_histo_

#ifdef NeXT
#define LEN_OTHER ,0
#define INT_OTHER ,int
#endif
#ifndef LEN_OTHER
#define LEN_OTHER
#endif
#ifndef INT_OTHER
#define INT_OTHER
#endif

#define Gethplopt(s1) \
        gethplopt_(s1,strlen(s1))

#define Gettitle(list,obj,lobj,title,ltitle) \
        gettitle_(list,obj,title LEN_OTHER ,lobj,ltitle)

#define Hplglb(i1,i2,s3,l3,i4) \
        hplglb_(i1,i2,s3,i4 LEN_OTHER LEN_OTHER ,l3);

#define Hplset(s1,r2) \
        hplset_(s1,r2,strlen(s1))

#define Igq(s1,r2) \
        igq_(s1,r2,strlen(s1))

#define Mhlattr(path,lpath,obj,titl,ltitl,nx,xmin,xmax,ny,ymin,ymax,isw,ntup) \
        mhlattr_(path,obj,titl,nx,xmin,xmax,ny,ymin,ymax,isw,ntup, \
                 lpath LEN_OTHER ,ltitl)

#define Plot_histo(id,lid,opt,lopt,x1,x2,y1,y2) \
        plot_histo_(id,opt,x1,x2,y1,y2,lid,lopt)

extern void plot_histo_(char*, char*, int*, int*, int*, int*, int, int);

#define Rebin(id,ifirst,ilast,ng,chdir,lchdir,chopt,lchopt) \
        rebin_(id,ifirst,ilast,ng,chdir,chopt \
               LEN_OTHER LEN_OTHER LEN_OTHER LEN_OTHER,lchdir,lchopt)

extern void rebin_(int*, int*, int*, int*, char*, char*
                   INT_OTHER INT_OTHER INT_OTHER INT_OTHER,int, int);


/* Fortran common blocks */
#if !defined(NeXT) && !defined(__convexc__)
#define pawlun               pawlun_
#endif
#if defined(__convexc__)
#define pawlun              _pawlun_
#endif /* __convex__ */

#endif

